
!wrf:MODEL_LAYER:DYNAMICS
!

#if (RWORDSIZE == 4)
# define VPOWX vspowx
# define VPOW  vspow
#else
# define VPOWX vpowx
# define VPOW  vpow
#endif


MODULE module_big_step_utilities_em

   USE module_model_constants
   USE module_state_description, only: p_qg, p_qs, p_qi, gdscheme, tiedtkescheme, ntiedtkescheme, kfetascheme, mskfscheme, &
       g3scheme, gfscheme,p_qv, param_first_scalar, p_qr, p_qc, DFI_FWD
   USE module_configure, ONLY : grid_config_rec_type
   USE module_wrf_error

CONTAINS

!-------------------------------------------------------------------------------

SUBROUTINE calc_mu_uv ( config_flags,                 &
                        mu, mub, muu, muv,            &
                        ids, ide, jds, jde, kds, kde, &
                        ims, ime, jms, jme, kms, kme, &
                        its, ite, jts, jte, kts, kte )

   IMPLICIT NONE
   
   ! Input data

   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte

   REAL, DIMENSION( ims:ime , jms:jme ) , INTENT(  OUT) :: muu, muv
   REAL, DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mu, mub

   !  local stuff

   INTEGER :: i, j, itf, jtf, im, jm

!<DESCRIPTION>
!
!  calc_mu_uv calculates the full column dry-air mass at the staggered
!  horizontal velocity points (u,v) and places the results in muu and muv.
!  This routine uses the reference state (mub) and perturbation state (mu)
!
!</DESCRIPTION>


      itf=ite
      jtf=MIN(jte,jde-1)

      IF      ( ( its .NE. ids ) .AND. ( ite .NE. ide ) ) THEN
         DO j=jts,jtf
         DO i=its,itf
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j)+MUB(i,j)+MUB(i-1,j))
         ENDDO
         ENDDO
      ELSE IF ( ( its .EQ. ids ) .AND. ( ite .NE. ide ) ) THEN
         DO j=jts,jtf
         DO i=its+1,itf
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j)+MUB(i,j)+MUB(i-1,j))
         ENDDO
         ENDDO
         i=its
         im = its
         if(config_flags%periodic_x) im = its-1
         DO j=jts,jtf
!            MUU(i,j) =      MU(i,j)          +MUB(i,j)
!  fix for periodic b.c., 13 march 2004, wcs
            MUU(i,j) = 0.5*(MU(i,j)+MU(im,j)+MUB(i,j)+MUB(im,j))
         ENDDO
      ELSE IF ( ( its .NE. ids ) .AND. ( ite .EQ. ide ) ) THEN
         DO j=jts,jtf
         DO i=its,itf-1
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j)+MUB(i,j)+MUB(i-1,j))
         ENDDO
         ENDDO
         i=ite
         im = ite-1
         if(config_flags%periodic_x) im = ite
         DO j=jts,jtf
!            MUU(i,j) =      MU(i-1,j)        +MUB(i-1,j)
!  fix for periodic b.c., 13 march 2004, wcs
            MUU(i,j) = 0.5*(MU(i-1,j)+MU(im,j)+MUB(i-1,j)+MUB(im,j))
         ENDDO
      ELSE IF ( ( its .EQ. ids ) .AND. ( ite .EQ. ide ) ) THEN
         DO j=jts,jtf
         DO i=its+1,itf-1
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j)+MUB(i,j)+MUB(i-1,j))
         ENDDO
         ENDDO
         i=its
         im = its
         if(config_flags%periodic_x) im = its-1
         DO j=jts,jtf
!            MUU(i,j) =      MU(i,j)          +MUB(i,j)
!  fix for periodic b.c., 13 march 2004, wcs
            MUU(i,j) = 0.5*(MU(i,j)+MU(im,j)+MUB(i,j)+MUB(im,j))
         ENDDO
         i=ite
         im = ite-1
         if(config_flags%periodic_x) im = ite
         DO j=jts,jtf
!            MUU(i,j) =      MU(i-1,j)        +MUB(i-1,j)
!  fix for periodic b.c., 13 march 2004, wcs
            MUU(i,j) = 0.5*(MU(i-1,j)+MU(im,j)+MUB(i-1,j)+MUB(im,j))
         ENDDO
      END IF

      itf=MIN(ite,ide-1)
      jtf=jte

      IF      ( ( jts .NE. jds ) .AND. ( jte .NE. jde ) ) THEN
         DO j=jts,jtf
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1)+MUB(i,j)+MUB(i,j-1))
         ENDDO
         ENDDO
      ELSE IF ( ( jts .EQ. jds ) .AND. ( jte .NE. jde ) ) THEN
         DO j=jts+1,jtf
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1)+MUB(i,j)+MUB(i,j-1))
         ENDDO
         ENDDO
         j=jts
         jm = jts
         if(config_flags%periodic_y) jm = jts-1
         DO i=its,itf
!             MUV(i,j) =      MU(i,j)          +MUB(i,j)
!  fix for periodic b.c., 13 march 2004, wcs
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,jm)+MUB(i,j)+MUB(i,jm))
         ENDDO
      ELSE IF ( ( jts .NE. jds ) .AND. ( jte .EQ. jde ) ) THEN
         DO j=jts,jtf-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1)+MUB(i,j)+MUB(i,j-1))
         ENDDO
         ENDDO
         j=jte
         jm = jte-1
         if(config_flags%periodic_y) jm = jte
         DO i=its,itf
!            MUV(i,j) =      MU(i,j-1)        +MUB(i,j-1)
!  fix for periodic b.c., 13 march 2004, wcs
             MUV(i,j) = 0.5*(MU(i,j-1)+MU(i,jm)+MUB(i,j-1)+MUB(i,jm))
         ENDDO
      ELSE IF ( ( jts .EQ. jds ) .AND. ( jte .EQ. jde ) ) THEN
         DO j=jts+1,jtf-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1)+MUB(i,j)+MUB(i,j-1))
         ENDDO
         ENDDO
         j=jts
         jm = jts
         if(config_flags%periodic_y) jm = jts-1
         DO i=its,itf
!             MUV(i,j) =      MU(i,j)          +MUB(i,j)
!  fix for periodic b.c., 13 march 2004, wcs
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,jm)+MUB(i,j)+MUB(i,jm))
         ENDDO
         j=jte
         jm = jte-1
         if(config_flags%periodic_y) jm = jte
         DO i=its,itf
!             MUV(i,j) =      MU(i,j-1)        +MUB(i,j-1)
!  fix for periodic b.c., 13 march 2004, wcs
             MUV(i,j) = 0.5*(MU(i,j-1)+MU(i,jm)+MUB(i,j-1)+MUB(i,jm))
         ENDDO
      END IF

END SUBROUTINE calc_mu_uv

!-------------------------------------------------------------------------------

SUBROUTINE calc_mu_uv_1 ( config_flags,                 &
                          mu, muu, muv,                 &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte )

   IMPLICIT NONE
   
   ! Input data

   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte

   REAL, DIMENSION( ims:ime , jms:jme ) , INTENT(  OUT) :: muu, muv
   REAL, DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mu

   !  local stuff

   INTEGER :: i, j, itf, jtf, im, jm

!<DESCRIPTION>
!
!  calc_mu_uv calculates the full column dry-air mass at the staggered
!  horizontal velocity points (u,v) and places the results in muu and muv.
!  This routine uses the full state (mu)
!
!</DESCRIPTION>
   
      itf=ite
      jtf=MIN(jte,jde-1)

      IF      ( ( its .NE. ids ) .AND. ( ite .NE. ide ) ) THEN
         DO j=jts,jtf
         DO i=its,itf
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j))
         ENDDO
         ENDDO
      ELSE IF ( ( its .EQ. ids ) .AND. ( ite .NE. ide ) ) THEN
         DO j=jts,jtf
         DO i=its+1,itf
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j))
         ENDDO
         ENDDO
         i=its
         im = its
         if(config_flags%periodic_x) im = its-1
         DO j=jts,jtf
            MUU(i,j) = 0.5*(MU(i,j)+MU(im,j))
         ENDDO
      ELSE IF ( ( its .NE. ids ) .AND. ( ite .EQ. ide ) ) THEN
         DO j=jts,jtf
         DO i=its,itf-1
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j))
         ENDDO
         ENDDO
         i=ite
         im = ite-1
         if(config_flags%periodic_x) im = ite
         DO j=jts,jtf
            MUU(i,j) = 0.5*(MU(i-1,j)+MU(im,j))
         ENDDO
      ELSE IF ( ( its .EQ. ids ) .AND. ( ite .EQ. ide ) ) THEN
         DO j=jts,jtf
         DO i=its+1,itf-1
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j))
         ENDDO
         ENDDO
         i=its
         im = its
         if(config_flags%periodic_x) im = its-1
         DO j=jts,jtf
            MUU(i,j) = 0.5*(MU(i,j)+MU(im,j))
         ENDDO
         i=ite
         im = ite-1
         if(config_flags%periodic_x) im = ite
         DO j=jts,jtf
            MUU(i,j) = 0.5*(MU(i-1,j)+MU(im,j))
         ENDDO
      END IF

      itf=MIN(ite,ide-1)
      jtf=jte

      IF      ( ( jts .NE. jds ) .AND. ( jte .NE. jde ) ) THEN
         DO j=jts,jtf
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1))
         ENDDO
         ENDDO
      ELSE IF ( ( jts .EQ. jds ) .AND. ( jte .NE. jde ) ) THEN
         DO j=jts+1,jtf
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1))
         ENDDO
         ENDDO
         j=jts
         jm = jts
         if(config_flags%periodic_y) jm = jts-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,jm))
         ENDDO
      ELSE IF ( ( jts .NE. jds ) .AND. ( jte .EQ. jde ) ) THEN
         DO j=jts,jtf-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1))
         ENDDO
         ENDDO
         j=jte
         jm = jte-1
         if(config_flags%periodic_y) jm = jte
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j-1)+MU(i,jm))
         ENDDO
      ELSE IF ( ( jts .EQ. jds ) .AND. ( jte .EQ. jde ) ) THEN
         DO j=jts+1,jtf-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1))
         ENDDO
         ENDDO
         j=jts
         jm = jts
         if(config_flags%periodic_y) jm = jts-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,jm))
         ENDDO
         j=jte
         jm = jte-1
         if(config_flags%periodic_y) jm = jte
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j-1)+MU(i,jm))
         ENDDO
      END IF

END SUBROUTINE calc_mu_uv_1

!-------------------------------------------------------------------------------

! Map scale factor comments for this routine:
! Locally not changed, but sent the correct map scale factors
! from module_em (msfuy, msfvx, msfty)

SUBROUTINE couple_momentum ( muu, ru, u, msfu,              &
                             muv, rv, v, msfv, msfv_inv,    &
                             mut, rw, w, msft,              &
                             c1h, c2h, c1f, c2f,            &
                             ids, ide, jds, jde, kds, kde,  &
                             ims, ime, jms, jme, kms, kme,  &
                             its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE

   ! Input data

   INTEGER ,             INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                          ims, ime, jms, jme, kms, kme, &
                                          its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(  OUT) :: ru, rv, rw

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: muu, muv, mut
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: msfu, msfv, msft, msfv_inv
   
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: u, v, w
   REAL , DIMENSION( kms:kme ) ,    INTENT(IN   ) :: c1h, c2h, c1f, c2f
   
   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   
!<DESCRIPTION>
!
! couple_momentum couples the velocities to the full column mass and
! the map factors.
!
!</DESCRIPTION>

   ktf=MIN(kte,kde-1)
   
      itf=ite
      jtf=MIN(jte,jde-1)

      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         ru(i,k,j)=u(i,k,j)*(c1h(k)*muu(i,j)+c2h(k))/msfu(i,j)
      ENDDO
      ENDDO
      ENDDO

      itf=MIN(ite,ide-1)
      jtf=jte

      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
           rv(i,k,j)=v(i,k,j)*(c1h(k)*muv(i,j)+c2h(k))*msfv_inv(i,j)
      ENDDO
      ENDDO
      ENDDO

      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)

      DO j=jts,jtf
      DO k=kts,kte
      DO i=its,itf
         rw(i,k,j)=w(i,k,j)*(c1f(k)*mut(i,j)+c2f(k))/msft(i,j)
      ENDDO
      ENDDO
      ENDDO

END SUBROUTINE couple_momentum

!-------------------------------------------------------------------

SUBROUTINE calc_mu_staggered ( mu, mub, muu, muv,            &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  its, ite, jts, jte, kts, kte )

   IMPLICIT NONE
   
   ! Input data

   INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte

   REAL, DIMENSION( ims:ime , jms:jme ) , INTENT(  OUT) :: muu, muv
   REAL, DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mu, mub

   !  local stuff

   INTEGER :: i, j, itf, jtf

!<DESCRIPTION>
!
! calc_mu_staggered calculates the full dry air mass at the staggered
! velocity points (u,v).
!
!</DESCRIPTION>
   
      itf=ite
      jtf=MIN(jte,jde-1)

      IF      ( ( its .NE. ids ) .AND. ( ite .NE. ide ) ) THEN
         DO j=jts,jtf
         DO i=its,itf
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j)+MUB(i,j)+MUB(i-1,j))
         ENDDO
         ENDDO
      ELSE IF ( ( its .EQ. ids ) .AND. ( ite .NE. ide ) ) THEN
         DO j=jts,jtf
         DO i=its+1,itf
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j)+MUB(i,j)+MUB(i-1,j))
         ENDDO
         ENDDO
         i=its
         DO j=jts,jtf
            MUU(i,j) =      MU(i,j)          +MUB(i,j)
         ENDDO
      ELSE IF ( ( its .NE. ids ) .AND. ( ite .EQ. ide ) ) THEN
         DO j=jts,jtf
         DO i=its,itf-1
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j)+MUB(i,j)+MUB(i-1,j))
         ENDDO
         ENDDO
         i=ite
         DO j=jts,jtf
            MUU(i,j) =      MU(i-1,j)        +MUB(i-1,j)
         ENDDO
      ELSE IF ( ( its .EQ. ids ) .AND. ( ite .EQ. ide ) ) THEN
         DO j=jts,jtf
         DO i=its+1,itf-1
            MUU(i,j) = 0.5*(MU(i,j)+MU(i-1,j)+MUB(i,j)+MUB(i-1,j))
         ENDDO
         ENDDO
         i=its
         DO j=jts,jtf
            MUU(i,j) =      MU(i,j)          +MUB(i,j)
         ENDDO
         i=ite
         DO j=jts,jtf
            MUU(i,j) =      MU(i-1,j)        +MUB(i-1,j)
         ENDDO
      END IF

      itf=MIN(ite,ide-1)
      jtf=jte

      IF      ( ( jts .NE. jds ) .AND. ( jte .NE. jde ) ) THEN
         DO j=jts,jtf
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1)+MUB(i,j)+MUB(i,j-1))
         ENDDO
         ENDDO
      ELSE IF ( ( jts .EQ. jds ) .AND. ( jte .NE. jde ) ) THEN
         DO j=jts+1,jtf
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1)+MUB(i,j)+MUB(i,j-1))
         ENDDO
         ENDDO
         j=jts
         DO i=its,itf
             MUV(i,j) =      MU(i,j)          +MUB(i,j)
         ENDDO
      ELSE IF ( ( jts .NE. jds ) .AND. ( jte .EQ. jde ) ) THEN
         DO j=jts,jtf-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1)+MUB(i,j)+MUB(i,j-1))
         ENDDO
         ENDDO
         j=jte
         DO i=its,itf
             MUV(i,j) =      MU(i,j-1)        +MUB(i,j-1)
         ENDDO
      ELSE IF ( ( jts .EQ. jds ) .AND. ( jte .EQ. jde ) ) THEN
         DO j=jts+1,jtf-1
         DO i=its,itf
             MUV(i,j) = 0.5*(MU(i,j)+MU(i,j-1)+MUB(i,j)+MUB(i,j-1))
         ENDDO
         ENDDO
         j=jts
         DO i=its,itf
             MUV(i,j) =      MU(i,j)          +MUB(i,j)
         ENDDO
         j=jte
         DO i=its,itf
             MUV(i,j) =      MU(i,j-1)        +MUB(i,j-1)
         ENDDO
      END IF

END SUBROUTINE calc_mu_staggered

!-------------------------------------------------------------------------------

SUBROUTINE couple ( mu, mub, rfield, field, name, &
                    msf, c1h, c2h, c1, c2,        &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

   IMPLICIT NONE

   ! Input data

   INTEGER ,             INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                          ims, ime, jms, jme, kms, kme, &
                                          its, ite, jts, jte, kts, kte

   CHARACTER(LEN=1) ,     INTENT(IN   ) :: name

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(  OUT) :: rfield

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mu, mub, msf
   
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: field

   REAL , DIMENSION( kms:kme ) ,    INTENT(IN   ) :: c1h, c2h, c1, c2
   
   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   REAL , DIMENSION(ims:ime,jms:jme) :: muu , muv

!<DESCRIPTION>
!
! subroutine couple couples the input variable with the dry-air
! column mass (mu).  
!
!</DESCRIPTION>

   
   ktf=MIN(kte,kde-1)
   
   IF (name .EQ. 'u')THEN

      CALL calc_mu_staggered ( mu, mub, muu, muv,            &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  its, ite, jts, jte, kts, kte )

      itf=ite
      jtf=MIN(jte,jde-1)

      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         rfield(i,k,j)=field(i,k,j)*(c1h(k)*muu(i,j)+c2h(k))/msf(i,j)
      ENDDO
      ENDDO
      ENDDO

   ELSE IF (name .EQ. 'v')THEN

      CALL calc_mu_staggered ( mu, mub, muu, muv,            &
                               ids, ide, jds, jde, kds, kde, &
                               ims, ime, jms, jme, kms, kme, &
                               its, ite, jts, jte, kts, kte )

      itf=ite
      itf=MIN(ite,ide-1)
      jtf=jte

      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
           rfield(i,k,j)=field(i,k,j)*(c1h(k)*muv(i,j)+c2h(k))/msf(i,j)
      ENDDO
      ENDDO
      ENDDO

   ELSE IF (name .EQ. 'w')THEN
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      DO j=jts,jtf
      DO k=kts,kte
      DO i=its,itf
         rfield(i,k,j)=field(i,k,j)*((c1(k)*mu(i,j))+(c1(k)*mub(i,j)+c2(k)))/msf(i,j)
      ENDDO
      ENDDO
      ENDDO

   ELSE IF (name .EQ. 'h')THEN
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      DO j=jts,jtf
      DO k=kts,kte
      DO i=its,itf
         rfield(i,k,j)=field(i,k,j)*((c1(k)*mu(i,j))+(c1(k)*mub(i,j)+c2(k)))
      ENDDO
      ENDDO
      ENDDO

   ELSE
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         rfield(i,k,j)=field(i,k,j)*((c1(k)*mu(i,j))+(c1(k)*mub(i,j)+c2(k)))
      ENDDO
      ENDDO
      ENDDO
   
   ENDIF

END SUBROUTINE couple


!-------------------------------------------------------------------------------

SUBROUTINE calc_ww_cp ( u, v, mup, mub, c1h, c2h, ww,    &
                        rdx, rdy, msftx, msfty,          &
                        msfux, msfuy, msfvx, msfvx_inv,  &
                        msfvy, dnw,                      &
                        ids, ide, jds, jde, kds, kde,    &
                        ims, ime, jms, jme, kms, kme,    &
                        its, ite, jts, jte, kts, kte    )

   IMPLICIT NONE

   ! Input data


   INTEGER ,    INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: u, v
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mup, mub, &
                                                            msftx, msfty, &
                                                            msfux, msfuy, &
                                                            msfvx, msfvy, &
                                                            msfvx_inv
   REAL , DIMENSION( kms:kme ) , INTENT(IN   ) :: dnw
   REAL , DIMENSION( kms:kme ) , INTENT(IN   ) :: c1h, c2h
   
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT  ) :: ww
   REAL , INTENT(IN   )  :: rdx, rdy
   
   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   REAL , DIMENSION( its:ite ) :: dmdt
   REAL , DIMENSION( its:ite, kts:kte ) :: divv
   REAL , DIMENSION( its:ite+1, jts:jte+1 ) :: muu, muv

!<DESCRIPTION>
!
!  calc_ww calculates omega using the velocities (u,v) and the dry-air
!  column mass (mup+mub).
!  The algorithm integrates the continuity equation through the column
!  followed by a diagnosis of omega.
!
!</DESCRIPTION>

!<DESCRIPTION>
!
!  calc_ww_cp calculates omega using the velocities (u,v) and the
!  column mass mu.
!
!</DESCRIPTION>

    jtf=MIN(jte,jde-1)
    ktf=MIN(kte,kde-1)  
    itf=MIN(ite,ide-1)

!  mu coupled with the appropriate map factor

      DO j=jts,jtf
      DO i=its,min(ite+1,ide)
        MUU(i,j) = 0.5*(MUP(i,j)+MUB(i,j)+MUP(i-1,j)+MUB(i-1,j))
      ENDDO
      ENDDO

      DO j=jts,min(jte+1,jde)
      DO i=its,itf
        MUV(i,j) = 0.5*(MUP(i,j)+MUB(i,j)+MUP(i,j-1)+MUB(i,j-1))
      ENDDO
      ENDDO

      DO j=jts,jtf

        DO i=its,ite
          dmdt(i) = 0.
          ww(i,1,j) = 0.
          ww(i,kte,j) = 0.
        ENDDO

!       Comments on the modifications for map scale factors
!       ADT eqn 47 / my (putting rho -> 'mu') is:
!       (1/my) partial d mu/dt = -mx partial d/dx(mu u/my)
!                                -mx partial d/dy(mu v/mx)
!                                -partial d/dz(mu w/my)
!
!       Using nu instead of z the last term becomes:
!                                -partial d/dnu((c1(k)*mu(dnu/dt))/my)
!
!       Integrating with respect to nu over ALL levels, with dnu/dt=0 at top
!       and bottom, the last term becomes = 0
!
!       Integral|bot->top[(1/my) partial d mu/dt]dnu =
!       Integral|bot->top[-mx partial d/dx(mu u/my)
!                         -mx partial d/dy(mu v/mx)]dnu
!
!       muu='mu'[on u]/my, muv='mu'[on v]/mx
!       (1/my) partial d mu/dt is independent of nu
!         => LHS = Integral|bot->top[con]dnu = conservation*(-1) = -dmdt
!
!         => dmdt = mx*Integral|bot->top[partial d/dx(mu u/my) +
!                                        partial d/dy(mu v/mx)]dnu
!         => dmdt = sum_bot->top[divv]
!       where
!         divv=mx*[partial d/dx(mu u/my) + partial d/dy(mu v/mx)]*delta nu

        DO k=kts,ktf
        DO i=its,itf

          divv(i,k) = msftx(i,j)*dnw(k)*( rdx*((c1h(k)*muu(i+1,j)+c2h(k))*u(i+1,k,j)/msfuy(i+1,j)-(c1h(k)*muu(i,j)+c2h(k))*u(i,k,j)/msfuy(i,j))  &
                                        +rdy*((c1h(k)*muv(i,j+1)+c2h(k))*v(i,k,j+1)*msfvx_inv(i,j+1)-(c1h(k)*muv(i,j)+c2h(k))*v(i,k,j)*msfvx_inv(i,j))   )

!          dmdt(i) = dmdt(i) + dnw(k)* ( rdx*(ru(i+1,k,j)-ru(i,k,j))  &
!                                       +rdy*(rv(i,k,j+1)-rv(i,k,j))   )

          dmdt(i) = dmdt(i) + divv(i,k)


        ENDDO
        ENDDO

!       Further map scale factor notes:
!       Now integrate from bottom to top, level by level:
!       mu dnu/dt/my [k+1] = mu dnu/dt/my [k] + [-(1/my) partial d mu/dt
!                           -mx partial d/dx(mu u/my)
!                           -mx partial d/dy(mu v/mx)]*dnu[k->k+1]
!       ww [k+1] = ww [k] -(1/my) partial d mu/dt * dnu[k->k+1] - divv[k]
!                = ww [k] -dmdt * dnw[k] - divv[k]

        DO k=2,ktf
        DO i=its,itf

!           ww(i,k,j)=ww(i,k-1,j)                                       &
!                        - dnw(k-1)* ( dmdt(i)                          &
!                                     +rdx*(ru(i+1,k-1,j)-ru(i,k-1,j))  &
!                                     +rdy*(rv(i,k-1,j+1)-rv(i,k-1,j)) )

           ww(i,k,j)=ww(i,k-1,j) - dnw(k-1)*c1h(k-1)*dmdt(i) - divv(i,k-1)

        ENDDO
        ENDDO
     ENDDO


END SUBROUTINE calc_ww_cp


!-------------------------------------------------------------------------------

SUBROUTINE calc_cq ( moist, cqu, cqv, cqw, n_moist, &
                     ids, ide, jds, jde, kds, kde,  &
                     ims, ime, jms, jme, kms, kme,  &
                     its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
   ! Input data

   INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte

   INTEGER ,          INTENT(IN   ) :: n_moist
   

   REAL, DIMENSION( ims:ime, kms:kme , jms:jme , n_moist ), INTENT(IN   ) :: moist
                                              
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(  OUT) :: cqu, cqv, cqw

   ! Local stuff

   ! Changes from Larry Meadows, Intel Corp.  Improve vectorization of this routine
   REAL :: qtot(its:ite)
   
   INTEGER :: i, j, k, itf, jtf, ktf, ispe

!<DESCRIPTION>
!
!  calc_cq calculates moist coefficients for the momentum equations.
!
!</DESCRIPTION>

      ktf=MIN(kte,kde-1)

      IF(  n_moist >= PARAM_FIRST_SCALAR ) THEN

        itf=ite
        jtf=MIN(jte,jde-1)
        DO j=jts,jtf
          DO k=kts,ktf
            qtot = 0.
            DO ispe=PARAM_FIRST_SCALAR,n_moist
              DO i=its,itf
                qtot(i) = qtot(i) + moist(i,k,j,ispe) + moist(i-1,k,j,ispe)
              ENDDO
            ENDDO
            DO i=its,itf
              cqu(i,k,j) = 1./(1.+0.5*qtot(i))
            ENDDO
        ENDDO
        ENDDO

        itf=MIN(ite,ide-1)
        jtf=jte
        DO j=jts,jtf
          DO k=kts,ktf
            qtot = 0.
            DO ispe=PARAM_FIRST_SCALAR,n_moist
              DO i=its,itf
                qtot(i) = qtot(i) + moist(i,k,j,ispe) + moist(i,k,j-1,ispe)
              ENDDO
            ENDDO
            DO i = its,itf
               cqv(i,k,j) = 1./(1.+0.5*qtot(i))
            ENDDO
          ENDDO
        ENDDO

        itf=MIN(ite,ide-1)
        jtf=MIN(jte,jde-1)
        DO j=jts,jtf
          DO k=kts+1,ktf
            qtot = 0.
            DO ispe=PARAM_FIRST_SCALAR,n_moist
              DO i=its,itf
                qtot(i) = qtot(i) + moist(i,k,j,ispe) + moist(i,k-1,j,ispe)
              ENDDO
            ENDDO
            DO i = its,itf
              cqw(i,k,j) = 0.5*qtot(i)
            ENDDO
        ENDDO
        ENDDO

      ELSE

        itf=ite
        jtf=MIN(jte,jde-1)
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           cqu(i,k,j) = 1.
        ENDDO
        ENDDO
        ENDDO

        itf=MIN(ite,ide-1)
        jtf=jte
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           cqv(i,k,j) = 1.
        ENDDO
        ENDDO
        ENDDO

        itf=MIN(ite,ide-1)
        jtf=MIN(jte,jde-1)
        DO j=jts,jtf
        DO k=kts+1,ktf
        DO i=its,itf
           cqw(i,k,j) = 0.
        ENDDO
        ENDDO
        ENDDO

      END IF

END SUBROUTINE calc_cq

!----------------------------------------------------------------------

SUBROUTINE calc_alt ( alt, al, alb,                  &
                      ids, ide, jds, jde, kds, kde,  &
                      ims, ime, jms, jme, kms, kme,  &
                      its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
   ! Input data

   INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: alb, al
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(  OUT) :: alt

   ! Local stuff

   INTEGER :: i, j, k, itf, jtf, ktf

!<DESCRIPTION>
!
! calc_alt computes the full inverse density
!
!</DESCRIPTION>

      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      ktf=MIN(kte,kde-1)

      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
        alt(i,k,j) = al(i,k,j)+alb(i,k,j)
      ENDDO
      ENDDO
      ENDDO


END SUBROUTINE calc_alt

!----------------------------------------------------------------------

SUBROUTINE calc_p_rho_phi ( moist, n_moist, hypsometric_opt,      &
                            al, alb, mu, muts,                    &
                            c1, c2, c3h, c4h, c3f, c4f,           &
                            ph, phb, p, pb,                       &
                            t, p0, t0, ptop, znu, znw, dnw, rdnw, &
                            rdn, non_hydrostatic, use_theta_m,    &
                            ids, ide, jds, jde, kds, kde,         &
                            ims, ime, jms, jme, kms, kme,         &
                            its, ite, jts, jte, kts, kte          )

  IMPLICIT NONE
   
   ! Input data

  LOGICAL ,          INTENT(IN   ) :: non_hydrostatic
  INTEGER ,          INTENT(IN   ) :: use_theta_m

  INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte

  INTEGER ,          INTENT(IN   ) :: n_moist
  INTEGER ,          INTENT(IN   ) :: hypsometric_opt

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: alb,  &
                                                                   pb,   &
                                                                   t

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme, n_moist ), INTENT(IN   ) :: moist

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(  OUT) :: al, p

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: ph, phb

  REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN   ) :: mu, muts

  REAL, DIMENSION( kms:kme ), INTENT(IN   ) :: znu, znw, dnw, rdnw, rdn

  REAL, DIMENSION( kms:kme ), INTENT(IN   ) :: c1, c2, c3h, c4h, c3f, c4f

  REAL,   INTENT(IN   ) :: t0, p0, ptop

  ! Local stuff

  INTEGER :: i, j, k, kk, itf, jtf, ktf, ispe
  REAL    :: qtot, qf1, qf2, qvf
  REAL, DIMENSION( its:ite) :: temp,cpovcv_v
  REAL    :: pfu, phm, pfd

!<DESCRIPTION>
!
! For the nonhydrostatic option, calc_p_rho_phi calculates the
! diagnostic quantities pressure and (inverse) density from the
! prognostic variables using the equation of state.
!
! For the hydrostatic option, calc_p_rho_phi calculates the
! diagnostic quantities (inverse) density and geopotential from the
! prognostic variables using the equation of state and the hydrostatic
! equation.
!
!</DESCRIPTION>

  itf=MIN(ite,ide-1)
  jtf=MIN(jte,jde-1)
  ktf=MIN(kte,kde-1)

#ifndef INTELMKL
  cpovcv_v = cpovcv
#endif

  nonhydro : IF (non_hydrostatic) THEN

      hypso_nonhydro : IF (hypsometric_opt == 1) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
          al(i,k,j)=-1./(c1(k)*muts(i,j)+c2(k))*(alb(i,k,j)*(c1(k)*mu(i,j)) + rdnw(k)*(ph(i,k+1,j)-ph(i,k,j)))
        END DO
        END DO
        END DO
      ELSE IF (hypsometric_opt == 2) THEN hypso_nonhydro

        ! The relation used to get specific volume, al, is: al = -dZ/dp,
        ! where dp = mut * d(eta). The pressure depth, dp, is replaced with
        ! p*(dp/p) ~ p*LOG((p+0.5dp)/(p-0.5dp)). Difference between dp and p*dLOG(p)
        ! is as follows: p*dLOG(p) - dp = 1/12*(dp/p)**3 + 1/90*(dp/p)**5 + ...
        ! Therefore, p*dLOG(p) is always larger than dp and the difference is
        ! in proportion to dp/p. TKW, 02/16/2010

        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
          pfu = c3f(k+1)*MUTS(i,j) + c4f(k+1) + ptop
          pfd = c3f(k  )*MUTS(i,j) + c4f(k  ) + ptop
          phm = c3h(k  )*MUTS(i,j) + c4h(k  ) + ptop
          al(i,k,j) = (ph(i,k+1,j)-ph(i,k,j)+phb(i,k+1,j)-phb(i,k,j))/phm/LOG(pfd/pfu)-alb(i,k,j)
        END DO
        END DO
        END DO
      ELSE hypso_nonhydro
        CALL wrf_error_fatal ( 'calc_p_rho_phi: hypsometric_opt should be 1 or 2' )
      END IF hypso_nonhydro

      moist_nonhydro : IF (n_moist >= PARAM_FIRST_SCALAR ) THEN  

        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
        IF ( use_theta_m .EQ. 1 ) THEN
          temp(i)=(r_d*(t0+t(i,k,j)))/(p0*(al(i,k,j)+alb(i,k,j)))
        ELSE
          qvf = 1.+rvovrd*moist(i,k,j,P_QV)
          temp(i)=(r_d*(t0+t(i,k,j))*qvf)/(p0*(al(i,k,j)+alb(i,k,j)))
        ENDIF
        ENDDO
#ifdef INTELMKL
        CALL VPOWX ( itf-its+1, temp(its), cpovcv, p(its,k,j) )
#else
! use vector version from libmassv or from compat lib in frame/libmassv.F
        CALL VPOW  ( p(its,k,j), temp(its), cpovcv_v(its), itf-its+1 )
#endif
        DO i=its,itf
           p(i,k,j)= p(i,k,j)*p0-pb(i,k,j)
        ENDDO
        ENDDO
        ENDDO

      ELSE moist_nonhydro           

        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
          p(i,k,j)=p0*( (r_d*(t0+t(i,k,j)))/                     &
                        (p0*(al(i,k,j)+alb(i,k,j))) )**cpovcv  &
                           -pb(i,k,j)
        ENDDO
        ENDDO
        ENDDO

      END IF moist_nonhydro

   ELSE nonhydro

!  hydrostatic pressure, al, and ph1 calc; WCS, 5 sept 2001


      moist_hydro : IF (n_moist >= PARAM_FIRST_SCALAR ) THEN  

        DO j=jts,jtf

          k=ktf          ! top layer
          DO i=its,itf

            qtot = 0.
            DO ispe=PARAM_FIRST_SCALAR,n_moist
              qtot = qtot + moist(i,k,j,ispe)
            ENDDO
            qf2 = 1.
            qf1 = qtot*qf2

            p(i,k,j) = - 0.5*((c1(k)*mu(i,j))+qf1*(c1(k)*muts(i,j)+c2(k)))/rdnw(k)/qf2
            IF ( use_theta_m .EQ. 1 ) THEN
            al(i,k,j) = (r_d/p1000mb)*(t(i,k,j)+t0)* &
                (((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm) - alb(i,k,j)
            ELSE
            qvf = 1.+rvovrd*moist(i,k,j,P_QV)
            al(i,k,j) = (r_d/p1000mb)*(t(i,k,j)+t0)*qvf* &
                (((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm) - alb(i,k,j)
            END IF

          ENDDO

          DO k=ktf-1,kts,-1  ! remaining layers, integrate down
            DO i=its,itf

            qtot = 0.
            DO ispe=PARAM_FIRST_SCALAR,n_moist
              qtot = qtot + 0.5*(  moist(i,k  ,j,ispe) + moist(i,k+1,j,ispe) )
            ENDDO
            qf2 = 1.
            qf1 = qtot*qf2

            p(i,k,j) = p(i,k+1,j) - ((c1(k)*mu(i,j)) + qf1*(c1(k)*muts(i,j)+c2(k)))/qf2/rdn(k+1)
            IF ( use_theta_m .EQ. 1 ) THEN
            al(i,k,j) = (r_d/p1000mb)*(t(i,k,j)+t0)* &
                        (((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm) - alb(i,k,j)
            ELSE
            qvf = 1.+rvovrd*moist(i,k,j,P_QV)
            al(i,k,j) = (r_d/p1000mb)*(t(i,k,j)+t0)*qvf* &
                        (((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm) - alb(i,k,j)
            END IF
            ENDDO
          ENDDO

        ENDDO

      ELSE moist_hydro

        DO j=jts,jtf

          k=ktf          ! top layer
          DO i=its,itf

            qtot = 0.
            qf2 = 1.
            qf1 = qtot*qf2

            p(i,k,j) = - 0.5*((c1(k)*mu(i,j))+qf1*(c1(k)*muts(i,j)+c2(k)))/rdnw(k)/qf2
            al(i,k,j) = (r_d/p1000mb)*(t(i,k,j)+t0)* &
                (((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm) - alb(i,k,j)

          ENDDO

          DO k=ktf-1,kts,-1  ! remaining layers, integrate down
            DO i=its,itf

            qtot = 0.
            qf2 = 1.
            qf1 = qtot*qf2

            p(i,k,j) = p(i,k+1,j) - ((c1(k)*mu(i,j)) + qf1*(c1(k)*muts(i,j)+c2(k)))/qf2/rdn(k+1)
            al(i,k,j) = (r_d/p1000mb)*(t(i,k,j)+t0)* &
                        (((p(i,k,j)+pb(i,k,j))/p1000mb)**cvpm) - alb(i,k,j)
            ENDDO
          ENDDO

        ENDDO

     END IF moist_hydro

     hypso_hydro : IF (hypsometric_opt == 1) THEN
        DO j=jts,jtf
          DO kk=2,ktf+1  ! integrate hydrostatic equation for geopotential
            k = kk-1
            DO i=its,itf
              ph(i,k+1,j) = ph(i,k,j) - (dnw(k))*(           &
                           ((c1(k)*muts(i,j)+c2(k)))*al(i,k,j)+            &
                            (c1(k)*mu(i,j))*alb(i,k,j)  )
            ENDDO
          ENDDO
        ENDDO
     ELSE hypso_hydro

     ! Revised hypsometric eq.: dZ=-al*p*dLOG(p), where p is dry pressure

      DO j=jts,jtf
        DO i=its,itf
           ph(i,kts,j) = phb(i,kts,j)
        END DO

        DO k=kts+1,ktf+1
          DO i=its,itf
            pfu = c3f(k  )*MUTS(i,j) + c4f(k  ) + ptop
            pfd = c3f(k-1)*MUTS(i,j) + c4f(k-1) + ptop
            phm = c3h(k-1)*MUTS(i,j) + c4h(k-1) + ptop
            ph(i,k,j) = ph(i,k-1,j) + (al(i,k-1,j)+alb(i,k-1,j))*phm*LOG(pfd/pfu)
          ENDDO
        ENDDO

        DO k=kts,ktf+1
          DO i=its,itf
             ph(i,k,j) = ph(i,k,j) - phb(i,k,j)
          END DO
        END DO
      END DO

     END IF hypso_hydro

   END IF nonhydro

END SUBROUTINE calc_p_rho_phi

!----------------------------------------------------------------------

SUBROUTINE calc_php ( php, ph, phb,                  &
                      ids, ide, jds, jde, kds, kde,  &
                      ims, ime, jms, jme, kms, kme,  &
                      its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
   ! Input data

   INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte

   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(IN   ) :: phb, ph
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(  OUT) :: php

   ! Local stuff

   INTEGER :: i, j, k, itf, jtf, ktf

!<DESCRIPTION>
!
!  calc_php calculates the full geopotential from the reference state
!  geopotential and the perturbation geopotential (phb_ph).
!
!</DESCRIPTION>

      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      ktf=MIN(kte,kde-1)

      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
        php(i,k,j) = 0.5*(phb(i,k,j)+phb(i,k+1,j)+ph(i,k,j)+ph(i,k+1,j))
      ENDDO
      ENDDO
      ENDDO

END SUBROUTINE calc_php

!-------------------------------------------------------------------------------

SUBROUTINE diagnose_w( ph_tend, ph_new, ph_old, w, mut,     &
                       c1f, c2f, dt,                        &
                       u, v, ht,                            &
                       cf1, cf2, cf3, rdx, rdy,             &
                       msftx, msfty,                        &
                       ids, ide, jds, jde, kds, kde,        &
                       ims, ime, jms, jme, kms, kme,        &
                       its, ite, jts, jte, kts, kte        )

   IMPLICIT NONE

   INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte

   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(IN   ) ::   ph_tend, &
                                                                     ph_new,  &
                                                                     ph_old,  &
                                                                     u,       &
                                                                     v


   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(  OUT) :: w

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) :: mut, ht, msftx, msfty

   REAL, DIMENSION( kms:kme ), INTENT(IN   ) :: c1f, c2f

   REAL, INTENT(IN   ) :: dt, cf1, cf2, cf3, rdx, rdy

   INTEGER :: i, j, k, itf, jtf

   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)

!<DESCRIPTION>
!
! diagnose_w diagnoses the vertical velocity from the geopoential equation.
! Used with the hydrostatic option.
!
!</DESCRIPTION>

   DO j = jts, jtf

!  lower b.c. on w

!  Notes on map scale factors:
!  Chain rule: if Z=Z(X,Y) [true at the surface] then
!  dZ/dt = dZ/dX * dX/dt + dZ/dY * dY/dt, U=dX/dt, V=dY/dt
!  Using capitals to denote actual values
!  In mapped values, u=U, v=V, z=Z, 1/dX=mx/dx, 1/dY=my/dy
!    => w = dz/dt = mx u dz/dx + my v dz/dy
!  [where dz/dx is just the surface height change between x
!   gridpoints, and dz/dy is the change between y gridpoints]
!  [NB: cf1, cf2 and cf3 do vertical weighting of u or v values
!   nearest the surface]

!  Previously msft multiplied by rdy and rdx terms.
!  Now msfty multiplies rdy term, and msftx multiplies msftx term
     DO i = its, itf
         w(i,1,j)=  msfty(i,j)*.5*rdy*(                      &
                           (ht(i,j+1)-ht(i,j  ))             &
          *(cf1*v(i,1,j+1)+cf2*v(i,2,j+1)+cf3*v(i,3,j+1))    &
                          +(ht(i,j  )-ht(i,j-1))             &
          *(cf1*v(i,1,j  )+cf2*v(i,2,j  )+cf3*v(i,3,j  ))  ) &
                 +msftx(i,j)*.5*rdx*(                        &
                           (ht(i+1,j)-ht(i,j  ))             &
          *(cf1*u(i+1,1,j)+cf2*u(i+1,2,j)+cf3*u(i+1,3,j))    &
                          +(ht(i,j  )-ht(i-1,j))             &
          *(cf1*u(i  ,1,j)+cf2*u(i  ,2,j)+cf3*u(i  ,3,j))  )
     ENDDO

!  use geopotential equation to diagnose w

!  Further notes on map scale factors
!  If ph_tend contains:  -mx partial d/dx(mu rho u/my)
!                        -mx partial d/dy(phi mu v/mx)
!                        -partial d/dz(phi mu w/my)
!  then phi eqn is: partial d/dt(mu phi/my) = ph_tend + mu g w/my
!    => w = [my/(mu*g)]*[partial d/dt(mu phi/my) - ph_tend]

     DO k = 2, kte
     DO i = its, itf
       w(i,k,j) =  msfty(i,j)*(  (ph_new(i,k,j)-ph_old(i,k,j))/dt       &
                               - ph_tend(i,k,j)/(c1f(k)*mut(i,j)+c2f(k))        )/g

     ENDDO
     ENDDO

   ENDDO

END SUBROUTINE diagnose_w

!-------------------------------------------------------------------------------

SUBROUTINE rhs_ph( ph_tend, u, v, ww,               &
                   ph, ph_old, phb, w,              &
                   mut, muuf, muvf,                 &
                   c1f, c2f,                        &
                   fnm, fnp,                        &
                   rdnw, cfn, cfn1, rdx, rdy,       &
                   msfux, msfuy, msfvx,             &
                   msfvx_inv, msfvy,                &
                   msftx, msfty,                    &
                   non_hydrostatic,                 &
                   config_flags,                    &
                   ids, ide, jds, jde, kds, kde,    &
                   ims, ime, jms, jme, kms, kme,    &
                   its, ite, jts, jte, kts, kte    )
   IMPLICIT NONE

   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte

   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(IN   ) ::        &
                                                                     u,   &
                                                                     v,   &
                                                                     ww,  &
                                                                     ph,  &
                                                                     ph_old, &
                                                                     phb, &
                                                                    w

   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(INOUT) :: ph_tend

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) :: muuf, muvf, mut,   &
                                                            msfux, msfuy, &
                                                            msfvx, msfvy, &
                                                            msftx, msfty, &
                                                            msfvx_inv

   REAL, DIMENSION( kms:kme ), INTENT(IN   ) :: rdnw, fnm, fnp

   REAL, DIMENSION( kms:kme ), INTENT(IN   ) :: c1f, c2f
   

   REAL,  INTENT(IN   ) :: cfn, cfn1, rdx, rdy

   LOGICAL,  INTENT(IN   )  ::  non_hydrostatic

   ! Local stuff

   INTEGER :: i, j, k, itf, jtf, ktf, kz, i_start, j_start
   REAL    :: ur, ul, ub, vr, vl, vb
   REAL, DIMENSION(its:ite,kts:kte) :: wdwn

   INTEGER :: advective_order

   LOGICAL :: specified

!<DESCRIPTION>
!
! rhs_ph calculates the large-timestep tendency terms for the geopotential
! equation.  These terms include the advection and "gw".  The geopotential
! equation is cast in advective form, so we don't use the flux form advection
! algorithms here.
!
!</DESCRIPTION>

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

   advective_order = config_flags%h_sca_adv_order

   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)
   ktf=MIN(kte,kde-1)

!  Notes on map scale factors (WCS, 2 march 2008)
!  phi equation is:   mu/my d/dt(phi) = -(1/my) mx mu u  d/dx(phi)
!                                       -(1/my) my mu v d/dy(phi)
!                                       - omega d/d_eta(phi)
!                                               +mu g w/my
!
!  A little further explanation...
!  The tendency term we are computing here is for mu/my d/dt(phi).  It is advective form
!  but it is multiplied be mu/my.  It will be decoupled from (mu/my) when the implicit w-phi
!  solution is computed in subourine advance_w.  The formulation dates from the early
!  days of the mass coordinate model when we were testing both a flux and an advective formulation
!  for the geopotential equation and different forms of the vertical momentum equation and the
!  vertically implicit solver.

! advective form for the geopotential equation

   DO j = jts, jtf

     DO k = 2, kte
     DO i = its, itf
          wdwn(i,k) = .5*(ww(i,k,j)+ww(i,k-1,j))*rdnw(k-1)               &
                        *(ph(i,k,j)-ph(i,k-1,j)+phb(i,k,j)-phb(i,k-1,j))
     ENDDO
     ENDDO

!  RHS term 3 is: - omega partial d/dnu(phi)

     DO k = 2, kte-1
     DO i = its, itf
           ph_tend(i,k,j) = ph_tend(i,k,j)                           &
                             - (fnm(k)*wdwn(i,k+1)+fnp(k)*wdwn(i,k))
     ENDDO
     ENDDO

   ENDDO

   IF (non_hydrostatic) THEN  ! add in "gw" term.
   DO j = jts, jtf            ! in hydrostatic mode, "gw" will be diagnosed
                              ! after the timestep to give us "w"
     DO i = its, itf
        ph_tend(i,kde,j) = 0.
     ENDDO

     DO k = 2, kte
     DO i = its, itf
        ! phi equation RHS term 4
        ph_tend(i,k,j) = ph_tend(i,k,j) + (c1f(k)*mut(i,j)+c2f(k))*g*w(i,k,j)/msfty(i,j)
     ENDDO
     ENDDO

   ENDDO

   END IF

!  Notes on map scale factors:
!  RHS terms 1 and 2 are: -(1/my) mx u mu partial d/dx(phi)
!                         -(1/my) my v mu partial d/dy(phi)

   IF (advective_order <= 2) THEN

!  y (v) advection

   i_start = its
   j_start = jts
   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)

   IF ( (config_flags%open_ys .or. specified) .and. jts == jds ) j_start = jts+1
   IF ( (config_flags%open_ye .or. specified) .and. jte == jde ) jtf = jtf-2

   DO j = j_start, jtf

     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))*          &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1)*   &
                  (phb(i,k,j+1)-phb(i,k,j  )+ph(i,k,j+1)-ph(i,k,j  ))   &
                  +(c1f(k)*muvf(i,j  )+c2f(k))*(v(i,k,j  )+v(i,k-1,j  ))*msfvy(i,j  )*   &
                  (phb(i,k,j  )-phb(i,k,j-1)+ph(i,k,j  )-ph(i,k,j-1)) )
     ENDDO
     ENDDO

     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))*                        &
                  ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1)*    &
                   (phb(i,k,j+1)-phb(i,k,j  )+ph(i,k,j+1)-ph(i,k,j  ))               &
                   +(c1f(k)*muvf(i,j  )+c2f(k))*(cfn*v(i,k-1,j  )+cfn1*v(i,k-2,j  ))*msfvy(i,j  )*    &
                   (phb(i,k,j  )-phb(i,k,j-1)+ph(i,k,j  )-ph(i,k,j-1))              )
     ENDDO

   ENDDO

!  x (u) advection

   i_start = its
   j_start = jts
   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)

   IF ( (config_flags%open_xs .or. specified) .and. its == ids ) i_start = its+1
   IF ( (config_flags%open_xe .or. specified) .and. ite == ide ) itf = itf-2

   DO j = j_start, jtf

     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))*         &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j)*  &
                  (phb(i+1,k,j)-phb(i  ,k,j)+ph(i+1,k,j)-ph(i  ,k,j))  &
                  +(c1f(k)*muuf(i  ,j)+c2f(k))*(u(i  ,k,j)+u(i  ,k-1,j))*msfux(i  ,j)*  &
                  (phb(i  ,k,j)-phb(i-1,k,j)+ph(i  ,k,j)-ph(i-1,k,j))  )
     ENDDO
     ENDDO

     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))*                        &
                  ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j)*    &
                   (phb(i+1,k,j)-phb(i  ,k,j)+ph(i+1,k,j)-ph(i  ,k,j))               &
                   +(c1f(k)*muuf(i  ,j)+c2f(k))*(cfn*u(i  ,k-1,j)+cfn1*u(i  ,k-2,j))*msfux(  i,j)*    &
                   (phb(i  ,k,j)-phb(i-1,k,j)+ph(i  ,k,j)-ph(i-1,k,j))             )
     ENDDO

   ENDDO

   ELSE IF (advective_order <= 4) THEN

!  y (v) advection

   i_start = its
   j_start = jts
   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)

   IF ( (config_flags%open_ys .or. specified) .and. jts == jds ) j_start = jts+2
   IF ( (config_flags%open_ye .or. specified) .and. jte == jde ) jtf = jtf-3

   DO j = j_start, jtf

     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))*(                     &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1)                &
                  +(c1f(k)*muvf(i,j  )+c2f(k))*(v(i,k,j  )+v(i,k-1,j  ))*msfvy(i,j  ))* (1./12.)*(   &
                    8.*(ph(i,k,j+1)-ph(i,k,j-1))                                    &
                      -(ph(i,k,j+2)-ph(i,k,j-2))                                    &
                   +8.*(phb(i,k,j+1)-phb(i,k,j-1))                                  &
                      -(phb(i,k,j+2)-phb(i,k,j-2))  )   )                


     ENDDO
     ENDDO

     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))*(                                 &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1)                &
                  +(c1f(k)*muvf(i,j  )+c2f(k))*(cfn*v(i,k-1,j  )+cfn1*v(i,k-2,j  ))*msfvy(i,j  ))* (1./12.)*(   &
                    8.*(ph(i,k,j+1)-ph(i,k,j-1))                                               &
                      -(ph(i,k,j+2)-ph(i,k,j-2))                                               &
                   +8.*(phb(i,k,j+1)-phb(i,k,j-1))                                             &
                      -(phb(i,k,j+2)-phb(i,k,j-2))  )   )                

     ENDDO

   ENDDO

!  pick up near boundary rows using 2nd order stencil for open and specified conditions

   IF ( (config_flags%open_ys .or. specified) .and. jts <= jds+1 )  THEN

     j = jds+1
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))*          &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1)*   &
                  (phb(i,k,j+1)-phb(i,k,j  )+ph(i,k,j+1)-ph(i,k,j  ))   &
                  +(c1f(k)*muvf(i,j  )+c2f(k))*(v(i,k,j  )+v(i,k-1,j  ))*msfvy(i,j  )*   &
                  (phb(i,k,j  )-phb(i,k,j-1)+ph(i,k,j  )-ph(i,k,j-1)) )
     ENDDO
     ENDDO

     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))*                        &
                  ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1)*    &
                   (phb(i,k,j+1)-phb(i,k,j  )+ph(i,k,j+1)-ph(i,k,j  ))               &
                   +(c1f(k)*muvf(i,j  )+c2f(k))*(cfn*v(i,k-1,j  )+cfn1*v(i,k-2,j  ))*msfvy(i,j  )*    &
                   (phb(i,k,j  )-phb(i,k,j-1)+ph(i,k,j  )-ph(i,k,j-1))              )
     ENDDO

   END IF

   IF ( (config_flags%open_ye .or. specified) .and. jte >= jde-2 )  THEN

     j = jde-2
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))*          &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1)*   &
                  (phb(i,k,j+1)-phb(i,k,j  )+ph(i,k,j+1)-ph(i,k,j  ))   &
                  +(c1f(k)*muvf(i,j  )+c2f(k))*(v(i,k,j  )+v(i,k-1,j  ))*msfvy(i,j  )*   &
                  (phb(i,k,j  )-phb(i,k,j-1)+ph(i,k,j  )-ph(i,k,j-1)) )
     ENDDO
     ENDDO

     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))*                        &
                  ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1)*    &
                   (phb(i,k,j+1)-phb(i,k,j  )+ph(i,k,j+1)-ph(i,k,j  ))               &
                   +(c1f(k)*muvf(i,j  )+c2f(k))*(cfn*v(i,k-1,j  )+cfn1*v(i,k-2,j  ))*msfvy(i,j  )*    &
                   (phb(i,k,j  )-phb(i,k,j-1)+ph(i,k,j  )-ph(i,k,j-1))              )
     ENDDO

   END IF

!  x (u) advection

   i_start = its
   j_start = jts
   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)

   IF ( (config_flags%open_xs) .and. its == ids ) i_start = its+2
   IF ( (config_flags%open_xe) .and. ite == ide ) itf = itf-3

   DO j = j_start, jtf

     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))*(                    &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j)               &
                  +(c1f(k)*muuf(i,j  )+c2f(k))*(u(i  ,k,j)+u(i  ,k-1,j))*msfux(i  ,j) )* (1./12.)*( &
                    8.*(ph(i+1,k,j)-ph(i-1,k,j))                                   &
                      -(ph(i+2,k,j)-ph(i-2,k,j))                                   &
                   +8.*(phb(i+1,k,j)-phb(i-1,k,j))                                 &
                      -(phb(i+2,k,j)-phb(i-2,k,j))  )   )                
     ENDDO
     ENDDO

     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))*(                                 &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j)                &
                  +(c1f(k)*muuf(i,j  )+c2f(k))*(cfn*u(i  ,k-1,j)+cfn1*u(i  ,k-2,j))*msfux(i  ,j) )* (1./12.)*(  &
                    8.*(ph(i+1,k,j)-ph(i-1,k,j))                                               &
                      -(ph(i+2,k,j)-ph(i-2,k,j))                                               &
                   +8.*(phb(i+1,k,j)-phb(i-1,k,j))                                             &
                      -(phb(i+2,k,j)-phb(i-2,k,j))  )     )
     ENDDO

   ENDDO

!  pick up near boundary rows using 2nd order stencil for open and specified conditions

   IF ( (config_flags%open_xs .or. specified) .and. its <= ids+1 ) THEN

     i = ids + 1

     DO j = j_start, jtf
     DO k = 2, kte-1
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))*         &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j)*  &
                  (phb(i+1,k,j)-phb(i  ,k,j)+ph(i+1,k,j)-ph(i  ,k,j))  &
                  +(c1f(k)*muuf(i  ,j)+c2f(k))*(u(i  ,k,j)+u(i  ,k-1,j))*msfux(i  ,j)*  &
                  (phb(i  ,k,j)-phb(i-1,k,j)+ph(i  ,k,j)-ph(i-1,k,j))  )
     ENDDO
     ENDDO

     k = kte
     DO j = j_start, jtf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))*                        &
                  ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j)*    &
                   (phb(i+1,k,j)-phb(i  ,k,j)+ph(i+1,k,j)-ph(i  ,k,j))               &
                   +(c1f(k)*muuf(i  ,j)+c2f(k))*(cfn*u(i  ,k-1,j)+cfn1*u(i  ,k-2,j))*msfux(  i,j)*    &
                   (phb(i  ,k,j)-phb(i-1,k,j)+ph(i  ,k,j)-ph(i-1,k,j))             )
     ENDDO

   END IF

   IF ( (config_flags%open_xe .or. specified) .and. ite >= ide-2 ) THEN

     i = ide-2
     DO j = j_start, jtf
     DO k = 2, kte-1
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))*         &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j)*  &
                  (phb(i+1,k,j)-phb(i  ,k,j)+ph(i+1,k,j)-ph(i  ,k,j))  &
                  +(c1f(k)*muuf(i  ,j)+c2f(k))*(u(i  ,k,j)+u(i  ,k-1,j))*msfux(i  ,j)*  &
                  (phb(i  ,k,j)-phb(i-1,k,j)+ph(i  ,k,j)-ph(i-1,k,j))  )
     ENDDO
     ENDDO

     k = kte
     DO j = j_start, jtf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))*                        &
                  ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j)*    &
                   (phb(i+1,k,j)-phb(i  ,k,j)+ph(i+1,k,j)-ph(i  ,k,j))               &
                   +(c1f(k)*muuf(i  ,j)+c2f(k))*(cfn*u(i  ,k-1,j)+cfn1*u(i  ,k-2,j))*msfux(  i,j)*    &
                   (phb(i  ,k,j)-phb(i-1,k,j)+ph(i  ,k,j)-ph(i-1,k,j))             )
     ENDDO

   END IF

!--------------------------

   ELSE IF (advective_order <= 6) THEN

!!! NOTE: this branch has been looked at and fixed with changes for overdecomposition
!!!       the branches covering the other advective_order cases have not.  20090923. JM

!  y (v) advection

   i_start = its
   j_start = jts
   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)

   IF (config_flags%open_ys .or. specified ) j_start = max(jts,jds+3)
   IF (config_flags%open_ye .or. specified ) jtf     = min(jtf,jde-4)

   DO j = j_start, jtf

     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))* (                    &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1)                &
                  +(c1f(k)*muvf(i,j  )+c2f(k))*(v(i,k,j  )+v(i,k-1,j  ))*msfvy(i,j  ) )* (1./60.)*(  &
                   45.*(ph(i,k,j+1)-ph(i,k,j-1))                                    &
                   -9.*(ph(i,k,j+2)-ph(i,k,j-2))                                    &
                      +(ph(i,k,j+3)-ph(i,k,j-3))                                    &
                  +45.*(phb(i,k,j+1)-phb(i,k,j-1))                                  &
                   -9.*(phb(i,k,j+2)-phb(i,k,j-2))                                  &
                      +(phb(i,k,j+3)-phb(i,k,j-3))  )   )                


     ENDDO
     ENDDO

     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))* (                                &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1)                &
                  +(c1f(k)*muvf(i,j  )+c2f(k))*(cfn*v(i,k-1,j  )+cfn1*v(i,k-2,j  ))*msfvy(i,j  ) )* (1./60.)*(  &
                   45.*(ph(i,k,j+1)-ph(i,k,j-1))                                               &
                   -9.*(ph(i,k,j+2)-ph(i,k,j-2))                                               &
                      +(ph(i,k,j+3)-ph(i,k,j-3))                                               &
                  +45.*(phb(i,k,j+1)-phb(i,k,j-1))                                             &
                   -9.*(phb(i,k,j+2)-phb(i,k,j-2))                                             &
                      +(phb(i,k,j+3)-phb(i,k,j-3))  )   )                

     ENDDO

   ENDDO

!  4th order stencil for open or specified conditions two in form the boundary

   IF ( (config_flags%open_ys .or. specified) .and. jts <= jds+2 .and. jds+2 <= jte )  THEN

     j = jds+2
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))* (                     &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1)                &
                  +(c1f(k)*muvf(i,j  )+c2f(k))*(v(i,k,j  )+v(i,k-1,j  ))*msfvy(i,j  ) )* (1./12.)*(  &
                    8.*(ph(i,k,j+1)-ph(i,k,j-1))                                    &
                      -(ph(i,k,j+2)-ph(i,k,j-2))                                    &
                   +8.*(phb(i,k,j+1)-phb(i,k,j-1))                                  &
                      -(phb(i,k,j+2)-phb(i,k,j-2))  )   )                


     ENDDO
     ENDDO

     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))* (                              &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1)              &
                  +(c1f(k)*muvf(i,j  )+c2f(k))*(cfn*v(i,k-1,j  )+cfn1*v(i,k-2,j  ))*msfvy(i,j) )* (1./12.)*(  &
                    8.*(ph(i,k,j+1)-ph(i,k,j-1))                                             &
                      -(ph(i,k,j+2)-ph(i,k,j-2))                                             &
                   +8.*(phb(i,k,j+1)-phb(i,k,j-1))                                           &
                      -(phb(i,k,j+2)-phb(i,k,j-2))  )   )                

     ENDDO

   END IF

   IF ( (config_flags%open_ye .or. specified) .and. jts <= jde-3 .and. jde-3 <= jte )  THEN
     j = jde-3
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))* (                  &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1)              &
                  +(c1f(k)*muvf(i,j  )+c2f(k))*(v(i,k,j  )+v(i,k-1,j  ))*msfvy(i,j) )* (1./12.)*(  &
                    8.*(ph(i,k,j+1)-ph(i,k,j-1))                                  &
                      -(ph(i,k,j+2)-ph(i,k,j-2))                                  &
                   +8.*(phb(i,k,j+1)-phb(i,k,j-1))                                &
                      -(phb(i,k,j+2)-phb(i,k,j-2))  )   )                


     ENDDO
     ENDDO

     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))* (                              &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1)              &
                  +(c1f(k)*muvf(i,j  )+c2f(k))*(cfn*v(i,k-1,j  )+cfn1*v(i,k-2,j  ))*msfvy(i,j) )* (1./12.)*(  &
                    8.*(ph(i,k,j+1)-ph(i,k,j-1))                                             &
                      -(ph(i,k,j+2)-ph(i,k,j-2))                                             &
                   +8.*(phb(i,k,j+1)-phb(i,k,j-1))                                           &
                      -(phb(i,k,j+2)-phb(i,k,j-2))  )   )                

     ENDDO

   END IF

!  2nd order stencil for open and specified conditions one row in from boundary

   IF ( (config_flags%open_ys .or. specified) .and. jts <= jds+1 .and. jds+1 <= jte )  THEN

     j = jds+1
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))*          &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1)*   &
                  (phb(i,k,j+1)-phb(i,k,j  )+ph(i,k,j+1)-ph(i,k,j  ))   &
                  +(c1f(k)*muvf(i,j  )+c2f(k))*(v(i,k,j  )+v(i,k-1,j  ))*msfvy(i,j  )*   &
                  (phb(i,k,j  )-phb(i,k,j-1)+ph(i,k,j  )-ph(i,k,j-1)) )
     ENDDO
     ENDDO

     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))*                        &
                  ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1)*    &
                   (phb(i,k,j+1)-phb(i,k,j  )+ph(i,k,j+1)-ph(i,k,j  ))               &
                   +(c1f(k)*muvf(i,j  )+c2f(k))*(cfn*v(i,k-1,j  )+cfn1*v(i,k-2,j  ))*msfvy(i,j  )*    &
                   (phb(i,k,j  )-phb(i,k,j-1)+ph(i,k,j  )-ph(i,k,j-1))              )
     ENDDO

   END IF

   IF ( (config_flags%open_ye .or. specified) .and. jts <= jde-2 .and. jde-2 <= jte )  THEN

     j = jde-2
     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdy/msfty(i,j))*          &
                 ( (c1f(k)*muvf(i,j+1)+c2f(k))*(v(i,k,j+1)+v(i,k-1,j+1))*msfvy(i,j+1)*   &
                  (phb(i,k,j+1)-phb(i,k,j  )+ph(i,k,j+1)-ph(i,k,j  ))   &
                  +(c1f(k)*muvf(i,j  )+c2f(k))*(v(i,k,j  )+v(i,k-1,j  ))*msfvy(i,j  )*   &
                  (phb(i,k,j  )-phb(i,k,j-1)+ph(i,k,j  )-ph(i,k,j-1)) )
     ENDDO
     ENDDO

     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdy/msfty(i,j))*                        &
                  ( (c1f(k)*muvf(i,j+1)+c2f(k))*(cfn*v(i,k-1,j+1)+cfn1*v(i,k-2,j+1))*msfvy(i,j+1)*    &
                   (phb(i,k,j+1)-phb(i,k,j  )+ph(i,k,j+1)-ph(i,k,j  ))               &
                   +(c1f(k)*muvf(i,j  )+c2f(k))*(cfn*v(i,k-1,j  )+cfn1*v(i,k-2,j  ))*msfvy(i,j  )*    &
                   (phb(i,k,j  )-phb(i,k,j-1)+ph(i,k,j  )-ph(i,k,j-1))              )
     ENDDO

   END IF

!  x (u) advection

   i_start = its
   j_start = jts
   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)

   IF (config_flags%open_xs .or. specified ) i_start = max(its,ids+3)
   IF (config_flags%open_xe .or. specified ) itf     = min(itf,ide-4)

   DO j = j_start, jtf

     DO k = 2, kte-1
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))*(                   &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j)              &
                  +(c1f(k)*muuf(i,j  )+c2f(k))*(u(i,k,j  )+u(i,k-1,j  ))*msfux(i,j) )* (1./60.)*(  &
                   45.*(ph(i+1,k,j)-ph(i-1,k,j))                                  &
                   -9.*(ph(i+2,k,j)-ph(i-2,k,j))                                  &
                      +(ph(i+3,k,j)-ph(i-3,k,j))                                  &
                  +45.*(phb(i+1,k,j)-phb(i-1,k,j))                                &
                   -9.*(phb(i+2,k,j)-phb(i-2,k,j))                                &
                      +(phb(i+3,k,j)-phb(i-3,k,j))  )   )                
     ENDDO
     ENDDO

     k = kte
     DO i = i_start, itf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))*(                             &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j)            &
                  +(c1f(k)*muuf(i,j  )+c2f(k))*(cfn*u(i  ,k-1,j)+cfn1*u(i,k-2,j))*msfux(i,j) )* (1./60.)*(  &
                   45.*(ph(i+1,k,j)-ph(i-1,k,j))                                           &
                   -9.*(ph(i+2,k,j)-ph(i-2,k,j))                                           &
                      +(ph(i+3,k,j)-ph(i-3,k,j))                                           &
                  +45.*(phb(i+1,k,j)-phb(i-1,k,j))                                         &
                   -9.*(phb(i+2,k,j)-phb(i-2,k,j))                                         &
                      +(phb(i+3,k,j)-phb(i-3,k,j))  )     )
     ENDDO

   ENDDO

!  4th order stencil two in from the boundary for open and specified conditions

   IF ( (config_flags%open_xs) .and. its <= ids+2 .and. ids+2 <= ite ) THEN
     i = ids + 2
     DO j = j_start, jtf
       DO k = 2, kte-1
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))*(                   &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j)              &
                  +(c1f(k)*muuf(i,j  )+c2f(k))*(u(i,k,j  )+u(i,k-1,j  ))*msfux(i,j) )* (1./12.)*(  &
                    8.*(ph(i+1,k,j)-ph(i-1,k,j))                                  &
                      -(ph(i+2,k,j)-ph(i-2,k,j))                                  &
                   +8.*(phb(i+1,k,j)-phb(i-1,k,j))                                &
                      -(phb(i+2,k,j)-phb(i-2,k,j))  )   )                
       ENDDO
       k = kte
       ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))*(                             &
                ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j)            &
                 +(c1f(k)*muuf(i,j  )+c2f(k))*(cfn*u(i  ,k-1,j)+cfn1*u(i,k-2,j))*msfux(i,j) )* (1./12.)*(  &
                   8.*(ph(i+1,k,j)-ph(i-1,k,j))                                           &
                     -(ph(i+2,k,j)-ph(i-2,k,j))                                           &
                  +8.*(phb(i+1,k,j)-phb(i-1,k,j))                                         &
                     -(phb(i+2,k,j)-phb(i-2,k,j))  )     )

     ENDDO
   END IF

   IF ( (config_flags%open_xe) .and. its <= ide-3 .and. ide-3 <= ite ) THEN
     i = ide-3
     DO j = j_start, jtf
       DO k = 2, kte-1
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))*(                   &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j)              &
                  +(c1f(k)*muuf(i,j  )+c2f(k))*(u(i,k,j  )+u(i,k-1,j  ))*msfux(i,j) )* (1./12.)*(  &
                    8.*(ph(i+1,k,j)-ph(i-1,k,j))                                  &
                      -(ph(i+2,k,j)-ph(i-2,k,j))                                  &
                   +8.*(phb(i+1,k,j)-phb(i-1,k,j))                                &
                      -(phb(i+2,k,j)-phb(i-2,k,j))  )   )                
       ENDDO
       k = kte
       ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))*(                             &
                ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j)            &
                 +(c1f(k)*muuf(i,j  )+c2f(k))*(cfn*u(i  ,k-1,j)+cfn1*u(i,k-2,j))*msfux(i,j) )* (1./12.)*(  &
                   8.*(ph(i+1,k,j)-ph(i-1,k,j))                                           &
                     -(ph(i+2,k,j)-ph(i-2,k,j))                                           &
                  +8.*(phb(i+1,k,j)-phb(i-1,k,j))                                         &
                     -(phb(i+2,k,j)-phb(i-2,k,j))  )     )

     ENDDO
   END IF

!  2nd order stencil for open and specified conditions one in from the boundary

   IF ( (config_flags%open_xs .or. specified) .and. its <= ids+1 .and. ids+1 <= ite ) THEN

     i = ids + 1

     DO j = j_start, jtf
     DO k = 2, kte-1
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))*         &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j)*  &
                  (phb(i+1,k,j)-phb(i  ,k,j)+ph(i+1,k,j)-ph(i  ,k,j))  &
                  +(c1f(k)*muuf(i  ,j)+c2f(k))*(u(i  ,k,j)+u(i  ,k-1,j))*msfux(i  ,j)*  &
                  (phb(i  ,k,j)-phb(i-1,k,j)+ph(i  ,k,j)-ph(i-1,k,j))  )
     ENDDO
     ENDDO

     k = kte
     DO j = j_start, jtf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))*                        &
                  ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j)*    &
                   (phb(i+1,k,j)-phb(i  ,k,j)+ph(i+1,k,j)-ph(i  ,k,j))               &
                   +(c1f(k)*muuf(i  ,j)+c2f(k))*(cfn*u(i  ,k-1,j)+cfn1*u(i  ,k-2,j))*msfux(  i,j)*    &
                   (phb(i  ,k,j)-phb(i-1,k,j)+ph(i  ,k,j)-ph(i-1,k,j))             )
     ENDDO

   END IF

   IF ( (config_flags%open_xe .or. specified) .and. its <= ide-2 .and. ide-2 <= ite ) THEN

     i = ide-2
     DO j = j_start, jtf
     DO k = 2, kte-1
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.25*rdx/msfty(i,j))*         &
                 ( (c1f(k)*muuf(i+1,j)+c2f(k))*(u(i+1,k,j)+u(i+1,k-1,j))*msfux(i+1,j)*  &
                  (phb(i+1,k,j)-phb(i  ,k,j)+ph(i+1,k,j)-ph(i  ,k,j))  &
                  +(c1f(k)*muuf(i  ,j)+c2f(k))*(u(i  ,k,j)+u(i  ,k-1,j))*msfux(i  ,j)*  &
                  (phb(i  ,k,j)-phb(i-1,k,j)+ph(i  ,k,j)-ph(i-1,k,j))  )
     ENDDO
     ENDDO

     k = kte
     DO j = j_start, jtf
        ph_tend(i,k,j)=ph_tend(i,k,j) - (0.5*rdx/msfty(i,j))*                        &
                  ( (c1f(k)*muuf(i+1,j)+c2f(k))*(cfn*u(i+1,k-1,j)+cfn1*u(i+1,k-2,j))*msfux(i+1,j)*    &
                   (phb(i+1,k,j)-phb(i  ,k,j)+ph(i+1,k,j)-ph(i  ,k,j))               &
                   +(c1f(k)*muuf(i  ,j)+c2f(k))*(cfn*u(i  ,k-1,j)+cfn1*u(i  ,k-2,j))*msfux(  i,j)*    &
                   (phb(i  ,k,j)-phb(i-1,k,j)+ph(i  ,k,j)-ph(i-1,k,j))             )
     ENDDO

   END IF

   END IF  ! 6th order advection

!  lateral open boundary conditions,
!  start with north and south (y) boundaries

   i_start = its
   itf=MIN(ite,ide-1)

   !  south

   IF ( (config_flags%open_ys) .and. jts == jds ) THEN

     j=jts

     DO k=2,kde
       kz = min(k,kde-1)
       DO i = its,itf
         vb =.5*( fnm(kz)*(v(i,kz  ,j+1)+v(i,kz  ,j  ))    &
                 +fnp(kz)*(v(i,kz-1,j+1)+v(i,kz-1,j  )) )
         vl=amin1(vb,0.)
         ph_tend(i,k,j)=ph_tend(i,k,j)-rdy*(c1f(k)*mut(i,j)+c2f(k))*(      &
                              +vl*(ph_old(i,k,j+1)-ph_old(i,k,j)))
       ENDDO
     ENDDO

   END IF

   ! north

   IF ( (config_flags%open_ye) .and. jte == jde ) THEN

     j=jte-1

     DO k=2,kde
       kz = min(k,kde-1)
       DO i = its,itf
        vb=.5*( fnm(kz)*(v(i,kz  ,j+1)+v(i,kz  ,j))   &
               +fnp(kz)*(v(i,kz-1,j+1)+v(i,kz-1,j)) )
        vr=amax1(vb,0.)
        ph_tend(i,k,j)=ph_tend(i,k,j)-rdy*(c1f(k)*mut(i,j)+c2f(k))*(      &
                   +vr*(ph_old(i,k,j)-ph_old(i,k,j-1)))
       ENDDO
     ENDDO

   END IF

   !  now the east and west (y) boundaries

   j_start = its
   jtf=MIN(jte,jde-1)

   !  west

   IF ( (config_flags%open_xs) .and. its == ids ) THEN

     i=its

     DO j = jts,jtf
       DO k=2,kde-1
         kz = k
         ub =.5*( fnm(kz)*(u(i+1,kz  ,j)+u(i  ,kz  ,j))     &
                 +fnp(kz)*(u(i+1,kz-1,j)+u(i  ,kz-1,j)) )
         ul=amin1(ub,0.)
         ph_tend(i,k,j)=ph_tend(i,k,j)-(msftx(i,j)/msfty(i,j))*rdx*(c1f(k)*mut(i,j)+c2f(k))*(       &
                              +ul*(ph_old(i+1,k,j)-ph_old(i,k,j)))
       ENDDO

         k = kde
         kz = k
         ub =.5*( fnm(kz)*(u(i+1,kz  ,j)+u(i  ,kz  ,j))     &
                 +fnp(kz)*(u(i+1,kz-1,j)+u(i  ,kz-1,j)) )
         ul=amin1(ub,0.)
         ph_tend(i,k,j)=ph_tend(i,k,j)-(msftx(i,j)/msfty(i,j))*rdx*(c1f(k)*mut(i,j)+c2f(k))*(       &
                              +ul*(ph_old(i+1,k,j)-ph_old(i,k,j)))
     ENDDO

   END IF

   ! east

   IF ( (config_flags%open_xe) .and. ite == ide ) THEN

     i = ite-1

     DO j = jts,jtf
       DO k=2,kde-1
        kz = k
        ub=.5*( fnm(kz)*(u(i+1,kz  ,j)+u(i,kz  ,j))  &
               +fnp(kz)*(u(i+1,kz-1,j)+u(i,kz-1,j)) )
        ur=amax1(ub,0.)
        ph_tend(i,k,j)=ph_tend(i,k,j)-(msftx(i,j)/msfty(i,j))*rdx*(c1f(k)*mut(i,j)+c2f(k))*( &
                   +ur*(ph_old(i,k,j)-ph_old(i-1,k,j)))
       ENDDO

        k = kde    
        kz = k-1
        ub=.5*( fnm(kz)*(u(i+1,kz  ,j)+u(i,kz  ,j))   &
               +fnp(kz)*(u(i+1,kz-1,j)+u(i,kz-1,j)) )
        ur=amax1(ub,0.)
        ph_tend(i,k,j)=ph_tend(i,k,j)-(msftx(i,j)/msfty(i,j))*rdx*(c1f(k)*mut(i,j)+c2f(k))*(  &
                   +ur*(ph_old(i,k,j)-ph_old(i-1,k,j)))

     ENDDO

   END IF

  END SUBROUTINE rhs_ph


!-------------------------------------------------------------------------------

SUBROUTINE horizontal_pressure_gradient( ru_tend,rv_tend,                &
                                         ph,alt,p,pb,al,php,cqu,cqv,     &
                                         muu,muv,mu,c1h,c2h,fnm,fnp,rdnw,&
                                         cf1,cf2,cf3,cfn,cfn1,           &
                                         rdx,rdy,msfux,msfuy,&
                                         msfvx,msfvy,msftx,msfty,        &
                                         config_flags, non_hydrostatic,  &
                                         top_lid,                        &
                                         ids, ide, jds, jde, kds, kde,   &
                                         ims, ime, jms, jme, kms, kme,   &
                                         its, ite, jts, jte, kts, kte   )

   IMPLICIT NONE
   
   ! Input data


   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   LOGICAL, INTENT (IN   ) :: non_hydrostatic, top_lid

   INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte

   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(IN   ) ::        &
                                                                     ph,  &
                                                                     alt, &
                                                                     al,  &
                                                                     p,   &
                                                                     pb,  &
                                                                     php, &
                                                                     cqu, &
                                                                     cqv


   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(INOUT) ::           &
                                                                    ru_tend, &
                                                                    rv_tend

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) :: muu, muv, mu,    &
                                                            msfux, msfuy, &
                                                            msfvx, msfvy, &
                                                            msftx, msfty

   REAL, DIMENSION( kms:kme ), INTENT(IN   ) :: rdnw, fnm, fnp

   REAL, DIMENSION( kms:kme ), INTENT(IN   ) :: c1h, c2h

   REAL,  INTENT(IN   ) :: rdx, rdy, cf1, cf2, cf3, cfn, cfn1

   INTEGER :: i,j,k, itf, jtf, ktf, i_start, j_start
   REAL, DIMENSION( ims:ime, kms:kme ) :: dpn
   REAL :: dpx, dpy
   REAL, DIMENSION( kms:kme ) :: c1

   LOGICAL :: specified

!<DESCRIPTION>
!
!  horizontal_pressure_gradient calculates the
!  horizontal pressure gradient terms for the large-timestep tendency
!  in the horizontal momentum equations (u,v).
!
!</DESCRIPTION>

   c1 = c1h
   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

!  Notes on map scale factors:
!  Calculates the pressure gradient terms in ADT eqns 44 and 45
!  With upper rho -> 'mu', these are:
!  Eqn 30: -mu*(mx/my)*(1/rho)*partial dp/dx
!  Eqn 31: -mu*(my/mx)*(1/rho)*partial dp/dy
!
!  As we are on nu, rather than height, surfaces:
!
!  mu dp/dx = mu alpha partial dp'/dx + (nu mu partial dmubar/dx) alpha'
!           + mu partial dphi'/dx + (partial dphi/dx)*(partial dp'/dnu - mu')
!
!  mu dp/dy = mu alpha partial dp'/dy + (nu mu partial dmubar/dy) alpha'
!           + mu partial dphi'/dy + (partial dphi/dy)*(partial dp'/dnu - mu')

! start with the north-south (y) pressure gradient

   itf=MIN(ite,ide-1)
   jtf=jte
   ktf=MIN(kte,kde-1)
   i_start = its
   j_start = jts
   IF ( (config_flags%open_ys .or. specified .or. &
         config_flags%nested .or. config_flags%polar ) .and. jts == jds ) j_start = jts+1
   IF ( (config_flags%open_ye .or. specified .or. &
         config_flags%nested .or. config_flags%polar ) .and. jte == jde ) jtf = jtf-1

   DO j = j_start, jtf

     IF ( non_hydrostatic )  THEN

        k=1

        DO i = i_start, itf
          dpn(i,k) = .5*( cf1*(p(i,k  ,j-1)+p(i,k  ,j))   &
                         +cf2*(p(i,k+1,j-1)+p(i,k+1,j))   &
                         +cf3*(p(i,k+2,j-1)+p(i,k+2,j))  )
          dpn(i,kde) = 0.
        ENDDO
        IF (top_lid) THEN
          DO i = i_start, itf
            dpn(i,kde) = .5*( cfn *(p(i,kde-1,j-1)+p(i,kde-1,j))   &
                             +cfn1*(p(i,kde-2,j-1)+p(i,kde-2,j)) )
          ENDDO
        ENDIF
               
        DO k=2,ktf
          DO i = i_start, itf
            dpn(i,k) = .5*( fnm(k)*(p(i,k  ,j-1)+p(i,k  ,j))  &
                           +fnp(k)*(p(i,k-1,j-1)+p(i,k-1,j)) )
          END DO
        END DO

!       ADT eqn 45: -mu*(my/mx)*(1/rho)*partial dp/dy
!       [alt, al are 1/rho terms; muv, mu are NOT coupled]
        DO K=1,ktf
          DO i = i_start, itf
            ! Here are mu dp/dy terms 1-3
            dpy = (msfvy(i,j)/msfvx(i,j))*.5*rdy*(c1h(k)*muv(i,j)+c2h(k))*(                 &
                     (ph (i,k+1,j)-ph (i,k+1,j-1) + ph(i,k,j)-ph(i,k,j-1))  &
                    +(alt(i,k  ,j)+alt(i,k  ,j-1))*(p (i,k,j)-p (i,k,j-1))  &
                    +(al (i,k  ,j)+al (i,k  ,j-1))*(pb(i,k,j)-pb(i,k,j-1)) )
            ! Here is mu dp/dy term 4
            dpy = dpy + (msfvy(i,j)/msfvx(i,j))*rdy*(php(i,k,j)-php(i,k,j-1))* &
                (rdnw(k)*(dpn(i,k+1)-dpn(i,k))-.5*((c1(k)*mu(i,j-1))+(c1(k)*mu(i,j))))
            rv_tend(i,k,j) = rv_tend(i,k,j)-cqv(i,k,j)*dpy
          END DO
        END DO

     ELSE

!       ADT eqn 45: -mu*(my/mx)*(1/rho)*partial dp/dy
!       [alt, al are 1/rho terms; muv, mu are NOT coupled]
        DO K=1,ktf
          DO i = i_start, itf
            ! Here are mu dp/dy terms 1-3; term 4 not needed if hydrostatic
            dpy = (msfvy(i,j)/msfvx(i,j))*.5*rdy*(c1h(k)*muv(i,j)+c2h(k))*(                 &
                     (ph (i,k+1,j)-ph (i,k+1,j-1) + ph(i,k,j)-ph(i,k,j-1))  &
                    +(alt(i,k  ,j)+alt(i,k  ,j-1))*(p (i,k,j)-p (i,k,j-1))  &
                    +(al (i,k  ,j)+al (i,k  ,j-1))*(pb(i,k,j)-pb(i,k,j-1)) )
            rv_tend(i,k,j) = rv_tend(i,k,j)-dpy
          END DO
        END DO

     END IF

   ENDDO

!  now the east-west (x) pressure gradient

   itf=ite
   jtf=MIN(jte,jde-1)
   ktf=MIN(kte,kde-1)
   i_start = its
   j_start = jts
   IF ( (config_flags%open_xs .or. specified .or. &
           config_flags%nested ) .and. its == ids ) i_start = its+1
   IF ( (config_flags%open_xe .or. specified .or. &
           config_flags%nested ) .and. ite == ide ) itf = itf-1
   IF ( config_flags%periodic_x ) i_start = its
   IF ( config_flags%periodic_x ) itf=ite

   DO j = j_start, jtf

     IF ( non_hydrostatic )  THEN

        k=1

        DO i = i_start, itf
          dpn(i,k) = .5*( cf1*(p(i-1,k  ,j)+p(i,k  ,j))   &
                         +cf2*(p(i-1,k+1,j)+p(i,k+1,j))   &
                         +cf3*(p(i-1,k+2,j)+p(i,k+2,j))  )
          dpn(i,kde) = 0.
        ENDDO
        IF (top_lid) THEN
          DO i = i_start, itf
            dpn(i,kde) = .5*( cfn *(p(i-1,kde-1,j)+p(i,kde-1,j))   &
                             +cfn1*(p(i-1,kde-2,j)+p(i,kde-2,j)) )
          ENDDO
        ENDIF
               
        DO k=2,ktf
          DO i = i_start, itf
            dpn(i,k) = .5*( fnm(k)*(p(i-1,k  ,j)+p(i,k  ,j))  &
                           +fnp(k)*(p(i-1,k-1,j)+p(i,k-1,j)) )
          END DO
        END DO

! ADT eqn 44: -mu*(mx/my)*(1/rho)*partial dp/dx
! [alt, al are 1/rho terms; muu, mu are NOT coupled]
        DO K=1,ktf
          DO i = i_start, itf
            ! Here are mu dp/dy terms 1-3
            dpx = (msfux(i,j)/msfuy(i,j))*.5*rdx*(c1h(k)*muu(i,j)+c2h(k))*(                    &
                        (ph (i,k+1,j)-ph (i-1,k+1,j) + ph(i,k,j)-ph(i-1,k,j))  &
                       +(alt(i,k  ,j)+alt(i-1,k  ,j))*(p (i,k,j)-p (i-1,k,j))  &
                       +(al (i,k  ,j)+al (i-1,k  ,j))*(pb(i,k,j)-pb(i-1,k,j)) )
            ! Here is mu dp/dy term 4
            dpx = dpx + (msfux(i,j)/msfuy(i,j))*rdx*(php(i,k,j)-php(i-1,k,j))* &
                (rdnw(k)*(dpn(i,k+1)-dpn(i,k))-.5*((c1(k)*mu(i-1,j))+(c1(k)*mu(i,j))))
            ru_tend(i,k,j) = ru_tend(i,k,j)-cqu(i,k,j)*dpx
          END DO
        END DO

     ELSE

!       ADT eqn 44: -mu*(mx/my)*(1/rho)*partial dp/dx
!       [alt, al are 1/rho terms; muu, mu are NOT coupled]
        DO K=1,ktf
          DO i = i_start, itf
            ! Here are mu dp/dy terms 1-3; term 4 not needed if hydrostatic
            dpx = (msfux(i,j)/msfuy(i,j))*.5*rdx*(c1h(k)*muu(i,j)+c2h(k))*(                    &
                        (ph (i,k+1,j)-ph (i-1,k+1,j) + ph(i,k,j)-ph(i-1,k,j))  &
                       +(alt(i,k  ,j)+alt(i-1,k  ,j))*(p (i,k,j)-p (i-1,k,j))  &
                       +(al (i,k  ,j)+al (i-1,k  ,j))*(pb(i,k,j)-pb(i-1,k,j)) )
            ru_tend(i,k,j) = ru_tend(i,k,j)-dpx
          END DO
        END DO

     END IF

   ENDDO

END SUBROUTINE horizontal_pressure_gradient

!-------------------------------------------------------------------------------

SUBROUTINE pg_buoy_w( rw_tend, p, cqw, muf, mubf,     &
                      c1f, c2f,                       &
                      rdnw, rdn, g, msftx, msfty,     &
                      ids, ide, jds, jde, kds, kde,   &
                      ims, ime, jms, jme, kms, kme,   &
                      its, ite, jts, jte, kts, kte   )

   IMPLICIT NONE
   
   ! Input data

   INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte

   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(IN   ) ::   p
   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(INOUT) ::   cqw


   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(INOUT) ::  rw_tend

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) :: mubf, muf, msftx, msfty

   REAL, DIMENSION( kms:kme ), INTENT(IN   ) :: rdnw, rdn

   REAL, DIMENSION( kms:kme ), INTENT(IN   ) :: c1f, c2f

   REAL,  INTENT(IN   ) :: g

   INTEGER :: itf, jtf, i, j, k
   REAL    :: cq1, cq2


!<DESCRIPTION>
!
!  pg_buoy_w calculates the
!  vertical pressure gradient and buoyancy terms for the large-timestep
!  tendency in the vertical momentum equation.
!
!</DESCRIPTION>

!  BUOYANCY AND PRESSURE GRADIENT TERM IN W EQUATION AT TIME T

!  Map scale factor notes
!  ADT eqn 46 RHS terms 6 and 7 (where 7 is "-rho g")
!  Dividing by my, and using mu and nu (see Klemp et al. eqns 32, 40)
!  term 6: +(g/my) partial dp'/dnu
!  term 7: -(g/my) mu'
!
!  For moisture-free atmosphere, cq1=1, cq2=0
!  => (1./msft(i,j)) * g * [rdn(k)*{p(i,k,j)-p(i,k-1,j)}-(c1(k)*mu(i,j))]

   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)

   DO j = jts,jtf

     k=kde
     DO i=its,itf
       cq1 = 1./(1.+cqw(i,k-1,j))
       cq2 = cqw(i,k-1,j)*cq1
       rw_tend(i,k,j) = rw_tend(i,k,j)+(1./msfty(i,j))*g*(      &
                        cq1*2.*rdnw(k-1)*(  -p(i,k-1,j))  &
                        -(c1f(k)*muf(i,j))-cq2*(c1f(k)*mubf(i,j)+c2f(k))            )
     END DO

     DO k = 2, kde-1
     DO i = its,itf
      cq1 = 1./(1.+cqw(i,k,j))
      cq2 = cqw(i,k,j)*cq1
      cqw(i,k,j) = cq1
      rw_tend(i,k,j) = rw_tend(i,k,j)+(1./msfty(i,j))*g*(      &
                       cq1*rdn(k)*(p(i,k,j)-p(i,k-1,j))  &
                       -(c1f(k)*muf(i,j))-cq2*(c1f(k)*mubf(i,j)+c2f(k))            )
     END DO
     ENDDO           


   ENDDO

END SUBROUTINE pg_buoy_w

!-------------------------------------------------------------------------------

SUBROUTINE w_damp( rw_tend, max_vert_cfl,max_horiz_cfl, &
                      u, v, ww, w, mut, c1f, c2f, rdnw, &
                      rdx, rdy, msfux, msfuy,           &
                      msfvx, msfvy, dt,                 &
                      config_flags,                     &
                      ids, ide, jds, jde, kds, kde,     &
                      ims, ime, jms, jme, kms, kme,     &
                      its, ite, jts, jte, kts, kte     )

   USE module_llxy
   IMPLICIT NONE

   ! Input data

   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte

   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(IN   ) ::   u, v, ww, w

   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(INOUT) ::  rw_tend

   REAL, INTENT(OUT) ::  max_vert_cfl
   REAL, INTENT(OUT) ::  max_horiz_cfl
   REAL              ::  horiz_cfl

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) :: mut

   REAL, DIMENSION( kms:kme ), INTENT(IN   ) :: rdnw

   REAL, DIMENSION( kms:kme ), INTENT(IN   ) :: c1f, c2f

   REAL, INTENT(IN)    :: dt
   REAL, INTENT(IN)    :: rdx, rdy
   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux, msfuy
   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfvx, msfvy

   REAL                :: vert_cfl, cf_n, cf_d, maxdub, maxdeta

   INTEGER :: itf, jtf, i, j, k, maxi, maxj, maxk
   INTEGER :: some
   CHARACTER*512 :: temp

   CHARACTER (LEN=256) :: time_str
   CHARACTER (LEN=256) :: grid_str

   integer :: total
   REAL :: msfuxt , msfxffl
   
!<DESCRIPTION>
!
!  w_damp computes a damping term for the vertical velocity when the
!  vertical Courant number is too large.  This was found to be preferable to
!  decreasing the timestep or increasing the diffusion in real-data applications
!  that produced potentially-unstable large vertical velocities because of
!  unphysically large heating rates coming from the cumulus parameterization
!  schemes run at moderately high resolutions (dx ~ O(10) km).
!
!  Additionally, w_damp returns the maximum cfl values due to vertical motion and
!  horizontal motion.  These values are returned via the max_vert_cfl and
!  max_horiz_cfl variables.  (Added by T. Hutchinson, WSI, 3/5/2007)
!
!</DESCRIPTION>

   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)

   some = 0
   max_vert_cfl = 0.
   max_horiz_cfl = 0.
   total = 0

   IF(config_flags%polar ) then
     msfxffl = 1.0/COS(config_flags%fft_filter_lat*degrad)
   END IF

   IF ( config_flags%w_damping == 1 ) THEN
#ifdef OPTIMIZE_CFL_TEST
! 20121025, L. Meadows vector optimization does not include special case for Cassini
     IF(config_flags%polar ) then
       CALL wrf_error_fatal('module_big_step_utilities_em.F: -DOPTIMIZE_CFL_TEST option does not support global domains')
     END IF
#endif

     DO j = jts,jtf

     DO k = 2, kde-1
     DO i = its,itf
#if 1
# ifdef OPTIMIZE_CFL_TEST
! L. Meadows, Intel, MIC optimization, 20121025
        msfuxt = msfux(i,j)
        vert_cfl = abs(ww(i,k,j)/(c1f(k)*mut(i,j)+c2f(k))*rdnw(k)*dt)
        IF ( vert_cfl > max_vert_cfl ) THEN
           max_vert_cfl = vert_cfl
        ENDIF
# else
        IF(config_flags%polar ) then
           msfuxt = MIN(msfux(i,j), msfxffl)
        ELSE
           msfuxt = msfux(i,j)
        END IF
        vert_cfl = abs(ww(i,k,j)/(c1f(k)*mut(i,j)+c2f(k))*rdnw(k)*dt)

        IF ( vert_cfl > max_vert_cfl ) THEN
           max_vert_cfl = vert_cfl ; maxi = i ; maxj = j ; maxk = k
           maxdub = w(i,k,j) ; maxdeta = -1./rdnw(k)
        ENDIF
# endif
        
        horiz_cfl = max( abs(u(i,k,j) * rdx * msfuxt * dt),                          &
             abs(v(i,k,j) * rdy * msfvy(i,j) * dt) )
        if (horiz_cfl > max_horiz_cfl) then
           max_horiz_cfl = horiz_cfl
        endif
        
        if(vert_cfl .gt. w_beta)then
#else
! restructure to get rid of divide
!
! This had been used for efficiency, but with the addition of returning the cfl values,
!   the old version (above) was reinstated.  (T. Hutchinson, 3/5/2007)
!
        cf_n = abs(ww(i,k,j)*rdnw(k)*dt)
        cf_d = abs((c1f(k)*mut(i,j)+c2f(k)))
        if(cf_n .gt. cf_d*w_beta )then
#endif
#ifndef OPTIMIZE_CFL_TEST
! This internal write is costly on newer Xeon processors because it breaks
! vectorization.  (J. Michalakes for L. Meadows at Intel, 12/13/2012)
           WRITE(temp,*)i,j,k,' vert_cfl,w,d(eta)=',vert_cfl,w(i,k,j),-1./rdnw(k)
           CALL wrf_debug ( 100 , TRIM(temp) )
#endif
           if ( vert_cfl > 2. ) some = some + 1
           rw_tend(i,k,j) = rw_tend(i,k,j)-sign(1.,w(i,k,j))*w_alpha*(vert_cfl-w_beta)*(c1f(k)*mut(i,j)+c2f(k))
        endif
     END DO
     ENDDO
     ENDDO
   ELSE
! just print
     DO j = jts,jtf

     DO k = 2, kde-1
     DO i = its,itf

#if 1
# ifdef OPTIMIZE_CFL_TEST
        msfuxt = msfux(i,j)
        vert_cfl = abs(ww(i,k,j)/(c1f(k)*mut(i,j)+c2f(k))*rdnw(k)*dt)
        IF ( vert_cfl > max_vert_cfl ) THEN
           max_vert_cfl = vert_cfl
        ENDIF
# else
! L. Meadows MIC optimization, 20121025
        IF(config_flags%polar ) then
           msfuxt = MIN(msfux(i,j), msfxffl)
        ELSE
           msfuxt = msfux(i,j)
        END IF
        vert_cfl = abs(ww(i,k,j)/(c1f(k)*mut(i,j)+c2f(k))*rdnw(k)*dt)
        
        IF ( vert_cfl > max_vert_cfl ) THEN
           max_vert_cfl = vert_cfl ; maxi = i ; maxj = j ; maxk = k
           maxdub = w(i,k,j) ; maxdeta = -1./rdnw(k)
        ENDIF
# endif
        
        horiz_cfl = max( abs(u(i,k,j) * rdx * msfuxt * dt),                          &
             abs(v(i,k,j) * rdy * msfvy(i,j) * dt) )

        if (horiz_cfl > max_horiz_cfl) then
           max_horiz_cfl = horiz_cfl
        endif
        
        if(vert_cfl .gt. w_beta)then
#else
! restructure to get rid of divide
!
! This had been used for efficiency, but with the addition of returning the cfl values,
!   the old version (above) was reinstated.  (T. Hutchinson, 3/5/2007)
!
        cf_n = abs(ww(i,k,j)*rdnw(k)*dt)
        cf_d = abs((c1f(k)*mut(i,j)+c2f(k)))
        if(cf_n .gt. cf_d*w_beta )then
#endif
#ifndef OPTIMIZE_CFL_TEST
           WRITE(temp,*)i,j,k,' vert_cfl,w,d(eta)=',vert_cfl,w(i,k,j),-1./rdnw(k)
           CALL wrf_debug ( 100 , TRIM(temp) )
#endif
           if ( vert_cfl > 2. ) some = some + 1
        endif
     END DO
     ENDDO
     ENDDO
   ENDIF
   IF ( some .GT. 0 ) THEN
     CALL get_current_time_string( time_str )
     CALL get_current_grid_name( grid_str )
     WRITE(temp,*)some,                                            &
            ' points exceeded cfl=2 in domain '//TRIM(grid_str)//' at time '//TRIM(time_str)//' hours'
     CALL wrf_debug ( 0 , TRIM(temp) )
#ifndef OPTIMIZE_CFL_TEST
     WRITE(temp,*)'MAX AT i,j,k: ',maxi,maxj,maxk,' vert_cfl,w,d(eta)=',max_vert_cfl, &
                             maxdub,maxdeta
     CALL wrf_debug ( 0 , TRIM(temp) )
#endif
   ENDIF

END SUBROUTINE w_damp

!-------------------------------------------------------------------------------

SUBROUTINE horizontal_diffusion ( name, field, tendency, MUT, c1, c2,  &
                                  config_flags,                        &
                                  msfux, msfuy, msfvx, msfvx_inv,      &
                                  msfvy, msftx, msfty,                 &
                                  khdif, xkmhd, rdx, rdy,              &
                                  ids, ide, jds, jde, kds, kde,        &
                                  ims, ime, jms, jme, kms, kme,        &
                                  its, ite, jts, jte, kts, kte        )

   IMPLICIT NONE
   
   ! Input data

   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,        INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte

   CHARACTER(LEN=1) ,                          INTENT(IN   ) :: name

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: field, xkmhd

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: MUT

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,      &
                                                                    msfuy,      &
                                                                    msfvx,      &
                                                                    msfvx_inv,  &
                                                                    msfvy,      &
                                                                    msftx,      &
                                                                    msfty

   REAL , DIMENSION( kms:kme )           , INTENT(IN   ) :: c1, c2

   REAL ,                                      INTENT(IN   ) :: rdx,       &
                                                                rdy,       &
                                                                khdif

   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf

   INTEGER :: i_start, i_end, j_start, j_end

   REAL :: mrdx, mkrdxm, mkrdxp, &
           mrdy, mkrdym, mkrdyp

   LOGICAL :: specified

!<DESCRIPTION>
!
!  horizontal_diffusion computes the horizontal diffusion tendency
!  on model horizontal coordinate surfaces.
!
!</DESCRIPTION>

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

   ktf=MIN(kte,kde-1)
   
   IF (name .EQ. 'u') THEN

      i_start = its
      i_end   = ite
      j_start = jts
      j_end   = MIN(jte,jde-1)

      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-1,ite)
      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end   = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite


      DO j = j_start, j_end
      DO k=kts,ktf
      DO i = i_start, i_end

         ! The interior is grad: (m_x*d/dx), the exterior is div: (m_x*m_y*d/dx(/m_y))
         ! setting up different averagings of m^2 partial d/dX and m^2 partial d/dY

         mkrdxm=(msftx(i-1,j)/msfty(i-1,j))*(c1(k)*MUT(i-1,j)+c2(k))*xkmhd(i-1,k,j)*rdx
         mkrdxp=(msftx(i,j)/msfty(i,j))*(c1(k)*MUT(i,j)+c2(k))*xkmhd(i,k,j)*rdx
         mrdx=msfux(i,j)*msfuy(i,j)*rdx
         mkrdym=( (msfuy(i,j)+msfuy(i,j-1))/(msfux(i,j)+msfux(i,j-1)) )* &
                0.25*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j-1)+c2(k))+(c1(k)*MUT(i-1,j-1)+c2(k))+(c1(k)*MUT(i-1,j)+c2(k)))* &
                0.25*(xkmhd(i,k,j)+xkmhd(i,k,j-1)+xkmhd(i-1,k,j-1)+xkmhd(i-1,k,j))*rdy
         mkrdyp=( (msfuy(i,j)+msfuy(i,j+1))/(msfux(i,j)+msfux(i,j+1)) )* &
                0.25*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j+1)+c2(k))+(c1(k)*MUT(i-1,j+1)+c2(k))+(c1(k)*MUT(i-1,j)+c2(k)))* &
                0.25*(xkmhd(i,k,j)+xkmhd(i,k,j+1)+xkmhd(i-1,k,j+1)+xkmhd(i-1,k,j))*rdy
         ! need to do four-corners (t) for diffusion coefficient as there are
         ! no values at u,v points
         ! msfuy - has to be y as part of d/dY
         !         has to be u as we're at a u point
         mrdy=msfux(i,j)*msfuy(i,j)*rdy

         ! correctly averaged version of rho~ * m^2 *
         !    [partial d/dX(partial du^/dX) + partial d/dY(partial du^/dY)]
            tendency(i,k,j)=tendency(i,k,j)+( &
                            mrdx*(mkrdxp*(field(i+1,k,j)-field(i  ,k,j))  &
                                 -mkrdxm*(field(i  ,k,j)-field(i-1,k,j))) &
                           +mrdy*(mkrdyp*(field(i,k,j+1)-field(i,k,j  ))  &
                                 -mkrdym*(field(i,k,j  )-field(i,k,j-1))))
      ENDDO
      ENDDO
      ENDDO
   
   ELSE IF (name .EQ. 'v')THEN

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = jte

      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-2,ite)
      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end   = MIN(jde-1,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)
      IF ( config_flags%polar ) j_start = MAX(jds+1,jts)
      IF ( config_flags%polar ) j_end   = MIN(jde-1,jte)

      DO j = j_start, j_end
      DO k=kts,ktf
      DO i = i_start, i_end

         mkrdxm=( (msfvx(i,j)+msfvx(i-1,j))/(msfvy(i,j)+msfvy(i-1,j)) )*    &
                0.25*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j-1)+c2(k))+(c1(k)*MUT(i-1,j-1)+c2(k))+(c1(k)*MUT(i-1,j)+c2(k)))* &
                0.25*(xkmhd(i,k,j)+xkmhd(i,k,j-1)+xkmhd(i-1,k,j-1)+xkmhd(i-1,k,j))*rdx
         mkrdxp=( (msfvx(i,j)+msfvx(i+1,j))/(msfvy(i,j)+msfvy(i+1,j)) )*    &
                0.25*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j-1)+c2(k))+(c1(k)*MUT(i+1,j-1)+c2(k))+(c1(k)*MUT(i+1,j)+c2(k)))* &
                0.25*(xkmhd(i,k,j)+xkmhd(i,k,j-1)+xkmhd(i+1,k,j-1)+xkmhd(i+1,k,j))*rdx
         mrdx=msfvx(i,j)*msfvy(i,j)*rdx
         mkrdym=(msfty(i,j-1)/msftx(i,j-1))*xkmhd(i,k,j-1)*rdy
         mkrdyp=(msfty(i,j)/msftx(i,j))*xkmhd(i,k,j)*rdy
         mrdy=msfvx(i,j)*msfvy(i,j)*rdy

            tendency(i,k,j)=tendency(i,k,j)+( &
                            mrdx*(mkrdxp*(field(i+1,k,j)-field(i  ,k,j))  &
                                 -mkrdxm*(field(i  ,k,j)-field(i-1,k,j))) &
                           +mrdy*(mkrdyp*(field(i,k,j+1)-field(i,k,j  ))  &
                                 -mkrdym*(field(i,k,j  )-field(i,k,j-1))))
      ENDDO
      ENDDO
      ENDDO
   
   ELSE IF (name .EQ. 'w')THEN

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-2,ite)
      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end   = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)

      DO j = j_start, j_end
      DO k=kts+1,ktf
      DO i = i_start, i_end

         mkrdxm=(msfux(i,j)/msfuy(i,j))*   &
                0.25*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i-1,j)+c2(k))+(c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i-1,j)+c2(k)))* &
                0.25*(xkmhd(i,k,j)+xkmhd(i-1,k,j)+xkmhd(i,k-1,j)+xkmhd(i-1,k-1,j))*rdx
         mkrdxp=(msfux(i+1,j)/msfuy(i+1,j))*   &
                0.25*((c1(k)*MUT(i+1,j)+c2(k))+(c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i+1,j)+c2(k))+(c1(k)*MUT(i,j)+c2(k)))* &
                0.25*(xkmhd(i+1,k,j)+xkmhd(i,k,j)+xkmhd(i+1,k-1,j)+xkmhd(i,k-1,j))*rdx
         mrdx=msftx(i,j)*msfty(i,j)*rdx
!         mkrdym=(msfvy(i,j)/msfvx(i,j))*   &
         mkrdym=(msfvy(i,j)*msfvx_inv(i,j))*   &
                0.25*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j-1)+c2(k))+(c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j-1)+c2(k)))* &
                0.25*(xkmhd(i,k,j)+xkmhd(i,k,j-1)+xkmhd(i,k-1,j)+xkmhd(i,k-1,j-1))*rdy
!         mkrdyp=(msfvy(i,j+1)/msfvx(i,j+1))*   &
         mkrdyp=(msfvy(i,j+1)*msfvx_inv(i,j+1))*   &
                0.25*((c1(k)*MUT(i,j+1)+c2(k))+(c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j+1)+c2(k))+(c1(k)*MUT(i,j)+c2(k)))* &
                0.25*(xkmhd(i,k,j+1)+xkmhd(i,k,j)+xkmhd(i,k-1,j+1)+xkmhd(i,k-1,j))*rdy
         mrdy=msftx(i,j)*msfty(i,j)*rdy

            tendency(i,k,j)=tendency(i,k,j)+( &
                            mrdx*(mkrdxp*(field(i+1,k,j)-field(i  ,k,j)) &
                                 -mkrdxm*(field(i  ,k,j)-field(i-1,k,j))) &
                           +mrdy*(mkrdyp*(field(i,k,j+1)-field(i,k,j  )) &
                                 -mkrdym*(field(i,k,j  )-field(i,k,j-1))))
      ENDDO
      ENDDO
      ENDDO
   
   ELSE


      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-2,ite)
      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end   = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)

      DO j = j_start, j_end
      DO k=kts,ktf
      DO i = i_start, i_end

         mkrdxm=(msfux(i,j)/msfuy(i,j))*0.5*(xkmhd(i,k,j)+xkmhd(i-1,k,j))*0.5*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i-1,j)+c2(k)))*rdx
         mkrdxp=(msfux(i+1,j)/msfuy(i+1,j))*0.5*(xkmhd(i+1,k,j)+xkmhd(i,k,j))*0.5*((c1(k)*MUT(i+1,j)+c2(k))+(c1(k)*MUT(i,j)+c2(k)))*rdx
         mrdx=msftx(i,j)*msfty(i,j)*rdx
!         mkrdym=(msfvy(i,j)/msfvx(i,j))*0.5*(xkmhd(i,k,j)+xkmhd(i,k,j-1))*0.5*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j-1)+c2(k)))*rdy
         mkrdym=(msfvy(i,j)*msfvx_inv(i,j))*0.5*(xkmhd(i,k,j)+xkmhd(i,k,j-1))*0.5*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j-1)+c2(k)))*rdy
!         mkrdyp=(msfvy(i,j+1)/msfvx(i,j+1))*0.5*(xkmhd(i,k,j+1)+xkmhd(i,k,j))*0.5*((c1(k)*MUT(i,j+1)+c2(k))+(c1(k)*MUT(i,j)+c2(k)))*rdy
         mkrdyp=(msfvy(i,j+1)*msfvx_inv(i,j+1))*0.5*(xkmhd(i,k,j+1)+xkmhd(i,k,j))*0.5*((c1(k)*MUT(i,j+1)+c2(k))+(c1(k)*MUT(i,j)+c2(k)))*rdy
         mrdy=msftx(i,j)*msfty(i,j)*rdy

            tendency(i,k,j)=tendency(i,k,j)+( &
                            mrdx*(mkrdxp*(field(i+1,k,j)-field(i  ,k,j))  &
                                 -mkrdxm*(field(i  ,k,j)-field(i-1,k,j))) &
                           +mrdy*(mkrdyp*(field(i,k,j+1)-field(i,k,j  ))  &
                                 -mkrdym*(field(i,k,j  )-field(i,k,j-1))))
      ENDDO
      ENDDO
      ENDDO
           
   ENDIF

END SUBROUTINE horizontal_diffusion

!-----------------------------------------------------------------------------------------

SUBROUTINE horizontal_diffusion_3dmp ( name, field, tendency, MUT, c1, c2,  &
                                       config_flags, base_3d,               &
                                       msfux, msfuy, msfvx, msfvx_inv,      &
                                       msfvy, msftx, msfty,                 &
                                       khdif, xkmhd, rdx, rdy,              &
                                       ids, ide, jds, jde, kds, kde,        &
                                       ims, ime, jms, jme, kms, kme,        &
                                       its, ite, jts, jte, kts, kte        )

   IMPLICIT NONE
   
   ! Input data
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,        INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte

   CHARACTER(LEN=1) ,                          INTENT(IN   ) :: name

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: field, &
                                                                      xkmhd, &
                                                                      base_3d

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: MUT

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,      &
                                                                    msfuy,      &
                                                                    msfvx,      &
                                                                    msfvx_inv,  &
                                                                    msfvy,      &
                                                                    msftx,      &
                                                                    msfty

   REAL , DIMENSION( kms:kme )           , INTENT(IN   ) :: c1, c2

   REAL ,                                      INTENT(IN   ) :: rdx,       &
                                                                rdy,       &
                                                                khdif

   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf

   INTEGER :: i_start, i_end, j_start, j_end

   REAL :: mrdx, mkrdxm, mkrdxp, &
           mrdy, mkrdym, mkrdyp

   LOGICAL :: specified

!<DESCRIPTION>
!
!  horizontal_diffusion_3dmp computes the horizontal diffusion tendency
!  on model horizontal coordinate surfaces.  This routine computes diffusion
!  a perturbation scalar (field-base_3d).
!
!</DESCRIPTION>

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

   ktf=MIN(kte,kde-1)
   
      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-2,ite)
      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end   = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)

      DO j = j_start, j_end
      DO k=kts,ktf
      DO i = i_start, i_end

         mkrdxm=(msfux(i,j)/msfuy(i,j))*0.5*(xkmhd(i,k,j)+xkmhd(i-1,k,j))*0.5*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i-1,j)+c2(k)))*rdx
         mkrdxp=(msfux(i+1,j)/msfuy(i+1,j))*0.5*(xkmhd(i+1,k,j)+xkmhd(i,k,j))*0.5*((c1(k)*MUT(i+1,j)+c2(k))+(c1(k)*MUT(i,j)+c2(k)))*rdx
         mrdx=msftx(i,j)*msfty(i,j)*rdx
!         mkrdym=(msfvy(i,j)/msfvx(i,j))*0.5*(xkmhd(i,k,j)+xkmhd(i,k,j-1))*0.5*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j-1)+c2(k)))*rdy
!         mkrdyp=(msfvy(i,j+1)/msfvx(i,j+1))*0.5*(xkmhd(i,k,j+1)+xkmhd(i,k,j))*0.5*((c1(k)*MUT(i,j+1)+c2(k))+(c1(k)*MUT(i,j)+c2(k)))*rdy
         mkrdym=(msfvy(i,j)*msfvx_inv(i,j))*0.5*(xkmhd(i,k,j)+xkmhd(i,k,j-1))*0.5*((c1(k)*MUT(i,j)+c2(k))+(c1(k)*MUT(i,j-1)+c2(k)))*rdy
         mkrdyp=(msfvy(i,j+1)*msfvx_inv(i,j+1))*0.5*(xkmhd(i,k,j+1)+xkmhd(i,k,j))*0.5*((c1(k)*MUT(i,j+1)+c2(k))+(c1(k)*MUT(i,j)+c2(k)))*rdy
         mrdy=msftx(i,j)*msfty(i,j)*rdy

            tendency(i,k,j)=tendency(i,k,j)+(                        &
                    mrdx*( mkrdxp*(   field(i+1,k,j)  -field(i  ,k,j)      &
                                   -base_3d(i+1,k,j)+base_3d(i  ,k,j) )    &
                          -mkrdxm*(   field(i  ,k,j)  -field(i-1,k,j)      &
                                   -base_3d(i  ,k,j)+base_3d(i-1,k,j) )  ) &
                   +mrdy*( mkrdyp*(   field(i,k,j+1)  -field(i,k,j  )      &
                                   -base_3d(i,k,j+1)+base_3d(i,k,j  ) )    &
                          -mkrdym*(   field(i,k,j  )  -field(i,k,j-1)      &
                                   -base_3d(i,k,j  )+base_3d(i,k,j-1) )  ) &
                                                                         )
      ENDDO
      ENDDO
      ENDDO

END SUBROUTINE horizontal_diffusion_3dmp

!-----------------------------------------------------------------------------------------

SUBROUTINE vertical_diffusion ( name, field, tendency,        &
                                config_flags, c1, c2,         &
                                alt, MUT, rdn, rdnw, kvdif,   &
                                ids, ide, jds, jde, kds, kde, &
                                ims, ime, jms, jme, kms, kme, &
                                its, ite, jts, jte, kts, kte )


   IMPLICIT NONE
   
   ! Input data
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,    INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte

   CHARACTER(LEN=1) ,                          INTENT(IN   ) :: name

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) ,                      &
                                               INTENT(IN   ) :: field,    &
                                                                alt

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: MUT

   REAL , DIMENSION( kms:kme ) ,                   INTENT(IN   ) :: rdn, rdnw

   REAL , DIMENSION( kms:kme )   ,                 INTENT(IN   ) :: c1, c2

   REAL ,                                      INTENT(IN   ) :: kvdif
   
   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end

   REAL , DIMENSION(its:ite, jts:jte) :: vfluxm, vfluxp, zz
   REAL , DIMENSION(its:ite, 0:kte+1) :: vflux

   REAL :: rdz

   LOGICAL :: specified

!<DESCRIPTION>
!
!  vertical_diffusion
!  computes vertical diffusion tendency.
!
!</DESCRIPTION>

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

   ktf=MIN(kte,kde-1)
   
   IF (name .EQ. 'w')THEN

   
   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)

j_loop_w : DO j = j_start, j_end

     DO k=kts,ktf-1
       DO i = i_start, i_end
          vflux(i,k)= (kvdif/alt(i,k,j))*rdnw(k)*(field(i,k+1,j)-field(i,k,j))
       ENDDO
     ENDDO

     DO i = i_start, i_end
       vflux(i,ktf)=0.
     ENDDO

     DO k=kts+1,ktf
       DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)                                         &
                              +rdn(k)*g*g/(c1(k)*MUT(i,j)+c2(k))/(0.5*(alt(i,k,j)+alt(i,k-1,j)))  &
                                         *(vflux(i,k)-vflux(i,k-1))
       ENDDO
     ENDDO

    ENDDO j_loop_w

   ELSE IF(name .EQ. 'm')THEN

     i_start = its
     i_end   = MIN(ite,ide-1)
     j_start = jts
     j_end   = MIN(jte,jde-1)

j_loop_s : DO j = j_start, j_end

     DO k=kts,ktf-1
       DO i = i_start, i_end
         vflux(i,k)=kvdif*rdn(k+1)/(0.5*(alt(i,k,j)+alt(i,k+1,j)))   &
                  *(field(i,k+1,j)-field(i,k,j))
       ENDDO
     ENDDO

     DO i = i_start, i_end
       vflux(i,0)=vflux(i,1)
     ENDDO

     DO i = i_start, i_end
       vflux(i,ktf)=0.
     ENDDO

     DO k=kts,ktf
       DO i = i_start, i_end
         tendency(i,k,j)=tendency(i,k,j)+g*g/(c1(k)*MUT(i,j)+c2(k))/alt(i,k,j)  &
                *rdnw(k)*(vflux(i,k)-vflux(i,k-1))
       ENDDO
     ENDDO

 ENDDO j_loop_s

   ENDIF

END SUBROUTINE vertical_diffusion


!-------------------------------------------------------------------------------

SUBROUTINE vertical_diffusion_mp ( field, tendency, config_flags, &
                                   base, c1, c2,                  &
                                   alt, MUT, rdn, rdnw, kvdif,    &
                                   ids, ide, jds, jde, kds, kde,  &
                                   ims, ime, jms, jme, kms, kme,  &
                                   its, ite, jts, jte, kts, kte  )


   IMPLICIT NONE
   
   ! Input data
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,    INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) ,                      &
                                               INTENT(IN   ) :: field,    &
                                                                alt

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: MUT

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: rdn,  &
                                                                  rdnw, &
                                                                  base

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: c1, c2

   REAL ,                                      INTENT(IN   ) :: kvdif
   
   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end

   REAL , DIMENSION(its:ite, 0:kte+1) :: vflux

   REAL :: rdz

   LOGICAL :: specified

!<DESCRIPTION>
!
!  vertical_diffusion_mp
!  computes vertical diffusion tendency of a perturbation variable
!  (field-base).  Note that base as a 1D (k) field.
!
!</DESCRIPTION>

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

   ktf=MIN(kte,kde-1)
   
     i_start = its
     i_end   = MIN(ite,ide-1)
     j_start = jts
     j_end   = MIN(jte,jde-1)

j_loop_s : DO j = j_start, j_end

     DO k=kts,ktf-1
       DO i = i_start, i_end
         vflux(i,k)=kvdif*rdn(k+1)/(0.5*(alt(i,k,j)+alt(i,k+1,j)))   &
                    *(field(i,k+1,j)-field(i,k,j)-base(k+1)+base(k))
       ENDDO
     ENDDO

     DO i = i_start, i_end
       vflux(i,0)=vflux(i,1)
     ENDDO

     DO i = i_start, i_end
       vflux(i,ktf)=0.
     ENDDO

     DO k=kts,ktf
       DO i = i_start, i_end
         tendency(i,k,j)=tendency(i,k,j)+g*g/(c1(k)*MUT(i,j)+c2(k))/alt(i,k,j)  &
                *rdnw(k)*(vflux(i,k)-vflux(i,k-1))
       ENDDO
     ENDDO

 ENDDO j_loop_s

END SUBROUTINE vertical_diffusion_mp


!-------------------------------------------------------------------------------

SUBROUTINE vertical_diffusion_3dmp ( field, tendency, config_flags, &
                                     base_3d, c1, c2,               &
                                     alt, MUT, rdn, rdnw, kvdif,    &
                                     ids, ide, jds, jde, kds, kde,  &
                                     ims, ime, jms, jme, kms, kme,  &
                                     its, ite, jts, jte, kts, kte  )


   IMPLICIT NONE
   
   ! Input data
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,    INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) ,                      &
                                               INTENT(IN   ) :: field,    &
                                                                alt,      &
                                                                base_3d

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: MUT

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: rdn,  &
                                                                  rdnw

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: c1, c2

   REAL ,                                      INTENT(IN   ) :: kvdif
   
   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end

   REAL , DIMENSION(its:ite, 0:kte+1) :: vflux

   REAL :: rdz

   LOGICAL :: specified

!<DESCRIPTION>
!
!  vertical_diffusion_3dmp
!  computes vertical diffusion tendency of a perturbation variable
!  (field-base_3d).  
!
!</DESCRIPTION>

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

   ktf=MIN(kte,kde-1)
   
     i_start = its
     i_end   = MIN(ite,ide-1)
     j_start = jts
     j_end   = MIN(jte,jde-1)

j_loop_s : DO j = j_start, j_end

     DO k=kts,ktf-1
       DO i = i_start, i_end
         vflux(i,k)=kvdif*rdn(k+1)/(0.5*(alt(i,k,j)+alt(i,k+1,j)))   &
                    *(   field(i,k+1,j)  -field(i,k,j)               &
                      -base_3d(i,k+1,j)+base_3d(i,k,j) )
       ENDDO
     ENDDO

     DO i = i_start, i_end
       vflux(i,0)=vflux(i,1)
     ENDDO

     DO i = i_start, i_end
       vflux(i,ktf)=0.
     ENDDO

     DO k=kts,ktf
       DO i = i_start, i_end
         tendency(i,k,j)=tendency(i,k,j)+g*g/(c1(k)*MUT(i,j)+c2(k))/alt(i,k,j)  &
                *rdnw(k)*(vflux(i,k)-vflux(i,k-1))
       ENDDO
     ENDDO

 ENDDO j_loop_s

END SUBROUTINE vertical_diffusion_3dmp


!-------------------------------------------------------------------------------


SUBROUTINE vertical_diffusion_u ( field, tendency,              &
                                  config_flags, u_base, c1h,c2h,&
                                  alt, muu, rdn, rdnw, kvdif,   &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  its, ite, jts, jte, kts, kte )


   IMPLICIT NONE
   
   ! Input data
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,    INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) ,                      &
                                               INTENT(IN   ) :: field,    &
                                                                alt

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: muu

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: rdn, rdnw, u_base

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: c1h, c2h

   REAL ,                                      INTENT(IN   ) :: kvdif
   
   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end

   REAL , DIMENSION(its:ite, 0:kte+1) :: vflux

   REAL :: rdz, zz

   LOGICAL :: specified

!<DESCRIPTION>
!
!  vertical_diffusion_u computes vertical diffusion tendency for
!  the u momentum equation.  This routine assumes a constant eddy
!  viscosity kvdif.
!
!</DESCRIPTION>

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

   ktf=MIN(kte,kde-1)

      i_start = its
      i_end   = ite
      j_start = jts
      j_end   = MIN(jte,jde-1)

      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-1,ite)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite


j_loop_u : DO j = j_start, j_end

     DO k=kts,ktf-1
       DO i = i_start, i_end
         vflux(i,k)=kvdif*rdn(k+1)/(0.25*( alt(i  ,k  ,j)      &
                                        +alt(i-1,k  ,j)      &
                                        +alt(i  ,k+1,j)      &
                                        +alt(i-1,k+1,j) ) )  &
                             *(field(i,k+1,j)-field(i,k,j)   &
                               -u_base(k+1)   +u_base(k)  )
       ENDDO
     ENDDO

     DO i = i_start, i_end
       vflux(i,0)=vflux(i,1)
     ENDDO

     DO i = i_start, i_end
       vflux(i,ktf)=0.
     ENDDO

     DO k=kts,ktf-1
       DO i = i_start, i_end
         tendency(i,k,j)=tendency(i,k,j)+                             &
                g*g*rdnw(k)/(c1h(k)*muu(i,j)+c2h(k))/(0.5*(alt(i-1,k,j)+alt(i,k,j)))* &
                              (vflux(i,k)-vflux(i,k-1))
       ENDDO
     ENDDO

 ENDDO j_loop_u
   
END SUBROUTINE vertical_diffusion_u

!-------------------------------------------------------------------------------


SUBROUTINE vertical_diffusion_v ( field, tendency,              &
                                  config_flags, v_base, c1h,c2h,&
                                  alt, muv, rdn, rdnw, kvdif,   &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  its, ite, jts, jte, kts, kte )


   IMPLICIT NONE
   
   ! Input data
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,    INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) ,                      &
                                               INTENT(IN   ) :: field,    &
                                                                alt
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: rdn, rdnw, v_base

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: c1h, c2h

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: muv

   REAL ,                                      INTENT(IN   ) :: kvdif
   
   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf, jm1
   INTEGER :: i_start, i_end, j_start, j_end

   REAL , DIMENSION(its:ite, 0:kte+1) :: vflux

   REAL :: rdz, zz

   LOGICAL :: specified

!<DESCRIPTION>
!
!  vertical_diffusion_v computes vertical diffusion tendency for
!  the v momentum equation.  This routine assumes a constant eddy
!  viscosity kvdif.
!
!</DESCRIPTION>

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

   ktf=MIN(kte,kde-1)
   
      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end   = MIN(jde-1,jte)

j_loop_v : DO j = j_start, j_end
!     jm1 = max(j-1,1)
     jm1 = j-1

     DO k=kts,ktf-1
       DO i = i_start, i_end
         vflux(i,k)=kvdif*rdn(k+1)/(0.25*( alt(i,k  ,j  )      &
                                        +alt(i,k  ,jm1)      &
                                        +alt(i,k+1,j  )      &
                                        +alt(i,k+1,jm1) ) )  &
                             *(field(i,k+1,j)-field(i,k,j)   &
                               -v_base(k+1)   +v_base(k)  )
       ENDDO
     ENDDO

     DO i = i_start, i_end
       vflux(i,0)=vflux(i,1)
     ENDDO

     DO i = i_start, i_end
       vflux(i,ktf)=0.
     ENDDO

     DO k=kts,ktf-1
       DO i = i_start, i_end
         tendency(i,k,j)=tendency(i,k,j)+                              &
                g*g*rdnw(k)/(c1h(k)*muv(i,j)+c2h(k))/(0.5*(alt(i,k,jm1)+alt(i,k,j)))*  &
                              (vflux(i,k)-vflux(i,k-1))
       ENDDO
     ENDDO

 ENDDO j_loop_v
   
END SUBROUTINE vertical_diffusion_v

!***************  end new mass coordinate routines

!-------------------------------------------------------------------------------

SUBROUTINE calculate_full ( rfield, rfieldb, rfieldp,     &
                            ids, ide, jds, jde, kds, kde, &
                            ims, ime, jms, jme, kms, kme, &
                            its, ite, jts, jte, kts, kte )

   IMPLICIT NONE
   
   ! Input data
   
   INTEGER ,      INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte
   
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: rfieldb, &
                                                                      rfieldp

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT  ) :: rfield
   
   ! Local indices.
   
   INTEGER :: i, j, k, itf, jtf, ktf
   
!<DESCRIPTION>
!
!  calculate_full
!  calculates full 3D field from pertubation and base field.
!
!</DESCRIPTION>

   itf=MIN(ite,ide-1)
   jtf=MIN(jte,jde-1)
   ktf=MIN(kte,kde-1)

   DO j=jts,jtf
   DO k=kts,ktf
   DO i=its,itf
      rfield(i,k,j)=rfieldb(i,k,j)+rfieldp(i,k,j)
   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE calculate_full

!------------------------------------------------------------------------------

SUBROUTINE coriolis ( ru, rv, rw, ru_tend, rv_tend, rw_tend, &
                      config_flags,                          &
                      msftx, msfty, msfux, msfuy,            &
                      msfvx, msfvy,                          &
                      f, e, sina, cosa, fzm, fzp,            &
                      ids, ide, jds, jde, kds, kde,          &
                      ims, ime, jms, jme, kms, kme,          &
                      its, ite, jts, jte, kts, kte          )

   IMPLICIT NONE
   
   ! Input data
   
   TYPE(grid_config_rec_type) ,           INTENT(IN   ) :: config_flags   

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: ru_tend, &
                                                                rv_tend, &
                                                                rw_tend
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: ru, &
                                                                rv, &
                                                                rw

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,      &
                                                                msfuy,      &
                                                                msfvx,      &
                                                                msfvy,      &
                                                                msftx,      &
                                                                msfty

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: f,    &
                                                                    e,    &
                                                                    sina, &
                                                                    cosa

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm, &
                                                                  fzp
   
   ! Local indices.
   
   INTEGER :: i, j , k, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   
   LOGICAL :: specified

!<DESCRIPTION>
!
!  coriolis calculates the large timestep tendency terms in the
!  u, v, and w momentum equations arise from the coriolis force.
!
!</DESCRIPTION>

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

   ktf=MIN(kte,kde-1)

! coriolis for u-momentum equation

!  Notes on map scale factor
!  cosa, sina are related to rotating the coordinate frame if desired
!  generally sina=0, cosa=1
!  ADT eqn 44, RHS terms 6 and 7: -2 mu w omega cos(lat)/my
!                                + 2 mu v omega sin(lat)/my
!  Define f=2 omega sin(lat), e=2 omega cos(lat)
!   => terms are: -e mu w / my + f mu v / my
!  rv = mu v / mx ; rw = mu w / my
!   => terms are: -e rw + f rv *mx / my

   i_start = its
   i_end   = ite
   IF ( config_flags%open_xs .or. specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. specified .or. &
        config_flags%nested) i_end   = MIN(ide-1,ite)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

   DO j = jts, MIN(jte,jde-1)

   DO k=kts,ktf
   DO i = i_start, i_end
   
     ru_tend(i,k,j)=ru_tend(i,k,j) + (msfux(i,j)/msfuy(i,j))*0.5*(f(i,j)+f(i-1,j)) &
       *0.25*(rv(i-1,k,j+1)+rv(i,k,j+1)+rv(i-1,k,j)+rv(i,k,j)) &
           - 0.5*(e(i,j)+e(i-1,j))*0.5*(cosa(i,j)+cosa(i-1,j)) &
       *0.25*(rw(i-1,k+1,j)+rw(i-1,k,j)+rw(i,k+1,j)+rw(i,k,j))

   ENDDO
   ENDDO

! boundary loops for coriolis not needed for open bdy  (commented out 20100611 JD)
!  IF ( (config_flags%open_xs) .and. (its == ids) ) THEN

!    DO k=kts,ktf
!  
!      ru_tend(its,k,j)=ru_tend(its,k,j) + (msfux(its,j)/msfuy(its,j))*0.5*(f(its,j)+f(its,j))   &
!        *0.25*(rv(its,k,j+1)+rv(its,k,j+1)+rv(its,k,j)+rv(its,k,j)) &
!            - 0.5*(e(its,j)+e(its,j))*0.5*(cosa(its,j)+cosa(its,j)) &
!        *0.25*(rw(its,k+1,j)+rw(its,k,j)+rw(its,k+1,j)+rw(its,k,j))

!    ENDDO

!  ENDIF

!  IF ( (config_flags%open_xe) .and. (ite == ide) ) THEN

!    DO k=kts,ktf
!  
!      ru_tend(ite,k,j)=ru_tend(ite,k,j) + (msfux(ite,j)/msfuy(ite,j))*0.5*(f(ite-1,j)+f(ite-1,j)) &
!        *0.25*(rv(ite-1,k,j+1)+rv(ite-1,k,j+1)+rv(ite-1,k,j)+rv(ite-1,k,j)) &
!            - 0.5*(e(ite-1,j)+e(ite-1,j))*0.5*(cosa(ite-1,j)+cosa(ite-1,j)) &
!        *0.25*(rw(ite-1,k+1,j)+rw(ite-1,k,j)+rw(ite-1,k+1,j)+rw(ite-1,k,j))

!    ENDDO

!  ENDIF

   ENDDO

!  coriolis term for v-momentum equation

!  Notes on map scale factors
!  ADT eqn 45, RHS terms 6 and 6b [0 for sina=0]: -2 mu u omega sin(lat)/mx + ?
!  Define f=2 omega sin(lat), e=2 omega cos(lat)
!   => terms are: -f mu u / mx
!  ru = mu u / my ; rw = mu w / my
!   => terms are: -f ru *my / mx + ?

   j_start = jts
   j_end   = jte

   IF ( config_flags%open_ys .or. specified .or. &
        config_flags%nested .or. config_flags%polar) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. specified .or. &
        config_flags%nested .or. config_flags%polar) j_end   = MIN(jde-1,jte)

! boundary loops for coriolis not needed for open bdy  (commented out 20100611 JD)
!  IF ( (config_flags%open_ys) .and. (jts == jds) ) THEN

!    DO k=kts,ktf
!    DO i=its,MIN(ide-1,ite)
!  
!       rv_tend(i,k,jts)=rv_tend(i,k,jts) - (msfvy(i,jts)/msfvx(i,jts))*0.5*(f(i,jts)+f(i,jts))    &
!        *0.25*(ru(i,k,jts)+ru(i+1,k,jts)+ru(i,k,jts)+ru(i+1,k,jts))   &
!            + (msfvy(i,jts)/msfvx(i,jts))*0.5*(e(i,jts)+e(i,jts))*0.5*(sina(i,jts)+sina(i,jts))   &
!            *0.25*(rw(i,k+1,jts)+rw(i,k,jts)+rw(i,k+1,jts)+rw(i,k,jts))

!    ENDDO
!    ENDDO

!  ENDIF

   DO j=j_start, j_end
   DO k=kts,ktf
   DO i=its,MIN(ide-1,ite)
   
      rv_tend(i,k,j)=rv_tend(i,k,j) - (msfvy(i,j)/msfvx(i,j))*0.5*(f(i,j)+f(i,j-1))    &
       *0.25*(ru(i,k,j)+ru(i+1,k,j)+ru(i,k,j-1)+ru(i+1,k,j-1)) &
           + (msfvy(i,j)/msfvx(i,j))*0.5*(e(i,j)+e(i,j-1))*0.5*(sina(i,j)+sina(i,j-1)) &
           *0.25*(rw(i,k+1,j-1)+rw(i,k,j-1)+rw(i,k+1,j)+rw(i,k,j))

   ENDDO
   ENDDO
   ENDDO


! boundary loops for coriolis not needed for open bdy  (commented out 20100611 JD)
!  IF ( (config_flags%open_ye) .and. (jte == jde) ) THEN

!    DO k=kts,ktf
!    DO i=its,MIN(ide-1,ite)
!  
!       rv_tend(i,k,jte)=rv_tend(i,k,jte) - (msfvy(i,jte)/msfvx(i,jte))*0.5*(f(i,jte-1)+f(i,jte-1))        &
!        *0.25*(ru(i,k,jte-1)+ru(i+1,k,jte-1)+ru(i,k,jte-1)+ru(i+1,k,jte-1))   &
!            + (msfvy(i,jte)/msfvx(i,jte))*0.5*(e(i,jte-1)+e(i,jte-1))*0.5*(sina(i,jte-1)+sina(i,jte-1))   &
!            *0.25*(rw(i,k+1,jte-1)+rw(i,k,jte-1)+rw(i,k+1,jte-1)+rw(i,k,jte-1))

!    ENDDO
!    ENDDO

!  ENDIF

! coriolis term for w-mometum

! Notes on map scale factors
! ADT eqn 46/my, RHS terms 5 and 5b [0 for sina=0]: 2 mu u omega cos(lat)/my +?
! Define e=2 omega cos(lat)
!  => terms are: e mu u / my + ???
! ru = mu u / my ; ru = mu v / mx
!  => terms are: e ru + ???

   DO j=jts,MIN(jte, jde-1)
   DO k=kts+1,ktf
   DO i=its,MIN(ite, ide-1)

       rw_tend(i,k,j)=rw_tend(i,k,j) + e(i,j)*           &
          (cosa(i,j)*0.5*(fzm(k)*(ru(i,k,j)+ru(i+1,k,j)) &
          +fzp(k)*(ru(i,k-1,j)+ru(i+1,k-1,j)))           &
          -(msftx(i,j)/msfty(i,j))*                      &
           sina(i,j)*0.5*(fzm(k)*(rv(i,k,j)+rv(i,k,j+1)) &
          +fzp(k)*(rv(i,k-1,j)+rv(i,k-1,j+1))))

   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE coriolis

!------------------------------------------------------------------------------

SUBROUTINE perturbation_coriolis ( ru_in, rv_in, rw, ru_tend, rv_tend, rw_tend, &
                                   config_flags,                                &
                                   u_base, v_base, z_base,                      &
                                   muu, muv, c1h, c2h, phb, ph,                 &
                                   msftx, msfty, msfux, msfuy, msfvx, msfvy,    &
                                   f, e, sina, cosa, fzm, fzp,                  &
                                   ids, ide, jds, jde, kds, kde,                &
                                   ims, ime, jms, jme, kms, kme,                &
                                   its, ite, jts, jte, kts, kte                )

   IMPLICIT NONE
   
   ! Input data
   
   TYPE(grid_config_rec_type) ,           INTENT(IN   ) :: config_flags   

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: ru_tend, &
                                                                rv_tend, &
                                                                rw_tend
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: ru_in, &
                                                                      rv_in, &
                                                                      rw,    &
                                                                      ph,    &
                                                                      phb


   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,      &
                                                                msfuy,      &
                                                                msfvx,      &
                                                                msfvy,      &
                                                                msftx,      &
                                                                msfty

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: f,    &
                                                                    e,    &
                                                                    sina, &
                                                                    cosa

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: muu, &
                                                                    muv
                                                                    

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm, &
                                                                  fzp

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: u_base,  &
                                                                  v_base,  &
                                                                  z_base

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: c1h, c2h
   
   ! Local storage

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) :: ru, &
                                                      rv

   REAL  :: z_at_u, z_at_v, wkp1, wk, wkm1

   ! Local indices.
   
   INTEGER :: i, j , k, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   
   LOGICAL :: specified

!<DESCRIPTION>
!
!  perturbation_coriolis calculates the large timestep tendency terms in the
!  u, v, and w momentum equations arise from the coriolis force.  This version
!  subtracts off the horizontal velocities from the initial sounding when
!  computing the forcing terms, hence "perturbation" coriolis.
!
!</DESCRIPTION>

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

   ktf=MIN(kte,kde-1)

! coriolis for u-momentum equation

   i_start = its
   i_end   = ite
   IF ( config_flags%open_xs .or. specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. specified .or. &
        config_flags%nested) i_end   = MIN(ide-1,ite)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

!  compute perturbation mu*v for use in u momentum equation

   DO j = jts, MIN(jte,jde-1)+1
   DO k=kts+1,ktf-1
   DO i = i_start-1, i_end
     z_at_v = 0.25*( phb(i,k,j  )+phb(i,k+1,j  )  &
                    +phb(i,k,j-1)+phb(i,k+1,j-1)  &
                    +ph(i,k,j  )+ph(i,k+1,j  )    &
                    +ph(i,k,j-1)+ph(i,k+1,j-1))/g
     wkp1 = min(1.,max(0.,z_at_v-z_base(k))/(z_base(k+1)-z_base(k)))
     wkm1 = min(1.,max(0.,z_base(k)-z_at_v)/(z_base(k)-z_base(k-1)))
     wk   = 1.-wkp1-wkm1
     rv(i,k,j) = rv_in(i,k,j) - (c1h(k)*muv(i,j)+c2h(k))*(            &
                                  wkm1*v_base(k-1)    &
                                 +wk  *v_base(k  )    &
                                 +wkp1*v_base(k+1)   )
   ENDDO
   ENDDO
   ENDDO


!  pick up top and bottom v

   DO j = jts, MIN(jte,jde-1)+1
   DO i = i_start-1, i_end

     k = kts
     z_at_v = 0.25*( phb(i,k,j  )+phb(i,k+1,j  )  &
                    +phb(i,k,j-1)+phb(i,k+1,j-1)  &
                    +ph(i,k,j  )+ph(i,k+1,j  )    &
                    +ph(i,k,j-1)+ph(i,k+1,j-1))/g
     wkp1 = min(1.,max(0.,z_at_v-z_base(k))/(z_base(k+1)-z_base(k)))
     wk   = 1.-wkp1
     rv(i,k,j) = rv_in(i,k,j) - (c1h(k)*muv(i,j)+c2h(k))*(            &
                                 +wk  *v_base(k  )    &
                                 +wkp1*v_base(k+1)   )

     k = ktf
     z_at_v = 0.25*( phb(i,k,j  )+phb(i,k+1,j  )  &
                    +phb(i,k,j-1)+phb(i,k+1,j-1)  &
                    +ph(i,k,j  )+ph(i,k+1,j  )    &
                    +ph(i,k,j-1)+ph(i,k+1,j-1))/g
     wkm1 = min(1.,max(0.,z_base(k)-z_at_v)/(z_base(k)-z_base(k-1)))
     wk   = 1.-wkm1
     rv(i,k,j) = rv_in(i,k,j) - (c1h(k)*muv(i,j)+c2h(k))*(            &
                                  wkm1*v_base(k-1)    &
                                 +wk  *v_base(k  )   )

   ENDDO
   ENDDO

!  compute coriolis forcing for u

!  Map scale factors: see comments above for Coriolis

   DO j = jts, MIN(jte,jde-1)

   DO k=kts,ktf
     DO i = i_start, i_end
       ru_tend(i,k,j)=ru_tend(i,k,j) + (msfux(i,j)/msfuy(i,j))*0.5*(f(i,j)+f(i-1,j)) &
         *0.25*(rv(i-1,k,j+1)+rv(i,k,j+1)+rv(i-1,k,j)+rv(i,k,j)) &
             - 0.5*(e(i,j)+e(i-1,j))*0.5*(cosa(i,j)+cosa(i-1,j)) &
         *0.25*(rw(i-1,k+1,j)+rw(i-1,k,j)+rw(i,k+1,j)+rw(i,k,j))
     ENDDO
   ENDDO

! boundary loops for perturbation coriolis is needed for open bdy  (20110225 JD)
   IF ( (config_flags%open_xs) .and. (its == ids) ) THEN

     DO k=kts,ktf
   
       ru_tend(its,k,j)=ru_tend(its,k,j) + (msfux(its,j)/msfuy(its,j))*0.5*(f(its,j)+f(its,j))   &
         *0.25*(rv(its,k,j+1)+rv(its,k,j+1)+rv(its,k,j)+rv(its,k,j)) &
             - 0.5*(e(its,j)+e(its,j))*0.5*(cosa(its,j)+cosa(its,j)) &
         *0.25*(rw(its,k+1,j)+rw(its,k,j)+rw(its,k+1,j)+rw(its,k,j))

     ENDDO

   ENDIF

   IF ( (config_flags%open_xe) .and. (ite == ide) ) THEN

     DO k=kts,ktf
   
       ru_tend(ite,k,j)=ru_tend(ite,k,j) + (msfux(ite,j)/msfuy(ite,j))*0.5*(f(ite-1,j)+f(ite-1,j)) &
         *0.25*(rv(ite-1,k,j+1)+rv(ite-1,k,j+1)+rv(ite-1,k,j)+rv(ite-1,k,j)) &
             - 0.5*(e(ite-1,j)+e(ite-1,j))*0.5*(cosa(ite-1,j)+cosa(ite-1,j)) &
         *0.25*(rw(ite-1,k+1,j)+rw(ite-1,k,j)+rw(ite-1,k+1,j)+rw(ite-1,k,j))

     ENDDO

   ENDIF

   ENDDO

!  coriolis term for v-momentum equation
!  Map scale factors: see comments above for Coriolis

   j_start = jts
   j_end   = jte

   IF ( config_flags%open_ys .or. specified .or. &
        config_flags%nested .or. config_flags%polar) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. specified .or. &
        config_flags%nested .or. config_flags%polar) j_end   = MIN(jde-1,jte)

!  compute perturbation mu*u for use in v momentum equation

   DO j = j_start-1,j_end
   DO k=kts+1,ktf-1
   DO i = its, MIN(ite,ide-1)+1
     z_at_u = 0.25*( phb(i  ,k,j)+phb(i  ,k+1,j)  &
                    +phb(i-1,k,j)+phb(i-1,k+1,j)  &
                    +ph(i  ,k,j)+ph(i  ,k+1,j)    &
                    +ph(i-1,k,j)+ph(i-1,k+1,j))/g
     wkp1 = min(1.,max(0.,z_at_u-z_base(k))/(z_base(k+1)-z_base(k)))
     wkm1 = min(1.,max(0.,z_base(k)-z_at_u)/(z_base(k)-z_base(k-1)))
     wk   = 1.-wkp1-wkm1
     ru(i,k,j) = ru_in(i,k,j) - (c1h(k)*muu(i,j)+c2h(k))*(            &
                                  wkm1*u_base(k-1)    &
                                 +wk  *u_base(k  )    &
                                 +wkp1*u_base(k+1)   )
   ENDDO
   ENDDO
   ENDDO

!  pick up top and bottom u

   DO j = j_start-1,j_end
   DO i = its, MIN(ite,ide-1)+1

     k = kts
     z_at_u = 0.25*( phb(i  ,k,j)+phb(i  ,k+1,j)  &
                    +phb(i-1,k,j)+phb(i-1,k+1,j)  &
                    +ph(i  ,k,j)+ph(i  ,k+1,j)    &
                    +ph(i-1,k,j)+ph(i-1,k+1,j))/g
     wkp1 = min(1.,max(0.,z_at_u-z_base(k))/(z_base(k+1)-z_base(k)))
     wk   = 1.-wkp1
     ru(i,k,j) = ru_in(i,k,j) - (c1h(k)*muu(i,j)+c2h(k))*(            &
                                 +wk  *u_base(k  )    &
                                 +wkp1*u_base(k+1)   )


     k = ktf
     z_at_u = 0.25*( phb(i  ,k,j)+phb(i  ,k+1,j)  &
                    +phb(i-1,k,j)+phb(i-1,k+1,j)  &
                    +ph(i  ,k,j)+ph(i  ,k+1,j)    &
                    +ph(i-1,k,j)+ph(i-1,k+1,j))/g
     wkm1 = min(1.,max(0.,z_base(k)-z_at_u)/(z_base(k)-z_base(k-1)))
     wk   = 1.-wkm1
     ru(i,k,j) = ru_in(i,k,j) - (c1h(k)*muu(i,j)+c2h(k))*(            &
                                  wkm1*u_base(k-1)    &
                                 +wk  *u_base(k  )   )

   ENDDO
   ENDDO

!  compute coriolis forcing for v momentum equation
!  Map scale factors: see comments above for Coriolis

! boundary loops for perturbation coriolis is needed for open bdy  (20110225 JD)
   IF ( (config_flags%open_ys) .and. (jts == jds) ) THEN

     DO k=kts,ktf
     DO i=its,MIN(ide-1,ite)
   
        rv_tend(i,k,jts)=rv_tend(i,k,jts) - (msfvy(i,jts)/msfvx(i,jts))*0.5*(f(i,jts)+f(i,jts))    &
         *0.25*(ru(i,k,jts)+ru(i+1,k,jts)+ru(i,k,jts)+ru(i+1,k,jts))   &
             + (msfvy(i,jts)/msfvx(i,jts))*0.5*(e(i,jts)+e(i,jts))*0.5*(sina(i,jts)+sina(i,jts))   &
             *0.25*(rw(i,k+1,jts)+rw(i,k,jts)+rw(i,k+1,jts)+rw(i,k,jts))

     ENDDO
     ENDDO

   ENDIF

   DO j=j_start, j_end
   DO k=kts,ktf
   DO i=its,MIN(ide-1,ite)
   
      rv_tend(i,k,j)=rv_tend(i,k,j) - (msfvy(i,j)/msfvx(i,j))*0.5*(f(i,j)+f(i,j-1))    &
       *0.25*(ru(i,k,j)+ru(i+1,k,j)+ru(i,k,j-1)+ru(i+1,k,j-1)) &
           + (msfvy(i,j)/msfvx(i,j))*0.5*(e(i,j)+e(i,j-1))*0.5*(sina(i,j)+sina(i,j-1)) &
           *0.25*(rw(i,k+1,j-1)+rw(i,k,j-1)+rw(i,k+1,j)+rw(i,k,j))

   ENDDO
   ENDDO
   ENDDO


! boundary loops for perturbation coriolis is needed for open bdy  (20110225 JD)
   IF ( (config_flags%open_ye) .and. (jte == jde) ) THEN

     DO k=kts,ktf
     DO i=its,MIN(ide-1,ite)
   
        rv_tend(i,k,jte)=rv_tend(i,k,jte) - (msfvy(i,jte)/msfvx(i,jte))*0.5*(f(i,jte-1)+f(i,jte-1))        &
         *0.25*(ru(i,k,jte-1)+ru(i+1,k,jte-1)+ru(i,k,jte-1)+ru(i+1,k,jte-1))   &
             + (msfvy(i,jte)/msfvx(i,jte))*0.5*(e(i,jte-1)+e(i,jte-1))*0.5*(sina(i,jte-1)+sina(i,jte-1))   &
             *0.25*(rw(i,k+1,jte-1)+rw(i,k,jte-1)+rw(i,k+1,jte-1)+rw(i,k,jte-1))

     ENDDO
     ENDDO

   ENDIF

! coriolis term for w-mometum
!  Map scale factors: see comments above for Coriolis

   DO j=jts,MIN(jte, jde-1)
   DO k=kts+1,ktf
   DO i=its,MIN(ite, ide-1)

       rw_tend(i,k,j)=rw_tend(i,k,j) + e(i,j)*           &
          (cosa(i,j)*0.5*(fzm(k)*(ru(i,k,j)+ru(i+1,k,j)) &
          +fzp(k)*(ru(i,k-1,j)+ru(i+1,k-1,j)))           &
          -(msftx(i,j)/msfty(i,j))*sina(i,j)*0.5*(fzm(k)*(rv(i,k,j)+rv(i,k,j+1)) &
          +fzp(k)*(rv(i,k-1,j)+rv(i,k-1,j+1))))

   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE perturbation_coriolis

!------------------------------------------------------------------------------

SUBROUTINE curvature ( ru, rv, rw, u, v, w, ru_tend, rv_tend, rw_tend, &
                        config_flags,                                       &
                        msfux, msfuy, msfvx, msfvy, msftx, msfty,       &
                        xlat, fzm, fzp, rdx, rdy,                       &
                        ids, ide, jds, jde, kds, kde,                   &
                        ims, ime, jms, jme, kms, kme,                   &
                        its, ite, jts, jte, kts, kte                   )


   IMPLICIT NONE
   
   ! Input data

   TYPE(grid_config_rec_type) ,           INTENT(IN   ) :: config_flags   

   INTEGER ,                  INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                               ims, ime, jms, jme, kms, kme, &
                                               its, ite, jts, jte, kts, kte
   
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) ,                     &
                                               INTENT(INOUT) :: ru_tend, &
                                                                rv_tend, &
                                                                rw_tend

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) ,                     &
                                               INTENT(IN   ) :: ru,      &
                                                                rv,      &
                                                                rw,      &
                                                                u,       &
                                                                v,       &
                                                                w

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,    &
                                                                msfuy,    &
                                                                msfvx,    &
                                                                msfvy,    &
                                                                msftx,    &
                                                                msfty,    &
                                                                xlat

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,     &
                                                                fzp

   REAL ,                                      INTENT(IN   ) :: rdx,     &
                                                                rdy
   
   ! Local data
   
!   INTEGER :: i, j, k, itf, jtf, ktf, kp1, im, ip, jm, jp
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
!   INTEGER :: irmin, irmax, jrmin, jrmax

   REAL , DIMENSION( its-1:ite , kts:kte, jts-1:jte ) :: vxgm

   LOGICAL :: specified

!<DESCRIPTION>
!
!  curvature calculates the large timestep tendency terms in the
!  u, v, and w momentum equations arise from the curvature terms.  
!
!</DESCRIPTION>

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      ktf=MIN(kte,kde-1)

!   irmin = ims
!   irmax = ime
!   jrmin = jms
!   jrmax = jme
!   IF ( config_flags%open_xs ) irmin = ids
!   IF ( config_flags%open_xe ) irmax = ide-1
!   IF ( config_flags%open_ys ) jrmin = jds
!   IF ( config_flags%open_ye ) jrmax = jde-1
   
! Define v cross grad m at scalar points - vxgm(i,j)

   i_start = its-1
   i_end   = ite
   j_start = jts-1
   j_end   = jte

   IF ( ( config_flags%open_xs .or. specified .or. &
        config_flags%nested) .and. (its == ids) ) i_start = its
   IF ( ( config_flags%open_xe .or. specified .or. &
        config_flags%nested) .and. (ite == ide) ) i_end   = ite-1
   IF ( ( config_flags%open_ys .or. specified .or. &
        config_flags%nested .or. config_flags%polar) .and. (jts == jds) ) j_start = jts
   IF ( ( config_flags%open_ye .or. specified .or. &
        config_flags%nested .or. config_flags%polar) .and. (jte == jde) ) j_end   = jte-1
      IF ( config_flags%periodic_x ) i_start = its-1
      IF ( config_flags%periodic_x ) i_end = ite

   DO j=j_start, j_end
   DO k=kts,ktf
   DO i=i_start, i_end
!     Map scale factor notes:
!     msf...y is constant everywhere for cylindrical map projection
!     msf...x varies with y only
!     But we know that this is not = 0 for cylindrical,
!     therefore use msfvX in 1st line
!     which => by symmetry use msfuY in 2nd line - ???  
      vxgm(i,k,j)=0.5*(u(i,k,j)+u(i+1,k,j))*(msfvx(i,j+1)-msfvx(i,j))*rdy - &
                  0.5*(v(i,k,j)+v(i,k,j+1))*(msfuy(i+1,j)-msfuy(i,j))*rdx
   ENDDO
   ENDDO
   ENDDO

!  Pick up the boundary rows for open (radiation) lateral b.c.
!  Rather crude at present, we are assuming there is no
!    variation in this term at the boundary.

   IF ( ( config_flags%open_xs .or. (specified .AND. .NOT. config_flags%periodic_x) .or. &
        config_flags%nested) .and. (its == ids) ) THEN

     DO j = jts, jte-1
     DO k = kts, ktf
       vxgm(its-1,k,j) =  vxgm(its,k,j)
     ENDDO
     ENDDO

   ENDIF

   IF ( ( config_flags%open_xe .or. (specified .AND. .NOT. config_flags%periodic_x) .or. &
        config_flags%nested) .and. (ite == ide) ) THEN

     DO j = jts, jte-1
     DO k = kts, ktf
       vxgm(ite,k,j) =  vxgm(ite-1,k,j)
     ENDDO
     ENDDO

   ENDIF

!  Polar boundary condition:
!  The following change is needed in case one tries using the vxgm route with
!  polar B.C.'s in the future, but not needed if 'tan' used
   IF ( ( config_flags%open_ys .or. specified .or. &
        config_flags%nested .or. config_flags%polar) .and. (jts == jds) ) THEN

     DO k = kts, ktf
     DO i = its-1, ite
       vxgm(i,k,jts-1) =  vxgm(i,k,jts)
     ENDDO
     ENDDO

   ENDIF

!  Polar boundary condition:
!  The following change is needed in case one tries using the vxgm route with
!  polar B.C.'s in the future, but not needed if 'tan' used
   IF ( ( config_flags%open_ye .or. specified .or. &
        config_flags%nested .or. config_flags%polar) .and. (jte == jde) ) THEN

     DO k = kts, ktf
     DO i = its-1, ite
       vxgm(i,k,jte) =  vxgm(i,k,jte-1)
     ENDDO
     ENDDO

   ENDIF

!  curvature term for u momentum eqn.

!  Map scale factor notes:
!  ADT eqn 44, RHS terms 4 and 5, in cylindrical: mu u v tan(lat)/(a my)
!                                               - mu u w /(a my)
!  ru = mu u / my ; rw = mu w / my ; rv = mu v / mx
!   => terms are:
!  (mx/my)*u rv tan(lat) / a - u rw / a = (u/a)*[(mx/my) rv tan(lat) - rw]
!  ru v tan(lat) / a - u rw / a
!  xlat defined with end points half grid space from pole,
!  hence are on u latitude points

   i_start = its
   IF ( config_flags%open_xs .or. specified .or. &
        config_flags%nested) i_start = MAX ( ids+1 , its )
   IF ( config_flags%open_xe .or. specified .or. &
        config_flags%nested) i_end   = MIN ( ide-1 , ite )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

!  Polar boundary condition
   IF ((config_flags%map_proj == 6) .OR. (config_flags%polar)) THEN

      DO j=jts,MIN(jde-1,jte)
      DO k=kts,ktf
      DO i=i_start,i_end

            ru_tend(i,k,j)=ru_tend(i,k,j) + u(i,k,j)*reradius*                 ( &
                        (msfux(i,j)/msfuy(i,j))*0.25*(rv(i-1,k,j+1)+rv(i,k,j+1)+ &
                                    rv(i-1,k,j)+rv(i,k,j))*tan(xlat(i,j)*degrad) &
                        - 0.25*(rw(i-1,k+1,j)+rw(i-1,k,j)+rw(i,k+1,j)+rw(i,k,j)) )
      ENDDO
      ENDDO
      ENDDO

   ELSE  ! normal code


      DO j=jts,MIN(jde-1,jte)
      DO k=kts,ktf
      DO i=i_start,i_end

         ru_tend(i,k,j)=ru_tend(i,k,j) + 0.5*(vxgm(i,k,j)+vxgm(i-1,k,j)) &
                 *0.25*(rv(i-1,k,j+1)+rv(i,k,j+1)+rv(i-1,k,j)+rv(i,k,j)) &
                  - u(i,k,j)*reradius &
                 *0.25*(rw(i-1,k+1,j)+rw(i-1,k,j)+rw(i,k+1,j)+rw(i,k,j))

      ENDDO
      ENDDO
      ENDDO

   END IF

!  curvature term for v momentum eqn.

!  Map scale factor notes
!  ADT eqn 45, RHS terms 4 and 5, in cylindrical:  - mu u*u tan(lat)/(a mx)
!                                               - mu v w /(a mx)
!  ru = mu u / my ; rw = mu w / my ; rv = mu v / mx
!  terms are:
!  - (my/mx)*u ru tan(lat) / a - (my/mx)*v rw / a
!  = - [my/(mx*a)]*[u ru tan(lat) + v rw]
!  - (1/a)*[(my/mx)*u ru tan(lat) + w rv]
!  xlat defined with end points half grid space from pole, hence are on
!  u latitude points => av here
!
!  in original wrf, there was a sign error for the rw contribution

   j_start = jts
   IF ( config_flags%open_ys .or. specified .or. &
        config_flags%nested .or. config_flags%polar) j_start = MAX ( jds+1 , jts )
   IF ( config_flags%open_ye .or. specified .or. &
        config_flags%nested .or. config_flags%polar) j_end   = MIN ( jde-1 , jte )

   IF ((config_flags%map_proj == 6) .OR. (config_flags%polar)) THEN

      DO j=j_start,j_end
      DO k=kts,ktf
      DO i=its,MIN(ite,ide-1)
            rv_tend(i,k,j)=rv_tend(i,k,j) - (msfvy(i,j)/msfvx(i,j))*reradius*   (  &
                        0.25*(u(i,k,j)+u(i+1,k,j)+u(i,k,j-1)+u(i+1,k,j-1))*     &
                        tan((xlat(i,j)+xlat(i,j-1))*0.5*degrad)*                &
                        0.25*(ru(i,k,j)+ru(i+1,k,j)+ru(i,k,j-1)+ru(i+1,k,j-1))  &
                       + v(i,k,j)*0.25*(rw(i,k+1,j-1)+rw(i,k,j-1)+              &
                                                      rw(i,k+1,j)+rw(i,k,j))    )
      ENDDO
      ENDDO
      ENDDO

   ELSE  ! normal code

      DO j=j_start,j_end
      DO k=kts,ktf
      DO i=its,MIN(ite,ide-1)

         rv_tend(i,k,j)=rv_tend(i,k,j) - 0.5*(vxgm(i,k,j)+vxgm(i,k,j-1)) &
                 *0.25*(ru(i,k,j)+ru(i+1,k,j)+ru(i,k,j-1)+ru(i+1,k,j-1)) &
                       - (msfvy(i,j)/msfvx(i,j))*v(i,k,j)*reradius       &
                 *0.25*(rw(i,k+1,j-1)+rw(i,k,j-1)+rw(i,k+1,j)+rw(i,k,j))

      ENDDO
      ENDDO
      ENDDO

   END IF

!  curvature term for vertical momentum eqn.

!  Notes on map scale factors:
!  ADT eqn 46, RHS term 4: [mu/(a my)]*[u*u + v*v]
!  ru = mu u / my ; rw = mu w / my ; rv = mu v / mx
!  terms are: u ru / a + (mx/my)v rv / a

   DO j=jts,MIN(jte,jde-1)
   DO k=MAX(2,kts),ktf
   DO i=its,MIN(ite,ide-1)

      rw_tend(i,k,j)=rw_tend(i,k,j) + reradius*                              &
    (0.5*(fzm(k)*(ru(i,k,j)+ru(i+1,k,j))+fzp(k)*(ru(i,k-1,j)+ru(i+1,k-1,j))) &
    *0.5*(fzm(k)*( u(i,k,j) +u(i+1,k,j))+fzp(k)*( u(i,k-1,j) +u(i+1,k-1,j)))     &
    +(msftx(i,j)/msfty(i,j))*0.5*(fzm(k)*(rv(i,k,j)+rv(i,k,j+1))+fzp(k)*(rv(i,k-1,j)+rv(i,k-1,j+1))) &
    *0.5*(fzm(k)*( v(i,k,j) +v(i,k,j+1))+fzp(k)*( v(i,k-1,j) +v(i,k-1,j+1))))

   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE curvature

#if 0
DANGER - this is a bad routine to have laying around - someone could use it
!------------------------------------------------------------------------------

SUBROUTINE decouple ( rr, rfield, field, name, config_flags, &
                      fzm, fzp,                          &
                      ids, ide, jds, jde, kds, kde,      &
                      ims, ime, jms, jme, kms, kme,      &
                      its, ite, jts, jte, kts, kte      )

   IMPLICIT NONE

   ! Input data

   TYPE(grid_config_rec_type) ,           INTENT(IN   ) :: config_flags   

   INTEGER ,                                   INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                                                ims, ime, jms, jme, kms, kme, &
                                                                its, ite, jts, jte, kts, kte

   CHARACTER(LEN=1) ,                          INTENT(IN   ) :: name

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: rfield

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: rr
   
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT  ) :: field
   
   REAL , DIMENSION( kms:kme ) , INTENT(IN   ) :: fzm, fzp
   
   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   
!<DESCRIPTION>
!
!  decouple decouples a variable from the column dry-air mass.
!
!</DESCRIPTION>

   ktf=MIN(kte,kde-1)
   
   IF (name .EQ. 'u')THEN
      itf=ite
      jtf=MIN(jte,jde-1)

      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         field(i,k,j)=rfield(i,k,j)/(0.5*(rr(i,k,j)+rr(i-1,k,j)))
      ENDDO
      ENDDO
      ENDDO

   ELSE IF (name .EQ. 'v')THEN
      itf=MIN(ite,ide-1)
      jtf=jte

      DO j=jts,jtf
      DO k=kts,ktf
        DO i=its,itf
             field(i,k,j)=rfield(i,k,j)/(0.5*(rr(i,k,j)+rr(i,k,j-1)))
        ENDDO
      ENDDO
      ENDDO

   ELSE IF (name .EQ. 'w')THEN
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      DO j=jts,jtf
      DO k=kts+1,ktf
      DO i=its,itf
         field(i,k,j)=rfield(i,k,j)/(fzm(k)*rr(i,k,j)+fzp(k)*rr(i,k-1,j))
      ENDDO
      ENDDO
      ENDDO

      DO j=jts,jtf
      DO i=its,itf
        field(i,kte,j) = 0.
      ENDDO
      ENDDO

   ELSE
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
   ! For theta we will decouple tb and tp and add them to give t afterwards
      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         field(i,k,j)=rfield(i,k,j)/rr(i,k,j)
      ENDDO
      ENDDO
      ENDDO
   
   ENDIF

END SUBROUTINE decouple
#endif

!-------------------------------------------------------------------------------

SUBROUTINE zero_tend ( tendency,                     &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )


   IMPLICIT NONE
   
   ! Input data
   
   INTEGER ,                                   INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                                                ims, ime, jms, jme, kms, kme, &
                                                                its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf

!<DESCRIPTION>
!
!  zero_tend sets the input tendency array to zero.
!
!</DESCRIPTION>

      DO j = jts, jte
      DO k = kts, kte
      DO i = its, ite
        tendency(i,k,j) = 0.
      ENDDO
      ENDDO
      ENDDO

      END SUBROUTINE zero_tend

!-------------------------------------------------------------------------------

SUBROUTINE zero_tend2d( tendency,                     &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )


   IMPLICIT NONE
   
   ! Input data
   
   INTEGER ,                                   INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                                                ims, ime, jms, jme, kms, kme, &
                                                                its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: tendency

   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf

!<DESCRIPTION>
!
!  zero_tend sets the input tendency array to zero.
!
!</DESCRIPTION>

      DO j = jts, jte
      DO i = its, ite
        tendency(i,j) = 0.
      ENDDO
      ENDDO

      END SUBROUTINE zero_tend2d

!-------------------------------------------------------------------------------

! Sets the an array on the polar v point(s) to zero
SUBROUTINE zero_pole ( field,                        &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )


  IMPLICIT NONE

  ! Input data
   
  INTEGER , INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             its, ite, jts, jte, kts, kte

  REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: field

  ! Local data

  INTEGER :: i, k

  IF (jts == jds) THEN
     DO k = kts, kte
     DO i = its-1, ite+1
        field(i,k,jts) = 0.
     END DO
     END DO
  END IF
  IF (jte == jde) THEN
     DO k = kts, kte
     DO i = its-1, ite+1
        field(i,k,jte) = 0.
     END DO
     END DO
  END IF

END SUBROUTINE zero_pole

!-------------------------------------------------------------------------------
! Sets the an array on the polar v point(s)
SUBROUTINE pole_point_bc ( field,                        &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )


  IMPLICIT NONE

  ! Input data
   
  INTEGER , INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             its, ite, jts, jte, kts, kte

  REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: field

  ! Local data

  INTEGER :: i, k

  IF (jts == jds) THEN
     DO k = kts, kte
     DO i = its, ite
!        field(i,k,jts) = 2*field(i,k,jts+1) - field(i,k,jts+2)
        field(i,k,jts) = field(i,k,jts+1)
     END DO
     END DO
  END IF
  IF (jte == jde) THEN
     DO k = kts, kte
     DO i = its, ite
!        field(i,k,jte) = 2*field(i,k,jte-1) - field(i,k,jte-2)
        field(i,k,jte) = field(i,k,jte-1)
     END DO
     END DO
  END IF

END SUBROUTINE pole_point_bc

!======================================================================
!   physics prep routines
!======================================================================

   SUBROUTINE phy_prep ( config_flags,                                &  ! input
                         mut, muu, muv,                               &
                         c1h, c2h, c1f, c2f,                          &
                         u, v, p, pb, alt, ph,                        &  ! input
                         phb, t, moist, n_moist,                      &  ! input
                         rho, th_phy, th_phy_m_t0,                    &  ! output
                         p_phy , pi_phy ,                             &  ! output
                         u_phy, v_phy, p8w, t_phy, t8w,               &  ! output
                         z, z_at_w, dz8w,                             &  ! output
                         p_hyd, p_hyd_w, dnw,                         &  ! output
                         fzm, fzp, znw, p_top,                        &  ! params
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


   REAL , DIMENSION( ims:ime, jms:jme ), INTENT(IN   )   ::     mut, muu, muv

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) ,                 &
          INTENT(  OUT)                                  ::   u_phy, &
                                                              v_phy, &
                                                             pi_phy, &
                                                              p_phy, &
                                                                p8w, &
                                                              t_phy, &
                                                             th_phy, &
                                                        th_phy_m_t0, &
                                                                t8w, &
                                                                rho, &
                                                                  z, &
                                                               dz8w, &
                                                              z_at_w

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) ,                 &
          INTENT(  OUT)                                  ::   p_hyd, &
                                                              p_hyd_w

   REAL , INTENT(IN   )                                  ::   p_top

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

   REAL , DIMENSION( kms:kme ) ,           INTENT(IN   ) ::     znw, &
                                                                dnw

   REAL, DIMENSION( kms:kme ) ,            INTENT(IN   ) ::     c1h, c2h, c1f, c2f

   REAL, DIMENSION( kms:kme ) :: c1, c2
   INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end, i_startu, j_startv
   INTEGER :: i, j, k
   REAL    :: w1, w2, z0, z1, z2
   REAL    :: qtot
   INTEGER :: n

!-----------------------------------------------------------------------

!<DESCRIPTION>
!
!  phys_prep calculates a number of diagnostic quantities needed by
!  the physics routines.  
!
!</DESCRIPTION>

    c1 = c1h
    c2 = c2h
!  set up loop bounds for this grid's boundary conditions

    i_start = its
    i_end   = min( ite,ide-1 )
    j_start = jts
    j_end   = min( jte,jde-1 )

    k_start = kts
    k_end = min( kte, kde-1 )

!  compute thermodynamics and velocities at pressure points (or half levels)

    IF ( ( config_flags%use_theta_m .EQ. 1 ) .AND. (P_Qv .GE. PARAM_FIRST_SCALAR) ) THEN
       do j = j_start,j_end
       do k = k_start, k_end
       do i = i_start, i_end
         th_phy(i,k,j) = (t(i,k,j)+t0)/(1.+R_v/R_d*moist(i,k,j,P_QV))
       enddo
       enddo
       enddo
    ELSE
       do j = j_start,j_end
       do k = k_start, k_end
       do i = i_start, i_end
         th_phy(i,k,j) =  t(i,k,j)+t0
       enddo
       enddo
       enddo
    END IF

    do j = j_start,j_end
    do k = k_start, k_end
    do i = i_start, i_end

      th_phy_m_t0(i,k,j) = th_phy(i,k,j)-t0
      p_phy(i,k,j) = p(i,k,j) + pb(i,k,j)
      pi_phy(i,k,j) = (p_phy(i,k,j)/p1000mb)**rcp
      t_phy(i,k,j) = th_phy(i,k,j)*pi_phy(i,k,j)
      rho(i,k,j) = 1./alt(i,k,j)*(1.+moist(i,k,j,P_QV))
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

!  compute z at p points or half levels (average of z at full levels)

    do j = j_start,j_end
    do k = k_start, k_end
    do i = i_start, i_end
      z(i,k,j) = 0.5*(z_at_w(i,k,j) +z_at_w(i,k+1,j) )
    enddo
    enddo
    enddo

!  interp t and p to full levels

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

! calculate hydrostatic pressure at both full and half levels
! first, full level p: assuming dry over model top

    do j = j_start,j_end
    do i = i_start, i_end
       p_hyd_w(i,kte,j) = p_top
    enddo
    enddo

    do j = j_start,j_end
    do k = kte-1, k_start, -1
    do i = i_start, i_end
       qtot = 0.
       do n = PARAM_FIRST_SCALAR,n_moist
              qtot = qtot + moist(i,k,j,n)
       enddo
       p_hyd_w(i,k,j) = p_hyd_w(i,k+1,j) - (1.+qtot)*(c1(k)*MUT(i,j)+c2(k))*dnw(k)
!      p_hyd_w(i,k,j) = p_hyd_w(i,k+1,j)+1./alt(i,k,j)*(1.+moist(i,k,j,P_QV))*g*dz8w(i,k,j)
    enddo
    enddo
    enddo

! now calculate hydrostatic pressure at half levels

    do j = j_start,j_end
    do k = k_start, k_end
    do i = i_start, i_end
       p_hyd(i,k,j) = 0.5*(p_hyd_w(i,k,j)+p_hyd_w(i,k+1,j))
    enddo
    enddo
    enddo

END SUBROUTINE phy_prep


!======================================================================
!   routine to decouple physics tendencies
!======================================================================

   SUBROUTINE phy_prep_part2 ( config_flags,                          &
                         mut,muu,muv,                                 &
                         c1h, c2h, c1f, c2f,                          &
                         RTHRATEN,                                    &
                         RTHBLTEN, RUBLTEN, RVBLTEN,                  &
                         RQVBLTEN, RQCBLTEN, RQIBLTEN,                &
                         RUCUTEN,  RVCUTEN,  RTHCUTEN,                &
                         RQVCUTEN, RQCCUTEN, RQRCUTEN,                &
                         RQICUTEN, RQSCUTEN,                          &
                         RUSHTEN,  RVSHTEN,  RTHSHTEN,                &
                         RQVSHTEN, RQCSHTEN, RQRSHTEN,                &
                         RQISHTEN, RQSSHTEN, RQGSHTEN,                &
                         RTHFTEN,  RQVFTEN,                           &
                         RUNDGDTEN, RVNDGDTEN, RTHNDGDTEN,            &
                         RPHNDGDTEN,RQVNDGDTEN, RMUNDGDTEN,           &
                         t_new, th_phy, qv,                           &
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

   REAL , DIMENSION( ims:ime, jms:jme ), INTENT(IN   )   ::     mut, muu, muv

   REAL,  DIMENSION( ims:ime , kms:kme, jms:jme ),                   &
          INTENT(INOUT)   ::                               RTHRATEN  

   REAL,  DIMENSION( ims:ime , kms:kme, jms:jme ),                   &
          INTENT(INOUT)   ::                                RUCUTEN, &
                                                            RVCUTEN, &
                                                           RTHCUTEN, &
                                                           RQVCUTEN, &
                                                           RQCCUTEN, &
                                                           RQRCUTEN, &
                                                           RQICUTEN, &
                                                           RQSCUTEN, &
                                                            RUSHTEN, &
                                                            RVSHTEN, &
                                                           RTHSHTEN, &
                                                           RQVSHTEN, &
                                                           RQCSHTEN, &
                                                           RQRSHTEN, &
                                                           RQISHTEN, &
                                                           RQSSHTEN, &
                                                           RQGSHTEN

   REAL,  DIMENSION( ims:ime, kms:kme, jms:jme )                   , &
          INTENT(INOUT)   ::                                RUBLTEN, &
                                                            RVBLTEN, &
                                                           RTHBLTEN, &
                                                           RQVBLTEN, &
                                                           RQCBLTEN, &
                                                           RQIBLTEN

   REAL,  DIMENSION( ims:ime, kms:kme, jms:jme )                   , &
          INTENT(INOUT)   ::                                RTHFTEN, &
                                                            RQVFTEN

   REAL,  DIMENSION( ims:ime, kms:kme, jms:jme )                   , &
          INTENT(INOUT)   ::                                RUNDGDTEN, &
                                                            RVNDGDTEN, &
                                                           RTHNDGDTEN, &
                                                           RPHNDGDTEN, &
                                                           RQVNDGDTEN

   REAL,  DIMENSION( ims:ime, jms:jme )                            , &
          INTENT(INOUT)   ::                               RMUNDGDTEN

   REAL,  DIMENSION( ims:ime, kms:kme, jms:jme )                   , &
          INTENT(IN   )   ::                                t_new, qv   

   REAL,  DIMENSION( ims:ime, kms:kme, jms:jme )                   , &
          INTENT(INOUT)   ::                                th_phy

   REAL, DIMENSION( kms:kme ) ,            INTENT(IN   ) ::     c1h, c2h, c1f, c2f

   REAL, DIMENSION( kms:kme ) :: c1, c2

   INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end, i_startu, j_startv
   INTEGER :: i, j, k

!-----------------------------------------------------------------------

!<DESCRIPTION>
!
!  It decouples the physics tendencies from
!  the column dry-air mass (the physics routines expect to see/update the
!  uncoupled tendencies).
!
!</DESCRIPTION>

!  set up loop bounds for this grid's boundary conditions

    i_start = its
    i_end   = min( ite,ide-1 )
    j_start = jts
    j_end   = min( jte,jde-1 )

    k_start = kts
    k_end = min( kte, kde-1 )

    c1 = c1h
    c2 = c2h

! decouple all physics tendencies

   IF (config_flags%ra_lw_physics .gt. 0 .or. config_flags%ra_sw_physics .gt. 0) THEN

      DO J=j_start,j_end
      DO K=k_start,k_end
      DO I=i_start,i_end
         RTHRATEN(I,K,J)=RTHRATEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
      ENDDO
      ENDDO
      ENDDO

   ENDIF

   IF (config_flags%cu_physics .gt. 0) THEN

      DO J=j_start,j_end
      DO I=i_start,i_end
      DO K=k_start,k_end
         RUCUTEN(I,K,J) =RUCUTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         RVCUTEN(I,K,J) =RVCUTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         RTHCUTEN(I,K,J)=RTHCUTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
      ENDDO
      ENDDO
      ENDDO

      IF (P_QV .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQVCUTEN(I,K,J)=RQVCUTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF (P_QC .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQCCUTEN(I,K,J)=RQCCUTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF (P_QR .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQRCUTEN(I,K,J)=RQRCUTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF (P_QI .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQICUTEN(I,K,J)=RQICUTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF(P_QS .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQSCUTEN(I,K,J)=RQSCUTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF

   ENDIF

   IF (config_flags%shcu_physics .gt. 0) THEN

      DO J=j_start,j_end
      DO I=i_start,i_end
      DO K=k_start,k_end
         RUSHTEN(I,K,J) =RUSHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         RVSHTEN(I,K,J) =RVSHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         RTHSHTEN(I,K,J)=RTHSHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
      ENDDO
      ENDDO
      ENDDO

      IF (P_QV .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQVSHTEN(I,K,J)=RQVSHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF (P_QC .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQCSHTEN(I,K,J)=RQCSHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF (P_QR .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQRSHTEN(I,K,J)=RQRSHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF (P_QI .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQISHTEN(I,K,J)=RQISHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF(P_QS .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQSSHTEN(I,K,J)=RQSSHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF(P_QG .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
         DO K=k_start,k_end
            RQGSHTEN(I,K,J)=RQGSHTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF

   ENDIF

   IF (config_flags%bl_pbl_physics .gt. 0) THEN

      DO J=j_start,j_end
      DO K=k_start,k_end
      DO I=i_start,i_end
         RUBLTEN(I,K,J) =RUBLTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         RVBLTEN(I,K,J) =RVBLTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         RTHBLTEN(I,K,J)=RTHBLTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
      ENDDO
      ENDDO
      ENDDO

      IF (P_QV .ge. PARAM_FIRST_SCALAR) THEN
         DO J=j_start,j_end
         DO K=k_start,k_end
         DO I=i_start,i_end
            RQVBLTEN(I,K,J)=RQVBLTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF (P_QC .ge. PARAM_FIRST_SCALAR) THEN
         DO J=j_start,j_end
         DO K=k_start,k_end
         DO I=i_start,i_end
           RQCBLTEN(I,K,J)=RQCBLTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF (P_QI .ge. PARAM_FIRST_SCALAR) THEN
         DO J=j_start,j_end
         DO K=k_start,k_end
         DO I=i_start,i_end
            RQIBLTEN(I,K,J)=RQIBLTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF

    ENDIF

!  decouple advective forcing required by a few CPS schemes

   IF(( config_flags%cu_physics == GDSCHEME )      .OR. &
      ( config_flags%cu_physics == GFSCHEME )      .OR. &
      ( config_flags%cu_physics == G3SCHEME )      .OR. &
      ( config_flags%cu_physics == KFETASCHEME )   .OR. &
      ( config_flags%cu_physics == MSKFSCHEME )    .OR. &
      ( config_flags%cu_physics == TIEDTKESCHEME ) .OR. &
      ( config_flags%cu_physics == NTIEDTKESCHEME )) THEN

      IF (P_QV .ge. PARAM_FIRST_SCALAR)THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
            DO K=k_start,k_end
               RQVFTEN(I,K,J)=RQVFTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
            ENDDO
         ENDDO
         ENDDO
      ENDIF

   END IF

   IF(( config_flags%cu_physics == GDSCHEME ) .OR.    &
      ( config_flags%cu_physics == GFSCHEME ) .OR.    &
      ( config_flags%cu_physics == G3SCHEME ) .OR.    &
      ( config_flags%cu_physics == NTIEDTKESCHEME )) THEN

      DO J=j_start,j_end
      DO I=i_start,i_end
         DO K=k_start,k_end
            RTHFTEN(I,K,J)=RTHFTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
      ENDDO
      ENDDO

!  If using moist theta, get dry theta tendency for CPSs
      IF ( config_flags%use_theta_m == 1 ) THEN
         DO J=j_start,j_end
         DO I=i_start,i_end
            DO K=k_start,k_end
               th_phy(i,k,j) = (t_new(i,k,j) + t0) / (1. + (R_v/R_d) * qv(i,k,j))
               rthften(i,k,j) = th_phy(i,k,j)/(t_new(i,k,j)+t0) *          &
                               (rthften(i,k,j) - (R_v/R_d) * th_phy(i,k,j) * rqvften(i,k,j))
            ENDDO
         ENDDO
         ENDDO
      END IF
   END IF

! fdda
! note fdda u and v tendencies are staggered, also only interior points have muu/muv,
!   so only decouple those

   IF (config_flags%grid_fdda .gt. 0) THEN

      i_startu=MAX(its,ids+1)
      j_startv=MAX(jts,jds+1)

      DO J=j_start,j_end
      DO K=k_start,k_end
      DO I=i_startu,i_end
         RUNDGDTEN(I,K,J) =RUNDGDTEN(I,K,J)/(c1h(k)*muu(I,J)+c2h(k))
      ENDDO
      ENDDO
      ENDDO
      DO J=j_startv,j_end
      DO K=k_start,k_end
      DO I=i_start,i_end
         RVNDGDTEN(I,K,J) =RVNDGDTEN(I,K,J)/(c1h(k)*muv(I,J)+c2h(k))
      ENDDO
      ENDDO
      ENDDO
      DO J=j_start,j_end
      DO K=k_start,k_end
      DO I=i_start,i_end
         RTHNDGDTEN(I,K,J)=RTHNDGDTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
!        RMUNDGDTEN(I,J) - no coupling
      ENDDO
      ENDDO
      ENDDO

      IF (config_flags%grid_fdda .EQ. 2) THEN
      DO J=j_start,j_end
      DO K=k_start,kte
      DO I=i_start,i_end
         RPHNDGDTEN(I,K,J)=RPHNDGDTEN(I,K,J)/(c1f(k)*mut(I,J)+c2f(k))
      ENDDO
      ENDDO
      ENDDO

      ELSE IF (config_flags%grid_fdda .GE. 1) THEN
      IF (P_QV .ge. PARAM_FIRST_SCALAR) THEN
         DO J=j_start,j_end
         DO K=k_start,k_end
         DO I=i_start,i_end
            RQVNDGDTEN(I,K,J)=RQVNDGDTEN(I,K,J)/(c1(k)*MUT(I,J)+c2(k))
         ENDDO
         ENDDO
         ENDDO
      ENDIF
      ENDIF

    ENDIF

END SUBROUTINE phy_prep_part2
!------------------------------------------------------------

   SUBROUTINE moist_physics_prep_em( t_new, t_old, t0, rho, al, alb, &
                                     p, p8w, p0, pb, ph, phb,        &
                                     th_phy, pii, pf,                &
                                     z, z_at_w, dz8w,                &
                                     dt,h_diabatic,                  &
                                     qv,qv_diabatic,                 &
                                     qc,qc_diabatic,                 &
                                     config_flags,fzm, fzp,          &
                                     ids,ide, jds,jde, kds,kde,      &
                                     ims,ime, jms,jme, kms,kme,      &
                                     its,ite, jts,jte, kts,kte      )

   IMPLICIT NONE

! Here we construct full fields
! needed by the microphysics

   TYPE(grid_config_rec_type),    INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
   INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
   INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

   REAL, INTENT(IN   )  ::  dt

   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),        &
         INTENT(IN   ) ::                           al,  &
                                                    alb, &
                                                    p,   &
                                                    pb,  &
                                                    ph,  &
                                                    phb, &
                                                    qv,  &
                                                    qc


   REAL , DIMENSION( kms:kme ) ,           INTENT(IN   ) ::   fzm, &
                                                              fzp

   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),       &
         INTENT(  OUT) ::                         rho,  &
                                               th_phy,  &
                                                  pii,  &
                                                  pf,   &
                                                    z,  &
                                               z_at_w,  &
                                                 dz8w,  &
                                                  p8w

   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),             &
         INTENT(INOUT) ::                         h_diabatic, &
                                                 qv_diabatic, &
                                                 qc_diabatic

   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),        &
         INTENT(INOUT) ::                         t_new, &
                                                  t_old

   REAL, INTENT(IN   ) :: t0, p0
   REAL                :: z0,z1,z2,w1,w2

   INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
   INTEGER :: i, j, k

!--------------------------------------------------------------------

!<DESCRIPTION>
!
!  moist_phys_prep_em calculates a number of diagnostic quantities needed by
!  the microphysics routines.
!
!</DESCRIPTION>

!  set up loop bounds for this grid's boundary conditions

    i_start = its    
    i_end   = min( ite,ide-1 )
    j_start = jts    
    j_end   = min( jte,jde-1 )

    k_start = kts
    k_end = min( kte, kde-1 )

     DO j = j_start, j_end
     DO k = k_start, kte
     DO i = i_start, i_end
       z_at_w(i,k,j) = (ph(i,k,j)+phb(i,k,j))/g
     ENDDO
     ENDDO
     ENDDO

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


           !  compute full pii, rho, and z at the new time-level
           !  (needed for physics).
           !  convert perturbation theta to full theta (th_phy)
           !  use h_diabatic to temporarily save pre-microphysics full theta

     IF ( ( config_flags%use_theta_m .EQ. 1 ) .AND. (P_Qv .GE. PARAM_FIRST_SCALAR) ) THEN
       DO j = j_start, j_end
       DO k = k_start, k_end
       DO i = i_start, i_end
           th_phy(i,k,j) = (t_new(i,k,j) + t0) / (1. + (R_v/R_d) * qv(i,k,j))
       ENDDO
       ENDDO
       ENDDO
     ELSE
       DO j = j_start, j_end
       DO k = k_start, k_end
       DO i = i_start, i_end
         th_phy(i,k,j) =  t_new(i,k,j) + t0
       ENDDO
       ENDDO
       ENDDO
     END IF

     DO j = j_start, j_end
     DO k = k_start, k_end
     DO i = i_start, i_end
       h_diabatic(i,k,j) = th_phy(i,k,j)
#if ( WRFPLUS == 1 )
       if ( P_QV >= PARAM_FIRST_SCALAR ) then
          qv_diabatic(i,k,j) = qv(i,k,j)
       else
          qv_diabatic(i,k,j) = 0.0
       end if
       if ( P_QC >= PARAM_FIRST_SCALAR ) then
          qc_diabatic(i,k,j) = qc(i,k,j)
       else
          qc_diabatic(i,k,j) = 0.0
       end if
#else
       qv_diabatic(i,k,j) = qv(i,k,j)
       qc_diabatic(i,k,j) = qc(i,k,j)
#endif
       rho(i,k,j)  = 1./(al(i,k,j)+alb(i,k,j))
       pii(i,k,j) = ((p(i,k,j)+pb(i,k,j))/p0)**rcp
       z(i,k,j) = 0.5*(z_at_w(i,k,j) +z_at_w(i,k+1,j) )
       pf(i,k,j) = p(i,k,j)+pb(i,k,j)

     ENDDO
     ENDDO
     ENDDO

!  interp p at w points

    do j = j_start,j_end
    do k = 2, k_end
    do i = i_start, i_end
      p8w(i,k,j) = fzm(k)*pf(i,k,j)+fzp(k)*pf(i,k-1,j)
    enddo
    enddo
    enddo

!  extrapolate p to surface and top.
!  we'll use an extrapolation in z for now

    do j = j_start,j_end
    do i = i_start, i_end

! bottom

      z0 = z_at_w(i,1,j)
      z1 = z(i,1,j)
      z2 = z(i,2,j)
      w1 = (z0 - z2)/(z1 - z2)
      w2 = 1. - w1
      p8w(i,1,j) = w1*pf(i,1,j)+w2*pf(i,2,j)

! top

      z0 = z_at_w(i,kte,j)
      z1 = z(i,k_end,j)
      z2 = z(i,k_end-1,j)
      w1 = (z0 - z2)/(z1 - z2)
      w2 = 1. - w1
!      p8w(i,kde,j) = w1*pf(i,kde-1,j)+w2*pf(i,kde-2,j)
      p8w(i,kde,j) = exp(w1*log(pf(i,kde-1,j))+w2*log(pf(i,kde-2,j)))

    enddo
    enddo

   END SUBROUTINE moist_physics_prep_em

!------------------------------------------------------------------------------

   SUBROUTINE moist_physics_finish_em( t_new, t_old, t0, mut,     &
                                       th_phy, h_diabatic, dt,    &
                                       qv,qv_diabatic,            &
                                       qc,qc_diabatic,            &
                                       th_phy_m_t0,               &
                                       config_flags,              &
#if ( WRF_DFI_RADAR == 1 )
                                       dfi_tten_rad,dfi_stage,    &
#endif
                                       ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte )

   IMPLICIT NONE

! Here we construct full fields
! needed by the microphysics

   TYPE(grid_config_rec_type),    INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
   INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
   INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),        &
         INTENT(INOUT) ::                         t_new, &
                                                  t_old, &
                                                 th_phy, &
                                                  h_diabatic, &
                                                 qv_diabatic, &
                                                 qc_diabatic

   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),        &
         INTENT(  OUT) ::                         th_phy_m_t0

   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),        &
         INTENT(IN   ) ::                           qv,  &
                                                    qc

#if ( WRF_DFI_RADAR == 1 )
   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),        &
         INTENT(IN), OPTIONAL ::               dfi_tten_rad
   INTEGER,      INTENT(IN   ) ,OPTIONAL   :: dfi_stage
   REAL :: dfi_tten_max, old_max
#endif

   REAL mpten, mptenmax, mptenmin
   REAL :: qvten,qcten

   REAL, DIMENSION( ims:ime , jms:jme ),  INTENT(INOUT) ::  mut


   REAL, INTENT(IN   ) :: t0, dt

   INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
   INTEGER :: i, j, k, imax, jmax, imin, jmin

!--------------------------------------------------------------------

!<DESCRIPTION>
!
!  moist_phys_finish_em resets theta to its perturbation value and
!  computes and stores the microphysics diabatic heating term.
!
!</DESCRIPTION>

!  set up loop bounds for this grid's boundary conditions


    i_start = its    
    i_end   = min( ite,ide-1 )
    j_start = jts    
    j_end   = min( jte,jde-1 )
    k_start = kts
    k_end = min( kte, kde-1 )

#if ( WRF_DFI_RADAR == 1 )
         IF ( PRESENT(dfi_stage) .and.  PRESENT(dfi_tten_rad) ) THEN
            IF ( dfi_stage ==DFI_FWD ) THEN
               WRITE(wrf_err_message,*)'Add radar tendency: i_start,j_start: ', i_start, j_start
               CALL wrf_debug ( 100 , TRIM(wrf_err_message) )
            ENDIF
         ENDIF
     dfi_tten_max=-999
     old_max=-999
#endif

!  add microphysics theta diff to perturbation theta, set h_diabatic

     IF ( config_flags%no_mp_heating .eq. 0 ) THEN
       mptenmax = 0.
       mptenmin = 999.
     DO j = j_start, j_end
     DO k = k_start, k_end
     DO i = i_start, i_end
          mpten = th_phy(i,k,j)-h_diabatic(i,k,j)
#if ( WRFPLUS == 1 )
          if ( P_QV >= PARAM_FIRST_SCALAR ) qvten = qv(i,k,j)-qv_diabatic(i,k,j)
          if ( P_QC >= PARAM_FIRST_SCALAR ) qcten = qc(i,k,j)-qc_diabatic(i,k,j)
#else
          qvten = qv(i,k,j)-qv_diabatic(i,k,j)
          qcten = qc(i,k,j)-qc_diabatic(i,k,j)
#endif
       if(mpten.gt.mptenmax) then
          mptenmax=mpten
          imax=i
          jmax=j
       endif
       if(mpten.lt.mptenmin) then
          mptenmin=mpten
          imin=i
          jmin=j
       endif
          mpten=min(config_flags%mp_tend_lim*dt, mpten)
          mpten=max(-config_flags%mp_tend_lim*dt, mpten)

#if ( WRF_DFI_RADAR == 1 )
compiler error, not handled yet
       if(k < k_end ) then
         if(dfi_tten_max < dfi_tten_rad(i,k,j) ) dfi_tten_max = dfi_tten_rad(i,k,j)
         if(old_max < (th_phy(i,k,j)-h_diabatic(i,k,j)) ) old_max=th_phy(i,k,j)-h_diabatic(i,k,j)
       endif

       IF ( PRESENT(dfi_stage) .and. PRESENT(dfi_tten_rad) ) THEN
          IF ( dfi_stage == DFI_FWD .and. dfi_tten_rad(i,k,j) >= -0.1 .and. &
               dfi_tten_rad(i,k,j) <= 0.1 .and. k < k_end ) THEN
! add radar temp tendency
! there is radar coverage
               t_new(i,k,j) = t_new(i,k,j) + (dfi_tten_rad(i,k,j))*dt
          ELSE
! no radar coverage
               t_new(i,k,j) = t_new(i,k,j) + mpten
          ENDIF
       ENDIF
         th_phy_m_t0(i,k,j) = t_new(i,k,j)
         h_diabatic(i,k,j) =  mpten / dt
#else
! pertubation theta_moist(new) = theta_dry(old)*(1+rv/rd qv(old))    +   ! term 1
!                                delta theta_dry * (1+rv/rd qv(new)) +   ! term 2
!                                (rv/rd)*delta qv * theta_dry(new)   -   ! term 3
!                                300                                     ! term 4
!                    
         IF ( ( config_flags%use_theta_m .EQ. 1 ) .AND. (P_Qv .GE. PARAM_FIRST_SCALAR) ) THEN
            t_new(i,k,j) =  h_diabatic(i,k,j)*(1. + (R_v/R_d)*qv_diabatic(i,k,j)) + \
                            mpten*(1. + (R_v/R_d)*qv(i,k,j)) + \
                            (R_v/R_d)*qvten*th_phy(i,k,j) - T0
            th_phy_m_t0(i,k,j) = (t_new(i,k,j)+T0)/(1.+(R_v/R_d)*qv(i,k,j)) - T0
            h_diabatic(i,k,j) =  ( mpten*(1. + (R_v/R_d)*qv(i,k,j)) + \
                                 (R_v/R_d)*qvten*th_phy(i,k,j) ) / dt
         ELSE
            t_new(i,k,j) = t_new(i,k,j) + mpten
            th_phy_m_t0(i,k,j) = t_new(i,k,j)
            h_diabatic(i,k,j) =  mpten/dt
         END IF
#endif
!!!         ! KLUDGE:
!!!         qvten = 0.0
!!!         qcten = 0.0
#if ( WRFPLUS == 1 )
         if ( P_QV >= PARAM_FIRST_SCALAR ) then
            qv_diabatic(i,k,j) =  qvten/dt
         else
            qv_diabatic(i,k,j) =  0.0
         end if
         if ( P_QC >= PARAM_FIRST_SCALAR ) then
            qc_diabatic(i,k,j) =  qcten/dt
         else
            qc_diabatic(i,k,j) =  0.0
         end if
#else
         qv_diabatic(i,k,j) =  qvten/dt
         qc_diabatic(i,k,j) =  qcten/dt
#endif
     ENDDO
     ENDDO
     ENDDO

     ELSE

     DO j = j_start, j_end
     DO k = k_start, k_end
     DO i = i_start, i_end
!        t_new(i,k,j) = t_new(i,k,j)
         h_diabatic(i,k,j) = 0.
         qv_diabatic(i,k,j) = 0.
         qc_diabatic(i,k,j) = 0.
     ENDDO
     ENDDO
     ENDDO
     ENDIF

   END SUBROUTINE moist_physics_finish_em

!----------------------------------------------------------------


   SUBROUTINE init_module_big_step
   END SUBROUTINE init_module_big_step

SUBROUTINE set_tend ( field, field_adv_tend, msf,       &
                      ids, ide, jds, jde, kds, kde,     &
                      ims, ime, jms, jme, kms, kme,     &
                      its, ite, jts, jte, kts, kte       )

   IMPLICIT NONE

   ! Input data

   INTEGER ,  INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                               ims, ime, jms, jme, kms, kme, &
                               its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: field

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN)  :: field_adv_tend

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN)  :: msf

   ! Local data

   INTEGER :: i, j, k, itf, jtf, ktf

!<DESCRIPTION>
!
!  set_tend copies the advective tendency array into the tendency array.
!
!</DESCRIPTION>

      jtf = MIN(jte,jde-1)
      ktf = MIN(kte,kde-1)
      itf = MIN(ite,ide-1)
      DO j = jts, jtf
      DO k = kts, ktf
      DO i = its, itf
         field(i,k,j) = field_adv_tend(i,k,j)*msf(i,j)
      ENDDO
      ENDDO
      ENDDO

END SUBROUTINE set_tend

!------------------------------------------------------------------------------

    SUBROUTINE rk_rayleigh_damp( ru_tendf, rv_tendf,              &
                                 rw_tendf, t_tendf,               &
                                 u, v, w, t, t_init,              &
                                 c1h, c2h, c1f, c2f,              &
                                 mut, muu, muv, ph, phb,          &
                                 u_base, v_base, t_base, z_base,  &
                                 dampcoef, zdamp,                 &
                                 ids, ide, jds, jde, kds, kde,    &
                                 ims, ime, jms, jme, kms, kme,    &
                                 its, ite, jts, jte, kts, kte   )

! History:     Apr 2005  Modifications by George Bryan, NCAR:
!                  - Generalized the code in a way that allows for
!                    simulations with steep terrain.
!
!              Jul 2004  Modifications by George Bryan, NCAR:
!                  - Modified the code to use u_base, v_base, and t_base
!                    arrays for the background state.  Removed the hard-wired
!                    base-state values.
!                  - Modified the code to use dampcoef, zdamp, and damp_opt,
!                    i.e., the upper-level damper variables in namelist.input.
!                    Removed the hard-wired variables in the older version.
!                    This damper is used when damp_opt = 2.
!                  - Modified the code to account for the movement of the
!                    model surfaces with time.  The code now obtains a base-
!                    state value by interpolation using the "_base" arrays.

!              Nov 2003  Bug fix by Jason Knievel, NCAR

!              Aug 2003  Meridional dimension, some comments, and
!                        changes in layout of the code added by
!                        Jason Knievel, NCAR

!              Jul 2003  Original code by Bill Skamarock, NCAR

! Purpose:     This routine applies Rayleigh damping to a layer at top
!              of the model domain.

!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    INTEGER, INTENT( IN )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: ru_tendf, rv_tendf, rw_tendf, t_tendf

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )  &
    :: u, v, w, t, t_init, ph, phb

    REAL, DIMENSION( ims:ime, jms:jme ),  INTENT( IN )  &
    :: mut, muu, muv

    REAL, DIMENSION( kms:kme ) ,  INTENT(IN   )  &
    :: u_base, v_base, t_base, z_base

    REAL, DIMENSION( kms:kme ) ,  INTENT(IN   )  &
    :: c1h, c2h, c1f, c2f

    REAL, INTENT(IN   )   &
    :: dampcoef, zdamp

! Local variables.

    INTEGER  &
    :: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k, ktf, k1, k2

    REAL, DIMENSION( kms:kme ) &
    :: c1, c2

    REAL  &
    :: pii, dcoef, z, ztop

    REAL :: wkp1, wk, wkm1

    REAL, DIMENSION( kms:kme ) :: z00, u00, v00, t00

! End declarations.
!-----------------------------------------------------------------------

    c1 = c1h
    c2 = c2h

    pii = 2.0 * asin(1.0)

    ktf = MIN( kte,   kde-1 )

!-----------------------------------------------------------------------
! Adjust u to base state.

    DO j = jts, MIN( jte, jde-1 )
    DO i = its, MIN( ite, ide   )

      ! Get height at top of model
      ztop = 0.5*( phb(i  ,kde,j)+phb(i-1,kde,j)   &
                  +ph(i  ,kde,j)+ph(i-1,kde,j) )/g

      ! Find bottom of damping layer
      k1 = ktf
      z = ztop
      DO WHILE( z >= (ztop-zdamp) )
        z = 0.25*( phb(i  ,k1,j)+phb(i  ,k1+1,j)  &
                  +phb(i-1,k1,j)+phb(i-1,k1+1,j)  &
                  +ph(i  ,k1,j)+ph(i  ,k1+1,j)    &
                  +ph(i-1,k1,j)+ph(i-1,k1+1,j))/g
        z00(k1) = z
        k1 = k1 - 1
      ENDDO
      k1 = k1 + 2

      ! Get reference state at model levels
      DO k = k1, ktf
        k2 = ktf
        DO WHILE( z_base(k2) .gt. z00(k) )
          k2 = k2 - 1
        ENDDO
        if(k2+1.gt.ktf)then
          u00(k) = u_base(k2) + ( u_base(k2) - u_base(k2-1) )   &
                              * (     z00(k) - z_base(k2)   )   &
                              / ( z_base(k2) - z_base(k2-1) )
        else
          u00(k) = u_base(k2) + ( u_base(k2+1) - u_base(k2) )   &
                              * (       z00(k) - z_base(k2) )   &
                              / ( z_base(k2+1) - z_base(k2) )
        endif
      ENDDO

      ! Apply the Rayleigh damper
      DO k = k1, ktf
        dcoef = 1.0 - MIN( 1.0, ( ztop - z00(k) ) / zdamp )
        dcoef = (SIN( 0.5 * pii * dcoef ) )**2
        ru_tendf(i,k,j) = ru_tendf(i,k,j) -                    &
                          (c1h(k)*muu(i,j)+c2h(k)) * ( dcoef * dampcoef ) *    &
                          ( u(i,k,j) - u00(k) )
      END DO

    END DO
    END DO

! End adjustment of u.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Adjust v to base state.

    DO j = jts, MIN( jte, jde   )
    DO i = its, MIN( ite, ide-1 )

      ! Get height at top of model
      ztop = 0.5*( phb(i,kde,j  )+phb(i,kde,j-1)   &
                  +ph(i,kde,j  )+ph(i,kde,j-1) )/g

      ! Find bottom of damping layer
      k1 = ktf
      z = ztop
      DO WHILE( z >= (ztop-zdamp) )
        z = 0.25*( phb(i,k1,j  )+phb(i,k1+1,j  )  &
                  +phb(i,k1,j-1)+phb(i,k1+1,j-1)  &
                  +ph(i,k1,j  )+ph(i,k1+1,j  )    &
                  +ph(i,k1,j-1)+ph(i,k1+1,j-1))/g
        z00(k1) = z
        k1 = k1 - 1
      ENDDO
      k1 = k1 + 2

      ! Get reference state at model levels
      DO k = k1, ktf
        k2 = ktf
        DO WHILE( z_base(k2) .gt. z00(k) )
          k2 = k2 - 1
        ENDDO
        if(k2+1.gt.ktf)then
          v00(k) = v_base(k2) + ( v_base(k2) - v_base(k2-1) )   &
                              * (     z00(k) - z_base(k2)   )   &
                              / ( z_base(k2) - z_base(k2-1) )
        else
          v00(k) = v_base(k2) + ( v_base(k2+1) - v_base(k2) )   &
                              * (       z00(k) - z_base(k2) )   &
                              / ( z_base(k2+1) - z_base(k2) )
        endif
      ENDDO

      ! Apply the Rayleigh damper
      DO k = k1, ktf
        dcoef = 1.0 - MIN( 1.0, ( ztop - z00(k) ) / zdamp )
        dcoef = (SIN( 0.5 * pii * dcoef ) )**2
        rv_tendf(i,k,j) = rv_tendf(i,k,j) -                    &
                          (c1h(k)*muv(i,j)+c2h(k)) * ( dcoef * dampcoef ) *    &
                          ( v(i,k,j) - v00(k) )
      END DO

    END DO
    END DO

! End adjustment of v.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Adjust w to base state.

    DO j = jts, MIN( jte,   jde-1 )
    DO i = its, MIN( ite,   ide-1 )
      ztop = ( phb(i,kde,j) + ph(i,kde,j) ) / g
      DO k = kts, MIN( kte,   kde   )
        z = ( phb(i,k,j) + ph(i,k,j) ) / g
        IF ( z >= (ztop-zdamp) ) THEN
          dcoef = 1.0 - MIN( 1.0, ( ztop - z ) / zdamp )
          dcoef = ( SIN( 0.5 * pii * dcoef ) )**2
          rw_tendf(i,k,j) = rw_tendf(i,k,j) -  &
                            (c1f(k)*mut(i,j)+c2f(k)) * ( dcoef * dampcoef ) * w(i,k,j)
        END IF
      END DO
    END DO
    END DO

! End adjustment of w.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Adjust potential temperature to base state.

    DO j = jts, MIN( jte,   jde-1 )
    DO i = its, MIN( ite,   ide-1 )

      ! Get height at top of model
      ztop = ( phb(i,kde,j) + ph(i,kde,j) ) / g

      ! Find bottom of damping layer
      k1 = ktf
      z = ztop
      DO WHILE( z >= (ztop-zdamp) )
        z = 0.5 * ( phb(i,k1,j) + phb(i,k1+1,j) +  &
                     ph(i,k1,j) +  ph(i,k1+1,j) ) / g
        z00(k1) = z
        k1 = k1 - 1
      ENDDO
      k1 = k1 + 2

      ! Get reference state at model levels
      DO k = k1, ktf
        k2 = ktf
        DO WHILE( z_base(k2) .gt. z00(k) )
          k2 = k2 - 1
        ENDDO
        if(k2+1.gt.ktf)then
          t00(k) = t_base(k2) + ( t_base(k2) - t_base(k2-1) )   &
                              * (     z00(k) - z_base(k2)   )   &
                              / ( z_base(k2) - z_base(k2-1) )
        else
          t00(k) = t_base(k2) + ( t_base(k2+1) - t_base(k2) )   &
                              * (       z00(k) - z_base(k2) )   &
                              / ( z_base(k2+1) - z_base(k2) )
        endif
      ENDDO

      ! Apply the Rayleigh damper
      DO k = k1, ktf
        dcoef = 1.0 - MIN( 1.0, ( ztop - z00(k) ) / zdamp )
        dcoef = (SIN( 0.5 * pii * dcoef ) )**2
        t_tendf(i,k,j) = t_tendf(i,k,j) -                      &
                         (c1(k)*MUT(i,j)+c2(k)) * ( dcoef * dampcoef )  *    &
                         ( t(i,k,j) - t00(k) )
      END DO

    END DO
    END DO

! End adjustment of potential temperature.
!-----------------------------------------------------------------------

    END SUBROUTINE rk_rayleigh_damp

!==============================================================================
!==============================================================================

 SUBROUTINE theta_relaxation( t_tendf, t, t_init,              &
                              MUT, c1, c2, ph, phb,            &
                              t_base, z_base,                  &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              its, ite, jts, jte, kts, kte   )

! Purpose:  Newtonian relaxation on potential temperature.  Serves two
!           purposes:  1) to mimic atmospheric radiation in a simple
!           manner, and 2) to keep the vertical profile of temperature
!           close to the initial (base-state) profile, which is useful
!           for certain idealized applications.

! Reference:  Rotunno and Emanuel, 1987, JAS, p. 546

!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    INTEGER, INTENT( IN )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: t_tendf

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )  &
    :: t, t_init, ph, phb

    REAL, DIMENSION( ims:ime, jms:jme ),  INTENT( IN )  &
    :: MUT

    REAL, DIMENSION( kms:kme),  INTENT( IN )  &
    :: c1, c2

    REAL, DIMENSION( kms:kme ) ,  INTENT(IN   )  &
    :: t_base, z_base

! Local variables.

    INTEGER :: i, j, k, ktf, k2
    REAL :: tau_r , rmax , rmin , inv_tau_r , inv_g , rterm
    REAL, DIMENSION( kms:kme ) :: z00,t00

! End declarations.
!-----------------------------------------------------------------------

    ! set tau_r to 12 h, following RE87
    tau_r = 12.0*3600.0

    ! limit rterm to +/- 2 K/day
    rmax =  2.0/86400.0
    rmin = -rmax

    ktf = MIN( kte,   kde-1 )
    inv_tau_r = 1.0/tau_r
    inv_g = 1.0/g

!-----------------------------------------------------------------------
! Adjust potential temperature to base state.

    DO j = jts, MIN( jte,   jde-1 )
    DO i = its, MIN( ite,   ide-1 )

      ! Get height of model levels:
      DO k = kts, ktf
        z00(k) = 0.5 * ( phb(i,k,j) + phb(i,k+1,j) +  &
                          ph(i,k,j) +  ph(i,k+1,j) ) * inv_g
      ENDDO

      ! Get reference state:
      DO k = kts, ktf
        k2 = ktf
        DO WHILE( z_base(k2) .gt. z00(k)  .and.  k2 .gt. 1 )
          k2 = k2 - 1
        ENDDO
        if(k2+1.gt.ktf)then
          t00(k) = t_base(k2) + ( t_base(k2) - t_base(k2-1) )   &
                              * (     z00(k) - z_base(k2)   )   &
                              / ( z_base(k2) - z_base(k2-1) )
        else
          t00(k) = t_base(k2) + ( t_base(k2+1) - t_base(k2) )   &
                              * (       z00(k) - z_base(k2) )   &
                              / ( z_base(k2+1) - z_base(k2) )
        endif
      ENDDO

      ! Apply the RE87 R term:
      DO k = kts, ktf
        rterm = -( t(i,k,j) - t00(k) )*inv_tau_r
        ! limit rterm:
        rterm = min( rterm , rmax )
        rterm = max( rterm , rmin )
        t_tendf(i,k,j) = t_tendf(i,k,j) + (c1(k)*MUT(i,j)+c2(k))*rterm
      END DO

    END DO
    END DO

 END SUBROUTINE theta_relaxation

!==============================================================================

      SUBROUTINE sixth_order_diffusion( name, field, tendency, MUT, dt,  &
                                        config_flags, c1, c2,            &
                                        diff_6th_opt, diff_6th_factor,   &
                                        phb, ph,                         &
                                        rdx, rdy,                        &
                                        msftx, msfty,                    &
                                        msfux, msfuy,                    &
                                        msfvx, msfvy,                    &
                                        ids, ide, jds, jde, kds, kde,    &
                                        ims, ime, jms, jme, kms, kme,    &
                                        its, ite, jts, jte, kts, kte )
                                                                                
! History:       14 Nov 2006   Name of variable changed by Jason Knievel
!                07 Jun 2006   Revised and generalized by Jason Knievel  
!                25 Apr 2005   Original code by Jason Knievel, NCAR
                                                                                
! Purpose:       Apply 6th-order, monotonic (flux-limited), numerical
!                diffusion to 3-d velocity and to scalars.
                                                                                
! References:    Ming Xue (MWR Aug 2000)
!                Durran ("Numerical Methods for Wave Equations..." 1999)
!                George Bryan (personal communication)

!------------------------------------------------------------------------------
! Begin: Declarations.

    IMPLICIT NONE

    INTEGER, INTENT(IN)  &
    :: ids, ide, jds, jde, kds, kde,   &
       ims, ime, jms, jme, kms, kme,   &
       its, ite, jts, jte, kts, kte

    TYPE(grid_config_rec_type), INTENT(IN)  &
    :: config_flags

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT)  &
    :: tendency

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN)  &
    :: field

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN)  &
    :: ph, phb
 
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN)  &
    :: MUT

    REAL, DIMENSION( kms:kme ), INTENT(IN)  &
    :: c1, c2

    REAL, INTENT(IN)  &
    :: dt, rdx, rdy

    REAL, INTENT(IN)  &
    :: diff_6th_factor

    INTEGER, INTENT(IN)  &
    :: diff_6th_opt

    CHARACTER(LEN=1) , INTENT(IN)  &
    :: name

    INTEGER  &
    :: i, j, k,         &
       i_start, i_end,  &
       j_start, j_end,  &
       k_start, k_end,  &
       ktf
 
    REAL , DIMENSION( ims:ime, jms:jme) , INTENT(IN   ) ::   msfux
    REAL , DIMENSION( ims:ime, jms:jme) , INTENT(IN   ) ::   msfuy
    REAL , DIMENSION( ims:ime, jms:jme) , INTENT(IN   ) ::   msfvx
    REAL , DIMENSION( ims:ime, jms:jme) , INTENT(IN   ) ::   msfvy
    REAL , DIMENSION( ims:ime, jms:jme) , INTENT(IN   ) ::   msftx
    REAL , DIMENSION( ims:ime, jms:jme) , INTENT(IN   ) ::   msfty

    REAL  &
    :: dflux_x_p0, dflux_y_p0,  &
       dflux_x_p1, dflux_y_p1,  &
       tendency_x, tendency_y,  &
       mu_avg_p0, mu_avg_p1,    &
       diff_6th_coef

    REAL  &
    :: dzmax_p0, dzmax_p1, dx, dy, slopedamp_p0, slopedamp_p1, dzthresh

    LOGICAL  &
    :: specified

! End: Declarations.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Begin: Translate the diffusion factor into a diffusion coefficient.  See
! Durran's text, section 2.4.3, then adjust for sixth-order diffusion (not
! fourth) and for diffusion in two dimensions (not one).  For reference, a
! factor of 1.0 would mean complete diffusion of a 2dx wave in one time step,
! although application of the flux limiter reduces somewhat the effects of
! diffusion for a given coefficient.

    diff_6th_coef = diff_6th_factor * 0.015625 / ( 2.0 * dt )  

! End: Translate diffusion factor.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Begin: Assign limits of spatial loops depending on variable to be diffused.
! The halo regions outside the physical domain are not defined, hence the loop
! indices should start 3 rows and columns in for specified boundary conditions.
! Also modified to work with idealized boundary conditions.

    ktf = MIN( kte, kde-1 )
    specified = .false.
    if(config_flags%specified .or. config_flags%nested) specified = .true.

    IF ( name .EQ. 'u' ) THEN

      i_start = its
      i_end   = ite
      j_start = jts
      j_end   = MIN(jde-1,jte)
      IF ( config_flags%open_xs ) THEN
        i_start = MAX(its,ids+3)
      END IF
      IF ( config_flags%open_ys ) THEN
        j_start = MAX(jts,jds+3)
      END IF
      IF ( config_flags%open_xe ) THEN
        i_end   = MIN(ite,ide-3)
      END IF
      IF ( config_flags%open_ye ) THEN
        j_end   = MIN(jte,jde-4)
      END IF
      IF ( specified ) THEN
        i_start = MAX(its,ids+3)
        i_end   = MIN(ite,ide-3)
        j_start = MAX(jts,jds+3)
        j_end   = MIN(jte,jde-4)
      END IF
      k_start = kts
      k_end   = ktf

    ELSE IF ( name .EQ. 'v' ) THEN

      i_start = its
      i_end   = MIN(ide-1,ite)
      j_start = jts
      j_end   = jte
      IF ( config_flags%open_xs ) THEN
        i_start = MAX(its,ids+3)
      END IF
      IF ( config_flags%open_ys ) THEN
        j_start = MAX(jts,jds+3)
      END IF
      IF ( config_flags%open_xe ) THEN
        i_end   = MIN(ite,ide-4)
      END IF
      IF ( config_flags%open_ye ) THEN
        j_end   = MIN(jte,jde-3)
      END IF
      IF ( config_flags%open_xs .or. specified ) THEN
        i_start = MAX(its,ids+3)
        i_end   = MIN(ite,ide-4)
        j_start = MAX(jts,jds+3)
        j_end   = MIN(jte,jde-3)
      END IF
      k_start = kts
      k_end   = ktf

    ELSE IF ( name .EQ. 'w' ) THEN

      i_start = its
      i_end   = MIN(ide-1,ite)
      j_start = jts
      j_end   = MIN(jde-1,jte)
      IF ( config_flags%open_xs ) THEN
        i_start = MAX(its,ids+3)
      END IF
      IF ( config_flags%open_ys ) THEN
        j_start = MAX(jts,jds+3)
      END IF
      IF ( config_flags%open_xe ) THEN
        i_end   = MIN(ite,ide-4)
      END IF
      IF ( config_flags%open_ye ) THEN
        j_end   = MIN(jte,jde-4)
      END IF
      IF ( specified ) THEN
        i_start = MAX(its,ids+3)
        i_end   = MIN(ide-4,ite)
        j_start = MAX(jts,jds+3)
        j_end   = MIN(jde-4,jte)
      END IF
      k_start = kts+1
      k_end   = ktf

    ELSE

      i_start = its
      i_end   = MIN(ide-1,ite)
      j_start = jts
      j_end   = MIN(jde-1,jte)
      IF ( config_flags%open_xs ) THEN
        i_start = MAX(its,ids+3)
      END IF
      IF ( config_flags%open_ys ) THEN
        j_start = MAX(jts,jds+3)
      END IF
      IF ( config_flags%open_xe ) THEN
        i_end   = MIN(ite,ide-4)
      END IF
      IF ( config_flags%open_ye ) THEN
        j_end   = MIN(jte,jde-4)
      END IF
      IF ( specified ) THEN
        i_start = MAX(its,ids+3)
        i_end   = MIN(ide-4,ite)
        j_start = MAX(jts,jds+3)
        j_end   = MIN(jde-4,jte)
      END IF
      k_start = kts
      k_end   = ktf

    ENDIF

! End: Assignment of limits of spatial loops.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Begin: Loop across spatial dimensions.

    DO j = j_start, j_end
    DO k = k_start, k_end
    DO i = i_start, i_end

!------------------------------------------------------------------------------
! Begin: Diffusion in x (i index).

! Calculate the diffusive flux in x direction (from Xue's eq. 3).

      dflux_x_p0 = (  10.0 * ( field(i,  k,j) - field(i-1,k,j) )    &
                     - 5.0 * ( field(i+1,k,j) - field(i-2,k,j) )    &
                     +       ( field(i+2,k,j) - field(i-3,k,j) ) )

      dflux_x_p1 = (  10.0 * ( field(i+1,k,j) - field(i  ,k,j) )    &
                     - 5.0 * ( field(i+2,k,j) - field(i-1,k,j) )    &
                     +       ( field(i+3,k,j) - field(i-2,k,j) ) )

! If requested in the namelist (diff_6th_opt=2), prohibit up-gradient diffusion
! (variation on Xue's eq. 10).

      IF ( diff_6th_opt .EQ. 2 ) THEN

        IF ( dflux_x_p0 * ( field(i  ,k,j)-field(i-1,k,j) ) .LE. 0.0 ) THEN
          dflux_x_p0 = 0.0
        END IF

        IF ( dflux_x_p1 * ( field(i+1,k,j)-field(i  ,k,j) ) .LE. 0.0 ) THEN
          dflux_x_p1 = 0.0
        END IF

      END IF

      slopedamp_p0 = 1.
      slopedamp_p1 = 1.

      IF (config_flags%diff_6th_slopeopt .ge. 1) THEN
         dx=1./rdx
         dzthresh = config_flags%diff_6th_thresh*9.81*dx
         IF      ( name .EQ. 'u' ) THEN
           dzmax_p0 = MAX( ABS(phb(i  ,k,j)-phb(i-1,k,j))*msfux(i  ,j), ABS(phb(i-1,k,j)-phb(i-2,k,j))*msfux(i-1,j) )
           dzmax_p1 = MAX( ABS(phb(i+1,k,j)-phb(i  ,k,j))*msfux(i+1,j), ABS(phb(i  ,k,j)-phb(i-1,k,j))*msfux(i  ,j) )
         ELSE IF ( name .EQ. 'v' ) THEN
           dzmax_p0 = max( ABS(phb(i  ,k,j) - phb(i-1,k,j))*msfux(i  ,j), ABS(phb(i  ,k,j-1) - phb(i-1,k,j-1))*msfux(i  ,j-1) )
           dzmax_p1 = max( ABS(phb(i+1,k,j) - phb(i  ,k,j))*msfux(i+1,j), ABS(phb(i+1,k,j-1) - phb(i  ,k,j-1))*msfux(i+1,j-1) )
         ELSE
           dzmax_p0 = ABS(phb(i  ,k,j) - phb(i-1,k,j))*msfux(i,j)
           dzmax_p1 = ABS(phb(i+1,k,j) - phb(i  ,k,j))*msfux(i+1,j)
         END IF
         slopedamp_p0 = MAX(1.- (dzmax_p0/dzthresh) , 0.)
         slopedamp_p1 = MAX(1.- (dzmax_p1/dzthresh) , 0.)
      ENDIF

! Apply 6th-order diffusion in x direction.

      IF      ( name .EQ. 'u' ) THEN
        mu_avg_p0 = (c1(k)*MUT(i-1,j)+c2(k))
        mu_avg_p1 = (c1(k)*MUT(i  ,j)+c2(k))
         tendency_x = diff_6th_coef * msfux(i,j) * &
                   ( ( slopedamp_p1 * mu_avg_p1 * dflux_x_p1 ) - ( slopedamp_p0 * mu_avg_p0 * dflux_x_p0 ) )
      ELSE IF ( name .EQ. 'v' ) THEN
        mu_avg_p0 = 0.25 * (       &
                    (c1(k)*MUT(i-1,j-1)+c2(k)) +  &
                    (c1(k)*MUT(i  ,j-1)+c2(k)) +  &
                    (c1(k)*MUT(i-1,j  )+c2(k)) +  &
                    (c1(k)*MUT(i  ,j  )+c2(k)) )
        mu_avg_p1 = 0.25 * (       &
                    (c1(k)*MUT(i  ,j-1)+c2(k)) +  &
                    (c1(k)*MUT(i+1,j-1)+c2(k)) +  &
                    (c1(k)*MUT(i  ,j  )+c2(k)) +  &
                    (c1(k)*MUT(i+1,j  )+c2(k)) )
        tendency_x = diff_6th_coef * msfvx(i,j) * &
                   ( ( slopedamp_p1 * mu_avg_p1 * dflux_x_p1 ) - ( slopedamp_p0 * mu_avg_p0 * dflux_x_p0 ) )
      ELSE
        mu_avg_p0 = 0.5 * (        &
                    (c1(k)*MUT(i-1,j)+c2(k)) +    &
                    (c1(k)*MUT(i  ,j)+c2(k)) )
        mu_avg_p1 = 0.5 * (        &
                    (c1(k)*MUT(i  ,j)+c2(k)) +    &
                    (c1(k)*MUT(i+1,j)+c2(k)) )
        tendency_x = diff_6th_coef * msftx(i,j) * &
                 ( ( slopedamp_p1 * mu_avg_p1 * dflux_x_p1 ) - ( slopedamp_p0 * mu_avg_p0 * dflux_x_p0 ) )
      END IF
  
! End: Diffusion in x.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Begin: Diffusion in y (j index).

! Calculate the diffusive flux in y direction (from Xue's eq. 3).

      dflux_y_p0 = (  10.0 * ( field(i,k,j  ) - field(i,k,j-1) )    &
                     - 5.0 * ( field(i,k,j+1) - field(i,k,j-2) )    &
                     +       ( field(i,k,j+2) - field(i,k,j-3) ) )

      dflux_y_p1 = (  10.0 * ( field(i,k,j+1) - field(i,k,j  ) )    &
                     - 5.0 * ( field(i,k,j+2) - field(i,k,j-1) )    &
                     +       ( field(i,k,j+3) - field(i,k,j-2) ) )

! If requested in the namelist (diff_6th_opt=2), prohibit up-gradient diffusion
! (variation on Xue's eq. 10).

      IF ( diff_6th_opt .EQ. 2 ) THEN

        IF ( dflux_y_p0 * ( field(i,k,j  )-field(i,k,j-1) ) .LE. 0.0 ) THEN
          dflux_y_p0 = 0.0
        END IF

        IF ( dflux_y_p1 * ( field(i,k,j+1)-field(i,k,j  ) ) .LE. 0.0 ) THEN
          dflux_y_p1 = 0.0
        END IF

      END IF

      slopedamp_p0 = 1. 
      slopedamp_p1 = 1. 

      IF (config_flags%diff_6th_slopeopt .ge. 1) THEN
         dy=1./rdy
         dzthresh = config_flags%diff_6th_thresh*9.81*dy
         IF      ( name .EQ. 'u' ) THEN
           dzmax_p0 = max( ABS(phb(i,k,j  ) - phb(i,k,j-1))*msfvy(i,j  ), ABS(phb(i-1,k,j  ) - phb(i-1,k,j-1))*msfvy(i-1,j  ) )
           dzmax_p1 = max( ABS(phb(i,k,j+1) - phb(i,k,j  ))*msfvy(i,j+1), ABS(phb(i-1,k,j+1) - phb(i-1,k,j  ))*msfvy(i-1,j+1) )
         ELSE IF ( name .EQ. 'v' ) THEN
           dzmax_p0 = MAX( ABS(phb(i,k,j  )-phb(i,k,j-1))*msfvy(i,j  ), ABS(phb(i,k,j-1)-phb(i,k,j-2))*msfvy(i,j-1) )
           dzmax_p1 = MAX( ABS(phb(i,k,j+1)-phb(i,k,j  ))*msfvy(i,j+1), ABS(phb(i,k,j  )-phb(i,k,j-1))*msfvy(i,j  ) )
         ELSE
           dzmax_p0 = ABS(phb(i,k,j  ) - phb(i,k,j-1))*msfvy(i,j  )
           dzmax_p1 = ABS(phb(i,k,j+1) - phb(i,k,j  ))*msfvy(i,j+1)
         END IF
         slopedamp_p0 = MAX(1.- (dzmax_p0/dzthresh) , 0.)
         slopedamp_p1 = MAX(1.- (dzmax_p1/dzthresh) , 0.)
      ENDIF

! Apply 6th-order diffusion in y direction.

      IF      ( name .EQ. 'u' ) THEN
        mu_avg_p0 = 0.25 * (       &
                    (c1(k)*MUT(i-1,j-1)+c2(k)) +  &
                    (c1(k)*MUT(i  ,j-1)+c2(k)) +  &
                    (c1(k)*MUT(i-1,j  )+c2(k)) +  &
                    (c1(k)*MUT(i  ,j  )+c2(k)) )
        mu_avg_p1 = 0.25 * (       &
                    (c1(k)*MUT(i-1,j  )+c2(k)) +  &
                    (c1(k)*MUT(i  ,j  )+c2(k)) +  &
                    (c1(k)*MUT(i-1,j+1)+c2(k)) +  &
                    (c1(k)*MUT(i  ,j+1)+c2(k)) )
        tendency_y = diff_6th_coef * msfuy(i,j) * &
                 ( ( slopedamp_p1 * mu_avg_p1 * dflux_y_p1 ) - ( slopedamp_p0 * mu_avg_p0 * dflux_y_p0 ) )
 
      ELSE IF ( name .EQ. 'v' ) THEN
        mu_avg_p0 = (c1(k)*MUT(i,j-1)+c2(k))
        mu_avg_p1 = (c1(k)*MUT(i,j  )+c2(k))
        tendency_y = diff_6th_coef * msfvy(i,j) * &
                   ( ( slopedamp_p1 * mu_avg_p1 * dflux_y_p1 ) - ( slopedamp_p0 * mu_avg_p0 * dflux_y_p0 ) )
      ELSE
        mu_avg_p0 = 0.5 * (      &
                    (c1(k)*MUT(i,j-1)+c2(k)) +  &
                    (c1(k)*MUT(i,j  )+c2(k)) )
        mu_avg_p1 = 0.5 * (      &
                    (c1(k)*MUT(i,j  )+c2(k)) +  &
                    (c1(k)*MUT(i,j+1)+c2(k)) )
        tendency_y = diff_6th_coef * msfty(i,j) * &
                   ( ( slopedamp_p1 * mu_avg_p1 * dflux_y_p1 ) - ( slopedamp_p0 * mu_avg_p0 * dflux_y_p0 ) )
      END IF
 
! End: Diffusion in y.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Begin: Combine diffusion in x and y.
     
      tendency(i,k,j) = tendency(i,k,j) + tendency_x + tendency_y

! End: Combine diffusion in x and y.
!------------------------------------------------------------------------------

    ENDDO
    ENDDO
    ENDDO

! End: Loop across spatial dimensions.
!------------------------------------------------------------------------------

    END SUBROUTINE sixth_order_diffusion

!==============================================================================

SUBROUTINE initialize_moist_old ( moist_old , moist , &
                                  ids, ide, jds, jde, kds, kde ,   &
                                  ims, ime, jms, jme, kms, kme ,   &
                                  its, ite, jts, jte, kts, kte     )

   !  For the theta_m option, the moist_old variable is uninitialized
   !  at the beginning of EACH of the RK steps.  So, just set the
   !  starting value of moist_old as the final value of moist from the
   !  previous time step.  Here "moist" is only the P_Qv index.

   IMPLICIT NONE

   INTEGER , INTENT(IN) ::   ids, ide, jds, jde, kds, kde ,   &
                             ims, ime, jms, jme, kms, kme ,   &
                             its, ite, jts, jte, kts, kte     
   REAL    , INTENT(IN   ) , DIMENSION(ims:ime,kms:kme,jms:jme) :: moist
   REAL    , INTENT(  OUT) , DIMENSION(ims:ime,kms:kme,jms:jme) :: moist_old

   !  Local variables

   INTEGER :: i , j , k

   DO j = jts , MIN(jte,jde-1)
      DO k = kts , kte-1
         DO i = its , MIN(ite,ide-1)
            moist_old(i,k,j) = moist(i,k,j)
         END DO
      END DO
   END DO

END SUBROUTINE initialize_moist_old

!==============================================================================

SUBROUTINE conv_t_tendf_to_moist ( t_1 , moist_old ,                &
                                   t_tendf  , moist_tend ,          &
                                   ids, ide, jds, jde, kds, kde ,   &
                                   ims, ime, jms, jme, kms, kme ,   &
                                   its, ite, jts, jte, kts, kte     )

   !  Convert dry potential temperature to "moist" theta:
   !  theta_m = theta ( 1 + Rv/Rd Qv )

   IMPLICIT NONE

   INTEGER , INTENT(IN) ::   ids, ide, jds, jde, kds, kde ,   &
                             ims, ime, jms, jme, kms, kme ,   &
                             its, ite, jts, jte, kts, kte     
   REAL    , INTENT(IN   ) , DIMENSION(ims:ime,kms:kme,jms:jme) :: moist_tend, moist_old, t_1
   REAL    , INTENT(INOUT) , DIMENSION(ims:ime,kms:kme,jms:jme) :: t_tendf

   !  Local variables

   INTEGER :: i , j , k

   !  This dry tendency is from the physics packages.  It is modified immediately after the
   !  call to the physics schemes, and the remains constant for the remainder of the RK loops.

   DO j = jts , MIN(jte,jde-1)
      DO k = kts , kte-1
         DO i = its , MIN(ite,ide-1)
            t_tendf(i,k,j) = (1. + (R_v/R_d) * moist_old(i,k,j))*t_tendf(i,k,j) + &
                             (R_v/R_d)*(t_1(i,k,j)+T0)/(1.+R_v/R_d*moist_old(i,k,j))*moist_tend(i,k,j)
         END DO
      END DO
   END DO

END SUBROUTINE conv_t_tendf_to_moist

!==============================================================================

END MODULE module_big_step_utilities_em
