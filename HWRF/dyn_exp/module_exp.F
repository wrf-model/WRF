!WRF:MODEL_LAYER:DYNAMICS
!

MODULE module_exp

   USE module_state_description

CONTAINS

!------------------------------------------------------------------------

SUBROUTINE relax_1_into_2  ( x1, x2,                        &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte    )

   IMPLICIT NONE


   !  Input data.

   INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte

   REAL , DIMENSION(  ims:ime , kms:kme, jms:jme ) ,                      &
                                               INTENT(IN   ) ::  x1

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme  ) ,                     &
                                               INTENT(  OUT) ::  x2

   integer :: k, i, j


   DO j = jts, jte
     IF ( j > jds .AND. j < jde-1 ) THEN    ! jde-1 because x is not staggered in Y
       DO k = kts, kte
         DO i = its, ite
           IF ( i > ids .AND. i < ide-1 ) THEN    ! ide-1 because x is not staggered in X
             x2(i,k,j) = 0.25*(x1(i+1,k,j)+x1(i-1,k,j)+x1(i,k,j+1)+x1(i,k,j-1))
           ENDIF
         ENDDO
       ENDDO
     ENDIF
   ENDDO

END SUBROUTINE relax_1_into_2

!-------------------------------------------------------------------------------

SUBROUTINE copy_2_into_1  ( x2, x1,                        &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte    )

   IMPLICIT NONE


   !  Input data.

   INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte

   REAL , DIMENSION(  ims:ime , kms:kme, jms:jme ) ,                      &
                                               INTENT(IN   ) ::  x2

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme  ) ,                     &
                                               INTENT(  OUT) ::  x1

   integer :: k, i, j

   DO j = jts, jte
     IF ( j > jds .AND. j < jde-1 ) THEN
       DO k = kts, kte
         DO i = its, ite
           IF ( i > ids .AND. i < ide-1 ) THEN
             x1(i,k,j) = x2(i,k,j)
           ENDIF
         ENDDO
       ENDDO
     ENDIF
   ENDDO

END SUBROUTINE copy_2_into_1

!-------------------------------------------------------------------------------

END MODULE module_exp

