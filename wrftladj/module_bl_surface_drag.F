!WRF:MODEL_LAYER:PHYSICS

MODULE module_bl_surface_drag


CONTAINS

   SUBROUTINE surface_drag (rublten, rvblten, u_phy, v_phy, xland,  &
                         z,    ht, kpbl2d,                           &
                         ids, ide, jds, jde, kds, kde,               &
                         ims, ime, jms, jme, kms, kme,               &
                         its, ite, jts, jte, kts, kte   )

   IMPLICIT NONE

   INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                        ims, ime, jms, jme, kms, kme, &
                                        its, ite, jts, jte, kts, kte

   INTEGER,  DIMENSION( ims:ime, jms:jme )                       , &
             INTENT(  OUT)   ::                          kpbl2d

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) ,               &
                                        INTENT(INOUT) :: rublten,  &
                                                         rvblten
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) ,               &
                                        INTENT(IN   ) :: u_phy,    &
                                                         v_phy,    &
                                                         z
   REAL , DIMENSION( ims:ime , jms:jme  ), INTENT(IN) :: xland, ht

! Local
   REAL :: V0,  tao_xz, tao_yz, cd, zh, zwt
   INTEGER :: i, j, i_start, i_end, i_endu, j_start, j_end, j_endv, k

! End declarations.
!-----------------------------------------------------------------------

   i_start = its
   i_end   = MIN(ite,ide-1)
   i_endu  = ite
   j_start = jts
   j_end   = MIN(jte,jde-1)
   j_endv  = jte

    DO j = j_start, j_end
    DO i = i_start, i_endu

       V0 =    sqrt((u_phy(i,kts,j)**2) +         &
                    (v_phy(i,kts,j)**2))

       IF ( ABS(xland(i,j)-xland(i-1,j)) .LT. 1.0E-10 ) THEN
          IF ( xland(i,j) .LT. 1.5 ) THEN
             ! land
             Cd=0.01
          ELSE
             ! water
             Cd=0.001
             Cd=MAX(Cd, 1.e-4 * V0)
             Cd=MIN(Cd, 0.003)

          ENDIF
       ELSE
          ! coast
          Cd=0.003
       ENDIF

       tao_xz=Cd*V0*u_phy(i,kts,j)

       DO k = kts, kte
          zh = (z(i,k,j)-ht(i,j))
          IF ( zh .LT. 1000. ) THEN
             zwt = 2. * (1000.-zh) / 1000.
             rublten(i,k,j)=rublten(i,k,j)            &
                            -zwt*0.5*tao_xz/1000.
             kpbl2d(i,j) = k
          ENDIF
       ENDDO

    ENDDO
    ENDDO

!
    DO j = j_start, j_endv
    DO i = i_start, i_end

       V0 = SQRT((u_phy(i,kts,j)**2) +         &
                 (v_phy(i,kts,j)**2))

       IF ( ABS(xland(i,j)-xland(i,j-1)) .LT. 1.0E-10 ) THEN
          IF ( xland(i,j) .LT. 1.5 ) THEN
             ! land
             Cd=0.01
          ELSE
             ! water
             Cd=0.001
             Cd=MAX(Cd, 1.e-4 * V0)
             Cd=MIN(Cd, 0.003)
          ENDIF
       ELSE
          ! coast
          Cd=0.003
       ENDIF

       tao_yz=Cd*V0*v_phy(i,kts,j)

       DO k = kts, kte
          zh = (z(i,k,j)-ht(i,j))
          IF ( zh .LT. 1000. ) THEN
             zwt = 2. * (1000.-zh) / 1000.
             rvblten(i,k,j)= rvblten(i,k,j)            &
                             -zwt*0.5*tao_yz/1000.
          ENDIF
       ENDDO

    ENDDO
    ENDDO

   END SUBROUTINE surface_drag

!===================================================================
   SUBROUTINE surface_drag_init(RUBLTEN,RVBLTEN,RTHBLTEN,          &
                      RQVBLTEN,restart,                            &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )
!-------------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------------
   LOGICAL , INTENT(IN)          :: restart 
   INTEGER , INTENT(IN)          ::  ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::         &
                                                         RUBLTEN, &
                                                         RVBLTEN, &
                                                         RTHBLTEN, &
                                                         RQVBLTEN
   INTEGER :: i, j, k, itf, jtf, ktf

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

   IF(.not.restart)THEN
     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
        RUBLTEN(i,k,j)=0.
        RVBLTEN(i,k,j)=0.
        RTHBLTEN(i,k,j)=0.
        RQVBLTEN(i,k,j)=0.
     ENDDO
     ENDDO
     ENDDO
   ENDIF

   END SUBROUTINE surface_drag_init

END MODULE module_bl_surface_drag
