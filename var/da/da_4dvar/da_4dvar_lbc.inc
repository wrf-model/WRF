 SUBROUTINE g_stuff_bdy ( g_data3d , g_space_bdy_xs, g_space_bdy_xe, g_space_bdy_ys, g_space_bdy_ye, &
                             char_stagger , &
                             spec_bdy_width , &
                             ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , & 
                             its, ite, jts, jte, kts, kte )
 
!-------------------------------------------------------------------------
!  Derived from share/module_bc.F
!  Author: Xin Zhang, 10/3/2010
!-------------------------------------------------------------------------
 !  This routine puts the data in the 3d arrays into the proper locations
 !  for the lateral boundary arrays.
 
    IMPLICIT NONE
 
    INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER , INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER , INTENT(IN) :: its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN) :: spec_bdy_width
    REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(IN) :: g_data3d
    REAL , DIMENSION(jms:jme,kds:kde,spec_bdy_width) , INTENT(OUT) :: g_space_bdy_xs, g_space_bdy_xe
    REAL , DIMENSION(ims:ime,kds:kde,spec_bdy_width) , INTENT(OUT) :: g_space_bdy_ys, g_space_bdy_ye
    CHARACTER (LEN=1) , INTENT(IN) :: char_stagger
 
    INTEGER :: i , ii , j , jj , k
 
    !  There are four lateral boundary locations that are stored.
 
    !  X start boundary
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          g_space_bdy_xs(j,k,i) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          g_space_bdy_xs(j,k,i) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          g_space_bdy_xs(j,k,i) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          g_space_bdy_xs(j,k,i) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    END IF
 
    !  X end boundary
 
    IF      ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide,ite) , MAX(ide - spec_bdy_width + 1,its) , -1
          ii = ide - i + 1
          g_space_bdy_xe(j,k,ii) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          g_space_bdy_xe(j,k,ii) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          g_space_bdy_xe(j,k,ii) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          g_space_bdy_xe(j,k,ii) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          g_space_bdy_xe(j,k,ii) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    END IF
 
    !  Y start boundary
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          g_space_bdy_ys(i,k,j) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          g_space_bdy_ys(i,k,j) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide,ite)
          g_space_bdy_ys(i,k,j) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          g_space_bdy_ys(i,k,j) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    END IF
 
    !  Y end boundary
 
    IF      ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde,jte) , MAX(jde - spec_bdy_width + 1,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j + 1
          g_space_bdy_ye(i,k,jj) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide,ite)
          jj = jde - j
          g_space_bdy_ye(i,k,jj) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          g_space_bdy_ye(i,k,jj) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          g_space_bdy_ye(i,k,jj) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          g_space_bdy_ye(i,k,jj) = g_data3d(i,j,k)
       END DO
       END DO
       END DO
    END IF
    
 END SUBROUTINE g_stuff_bdy

 SUBROUTINE a_stuff_bdy ( a_data3d , a_space_bdy_xs, a_space_bdy_xe, a_space_bdy_ys, a_space_bdy_ye, &
                             char_stagger , &
                             spec_bdy_width , &
                             ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , & 
                             its, ite, jts, jte, kts, kte )
 
!-------------------------------------------------------------------------
!  Derived from share/module_bc.F
!  Author: Xin Zhang, 10/3/2010
!-------------------------------------------------------------------------
 !  This routine puts the data in the 3d arrays into the proper locations
 !  for the lateral boundary arrays.
 
    IMPLICIT NONE
 
    INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER , INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER , INTENT(IN) :: its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN) :: spec_bdy_width
    REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(INOUT) :: a_data3d
    REAL , DIMENSION(jms:jme,kds:kde,spec_bdy_width) , INTENT(INOUT) :: a_space_bdy_xs, a_space_bdy_xe
    REAL , DIMENSION(ims:ime,kds:kde,spec_bdy_width) , INTENT(INOUT) :: a_space_bdy_ys, a_space_bdy_ye
    CHARACTER (LEN=1) , INTENT(IN) :: char_stagger
 
    INTEGER :: i , ii , j , jj , k
 
    !  There are four lateral boundary locations that are stored.
 
    !  X start boundary
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_xs(j,k,i)
          a_space_bdy_xs(j,k,i) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_xs(j,k,i)
          a_space_bdy_xs(j,k,i) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_xs(j,k,i)
          a_space_bdy_xs(j,k,i) = 0.0
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_xs(j,k,i)
          a_space_bdy_xs(j,k,i) = 0.0
       END DO
       END DO
       END DO
    END IF
 
    !  X end boundary
 
    IF      ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide,ite) , MAX(ide - spec_bdy_width + 1,its) , -1
          ii = ide - i + 1
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_xe(j,k,ii) 
          a_space_bdy_xe(j,k,ii) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_xe(j,k,ii) 
          a_space_bdy_xe(j,k,ii) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_xe(j,k,ii) 
          a_space_bdy_xe(j,k,ii) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_xe(j,k,ii) 
          a_space_bdy_xe(j,k,ii) = 0.0
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_xe(j,k,ii) 
          a_space_bdy_xe(j,k,ii) = 0.0
       END DO
       END DO
       END DO
    END IF
 
    !  Y start boundary
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_ys(i,k,j)
          a_space_bdy_ys(i,k,j) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_ys(i,k,j)
          a_space_bdy_ys(i,k,j) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide,ite)
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_ys(i,k,j)
          a_space_bdy_ys(i,k,j) = 0.0
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_ys(i,k,j)
          a_space_bdy_ys(i,k,j) = 0.0
       END DO
       END DO
       END DO
    END IF
 
    !  Y end boundary
 
    IF      ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde,jte) , MAX(jde - spec_bdy_width + 1,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j + 1
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_ye(i,k,jj)
          a_space_bdy_ye(i,k,jj) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide,ite)
          jj = jde - j
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_ye(i,k,jj)
          a_space_bdy_ye(i,k,jj) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_ye(i,k,jj)
          a_space_bdy_ye(i,k,jj) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_ye(i,k,jj)
          a_space_bdy_ye(i,k,jj) = 0.0
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          a_data3d(i,j,k) = a_data3d(i,j,k) + a_space_bdy_ye(i,k,jj)
          a_space_bdy_ye(i,k,jj) = 0.0
       END DO
       END DO
       END DO
    END IF
    
 END SUBROUTINE a_stuff_bdy

 SUBROUTINE g_stuff_bdytend ( g_data3dnew , g_data3dold , time_diff , &
                             g_space_bdy_xs, g_space_bdy_xe, g_space_bdy_ys, g_space_bdy_ye, &
                             char_stagger , &
                             spec_bdy_width , &
                             ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , &
                             its, ite, jts, jte, kts, kte )

!-------------------------------------------------------------------------
!  Derived from share/module_bc.F
!  Author: Xin Zhang, 10/3/2010
!-------------------------------------------------------------------------
 !  This routine puts the tendency data into the proper locations
 !  for the lateral boundary arrays.

    IMPLICIT NONE

    INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER , INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER , INTENT(IN) :: its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN) :: spec_bdy_width
    REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(IN) :: g_data3dnew , g_data3dold
    REAL , DIMENSION(jms:jme,kds:kde,spec_bdy_width) , INTENT(OUT) :: g_space_bdy_xs, g_space_bdy_xe
    REAL , DIMENSION(ims:ime,kds:kde,spec_bdy_width) , INTENT(OUT) :: g_space_bdy_ys, g_space_bdy_ye
    CHARACTER (LEN=1) , INTENT(IN) :: char_stagger
    REAL , INTENT(IN) :: time_diff ! seconds

    INTEGER :: i , ii , j , jj , k

    !  There are four lateral boundary locations that are stored.

    !  X start boundary

    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          g_space_bdy_xs(j,k,i) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          g_space_bdy_xs(j,k,i) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          g_space_bdy_xs(j,k,i) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          g_space_bdy_xs(j,k,i) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    END IF

    !  X end boundary

    IF      ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide,ite) , MAX(ide - spec_bdy_width + 1,its) , -1
          ii = ide - i + 1
          g_space_bdy_xe(j,k,ii) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          g_space_bdy_xe(j,k,ii) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          g_space_bdy_xe(j,k,ii) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          g_space_bdy_xe(j,k,ii) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          g_space_bdy_xe(j,k,ii) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    END IF

    !  Y start boundary

    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          g_space_bdy_ys(i,k,j) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          g_space_bdy_ys(i,k,j) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide,ite)
          g_space_bdy_ys(i,k,j) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          g_space_bdy_ys(i,k,j) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    END IF

    !  Y end boundary

    IF      ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde,jte) , MAX(jde - spec_bdy_width + 1,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j + 1
          g_space_bdy_ye(i,k,jj) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide,ite)
          jj = jde - j
          g_space_bdy_ye(i,k,jj) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          g_space_bdy_ye(i,k,jj) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          g_space_bdy_ye(i,k,jj) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          g_space_bdy_ye(i,k,jj) = ( g_data3dnew(i,j,k) - g_data3dold(i,j,k) ) / time_diff
       END DO
       END DO
       END DO
    END IF

 END SUBROUTINE g_stuff_bdytend

 SUBROUTINE a_stuff_bdytend_new ( a_data3dnew , time_diff , &
                             a_space_bdy_xs, a_space_bdy_xe, a_space_bdy_ys, a_space_bdy_ye, &
                             char_stagger , &
                             spec_bdy_width , &
                             ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , &
                             its, ite, jts, jte, kts, kte )

!-------------------------------------------------------------------------
!  Derived from share/module_bc.F
!  Author: Xin Zhang, 10/3/2010
!-------------------------------------------------------------------------
 !  This routine puts the tendency data into the proper locations
 !  for the lateral boundary arrays.

    IMPLICIT NONE

    INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER , INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER , INTENT(IN) :: its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN) :: spec_bdy_width
    REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(INOUT) :: a_data3dnew
    REAL , DIMENSION(jms:jme,kds:kde,spec_bdy_width) , INTENT(IN) :: a_space_bdy_xs, a_space_bdy_xe
    REAL , DIMENSION(ims:ime,kds:kde,spec_bdy_width) , INTENT(IN) :: a_space_bdy_ys, a_space_bdy_ye
    CHARACTER (LEN=1) , INTENT(IN) :: char_stagger
    REAL , INTENT(IN) :: time_diff ! seconds

    INTEGER :: i , ii , j , jj , k

    !  There are four lateral boundary locations that are stored.

    !  X start boundary

    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_xs(j,k,i) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_xs(j,k,i) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_xs(j,k,i) / time_diff
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_xs(j,k,i) / time_diff
       END DO
       END DO
       END DO
    END IF

    !  X end boundary

    IF      ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide,ite) , MAX(ide - spec_bdy_width + 1,its) , -1
          ii = ide - i + 1
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_xe(j,k,ii) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_xe(j,k,ii) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_xe(j,k,ii) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_xe(j,k,ii) / time_diff
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_xe(j,k,ii) / time_diff
       END DO
       END DO
       END DO
    END IF

    !  Y start boundary

    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_ys(i,k,j) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_ys(i,k,j) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide,ite)
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_ys(i,k,j) / time_diff
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_ys(i,k,j) / time_diff
       END DO
       END DO
       END DO
    END IF

    !  Y end boundary

    IF      ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde,jte) , MAX(jde - spec_bdy_width + 1,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j + 1
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_ye(i,k,jj) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide,ite)
          jj = jde - j
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_ye(i,k,jj) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_ye(i,k,jj) / time_diff
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_ye(i,k,jj) / time_diff
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          a_data3dnew(i,j,k) = a_data3dnew(i,j,k) + a_space_bdy_ye(i,k,jj) / time_diff
       END DO
       END DO
       END DO
    END IF

 END SUBROUTINE a_stuff_bdytend_new

 SUBROUTINE a_stuff_bdytend_old ( a_data3dold , time_diff , &
                             a_space_bdy_xs, a_space_bdy_xe, a_space_bdy_ys, a_space_bdy_ye, &
                             char_stagger , &
                             spec_bdy_width , &
                             ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , &
                             its, ite, jts, jte, kts, kte )

!-------------------------------------------------------------------------
!  Derived from share/module_bc.F
!  Author: Xin Zhang, 10/3/2010
!-------------------------------------------------------------------------
 !  This routine puts the tendency data into the proper locations
 !  for the lateral boundary arrays.

    IMPLICIT NONE

    INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER , INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER , INTENT(IN) :: its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN) :: spec_bdy_width
    REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(INOUT) :: a_data3dold
    REAL , DIMENSION(jms:jme,kds:kde,spec_bdy_width) , INTENT(INOUT) :: a_space_bdy_xs, a_space_bdy_xe
    REAL , DIMENSION(ims:ime,kds:kde,spec_bdy_width) , INTENT(INOUT) :: a_space_bdy_ys, a_space_bdy_ye
    CHARACTER (LEN=1) , INTENT(IN) :: char_stagger
    REAL , INTENT(IN) :: time_diff ! seconds

    INTEGER :: i , ii , j , jj , k

    !  There are four lateral boundary locations that are stored.

    !  X start boundary

    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_xs(j,k,i) / time_diff
          a_space_bdy_xs(j,k,i) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_xs(j,k,i) / time_diff
          a_space_bdy_xs(j,k,i) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_xs(j,k,i) / time_diff
          a_space_bdy_xs(j,k,i) = 0.0
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_xs(j,k,i) / time_diff
          a_space_bdy_xs(j,k,i) = 0.0
       END DO
       END DO
       END DO
    END IF

    !  X end boundary

    IF      ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide,ite) , MAX(ide - spec_bdy_width + 1,its) , -1
          ii = ide - i + 1
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_xe(j,k,ii) / time_diff
          a_space_bdy_xe(j,k,ii) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_xe(j,k,ii) / time_diff
          a_space_bdy_xe(j,k,ii) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_xe(j,k,ii) / time_diff
          a_space_bdy_xe(j,k,ii) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_xe(j,k,ii) / time_diff
          a_space_bdy_xe(j,k,ii) = 0.0
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_xe(j,k,ii) / time_diff
          a_space_bdy_xe(j,k,ii) = 0.0
       END DO
       END DO
       END DO
    END IF

    !  Y start boundary

    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_ys(i,k,j) / time_diff
          a_space_bdy_ys(i,k,j) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO k = kds , kde
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_ys(i,k,j) / time_diff
          a_space_bdy_ys(i,k,j) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide,ite)
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_ys(i,k,j) / time_diff
          a_space_bdy_ys(i,k,j) = 0.0
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_ys(i,k,j) / time_diff
          a_space_bdy_ys(i,k,j) = 0.0
       END DO
       END DO
       END DO
    END IF

    !  Y end boundary

    IF      ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde,jte) , MAX(jde - spec_bdy_width + 1,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j + 1
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_ye(i,k,jj) / time_diff
          a_space_bdy_ye(i,k,jj) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide,ite)
          jj = jde - j
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_ye(i,k,jj) / time_diff
          a_space_bdy_ye(i,k,jj) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_ye(i,k,jj) / time_diff
          a_space_bdy_ye(i,k,jj) = 0.0
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_ye(i,k,jj) / time_diff
          a_space_bdy_ye(i,k,jj) = 0.0
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          a_data3dold(i,j,k) = a_data3dold(i,j,k) - a_space_bdy_ye(i,k,jj) / time_diff
          a_space_bdy_ye(i,k,jj) = 0.0
       END DO
       END DO
       END DO
    END IF

 END SUBROUTINE a_stuff_bdytend_old

SUBROUTINE g_couple ( config_flags, mu, g_mu, mub, g_rfield, field, &
                    g_field, name, msf,               &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

!-------------------------------------------------------------------------
!  Derived from dyn_em/module_big_step_utilities_em.F
!  Author: Xin Zhang, 10/2/2010
!-------------------------------------------------------------------------
   IMPLICIT NONE

   ! Input data

   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER ,             INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                          ims, ime, jms, jme, kms, kme, &
                                          its, ite, jts, jte, kts, kte

   CHARACTER(LEN=1) ,     INTENT(IN   ) :: name

   REAL , DIMENSION( ims:ime , jms:jme , kms:kme ) , INTENT(  OUT) :: g_rfield

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mu, g_mu, mub, msf
   
   REAL , DIMENSION( ims:ime , jms:jme , kms:kme ) , INTENT(IN   ) :: field, g_field
   
   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   REAL , DIMENSION(ims:ime,jms:jme) :: muu , muv
   REAL , DIMENSION(ims:ime,jms:jme) :: g_muu , g_muv

!<DESCRIPTION>
!
! subroutine couple couples the input variable with the dry-air 
! column mass (mu).  
!
!</DESCRIPTION>

   ktf=MIN(kte,kde-1)
   
   IF (name .EQ. 'u')THEN

      muu = 0.0
      muv = 0.0
      g_muu = 0.0
      g_muv = 0.0
   
      CALL g_calc_mu_uv ( config_flags, mu, g_mu, mub, &
                          muu, g_muu, muv, g_muv,      &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte )

      itf=ite
      jtf=MIN(jte,jde-1)

      DO k=kts,ktf
      DO j=jts,jtf
      DO i=its,itf
         g_rfield(i,j,k)=g_field(i,j,k)*muu(i,j)/msf(i,j) + &
                         field(i,j,k)*g_muu(i,j)/msf(i,j)
      ENDDO
      ENDDO
      ENDDO

   ELSE IF (name .EQ. 'v')THEN

      muu = 0.0
      muv = 0.0
      g_muu = 0.0
      g_muv = 0.0
   
      CALL g_calc_mu_uv ( config_flags, mu, g_mu, mub, &
                          muu, g_muu, muv, g_muv,      &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte )

      itf=ite
      itf=MIN(ite,ide-1)
      jtf=jte

      DO k=kts,ktf
      DO j=jts,jtf
      DO i=its,itf
           g_rfield(i,j,k)=g_field(i,j,k)*muv(i,j)/msf(i,j) + &
                         field(i,j,k)*g_muv(i,j)/msf(i,j)
      ENDDO
      ENDDO
      ENDDO

   ELSE IF (name .EQ. 'w')THEN
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      DO k=kts,kte
      DO j=jts,jtf
      DO i=its,itf
         g_rfield(i,j,k)=g_field(i,j,k)*(mu(i,j)+mub(i,j))/msf(i,j) + &
                       field(i,j,k)*g_mu(i,j)/msf(i,j)
      ENDDO
      ENDDO
      ENDDO

   ELSE IF (name .EQ. 'h')THEN
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      DO k=kts,kte
      DO j=jts,jtf
      DO i=its,itf
         g_rfield(i,j,k)=g_field(i,j,k)*(mu(i,j)+mub(i,j)) + &
                       field(i,j,k)*g_mu(i,j)
      ENDDO
      ENDDO
      ENDDO

   ELSE 
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      DO k=kts,ktf
      DO j=jts,jtf
      DO i=its,itf
         g_rfield(i,j,k)=g_field(i,j,k)*(mu(i,j)+mub(i,j)) + &
                       field(i,j,k)*g_mu(i,j)
      ENDDO
      ENDDO
      ENDDO
   
   ENDIF

END SUBROUTINE g_couple

SUBROUTINE a_couple ( config_flags, mu, a_mu, mub, a_rfield, field, &
                    a_field, name, msf,               &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

!-------------------------------------------------------------------------
!  Derived from dyn_em/module_big_step_utilities_em.F
!  Author: Xin Zhang, 10/2/2010
!-------------------------------------------------------------------------
   IMPLICIT NONE

   ! Input data

   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER ,             INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                          ims, ime, jms, jme, kms, kme, &
                                          its, ite, jts, jte, kts, kte

   CHARACTER(LEN=1) ,     INTENT(IN   ) :: name

   REAL , DIMENSION( ims:ime , jms:jme , kms:kme ) , INTENT(INOUT) :: a_rfield

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mu,  mub, msf
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: a_mu
   
   REAL , DIMENSION( ims:ime , jms:jme , kms:kme ) , INTENT(IN   ) :: field
   REAL , DIMENSION( ims:ime , jms:jme , kms:kme ) , INTENT(INOUT) :: a_field
   
   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   REAL , DIMENSION(ims:ime,jms:jme) :: muu , muv
   REAL , DIMENSION(ims:ime,jms:jme) :: a_muu , a_muv

!<DESCRIPTION>
!
! subroutine couple couples the input variable with the dry-air 
! column mass (mu).  
!
!</DESCRIPTION>

   ktf=MIN(kte,kde-1)
   
   IF (name .EQ. 'u')THEN

      muu = 0.0
      muv = 0.0
      a_muu = 0.0 
      a_muv = 0.0 

      CALL calc_mu_uv ( config_flags,                 &
                        mu, mub, muu, muv,            &
                        ids, ide, jds, jde, kds, kde, &
                        ims, ime, jms, jme, kms, kme, &
                        its, ite, jts, jte, kts, kte )

      itf=ite
      jtf=MIN(jte,jde-1)

      DO k=kts,ktf
      DO j=jts,jtf
      DO i=its,itf
         a_field(i,j,k)=a_field(i,j,k) + a_rfield(i,j,k)*muu(i,j)/msf(i,j)
         a_muu(i,j)=a_muu(i,j) + a_rfield(i,j,k)*field(i,j,k)/msf(i,j)
         a_rfield(i,j,k) = 0.0
      ENDDO
      ENDDO
      ENDDO

      CALL a_calc_mu_uv ( config_flags,                 &
                          a_mu, a_muu, a_muv,  &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte )

   ELSE IF (name .EQ. 'v')THEN

      muu = 0.0
      muv = 0.0
      a_muu = 0.0 
      a_muv = 0.0 

      CALL calc_mu_uv ( config_flags,                 &
                        mu, mub, muu, muv,            &
                        ids, ide, jds, jde, kds, kde, &
                        ims, ime, jms, jme, kms, kme, &
                        its, ite, jts, jte, kts, kte )

      itf=ite
      itf=MIN(ite,ide-1)
      jtf=jte

      DO k=kts,ktf
      DO j=jts,jtf
      DO i=its,itf
         a_field(i,j,k)=a_field(i,j,k) + a_rfield(i,j,k)*muv(i,j)/msf(i,j)
         a_muv(i,j)=a_muv(i,j) + a_rfield(i,j,k)*field(i,j,k)/msf(i,j)
         a_rfield(i,j,k) = 0.0
      ENDDO
      ENDDO
      ENDDO

      CALL a_calc_mu_uv ( config_flags,                 &
                          a_mu, a_muu, a_muv,  &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte )

   ELSE IF (name .EQ. 'w')THEN
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      DO k=kts,kte
      DO j=jts,jtf
      DO i=its,itf
         a_field(i,j,k)=a_field(i,j,k) + a_rfield(i,j,k)*(mu(i,j)+mub(i,j))/msf(i,j)
         a_mu(i,j)=a_mu(i,j) + a_rfield(i,j,k)*field(i,j,k)/msf(i,j)
         a_rfield(i,j,k) = 0.0
      ENDDO
      ENDDO
      ENDDO

   ELSE IF (name .EQ. 'h')THEN
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      DO k=kts,kte
      DO j=jts,jtf
      DO i=its,itf
         a_field(i,j,k)=a_field(i,j,k) + a_rfield(i,j,k)*(mu(i,j)+mub(i,j))
         a_mu(i,j)=a_mu(i,j) + a_rfield(i,j,k)*field(i,j,k)
         a_rfield(i,j,k) = 0.0
      ENDDO
      ENDDO
      ENDDO

   ELSE 
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      DO k=kts,ktf
      DO j=jts,jtf
      DO i=its,itf
         a_field(i,j,k)=a_field(i,j,k) + a_rfield(i,j,k)*(mu(i,j)+mub(i,j))
         a_mu(i,j)=a_mu(i,j) + a_rfield(i,j,k)*field(i,j,k)
         a_rfield(i,j,k) = 0.0
      ENDDO
      ENDDO
      ENDDO
   
   ENDIF

END SUBROUTINE a_couple

 SUBROUTINE da_calc_2nd_fg ( data3d , space_bdy_xs, space_bdy_xe, space_bdy_ys, space_bdy_ye, &
                             space_bdy_txs, space_bdy_txe, space_bdy_tys, space_bdy_tye, &
                             time_diff, char_stagger , &
                             spec_bdy_width , &
                             ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , & 
                             its, ite, jts, jte, kts, kte )
 
!-------------------------------------------------------------------------
!  Calculate the first guess at the end of thr time window
!  Author: Xin Zhang, 10/7/2010
!-------------------------------------------------------------------------
 
    IMPLICIT NONE
 
    INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER , INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER , INTENT(IN) :: its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN) :: spec_bdy_width
    REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(INOUT) :: data3d
    REAL , DIMENSION(jms:jme,kds:kde,spec_bdy_width) , INTENT(IN) :: space_bdy_xs, space_bdy_xe
    REAL , DIMENSION(ims:ime,kds:kde,spec_bdy_width) , INTENT(IN) :: space_bdy_ys, space_bdy_ye
    REAL , DIMENSION(jms:jme,kds:kde,spec_bdy_width) , INTENT(IN) :: space_bdy_txs, space_bdy_txe
    REAL , DIMENSION(ims:ime,kds:kde,spec_bdy_width) , INTENT(IN) :: space_bdy_tys, space_bdy_tye
    CHARACTER (LEN=1) , INTENT(IN) :: char_stagger
    REAL , INTENT(IN) :: time_diff
 
    INTEGER :: i , ii , j , jj , k
 
    !  There are four lateral boundary locations that are stored.
 
    !  X start boundary
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          data3d(i,j,k) = space_bdy_xs(j,k,i) + time_diff * space_bdy_txs(j,k,i)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          data3d(i,j,k) = space_bdy_xs(j,k,i) + time_diff * space_bdy_txs(j,k,i)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          data3d(i,j,k) = space_bdy_xs(j,k,i) + time_diff * space_bdy_txs(j,k,i)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          data3d(i,j,k) = space_bdy_xs(j,k,i) + time_diff * space_bdy_txs(j,k,i)
       END DO
       END DO
       END DO
    END IF
 
    !  X end boundary
 
    IF      ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide,ite) , MAX(ide - spec_bdy_width + 1,its) , -1
          ii = ide - i + 1
          data3d(i,j,k) = space_bdy_xe(j,k,ii) + time_diff * space_bdy_txe(j,k,ii)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          data3d(i,j,k) = space_bdy_xe(j,k,ii) + time_diff * space_bdy_txe(j,k,ii)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          data3d(i,j,k) = space_bdy_xe(j,k,ii) + time_diff * space_bdy_txe(j,k,ii)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          data3d(i,j,k) = space_bdy_xe(j,k,ii) + time_diff * space_bdy_txe(j,k,ii)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jde-1,jte)
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          data3d(i,j,k) = space_bdy_xe(j,k,ii) + time_diff * space_bdy_txe(j,k,ii)
       END DO
       END DO
       END DO
    END IF
 
    !  Y start boundary
 
    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          data3d(i,j,k) = space_bdy_ys(i,k,j) + time_diff * space_bdy_tys(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          data3d(i,j,k) = space_bdy_ys(i,k,j) + time_diff * space_bdy_tys(i,k,j)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide,ite)
          data3d(i,j,k) = space_bdy_ys(i,k,j) + time_diff * space_bdy_tys(i,k,j)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          data3d(i,j,k) = space_bdy_ys(i,k,j) + time_diff * space_bdy_tys(i,k,j)
       END DO
       END DO
       END DO
    END IF
 
    !  Y end boundary
 
    IF      ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde,jte) , MAX(jde - spec_bdy_width + 1,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j + 1
          data3d(i,j,k) = space_bdy_ye(i,k,jj) + time_diff * space_bdy_tye(i,k,jj)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide,ite)
          jj = jde - j
          data3d(i,j,k) = space_bdy_ye(i,k,jj) + time_diff * space_bdy_tye(i,k,jj)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          data3d(i,j,k) = space_bdy_ye(i,k,jj) + time_diff * space_bdy_tye(i,k,jj)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          data3d(i,j,k) = space_bdy_ye(i,k,jj) + time_diff * space_bdy_tye(i,k,jj)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = MAX(ids,its) , MIN(ide-1,ite)
          jj = jde - j
          data3d(i,j,k) = space_bdy_ye(i,k,jj) + time_diff * space_bdy_tye(i,k,jj)
       END DO
       END DO
       END DO
    END IF
    
 END SUBROUTINE da_calc_2nd_fg

SUBROUTINE decouple ( mu, mub, field, name, &
                    msf,                          &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

!-------------------------------------------------------------------------
!  Decouple variables
!  Author: Xin Zhang, 10/7/2010
!-------------------------------------------------------------------------
   IMPLICIT NONE

   ! Input data

   INTEGER ,             INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                          ims, ime, jms, jme, kms, kme, &
                                          its, ite, jts, jte, kts, kte

   CHARACTER(LEN=1) ,     INTENT(IN   ) :: name

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mu, mub, msf
   REAL , DIMENSION( ims:ime , jms:jme , kms:kme ) , INTENT(INOUT) :: field

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

      CALL calc_mu_uv ( config_flags,                 &
                        mu, mub, muu, muv,            &
                        ids, ide, jds, jde, kds, kde, &
                        ims, ime, jms, jme, kms, kme, &
                        its, ite, jts, jte, kts, kte )

      itf=ite
      jtf=MIN(jte,jde-1)

      DO k=kts,ktf
      DO j=jts,jtf
      DO i=its,itf
         field(i,j,k)=field(i,j,k)/muu(i,j)*msf(i,j)
      ENDDO
      ENDDO
      ENDDO

   ELSE IF (name .EQ. 'v')THEN

      CALL calc_mu_uv ( config_flags,                 &
                        mu, mub, muu, muv,            &
                        ids, ide, jds, jde, kds, kde, &
                        ims, ime, jms, jme, kms, kme, &
                        its, ite, jts, jte, kts, kte )

      itf=ite
      itf=MIN(ite,ide-1)
      jtf=jte

      DO k=kts,ktf
      DO j=jts,jtf
      DO i=its,itf
           field(i,j,k)=field(i,j,k)/muv(i,j)*msf(i,j)
      ENDDO
      ENDDO
      ENDDO

   ELSE IF (name .EQ. 'w')THEN
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      DO k=kts,kte
      DO j=jts,jtf
      DO i=its,itf
         field(i,j,k)=field(i,j,k)/(mu(i,j)+mub(i,j))*msf(i,j)
      ENDDO
      ENDDO
      ENDDO

   ELSE IF (name .EQ. 'h')THEN
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      DO k=kts,kte
      DO j=jts,jtf
      DO i=its,itf
         field(i,j,k)=field(i,j,k)/(mu(i,j)+mub(i,j))
      ENDDO
      ENDDO
      ENDDO

   ELSE
      itf=MIN(ite,ide-1)
      jtf=MIN(jte,jde-1)
      DO k=kts,ktf
      DO j=jts,jtf
      DO i=its,itf
         field(i,j,k)=field(i,j,k)/(mu(i,j)+mub(i,j))
      ENDDO
      ENDDO
      ENDDO
  
   ENDIF

END SUBROUTINE decouple

SUBROUTINE da_model_lbc_off

   CALL nl_set_io_form_boundary( head_grid%id, 0 )

END SUBROUTINE da_model_lbc_off

SUBROUTINE da_bdy_fields_halo (data3du, data3dv, data3dt, data3dph, data3dmu, &
                             data3dm, dir, xy, spec_bdy_width,              &
                             u_bxs,  u_bxe,  u_bys,  u_bye,                 &
                             v_bxs,  v_bxe,  v_bys,  v_bye,                 &
                             t_bxs,  t_bxe,  t_bys,  t_bye,                 &
                             ph_bxs, ph_bxe, ph_bys, ph_bye,                &
                             mu_bxs, mu_bxe, mu_bys, mu_bye,                &
                             moist_bxs, moist_bxe, moist_bys, moist_bye,    &
                             ids, ide, jds, jde, kds, kde ,                 &
                             ims, ime, jms, jme, kms, kme ,                 &
                             its, ite, jts, jte, kts, kte )

    USE module_state_description
       
    IMPLICIT NONE

    INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER , INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER , INTENT(IN) :: its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN) :: spec_bdy_width
    INTEGER , INTENT(IN) :: dir ! 0----pack ; 1----unpack
    INTEGER , INTENT(IN) :: xy  ! 0----X ; 1----Y
    REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(INOUT) :: data3du , data3dv, data3dt, &
                                                                 data3dph, data3dm
    REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: data3dmu
    REAL , DIMENSION(jms:jme,kds:kde,spec_bdy_width) , INTENT(INOUT) :: u_bxs, u_bxe, v_bxs, v_bxe, &
                                                                        t_bxs, t_bxe, ph_bxs, ph_bxe, &
                                                                        moist_bxs, moist_bxe
    REAL , DIMENSION(jms:jme,1:1,spec_bdy_width) , INTENT(INOUT) :: mu_bxs, mu_bxe
    REAL , DIMENSION(ims:ime,kds:kde,spec_bdy_width) , INTENT(INOUT) :: u_bys, u_bye, v_bys, v_bye, &
                                                                        t_bys, t_bye, ph_bys, ph_bye, &
                                                                        moist_bys, moist_bye
    REAL , DIMENSION(ims:ime,1:1,spec_bdy_width) , INTENT(INOUT) :: mu_bys, mu_bye

   CALL da_bdy_fields_pack ( data3du, u_bxs, u_bxe, u_bys, u_bye,        &
                                    'U' , dir, xy, spec_bdy_width,    &
                                    ids, ide, jds, jde, kds, kde,     &
                                    ims, ime, jms, jme, kms, kme,     &
                                    its, ite, jts, jte, kts, kte      )

   CALL da_bdy_fields_pack ( data3dv, v_bxs, v_bxe, v_bys, v_bye,        &
                                    'V' , dir, xy, spec_bdy_width,    &
                                    ids, ide, jds, jde, kds, kde,     &
                                    ims, ime, jms, jme, kms, kme,     &
                                    its, ite, jts, jte, kts, kte      )

   CALL da_bdy_fields_pack ( data3dt , t_bxs, t_bxe, t_bys, t_bye,       &
                                    'T' , dir, xy, spec_bdy_width,    &
                                    ids, ide, jds, jde, kds, kde,     &
                                    ims, ime, jms, jme, kms, kme,     &
                                    its, ite, jts, jte, kts, kte      )
   
   CALL da_bdy_fields_pack ( data3dph , ph_bxs, ph_bxe, ph_bys, ph_bye,  &
                                    'W' , dir, xy, spec_bdy_width,    &
                                    ids, ide, jds, jde, kds, kde,     &
                                    ims, ime, jms, jme, kms, kme,     &
                                    its, ite, jts, jte, kts, kte      )

   CALL da_bdy_fields_pack ( data3dmu , mu_bxs, mu_bxe, mu_bys, mu_bye,  &
                                    'M' , dir, xy, spec_bdy_width  ,  &
                                    ids, ide, jds, jde, 1  , 1  ,     &
                                    ims, ime, jms, jme, 1  , 1  ,     &
                                    its, ite, jts, jte, 1  , 1        )

   CALL da_bdy_fields_pack ( data3dm , moist_bxs, moist_bxe, moist_bys, moist_bye,       &
                                    'T' , dir, xy, spec_bdy_width,    &
                                    ids, ide, jds, jde, kds, kde,     &
                                    ims, ime, jms, jme, kms, kme,     &
                                    its, ite, jts, jte, kts, kte      )


END SUBROUTINE da_bdy_fields_halo

SUBROUTINE da_bdy_fields_pack ( data3d , space_bdy_xs, space_bdy_xe, space_bdy_ys, space_bdy_ye, &
                             char_stagger , dir , xy ,&
                             spec_bdy_width , &         
                             ids, ide, jds, jde, kds, kde , &
                             ims, ime, jms, jme, kms, kme , &
                             its, ite, jts, jte, kts, kte )
                          
!-------------------------------------------------------------------------
!  Calculate the first guess at the end of thr time window
!  Author: Xin Zhang, 10/7/2010
!-------------------------------------------------------------------------
      
    IMPLICIT NONE
      
    INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde
    INTEGER , INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER , INTENT(IN) :: its, ite, jts, jte, kts, kte
    INTEGER , INTENT(IN) :: spec_bdy_width
    REAL , DIMENSION(ims:ime,jms:jme,kms:kme) , INTENT(INOUT) :: data3d
    REAL , DIMENSION(jms:jme,kds:kde,spec_bdy_width) , INTENT(INOUT) :: space_bdy_xs, space_bdy_xe
    REAL , DIMENSION(ims:ime,kds:kde,spec_bdy_width) , INTENT(INOUT) :: space_bdy_ys, space_bdy_ye
    CHARACTER (LEN=1) , INTENT(IN) :: char_stagger
    INTEGER , INTENT(IN) :: dir ! 0----pack ; 1----unpack
    INTEGER , INTENT(IN) :: xy  ! 0----X ; 1----Y
      
    INTEGER :: i , ii , j , jj , k
         
    !  There are four lateral boundary locations that are stored.
         
IF (dir == 0 ) THEN  ! ----Pack
   IF ( xy == 0 ) THEN  ! ----X 

    !  X start boundary
      
    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = jms, jme
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          data3d(i,j,k) = space_bdy_xs(j,k,i)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = jms, jme
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          data3d(i,j,k) = space_bdy_xs(j,k,i)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = jms, jme
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          data3d(i,j,k) = space_bdy_xs(j,k,i)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = jms, jme
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          data3d(i,j,k) = space_bdy_xs(j,k,i)
       END DO
       END DO
       END DO
    END IF

    !  X end boundary

    IF      ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = jms, jme
       DO i = MIN(ide,ite) , MAX(ide - spec_bdy_width + 1,its) , -1
          ii = ide - i + 1
          data3d(i,j,k) = space_bdy_xe(j,k,ii)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = jms, jme
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          data3d(i,j,k) = space_bdy_xe(j,k,ii)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = jms, jme
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          data3d(i,j,k) = space_bdy_xe(j,k,ii)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = jms, jme
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          data3d(i,j,k) = space_bdy_xe(j,k,ii)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = jms, jme
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          data3d(i,j,k) = space_bdy_xe(j,k,ii)
       END DO
       END DO
       END DO
    END IF
    
ELSE    !    Y 
    !  Y start boundary
    
    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = ims, ime
          data3d(i,j,k) = space_bdy_ys(i,k,j)
       END DO
       END DO
       END DO 
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = ims, ime
          data3d(i,j,k) = space_bdy_ys(i,k,j)
       END DO
       END DO
       END DO 
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = ims, ime
          data3d(i,j,k) = space_bdy_ys(i,k,j)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = ims, ime
          data3d(i,j,k) = space_bdy_ys(i,k,j)
       END DO
       END DO
       END DO
    END IF

    !  Y end boundary

    IF      ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde,jte) , MAX(jde - spec_bdy_width + 1,jts) , -1
       DO i = ims, ime
          jj = jde - j + 1
          data3d(i,j,k) = space_bdy_ye(i,k,jj)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = ims, ime
          jj = jde - j
          data3d(i,j,k) = space_bdy_ye(i,k,jj)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = ims, ime
          jj = jde - j
          data3d(i,j,k) = space_bdy_ye(i,k,jj)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = ims, ime
          jj = jde - j
          data3d(i,j,k) = space_bdy_ye(i,k,jj)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = ims, ime
          jj = jde - j
          data3d(i,j,k) = space_bdy_ye(i,k,jj)
       END DO
       END DO
       END DO
    END IF

   END IF
END IF

IF ( dir == 1 ) THEN  ! ---- Unpack
   IF ( xy == 0 ) THEN !----- X

    !  X start boundary
      
    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = jms, jme
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = jms, jme
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = jms, jme
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = jms, jme
       DO i = MAX(ids,its) , MIN(ids + spec_bdy_width - 1,ite)
          space_bdy_xs(j,k,i) = data3d(i,j,k)
       END DO
       END DO
       END DO
    END IF

    !  X end boundary

    IF      ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = jms, jme
       DO i = MIN(ide,ite) , MAX(ide - spec_bdy_width + 1,its) , -1
          ii = ide - i + 1
          space_bdy_xe(j,k,ii) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = jms, jme
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = jms, jme
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = jms, jme
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = jms, jme
       DO i = MIN(ide - 1,ite) , MAX(ide - spec_bdy_width,its) , -1
          ii = ide - i
          space_bdy_xe(j,k,ii) = data3d(i,j,k)
       END DO
       END DO
       END DO
    END IF
    
ELSE   !  Y  
    !  Y start boundary
    
    IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = ims, ime
          space_bdy_ys(i,k,j) = data3d(i,j,k)
       END DO
       END DO
       END DO 
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = ims, ime
          space_bdy_ys(i,k,j) = data3d(i,j,k)
       END DO
       END DO
       END DO 
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = ims, ime
          space_bdy_ys(i,k,j) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MAX(jds,jts) , MIN(jds + spec_bdy_width - 1,jte)
       DO i = ims, ime
          space_bdy_ys(i,k,j) = data3d(i,j,k)
       END DO
       END DO
       END DO
    END IF

    !  Y end boundary

    IF      ( char_stagger .EQ. 'V' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde,jte) , MAX(jde - spec_bdy_width + 1,jts) , -1
       DO i = ims, ime
          jj = jde - j + 1
          space_bdy_ye(i,k,jj) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'U' ) THEN
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = ims, ime
          jj = jde - j
          space_bdy_ye(i,k,jj) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'W' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = ims, ime
          jj = jde - j
          space_bdy_ye(i,k,jj) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE IF ( char_stagger .EQ. 'M' ) THEN
       DO k = kds , kde
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = ims, ime
          jj = jde - j
          space_bdy_ye(i,k,jj) = data3d(i,j,k)
       END DO
       END DO
       END DO
    ELSE
       DO k = kds , kde - 1
       DO j = MIN(jde-1,jte) , MAX(jde - spec_bdy_width,jts) , -1
       DO i = ims, ime
          jj = jde - j
          space_bdy_ye(i,k,jj) = data3d(i,j,k)
       END DO
       END DO
       END DO
    END IF

   END IF
END IF

END SUBROUTINE da_bdy_fields_pack
