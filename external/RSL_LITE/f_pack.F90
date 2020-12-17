      MODULE duplicate_of_driver_constants
! These definitions must be the same as frame/module_driver_constants
! and also the same as the definitions in rsl_lite.h
         INTEGER , PARAMETER :: DATA_ORDER_XYZ = 1
         INTEGER , PARAMETER :: DATA_ORDER_YXZ = 2
         INTEGER , PARAMETER :: DATA_ORDER_ZXY = 3
         INTEGER , PARAMETER :: DATA_ORDER_ZYX = 4
         INTEGER , PARAMETER :: DATA_ORDER_XZY = 5
         INTEGER , PARAMETER :: DATA_ORDER_YZX = 6
      END MODULE duplicate_of_driver_constants

      SUBROUTINE f_pack_int ( inbuf, outbuf, memorder, js, je, ks, ke,            &
     &                    is, ie, jms, jme, kms, kme, ims, ime, curs )
        USE duplicate_of_driver_constants
        IMPLICIT NONE
        INTEGER, INTENT(IN) ::  memorder
        INTEGER ims, ime, jms, jme, kms, kme
        INTEGER inbuf(*), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        SELECT CASE ( memorder )
          CASE ( DATA_ORDER_XYZ )
            CALL f_pack_int_ijk( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_YXZ )
            CALL f_pack_int_jik( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_XZY )
            CALL f_pack_int_ikj( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_YZX )
            CALL f_pack_int_jki( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_ZXY )
            CALL f_pack_int_kij( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_ZYX )
            CALL f_pack_int_kji( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
        END SELECT
        RETURN
      END SUBROUTINE f_pack_int
     
      SUBROUTINE f_pack_lint ( inbuf, outbuf, memorder, js, je, ks, ke,           &
     &                     is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        USE duplicate_of_driver_constants
        IMPLICIT NONE
        INTEGER, INTENT(IN) ::  memorder
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 inbuf(*), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        SELECT CASE ( memorder )
          CASE ( DATA_ORDER_XYZ )
            CALL f_pack_lint_ijk( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_YXZ )
            CALL f_pack_lint_jik( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_XZY )
            CALL f_pack_lint_ikj( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_YZX )
            CALL f_pack_lint_jki( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_ZXY )
            CALL f_pack_lint_kij( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_ZYX )
            CALL f_pack_lint_kji( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
        END SELECT
        RETURN
      END SUBROUTINE f_pack_lint
     
      SUBROUTINE f_unpack_int ( inbuf, outbuf, memorder, js, je, ks, ke,           &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        USE duplicate_of_driver_constants
        IMPLICIT NONE
        INTEGER, INTENT(IN) ::  memorder
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER outbuf(*), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        SELECT CASE ( memorder )
          CASE ( DATA_ORDER_XYZ )
            CALL f_unpack_int_ijk( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_YXZ )
            CALL f_unpack_int_jik( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_XZY )
            CALL f_unpack_int_ikj( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_YZX )
            CALL f_unpack_int_jki( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_ZXY )
            CALL f_unpack_int_kij( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_ZYX )
            CALL f_unpack_int_kji( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
        END SELECT
        RETURN
      END SUBROUTINE f_unpack_int
     
      SUBROUTINE f_unpack_lint ( inbuf, outbuf, memorder, js, je, ks,               &
     &                 ke, is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        USE duplicate_of_driver_constants
        IMPLICIT NONE
        INTEGER, INTENT(IN) ::  memorder
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 outbuf(*), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        SELECT CASE ( memorder )
          CASE ( DATA_ORDER_XYZ )
            CALL f_unpack_lint_ijk( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_YXZ )
            CALL f_unpack_lint_jik( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_XZY )
            CALL f_unpack_lint_ikj( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_YZX )
            CALL f_unpack_lint_jki( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_ZXY )
            CALL f_unpack_lint_kij( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_ZYX )
            CALL f_unpack_lint_kji( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
        END SELECT
        RETURN
      END SUBROUTINE f_unpack_lint

!ikj
      SUBROUTINE f_pack_int_ikj ( inbuf, outbuf, js, je, ks, ke,              &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER inbuf(ims:ime,kms:kme,jms:jme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
        DO j = js, je
#ifdef _OPENMP
        p = (j-js)*(ie-is+1)*(ke-ks+1)+1
#endif
          DO k = ks, ke
            DO i = is, ie
              outbuf(p) = inbuf(i,k,j)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL

        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_pack_int_ikj
     
      SUBROUTINE f_pack_lint_ikj ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 inbuf(ims:ime,kms:kme,jms:jme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
        DO j = js, je
#ifdef _OPENMP
        p = (j-js)*(ie-is+1)*(ke-ks+1)+1
#endif
          DO k = ks, ke
            DO i = is, ie
              outbuf(p) = inbuf(i,k,j)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_pack_lint_ikj
     
      SUBROUTINE f_unpack_int_ikj ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER outbuf(ims:ime,kms:kme,jms:jme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
        DO j = js, je
#ifdef _OPENMP
        p = (j-js)*(ie-is+1)*(ke-ks+1)+1
#endif
          DO k = ks, ke
            DO i = is, ie
              outbuf(i,k,j) = inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_unpack_int_ikj
     
      SUBROUTINE f_unpack_lint_ikj ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 outbuf(ims:ime,kms:kme,jms:jme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
        DO j = js, je
#ifdef _OPENMP
        p = (j-js)*(ie-is+1)*(ke-ks+1)+1
#endif
          DO k = ks, ke
            DO i = is, ie
              outbuf(i,k,j) = inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_unpack_lint_ikj

!jki
      SUBROUTINE f_pack_int_jki ( inbuf, outbuf, js, je, ks, ke,              &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER inbuf(jms:jme,kms:kme,ims:ime), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
            DO i = is, ie
#ifdef _OPENMP
        p = (i-is)*(je-js+1)*(ke-ks+1)+1
#endif
          DO k = ks, ke
        DO j = js, je
              outbuf(p) = inbuf(j,k,i)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_pack_int_jki
     
      SUBROUTINE f_pack_lint_jki ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 inbuf(jms:jme,kms:kme,ims:ime), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
            DO i = is, ie
#ifdef _OPENMP
        p = (i-is)*(je-js+1)*(ke-ks+1)+1
#endif
          DO k = ks, ke
        DO j = js, je
              outbuf(p) = inbuf(j,k,i)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_pack_lint_jki
     
      SUBROUTINE f_unpack_int_jki ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER outbuf(jms:jme,kms:kme,ims:ime), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
            DO i = is, ie
#ifdef _OPENMP
        p = (i-is)*(je-js+1)*(ke-ks+1)+1
#endif
          DO k = ks, ke
        DO j = js, je
              outbuf(j,k,i) = inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_unpack_int_jki
     
      SUBROUTINE f_unpack_lint_jki ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 outbuf(jms:jme,kms:kme,ims:ime), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
            DO i = is, ie
#ifdef _OPENMP
        p = (i-is)*(je-js+1)*(ke-ks+1)+1
#endif
          DO k = ks, ke
        DO j = js, je
              outbuf(j,k,i) = inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_unpack_lint_jki

!ijk
      SUBROUTINE f_pack_int_ijk ( inbuf, outbuf, js, je, ks, ke,              &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER inbuf(ims:ime,jms:jme,kms:kme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
        DO k = ks, ke
#ifdef _OPENMP
        p = (k-ks)*(je-js+1)*(ie-is+1)+1
#endif
          DO j = js, je
            DO i = is, ie
              outbuf(p) = inbuf(i,j,k)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_pack_int_ijk
     
      SUBROUTINE f_pack_lint_ijk ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 inbuf(ims:ime,jms:jme,kms:kme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
        DO k = ks, ke
#ifdef _OPENMP
        p = (k-ks)*(je-js+1)*(ie-is+1)+1
#endif
          DO j = js, je
            DO i = is, ie
              outbuf(p) = inbuf(i,j,k)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_pack_lint_ijk
     
      SUBROUTINE f_unpack_int_ijk ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER outbuf(ims:ime,jms:jme,kms:kme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
        DO k = ks, ke
#ifdef _OPENMP
        p = (k-ks)*(je-js+1)*(ie-is+1)+1
#endif
          DO j = js, je
            DO i = is, ie
              outbuf(i,j,k) = inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_unpack_int_ijk
     
      SUBROUTINE f_unpack_lint_ijk ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 outbuf(ims:ime,jms:jme,kms:kme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
        DO k = ks, ke
#ifdef _OPENMP
        p = (k-ks)*(je-js+1)*(ie-is+1)+1
#endif
          DO j = js, je
            DO i = is, ie
              outbuf(i,j,k) = inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_unpack_lint_ijk
     
!jik
      SUBROUTINE f_pack_int_jik ( inbuf, outbuf, js, je, ks, ke,              &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER inbuf(jms:jme,ims:ime,kms:kme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
        DO k = ks, ke
#ifdef _OPENMP
        p = (k-ks)*(je-js+1)*(ie-is+1)+1
#endif
          DO i = is, ie
            DO j = js, je
              outbuf(p) = inbuf(j,i,k)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_pack_int_jik
     
      SUBROUTINE f_pack_lint_jik ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 inbuf(jms:jme,ims:ime,kms:kme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
        DO k = ks, ke
#ifdef _OPENMP
        p = (k-ks)*(je-js+1)*(ie-is+1)+1
#endif
          DO i = is, ie
            DO j = js, je
              outbuf(p) = inbuf(j,i,k)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_pack_lint_jik
     
      SUBROUTINE f_unpack_int_jik ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER outbuf(jms:jme,ims:ime,kms:kme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
        DO k = ks, ke
#ifdef _OPENMP
        p = (k-ks)*(je-js+1)*(ie-is+1)+1
#endif
          DO i = is, ie
            DO j = js, je
              outbuf(j,i,k) = inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_unpack_int_jik
     
      SUBROUTINE f_unpack_lint_jik ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 outbuf(jms:jme,ims:ime,kms:kme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
        DO k = ks, ke
#ifdef _OPENMP
        p = (k-ks)*(je-js+1)*(ie-is+1)+1
#endif
          DO i = is, ie
            DO j = js, je
              outbuf(j,i,k) = inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_unpack_lint_jik

!kij
      SUBROUTINE f_pack_int_kij ( inbuf, outbuf, js, je, ks, ke,              &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER inbuf(kms:kme,ims:ime,jms:jme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
        DO j = js, je
#ifdef _OPENMP
        p = (j-js)*(ke-ks+1)*(ie-is+1)+1
#endif
          DO i = is, ie
            DO k = ks, ke
              outbuf(p) = inbuf(k,i,j)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_pack_int_kij
     
      SUBROUTINE f_pack_lint_kij ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 inbuf(kms:kme,ims:ime,jms:jme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
        DO j = js, je
#ifdef _OPENMP
        p = (j-js)*(ke-ks+1)*(ie-is+1)+1
#endif
          DO i = is, ie
            DO k = ks, ke
              outbuf(p) = inbuf(k,i,j)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_pack_lint_kij
     
      SUBROUTINE f_unpack_int_kij ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER outbuf(kms:kme,ims:ime,jms:jme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
        DO j = js, je
#ifdef _OPENMP
        p = (j-js)*(ke-ks+1)*(ie-is+1)+1
#endif
          DO i = is, ie
            DO k = ks, ke
              outbuf(k,i,j) = inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_unpack_int_kij
     
      SUBROUTINE f_unpack_lint_kij ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 outbuf(kms:kme,ims:ime,jms:jme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
        DO j = js, je
#ifdef _OPENMP
        p = (j-js)*(ke-ks+1)*(ie-is+1)+1
#endif
          DO i = is, ie
            DO k = ks, ke
              outbuf(k,i,j) = inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_unpack_lint_kij

!kji
      SUBROUTINE f_pack_int_kji ( inbuf, outbuf, js, je, ks, ke,              &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER inbuf(kms:kme,jms:jme,ims:ime), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
          DO i = is, ie
#ifdef _OPENMP
        p = (i-is)*(ke-ks+1)*(je-js+1)+1
#endif
        DO j = js, je
            DO k = ks, ke
              outbuf(p) = inbuf(k,j,i)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_pack_int_kji
     
      SUBROUTINE f_pack_lint_kji ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 inbuf(kms:kme,jms:jme,ims:ime), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
          DO i = is, ie
#ifdef _OPENMP
        p = (i-is)*(ke-ks+1)*(je-js+1)+1
#endif
        DO j = js, je
            DO k = ks, ke
              outbuf(p) = inbuf(k,j,i)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_pack_lint_kji
     
      SUBROUTINE f_unpack_int_kji ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER outbuf(kms:kme,jms:jme,ims:ime), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
          DO i = is, ie
#ifdef _OPENMP
        p = (i-is)*(ke-ks+1)*(je-js+1)+1
#endif
        DO j = js, je
            DO k = ks, ke
              outbuf(k,j,i) = inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_unpack_int_kji
     
      SUBROUTINE f_unpack_lint_kji ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 outbuf(kms:kme,jms:jme,ims:ime), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
!$OMP PARALLEL PRIVATE (i,j,k,p) 
#ifndef _OPENMP
        p = 1
#endif
!$OMP DO SCHEDULE(RUNTIME) 
          DO i = is, ie
#ifdef _OPENMP
        p = (i-is)*(ke-ks+1)*(je-js+1)+1
#endif
        DO j = js, je
            DO k = ks, ke
              outbuf(k,j,i) = inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
!$OMP END PARALLEL
        curs = (ie-is+1)*(je-js+1)*(ke-ks+1)
        RETURN
      END SUBROUTINE f_unpack_lint_kji

#if ( WRFPLUS == 1 )
!--------------------------------------------------------------------------------
      SUBROUTINE f_pack_int_ad ( inbuf, outbuf, memorder, js, je, ks, ke,            &
     &                    is, ie, jms, jme, kms, kme, ims, ime, curs )
        USE duplicate_of_driver_constants
        IMPLICIT NONE
        INTEGER, INTENT(IN) ::  memorder
        INTEGER ims, ime, jms, jme, kms, kme
        INTEGER inbuf(*), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        SELECT CASE ( memorder )
          CASE ( DATA_ORDER_XYZ )
            CALL f_pack_real_ad_ijk( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_YXZ )
            CALL f_pack_real_ad_jik( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_XZY )
            CALL f_pack_real_ad_ikj( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_YZX )
            CALL f_pack_real_ad_jki( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_ZXY )
            CALL f_pack_real_ad_kij( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_ZYX )
            CALL f_pack_real_ad_kji( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
        END SELECT
        RETURN
      END SUBROUTINE f_pack_int_ad
     
      SUBROUTINE f_pack_lint_ad ( inbuf, outbuf, memorder, js, je, ks, ke,           &
     &                     is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        USE duplicate_of_driver_constants
        IMPLICIT NONE
        INTEGER, INTENT(IN) ::  memorder
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 inbuf(*), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        SELECT CASE ( memorder )
          CASE ( DATA_ORDER_XYZ )
            CALL f_pack_lreal_ad_ijk( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_YXZ )
            CALL f_pack_lreal_ad_jik( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_XZY )
            CALL f_pack_lreal_ad_ikj( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_YZX )
            CALL f_pack_lreal_ad_jki( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_ZXY )
            CALL f_pack_lreal_ad_kij( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_ZYX )
            CALL f_pack_lreal_ad_kji( inbuf, outbuf, js, je, ks, ke, is, ie,           &
     &                           jms, jme, kms, kme, ims, ime, curs )
        END SELECT
        RETURN
      END SUBROUTINE f_pack_lint_ad
     
      SUBROUTINE f_unpack_int_ad ( inbuf, outbuf, memorder, js, je, ks, ke,           &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        USE duplicate_of_driver_constants
        IMPLICIT NONE
        INTEGER, INTENT(IN) ::  memorder
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER outbuf(*), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        SELECT CASE ( memorder )
          CASE ( DATA_ORDER_XYZ )
            CALL f_unpack_real_ad_ijk( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_YXZ )
            CALL f_unpack_real_ad_jik( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_XZY )
            CALL f_unpack_real_ad_ikj( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_YZX )
            CALL f_unpack_real_ad_jki( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_ZXY )
            CALL f_unpack_real_ad_kij( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_ZYX )
            CALL f_unpack_real_ad_kji( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
        END SELECT
        RETURN
      END SUBROUTINE f_unpack_int_ad
     
      SUBROUTINE f_unpack_lint_ad ( inbuf, outbuf, memorder, js, je, ks,               &
     &                 ke, is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        USE duplicate_of_driver_constants
        IMPLICIT NONE
        INTEGER, INTENT(IN) ::  memorder
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 outbuf(*), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        SELECT CASE ( memorder )
          CASE ( DATA_ORDER_XYZ )
            CALL f_unpack_lreal_ad_ijk( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_YXZ )
            CALL f_unpack_lreal_ad_jik( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_XZY )
            CALL f_unpack_lreal_ad_ikj( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_YZX )
            CALL f_unpack_lreal_ad_jki( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_ZXY )
            CALL f_unpack_lreal_ad_kij( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
          CASE ( DATA_ORDER_ZYX )
            CALL f_unpack_lreal_ad_kji( inbuf, outbuf, js, je, ks, ke,                   &
     &                      is, ie, jms, jme, kms, kme, ims, ime, curs )
        END SELECT
        RETURN
      END SUBROUTINE f_unpack_lint_ad

!ikj
      SUBROUTINE f_pack_real_ad_ikj ( inbuf, outbuf, js, je, ks, ke,              &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL    inbuf(ims:ime,kms:kme,jms:jme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO j = js, je
          DO k = ks, ke
            DO i = is, ie
              outbuf(p) = inbuf(i,k,j)
              inbuf(i,k,j) = 0.0
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_pack_real_ad_ikj
     
      SUBROUTINE f_pack_lreal_ad_ikj ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL*8  inbuf(ims:ime,kms:kme,jms:jme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO j = js, je
          DO k = ks, ke
            DO i = is, ie
              outbuf(p) = inbuf(i,k,j)
              inbuf(i,k,j) = 0.0
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_pack_lreal_ad_ikj
     
      SUBROUTINE f_unpack_real_ad_ikj ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL    outbuf(ims:ime,kms:kme,jms:jme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO j = js, je
          DO k = ks, ke
            DO i = is, ie
              outbuf(i,k,j) = outbuf(i,k,j) + inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_unpack_real_ad_ikj
     
      SUBROUTINE f_unpack_lreal_ad_ikj ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL*8  outbuf(ims:ime,kms:kme,jms:jme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO j = js, je
          DO k = ks, ke
            DO i = is, ie
              outbuf(i,k,j) = outbuf(i,k,j) + inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_unpack_lreal_ad_ikj

!jki
      SUBROUTINE f_pack_real_ad_jki ( inbuf, outbuf, js, je, ks, ke,              &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL    inbuf(jms:jme,kms:kme,ims:ime), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
            DO i = is, ie
          DO k = ks, ke
        DO j = js, je
              outbuf(p) = inbuf(j,k,i)
              inbuf(j,k,i) = 0.0
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_pack_real_ad_jki
     
      SUBROUTINE f_pack_lreal_ad_jki ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL*8  inbuf(jms:jme,kms:kme,ims:ime), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
            DO i = is, ie
          DO k = ks, ke
        DO j = js, je
              outbuf(p) = inbuf(j,k,i)
              inbuf(j,k,i) = 0.0
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_pack_lreal_ad_jki
     
      SUBROUTINE f_unpack_real_ad_jki ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL    outbuf(jms:jme,kms:kme,ims:ime), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
            DO i = is, ie
          DO k = ks, ke
        DO j = js, je
              outbuf(j,k,i) = outbuf(j,k,i) + inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_unpack_real_ad_jki
     
      SUBROUTINE f_unpack_lreal_ad_jki ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL*8  outbuf(jms:jme,kms:kme,ims:ime), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
            DO i = is, ie
          DO k = ks, ke
        DO j = js, je
              outbuf(j,k,i) = outbuf(j,k,i) + inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_unpack_lreal_ad_jki

!ijk
      SUBROUTINE f_pack_real_ad_ijk ( inbuf, outbuf, js, je, ks, ke,              &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL    inbuf(ims:ime,jms:jme,kms:kme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO k = ks, ke
          DO j = js, je
            DO i = is, ie
              outbuf(p) = inbuf(i,j,k)
              inbuf(i,j,k) = 0.0
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_pack_real_ad_ijk
     
      SUBROUTINE f_pack_lreal_ad_ijk ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL*8  inbuf(ims:ime,jms:jme,kms:kme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO k = ks, ke
          DO j = js, je
            DO i = is, ie
              outbuf(p) = inbuf(i,j,k)
              inbuf(i,j,k) = 0.0
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_pack_lreal_ad_ijk
     
      SUBROUTINE f_unpack_real_ad_ijk ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL    outbuf(ims:ime,jms:jme,kms:kme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO k = ks, ke
          DO j = js, je
            DO i = is, ie
              outbuf(i,j,k) = outbuf(i,j,k) + inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_unpack_real_ad_ijk
     
      SUBROUTINE f_unpack_lreal_ad_ijk ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL*8  outbuf(ims:ime,jms:jme,kms:kme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO k = ks, ke
          DO j = js, je
            DO i = is, ie
              outbuf(i,j,k) = outbuf(i,j,k) + inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_unpack_lreal_ad_ijk
     
!jik
      SUBROUTINE f_pack_real_ad_jik ( inbuf, outbuf, js, je, ks, ke,              &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL    inbuf(jms:jme,ims:ime,kms:kme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO k = ks, ke
          DO i = is, ie
            DO j = js, je
              outbuf(p) = inbuf(j,i,k)
              inbuf(j,i,k) = 0.0
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_pack_real_ad_jik
     
      SUBROUTINE f_pack_lreal_ad_jik ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL*8  inbuf(jms:jme,ims:ime,kms:kme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO k = ks, ke
          DO i = is, ie
            DO j = js, je
              outbuf(p) = inbuf(j,i,k)
              inbuf(j,i,k) = 0.0
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_pack_lreal_ad_jik
     
      SUBROUTINE f_unpack_real_ad_jik ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL    outbuf(jms:jme,ims:ime,kms:kme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO k = ks, ke
          DO i = is, ie
            DO j = js, je
              outbuf(j,i,k) = outbuf(j,i,k) + inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_unpack_real_ad_jik
     
      SUBROUTINE f_unpack_lreal_ad_jik ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL*8  outbuf(jms:jme,ims:ime,kms:kme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO k = ks, ke
          DO i = is, ie
            DO j = js, je
              outbuf(j,i,k) = outbuf(j,i,k) + inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_unpack_lreal_ad_jik

!kij
      SUBROUTINE f_pack_real_ad_kij ( inbuf, outbuf, js, je, ks, ke,              &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL    inbuf(kms:kme,ims:ime,jms:jme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO j = js, je
          DO i = is, ie
            DO k = ks, ke
              outbuf(p) = inbuf(k,i,j)
              inbuf(k,i,j) = 0.0
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_pack_real_ad_kij
     
      SUBROUTINE f_pack_lreal_ad_kij ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL*8  inbuf(kms:kme,ims:ime,jms:jme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO j = js, je
          DO i = is, ie
            DO k = ks, ke
              outbuf(p) = inbuf(k,i,j)
              inbuf(k,i,j) = 0.0
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_pack_lreal_ad_kij
     
      SUBROUTINE f_unpack_real_ad_kij ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL    outbuf(kms:kme,ims:ime,jms:jme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO j = js, je
          DO i = is, ie
            DO k = ks, ke
              outbuf(k,i,j) = outbuf(k,i,j) + inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_unpack_real_ad_kij
     
      SUBROUTINE f_unpack_lreal_ad_kij ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL*8  outbuf(kms:kme,ims:ime,jms:jme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO j = js, je
          DO i = is, ie
            DO k = ks, ke
              outbuf(k,i,j) = outbuf(k,i,j) + inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_unpack_lreal_ad_kij

!kji
      SUBROUTINE f_pack_real_ad_kji ( inbuf, outbuf, js, je, ks, ke,              &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL    inbuf(kms:kme,jms:jme,ims:ime), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
          DO i = is, ie
        DO j = js, je
            DO k = ks, ke
              outbuf(p) = inbuf(k,j,i)
              inbuf(k,j,i) = 0.0
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_pack_real_ad_kji
     
      SUBROUTINE f_pack_lreal_ad_kji ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL*8  inbuf(kms:kme,jms:jme,ims:ime), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
          DO i = is, ie
        DO j = js, je
            DO k = ks, ke
              outbuf(p) = inbuf(k,j,i)
              inbuf(k,j,i) = 0.0
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_pack_lreal_ad_kji
     
      SUBROUTINE f_unpack_real_ad_kji ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL    outbuf(kms:kme,jms:jme,ims:ime), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
          DO i = is, ie
        DO j = js, je
            DO k = ks, ke
              outbuf(k,j,i) = outbuf(k,j,i) + inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_unpack_real_ad_kji
     
      SUBROUTINE f_unpack_lreal_ad_kji ( inbuf, outbuf, js, je, ks, ke,            &
     &                is, ie, jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        REAL*8  outbuf(kms:kme,jms:jme,ims:ime), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
          DO i = is, ie
        DO j = js, je
            DO k = ks, ke
              outbuf(k,j,i) = outbuf(k,j,i) + inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_unpack_lreal_ad_kji
#endif
