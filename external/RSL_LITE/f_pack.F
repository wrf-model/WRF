     
      SUBROUTINE f_pack_int ( inbuf, outbuf, js, je, ks, ke, is, ie,      &
     &                        jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER inbuf(ims:ime,kms:kme,jms:jme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO j = js, je
          DO k = ks, ke
            DO i = is, ie
              outbuf(p) = inbuf(i,k,j)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_pack_int
     
      SUBROUTINE f_pack_lint ( inbuf, outbuf, js, je, ks, ke, is, ie,    &
     &                         jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 inbuf(ims:ime,kms:kme,jms:jme), outbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO j = js, je
          DO k = ks, ke
            DO i = is, ie
              outbuf(p) = inbuf(i,k,j)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_pack_lint
     
      SUBROUTINE f_unpack_int ( inbuf, outbuf, js, je, ks, ke, is, ie,    &
     &                          jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER outbuf(ims:ime,kms:kme,jms:jme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO j = js, je
          DO k = ks, ke
            DO i = is, ie
              outbuf(i,k,j) = inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_unpack_int
     
      SUBROUTINE f_unpack_lint ( inbuf, outbuf, js, je, ks, ke, is, ie,    &
     &                           jms, jme, kms, kme, ims, ime, curs ) 
        IMPLICIT NONE
        INTEGER jms, jme, kms, kme, ims, ime
        INTEGER*8 outbuf(ims:ime,kms:kme,jms:jme), inbuf(*)
        INTEGER js, je, ks, ke, is, ie, curs
        ! Local
        INTEGER i,j,k,p
        p = 1
        DO j = js, je
          DO k = ks, ke
            DO i = is, ie
              outbuf(i,k,j) = inbuf(p)
              p = p + 1
            ENDDO
          ENDDO
        ENDDO
        curs = p - 1
        RETURN
      END SUBROUTINE f_unpack_lint
     
