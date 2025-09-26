PROGRAM write_decomp
!
! written by M. Salzmann
! inspired by a code from  Edwin Spee, CWI, Amsterdam
!

#include "decomp_uses.inc" 

    IMPLICIT NONE

    INTEGER, PARAMETER :: iun=276
    INTEGER, PARAMETER :: oun=277

    
    INTEGER, PARAMETER :: clen= 40

    CHARACTER (LEN= 400 ) :: mech, fname
    CHARACTER (LEN= clen ) :: cj, cjj


    INTEGER  :: k, kk, j, jj



    PRINT *, "  -- in write_decomp"

   
    OPEN (UNIT=iun, FILE='mech.tmp', STATUS='OLD')

     READ (iun, *) mech

    CLOSE (iun)

    PRINT *, "    for kpp mechanism: ", TRIM(mech)


!    OPEN (UNIT=oun, FILE='module_kpp_mechX_decomp.F', STATUS='UNKNOWN')
    

!   WRITE(fname,*) 'decomp_',TRIM(mech),'.inc'
    WRITE(fname,fmt='(3a)') 'decomp_',TRIM(mech),'.inc'
    PRINT *,"fname:<",TRIM(fname),">"
    OPEN (UNIT=oun, FILE=TRIM(fname), STATUS='UNKNOWN')

!     WRITE(oun, *) "MODULE module_decomp_", TRIM(mech)
!     WRITE(oun, *) " CONTAINS "
     WRITE(oun, *) "  SUBROUTINE decomp_",TRIM(mech),"( JVS, IER )"
     WRITE(oun, *) "  " 
     WRITE(oun, *) "    IMPLICIT NONE" 
     WRITE(oun, *) "  " 
     WRITE(oun, *) "     INTEGER  :: IER"
     WRITE(oun, *) "     REAL(kind=dp) :: JVS(LU_NONZERO), W(NVAR), a"
     WRITE(oun, *) "  " 
     WRITE(oun, *) "  " 

     WRITE(oun, *) " a = 0._dp"




     WRITE(oun, *) " ier = 0 "
     WRITE(oun, *) "  "



     DO k=1,NVAR

      CALL int2c(LU_DIAG(k),cj, clen)
      CALL int2c(k,cjj, clen)
      
      WRITE(oun, *) " IF ( ABS(  JVS( ", TRIM(cj),  " )) < TINY(a) ) THEN"
      WRITE(oun, *) "        IER = ", cjj
      WRITE(oun, *) "        RETURN"
      WRITE(oun, *) " END IF"


       DO kk = LU_CROW(k), LU_CROW(k+1)-1

         CALL int2c(LU_ICOL(kk),cj, clen)
         CALL int2c(kk,cjj, clen)

         WRITE(oun, *) "  W( ", TRIM(cj) , " ) = JVS( ", TRIM(cjj), " )"

       END DO

 
        DO kk = LU_CROW(k), LU_DIAG(k)-1
 
            j = LU_ICOL(kk)

            CALL int2c(j,cj, clen)

 
            WRITE(oun, *) " a = -W( ",TRIM(cj)," ) / JVS( ", LU_DIAG(j), " )"
            WRITE(oun, *) " W( ",TRIM(cj)," ) = -a"
            DO jj = LU_DIAG(j)+1, LU_CROW(j+1)-1

                 CALL int2c(LU_ICOL(jj),cj, clen)
                 CALL int2c(jj,cjj, clen)

               WRITE(oun, *) " W( ", TRIM(cj)," ) = W( ", TRIM(cj), &
                 " ) + a*JVS( ",TRIM(cjj)," )"
            END DO
         END DO
         DO kk = LU_CROW(k), LU_CROW(k+1)-1

         CALL int2c(LU_ICOL(kk),cj, clen)
         CALL int2c(kk,cjj, clen)

            WRITE(oun, *) " JVS( ", TRIM(cjj), ") = W( ",TRIM(cj)," )"
         END DO
      END DO
    


      WRITE(oun, *) "  " 
      WRITE(oun, *) "  END SUBROUTINE decomp_",TRIM(mech)
!     WRITE(oun, *) "END MODULE module_decomp_", TRIM(mech)
    CLOSE (oun)





END PROGRAM write_decomp

SUBROUTINE int2c(i, c, cl )

 INTEGER, INTENT(IN ) :: i, cl
 CHARACTER ( LEN = cl ), INTENT(INOUT ) :: c

 CHARACTER ( LEN = 20 ) :: fmtstr
 INTEGER :: l

 l= INT(ALOG10(REAL(i,4))) + 1

 
 WRITE(fmtstr, FMT='("(I",I1,")")') l

 WRITE(c, FMT=TRIM(fmtstr)) i

 


END SUBROUTINE int2c
