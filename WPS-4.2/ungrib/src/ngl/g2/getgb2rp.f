C-----------------------------------------------------------------------
      SUBROUTINE GETGB2RP(LUGB,CINDEX,EXTRACT,GRIBM,LENG,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: GETGB2RP       EXTRACTS A GRIB MESSAGE FROM A FILE
C   PRGMMR: GILBERT          ORG: W/NMC23     DATE: 2003-12-31
C
C ABSTRACT: FIND AND EXTRACTS A GRIB MESSAGE FROM A FILE GIVEN THE 
C   INDEX FOR THE REQUESTED FIELD.
C   THE GRIB MESSAGE RETURNED CAN CONTAIN ONLY THE REQUESTED FIELD
C   (EXTRACT=.TRUE.). OR THE COMPLETE GRIB MESSAGE ORIGINALLY CONTAINING
C   THE DESIRED FIELD CAN BE RETURNED (EXTRACT=.FALSE.) EVEN IF OTHER
C   FIELDS WERE INCLUDED IN THE GRIB MESSAGE.
C   IF THE GRIB FIELD IS NOT FOUND, THEN THE RETURN CODE WILL BE NONZERO.
C
C PROGRAM HISTORY LOG:
C 2003-12-31  GILBERT
C
C USAGE:    CALL GETGB2RP(LUGB,CINDEX,EXTRACT,GRIBM,LENG,IRET)
C   INPUT ARGUMENTS:
C     LUGB         INTEGER UNIT OF THE UNBLOCKED GRIB DATA FILE.
C                  FILE MUST BE OPENED WITH BAOPEN OR BAOPENR BEFORE CALLING 
C                  THIS ROUTINE.
C     CINDEX       INDEX RECORD OF THE GRIB FILE  ( SEE DOCBLOCK OF
C                  SUBROUTINE IXGB2 FOR DESCRIPTION OF AN INDEX RECORD.)
C     EXTRACT       LOGICAL VALUE INDICATING WHETHER TO RETURN A GRIB2 
C                   MESSAGE WITH JUST THE REQUESTED FIELD, OR THE ENTIRE
C                   GRIB2 MESSAGE CONTAINING THE REQUESTED FIELD.
C                  .TRUE. = RETURN GRIB2 MESSAGE CONTAINING ONLY THE REQUESTED
C                           FIELD.
C                  .FALSE. = RETURN ENTIRE GRIB2 MESSAGE CONTAINING THE
C                            REQUESTED FIELD.
C
C   OUTPUT ARGUMENTS:
C     GRIBM         RETURNED GRIB MESSAGE.
C     LENG         LENGTH OF RETURNED GRIB MESSAGE IN BYTES.
C     IRET         INTEGER RETURN CODE
C                    0      ALL OK
C                    97     ERROR READING GRIB FILE
C
C SUBPROGRAMS CALLED:
C   BAREAD          BYTE-ADDRESSABLE READ
C
C REMARKS: NONE 
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$

      INTEGER,INTENT(IN) :: LUGB
      CHARACTER(LEN=1),INTENT(IN) :: CINDEX(*)
      LOGICAL,INTENT(IN) :: EXTRACT
      INTEGER,INTENT(OUT) :: LENG,IRET
      CHARACTER(LEN=1),POINTER,DIMENSION(:) :: GRIBM
 
      INTEGER,PARAMETER :: ZERO=0
      CHARACTER(LEN=1),ALLOCATABLE,DIMENSION(:) :: CSEC2,CSEC6,CSEC7
      CHARACTER(LEN=4) :: Ctemp

      IRET=0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  EXTRACT GRIB MESSAGE FROM FILE
      IF ( EXTRACT ) THEN
         LEN0=16
         LEN8=4
         CALL GBYTE(CINDEX,ISKIP,4*8,4*8)    ! BYTES TO SKIP IN FILE
         CALL GBYTE(CINDEX,ISKP2,8*8,4*8)    ! BYTES TO SKIP FOR section 2
         if ( iskp2 .gt. 0 ) then
            CALL BAREAD(LUGB,ISKIP+ISKP2,4,LREAD,ctemp)
            CALL GBYTE(Ctemp,LEN2,0,4*8)      ! LENGTH OF SECTION 2
            ALLOCATE(csec2(len2))
            CALL BAREAD(LUGB,ISKIP+ISKP2,LEN2,LREAD,csec2)
         else
            LEN2=0
         endif
         CALL GBYTE(CINDEX,LEN1,44*8,4*8)      ! LENGTH OF SECTION 1
         IPOS=44+LEN1
         CALL GBYTE(CINDEX,LEN3,IPOS*8,4*8)      ! LENGTH OF SECTION 3
         IPOS=IPOS+LEN3
         CALL GBYTE(CINDEX,LEN4,IPOS*8,4*8)      ! LENGTH OF SECTION 4
         IPOS=IPOS+LEN4
         CALL GBYTE(CINDEX,LEN5,IPOS*8,4*8)      ! LENGTH OF SECTION 5
         IPOS=IPOS+LEN5
         CALL GBYTE(CINDEX,LEN6,IPOS*8,4*8)      ! LENGTH OF SECTION 6
         IPOS=IPOS+5
         CALL GBYTE(CINDEX,IBMAP,IPOS*8,1*8)      ! Bitmap indicator
         IF ( IBMAP .eq. 254 ) THEN
            CALL GBYTE(CINDEX,ISKP6,24*8,4*8)    ! BYTES TO SKIP FOR section 6
            CALL BAREAD(LUGB,ISKIP+ISKP6,4,LREAD,ctemp)
            CALL GBYTE(Ctemp,LEN6,0,4*8)      ! LENGTH OF SECTION 6
         ENDIF
         !
         !  READ IN SECTION 7 from file
         !
         CALL GBYTE(CINDEX,ISKP7,28*8,4*8)    ! BYTES TO SKIP FOR section 7
         CALL BAREAD(LUGB,ISKIP+ISKP7,4,LREAD,ctemp)
         CALL GBYTE(Ctemp,LEN7,0,4*8)      ! LENGTH OF SECTION 7
         ALLOCATE(csec7(len7))
         CALL BAREAD(LUGB,ISKIP+ISKP7,LEN7,LREAD,csec7)

         LENG=LEN0+LEN1+LEN2+LEN3+LEN4+LEN5+LEN6+LEN7+LEN8
         IF (.NOT. ASSOCIATED(GRIBM)) ALLOCATE(GRIBM(LENG))

         ! Create Section 0
         !
         GRIBM(1)='G'
         GRIBM(2)='R'
         GRIBM(3)='I'
         GRIBM(4)='B'
         GRIBM(5)=CHAR(0)
         GRIBM(6)=CHAR(0)
         GRIBM(7)=CINDEX(42)
         GRIBM(8)=CINDEX(41)
         GRIBM(9)=CHAR(0)
         GRIBM(10)=CHAR(0)
         GRIBM(11)=CHAR(0)
         GRIBM(12)=CHAR(0)
         CALL SBYTE(GRIBM,LENG,12*8,4*8)
         !
         ! Copy Section 1
         !
         GRIBM(17:16+LEN1)=CINDEX(45:44+LEN1)
         lencur=16+LEN1
         ipos=44+len1
         !
         ! Copy Section 2, if necessary
         !
         if ( iskp2 .gt. 0 ) then
           GRIBM(lencur+1:lencur+LEN2)=csec2(1:LEN2)
           lencur=lencur+LEN2
         endif
         !
         ! Copy Sections 3 through 5
         !
         GRIBM(lencur+1:lencur+LEN3+LEN4+LEN5)=
     &                      CINDEX(ipos+1:ipos+LEN3+LEN4+LEN5)
         lencur=lencur+LEN3+LEN4+LEN5
         ipos=ipos+LEN3+LEN4+LEN5
         !
         ! Copy Section 6
         !
         if ( LEN6 .eq. 6 .AND. IBMAP .ne. 254 ) then
            GRIBM(lencur+1:lencur+LEN6)=CINDEX(ipos+1:ipos+LEN6)
            lencur=lencur+LEN6
         else
            CALL GBYTE(CINDEX,ISKP6,24*8,4*8)    ! BYTES TO SKIP FOR section 6
            CALL BAREAD(LUGB,ISKIP+ISKP6,4,LREAD,ctemp)
            CALL GBYTE(Ctemp,LEN6,0,4*8)      ! LENGTH OF SECTION 6
            ALLOCATE(csec6(len6))
            CALL BAREAD(LUGB,ISKIP+ISKP6,LEN6,LREAD,csec6)
            GRIBM(lencur+1:lencur+LEN6)=csec6(1:LEN6)
            lencur=lencur+LEN6
            IF ( allocated(csec6)) DEALLOCATE(csec6)
         endif
         !
         ! Copy Section 7
         !
         GRIBM(lencur+1:lencur+LEN7)=csec7(1:LEN7)
         lencur=lencur+LEN7
         !
         ! Section 8
         !
         GRIBM(lencur+1)='7'
         GRIBM(lencur+2)='7'
         GRIBM(lencur+3)='7'
         GRIBM(lencur+4)='7'

         !  clean up
         !
         IF ( allocated(csec2)) DEALLOCATE(csec2)
         IF ( allocated(csec7)) deallocate(csec7)

      ELSE    ! DO NOT extract field from message :  Get entire message

         CALL GBYTE(CINDEX,ISKIP,4*8,4*8)    ! BYTES TO SKIP IN FILE
         CALL GBYTE(CINDEX,LENG,36*8,4*8)      ! LENGTH OF GRIB MESSAGE
         IF (.NOT. ASSOCIATED(GRIBM)) ALLOCATE(GRIBM(LENG))
         CALL BAREAD(LUGB,ISKIP,LENG,LREAD,GRIBM)
         IF ( LENG .NE. LREAD ) THEN
            DEALLOCATE(GRIBM)
            NULLIFY(GRIBM)
            IRET=97
            RETURN
         ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
