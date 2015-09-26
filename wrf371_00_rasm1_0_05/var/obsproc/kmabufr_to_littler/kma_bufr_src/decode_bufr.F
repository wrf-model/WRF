      PROGRAM BUFR
C
C**** *BUFR*
C
C
C     PURPOSE.
C     --------
C         EXAMPLE OF USING BUFR UNPACKING/PACKING SOFTWARE.
C
C
C**   INTERFACE.
C     ----------
C
C          NONE.
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C         CALL BUSEL
C         CALL BUFREX
C         CALL BUFREN
C         CALL BUPRS0
C         CALL BUPRS1
C         CALL BUPRS2
C         CALL BUPRS3
C         CALL BUPRT
C         CALL BUUKEY
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/09/87.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 64 ,JSEC3=    4,
     1        JSEC4=   2,JELEM=80000,JSUBS=400,JCVAL=150 ,JBUFL=512000,
#ifdef JBPW_64
     2        JBPW =  64,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT=1200,
#else
     2        JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT=1200,
#endif
     3        JWORK=4096000,JKEY=46,JBYTE=2048000)
C
      PARAMETER (KELEM=40000)
      PARAMETER (KVALS=4096000)
C 
      DIMENSION KBUFF(JBUFL)
      DIMENSION KBUFR(JBUFL)
      DIMENSION KSUP(JSUP)  ,KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION KSEC2(JSEC2),KSEC3(JSEC3),KSEC4(JSEC4)
      DIMENSION KEY  (JKEY),KREQ(2)
      DIMENSION NREQUEST(2)
C
      REAL*8 VALUES(KVALS),VALUE(KVALS)
      REAL*8 VALS(KVALS)
      REAL*8 RQV(KELEM)
      REAL*8 RVIND,EPS
C
      DIMENSION KTDLST(JELEM),KTDEXP(JELEM),KRQ(KELEM)
      DIMENSION KDATA(200),KBOXR(JELEM*4)

C
      CHARACTER*256 CFIN,COUT,CARG(4)
      CHARACTER*64 CNAMES(KELEM),CBOXN(JELEM*4)
      CHARACTER*24 CUNITS(KELEM),CBOXU(JELEM*4)
      CHARACTER*80 CVALS(kelem)
      CHARACTER*80 CVAL(kelem)
      CHARACTER*80 YENC
C
C     ------------------------------------------------------------------
C*          1. INITIALIZE CONSTANTS AND VARIABLES.
C              -----------------------------------
 100  CONTINUE
C
C     MISSING VALUE INDICATOR
C 
      RVIND=1.7E38
      NVIND=2147483647
C
      NBYTPW=JBPW/8
      IOBS=0
      EPS=1.E-8
      NPACK=0
      N=0
      OO=.FALSE.
      KKK=0
C
C     GET INPUT AND OUTPUT FILE NAME.
C
      NARG=COMMAND_ARGUMENT_COUNT()
C
C
      DO 104 J=1,NARG
      CALL GET_COMMAND_ARGUMENT(number=J, value=CARG(J))
 104  CONTINUE

      II=0
      IO=0
      DO 105 J=1,NARG
      IF(CARG(J).EQ.'-i') THEN
         IN=J
      ELSEIF(CARG(J).EQ.'-o') THEN
         IO=J
      END IF
 105  CONTINUE
      IF(IN.EQ.0) THEN
         PRINT*,'USAGE -- decode_bufr -i infile'
         STOP
      END IF
      IF(IO.EQ.0.and.IN.EQ.0) THEN
         PRINT*,'USAGE -- decode_bufr -i infile -o outfile'
         STOP
      END IF

C
      IF(IO.NE.0)  COUT=CARG(IO+1)
C
      IF(IO.LT.IN) THEN
         IST=IN+1
         IEND=NARG
      ELSE
         IST=IN+1
         IEND=IO-1
      END IF
C
C
      IF(IO.NE.0) THEN
         JJ=INDEX(COUT,' ')
         JJ=JJ-1
         CALL PBOPEN(IUNIT1,COUT(1:JJ),'W',IRET)
         IF(IRET.EQ.-1) STOP 'OPEN FAILED ON BUFR.DAT'
         IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
         IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'
      END IF
C
      DO 101 II=IST,IEND

      CFIN=CARG(II)
      ILN=INDEX(CFIN,' ')
      ILN=ILN-1
C
C     SET REQUEST FOR PARTIAL EXPANSION
C
      KRQL=0
      NR=0
      KREQ(1)=0
      KREQ(2)=0
      DO 103 I=1,KELEM
      RQV(I)=RVIND
      KRQ(I)=NVIND
 103  CONTINUE
C
C*          1.2 OPEN FILE CONTAINING BUFR DATA.
C               -------------------------------
 120  CONTINUE
C
      IRET=0 
      CALL PBOPEN(IUNIT,CFIN(1:ILN),'R',IRET)
      IF(IRET.EQ.-1) STOP 'OPEN FAILED'
      IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'


C
C
C     ----------------------------------------------------------------- 
C*          2. SET REQUEST FOR EXPANSION.
C              --------------------------
 200  CONTINUE
C
      OPRT=.FALSE.
      OENC=.FALSE.
      WRITE(*,'(A,$)') ' DO YOU WANT TO PRINT( Y/N ) : '
      READ (*,'(A)') YENC
      IF(YENC(1:1).EQ.'Y'.OR.YENC(1:1).EQ.'y') THEN
         OPRT=.TRUE.
      END IF
      ICODE=0
      WRITE(*,'(A,$)') ' CODE TABLES TO BE PRINTED ( Y/N ) : '
      READ (*,'(A)') YCODC
      IF(YCODC(1:1).EQ.'Y'.OR.YCODC(1:1).EQ.'y') THEN
         ICODE=1
      END IF
      WRITE(*,'(A,$)') ' DO YOU WANT ENCODING( Y/N ) : '
      READ (*,'(A)') YENC
      IF(YENC(1:1).EQ.'Y'.OR.YENC(1:1).EQ.'y') THEN
          OENC=.TRUE.
         WRITE(*,'(A,$)') ' NUMBER OF SUBSETS TO PACK : '
         READ(*,'(BN,I4)')   NCOM
         OCOMP=.FALSE.
         WRITE(*,'(A,$)') ' DO YOU WANT COMPRESSION( Y/N ) : '
         READ (*,'(A)') YCOMP
         IF(YCOMP(1:1).EQ.'Y'.OR.YCOMP(1:1).EQ.'y') OCOMP=.TRUE.
      END IF
      WRITE(*,'(A,$)') ' RECORD NUMBER TO START FROM : '
      READ(*,'(BN,I6)')   NR
C
 201  CONTINUE
C
      WRITE(*,'(A,$)') ' REQUESTED ELEMENT : ' 
      READ(*,'(BN,I6)')   IEL 
      WRITE(*,'(A,$)') ' REQUESTED VALUE   : '
      READ(*,'(BN,F12.2)')   VAL
      IF(IEL.EQ.0) THEN
         KRQL=J
      ELSE
         J=J+1
         KRQ(J)=IEL
         RQV(J)=VAL
         IF(VAL.EQ.0.) RQV(J)=RVIND
         GO TO 201
      END IF
C
      WRITE(*,'(A,$)') ' REQUESTED FLAG 1  : '
      READ(*,'(BN,I6)')  KREQ(1)
C
      WRITE(*,'(A,$)') ' REQUESTED FLAG 2  : '
      READ(*,'(BN,I6)')  KREQ(2)
C
      WRITE(*,'(A,$)') ' DO YOU WANT TO PRINT SECTION 0-3( Y/N ) : '
      READ (*,'(A,$)') YENC
      OSEC3=.FALSE.
      IF(YENC(1:1).EQ.'Y'.OR.YENC(1:1).EQ.'y') OSEC3=.TRUE.
C
C*          2.1 SET REQUEST FOR PARTIAL EXPANSION.
C               ----------------------------------
 210  CONTINUE
C
c     KERR=0
      CALL BUSRQ(KREQ,KRQL,KRQ,RQV,KERR)
C
C     SET VARIABLE TO PACK BIG VALUES AS MISSING VALUE INDICATOR
C
      KPMISS=1
      KPRUS=0
      KOKEY=0
      CALL BUPRQ(KPMISS,KPRUS,KOKEY)
C
C     -----------------------------------------------------------------
C*          3.  READ BUFR MESSAGE.
C               ------------------
 300  CONTINUE
C
      IERR=0
      KBUFL=0
C
      IRET=0
      CALL PBBUFR(IUNIT,KBUFF,JBYTE,KBUFL,IRET) 
      IF(IRET.EQ.-1) THEN
         IF(IO.NE.0) THEN
            IF(NPACK.NE.0) GO TO 600
         END IF
         PRINT*,'NUMBER OF SUBSETS     ',IOBS
         PRINT*,'NUMBER OF MESSAGES    ',N
         CALL PBCLOSE(IUNIT,IRET)
         GO TO 101
      END IF
      IF(IRET.EQ.-2) STOP 'FILE HANDLING PROBLEM' 
      IF(IRET.EQ.-3) STOP 'ARRAY TOO SMALL FOR PRODUCT'
C
      N=N+1
      PRINT*,'----------------------------------',N,' ',KBUFL
      KBUFL=KBUFL/NBYTPW+1
      IF(N.LT.NR) GO TO 300
C
C     -----------------------------------------------------------------
C*          4. EXPAND BUFR MESSAGE.
C              --------------------
 400  CONTINUE
C
      CALL BUS012(KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KSEC2,KERR)
      IF(KERR.NE.0) THEN
         PRINT*,'ERROR IN BUS012: ',KERR
         PRINT*,' BUFR MESSAGE NUMBER ',N,' CORRUPTED.'
         KERR=0
         GO TO 300
      END IF
C
      KEL=KVALS/KSUP(6)
      IF(KEL.GT.JELEM) KEL=JELEM
C
         CALL BUFREX(KBUFL,KBUFF,KSUP,KSEC0 ,KSEC1,KSEC2 ,KSEC3 ,KSEC4,
     1            KEL,CNAMES,CUNITS,KVALS,VALUES,CVALS,IERR)
C
      IF(IERR.NE.0) THEN
         IF(IERR.EQ.39) GO TO 300
         CALL EXIT(2)
      END IF
C
C
      IOBS=IOBS+KSEC3(3)
C
      NPACK=NPACK+1 
C
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
      IF(KERR.NE.0) CALL EXIT(2)

C
c      DO 401 IK=1,KSEC3(3)
c
c      CALL BUBOX(IK,KSUP,KEL,KTDEXP,CNAMES,CUNITS,KVALS,VALUES,
c    1            KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU,IERR)
C
c 401  CONTINUE
C
c      IF(IERR.NE.0) THEN
c        PRINT*,'bubox error----------------------------------',N
c      END IF
C
C*          4.1 PRINT CONTENT OF EXPANDED DATA.
C               -------------------------------
 410  CONTINUE
C
      IF(.NOT.OPRT) GO TO 500
      IF(.NOT.OSEC3) GO TO 450
C
C*          4.2 PRINT SECTION ZERO OF BUFR MESSAGE.
C               -----------------------------------
 420  CONTINUE
C

      CALL BUPRS0(KSEC0)
C
C*          4.3 PRINT SECTION ONE OF BUFR MESSAGE.
C               -----------------------------------
 430  CONTINUE
C
      CALL BUPRS1(KSEC1)
C
C
C*          4.4 PRINT SECTION TWO OF BUFR MESSAGE.
C               -----------------------------------
 440  CONTINUE
C
C              AT ECMWF SECTION 2 CONTAINS RDB KEY.
C              SO UNPACK KEY
C
      CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)
C
C              PRINT KEY
C
      CALL BUPRS2(KSUP ,KEY)
C
C*          4.5 PRINT SECTION 3 OF BUFR MESSAGE.
C               -----------------------------------
 450  CONTINUE
C
C               FIRST GET DATA DESCRIPTORS
C
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
C     IF(KERR.NE.0) CALL EXIT(2)
C
C               PRINT  CONTENT
C
      IF(OSEC3) THEN
         CALL BUPRS3(KSEC3,KTDLEN,KTDLST,KTDEXL,KTDEXP,KEL,CNAMES)
      END IF
C
C*         4.6 PRINT SECTION 4 (DATA).
C              -----------------------
 460  CONTINUE
C
C          IN THE CASE OF MANY SUBSETS DEFINE RANGE OF SUBSETS
C
      IF(.NOT.OO) THEN
      WRITE(*,'(A,$)') ' STARTING SUBSET TO BE PRINTED : '
      READ(*,'(BN,I6)')   IST
      WRITE(*,'(A,$)') ' ENDING SUBSET TO BE PRINTED : '
      READ(*,'(BN,I6)')   IEND
      OO=.FALSE.
      END IF
C
C              PRINT DATA
C
      ICODE=0
 
C     IF(KSEC1(6).EQ.11) THEN
C
         CALL BUPRT(ICODE,IST,IEND,KEL,CNAMES,CUNITS,CVALS,
     1              KVALS,VALUES,KSUP,KSEC1,IERR)
C     ELSE
C
C              RESOLVE BIT MAPS 
C
C        IF(IEND.GT.KSEC3(3)) IEND=KSEC3(3)
c
C        DO 461 IK=IST,IEND
c 
C        CALL BUBOX(IK,KSUP,KEL,KTDEXP,CNAMES,CUNITS,KVALS,VALUES,
C    1              KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU,IERR)
C        IF(IERR.NE.0) CALL EXIT(2)
c
C        CALL BUPRTBOX(KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU)
c
C461     CONTINUE
C     END IF
C
C     -----------------------------------------------------------------
C*          5. COLLECT DATA FOR REPACKING.
C              ---------------------------
 500  CONTINUE
C      
C 
      IF(.NOT.OENC) GO TO 300
C
C               FIRST GET DATA DESCRIPTORS
C
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
      IF(KERR.NE.0) CALL EXIT(2)
C
      DO 501 I=1,KSUP(5)
         IN=I+(NPACK-1)*KSUP(5)
C
         IF(CUNITS(I).EQ.'CCITTIA5') THEN
            IPOS =VALUES(I)/1000.
            ICH=NINT(VALUES(I)-IPOS*1000)
            KKK=KKK+1
            VALUE(IN)=KKK*1000+ICH
            CVAL(KKK)=CVALS(IPOS)
         ELSE
            VALUE(IN)=VALUES(I)
         END IF
 501  CONTINUE
C
      IF(NPACK.EQ.NCOM)    GO TO 600
C
      GO TO 300
C     -----------------------------------------------------------------
C*          6. PACK BUFR MESSAGE BACK INTO BUFR.
C              ---------------------------------
 600  CONTINUE
C
    
      KKK=0
      KSEC1(5)=0
      KSEC3(3)=NPACK
      KSEC1(8)=1
      KSEC1(15)=6
      KSEC0(3)=4              ! EDITION 4 OF BUFR MESSAGE
      KSEC3(4)=128            ! NO COMPRESSION
      IF(OCOMP) KSEC3(4)=192  ! COMPRESSION  
      KBUFL=JBUFL
C
C     GET REPLICATION FACTORS
C
      KK=0
      DO 601 K=1,KSUP(5)
      IF(KTDEXP(K).EQ.31001.OR.KTDEXP(K).EQ.31002) THEN
         KK=KK+1
         KDATA(KK)=NINT(VALUE(K))
      END IF
 601  CONTINUE
C
      KDLEN=2
      IF(KK.NE.0) KDLEN=KK
      IF(KSEC1(5).NE.0) THEN
         CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)
         IF(KERR.NE.0) THEN
            PRINT*,'BUUKEY: ERROR ',KERR
            CALL EXIT(2)
         END IF
C
C
C        GET INFORMATION FOR RDB KEY
C
         CALL BUCRKEY(KSUP(5),KTDEXP,KSUP,KSEC1,KSEC3,KEY,
     1                 VALUE,CVAL,KERR)
         IF(KERR.NE.0) THEN
            PRINT*,'ERROR IN BUCREKEY.'
            call exit(2)
         END IF
C
C
C        PACK NEW RDB KEY
C
C
         CALL BUPKEY(KEY,KSEC1,KSEC2,KERR)
         IF(KERR.NE.0) CALL EXIT(2)

      END IF
C
      IF(KSUP(6).EQ.0) THEN
         PRINT*,'ZERO SUBSETS'
         CALL EXIT(2)
      END IF
C
C
C*          6.2 ENCODE DATA INTO BUFR MESSAGE.
C               ------------------------------
 620  CONTINUE
C
      CALL BUFREN( KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
     1             KTDLEN,KTDLST,KDLEN,KDATA,KSUP(5),    ! KEL
     2             KVALS,VALUE,CVAL,KBUFL,KBUFR,KERR)
C
      IF(KERR.NE.0) THEN
         PRINT*,'ERROR IS ',KERR
         PRINT*,'ERROR DURING ENCODING.'
         CALL EXIT(2)
      END IF
C
C           6.3 WRITE PACKED BUFR MESSAGE INTO FILE.
C               ------------------------------------
 630  CONTINUE
C
      ILEN=KBUFL*NBYTPW
C     
      IERR=0
C
       CALL PBWRITE(IUNIT1,KBUFR,ILEN,IERR)
       IF(IERR.LT.0) THEN
          PRINT*,'ERROR WRITING INTO TARGET FILE.'
          CALL EXIT(2)
       END IF
       PRINT*,'RECORD WRITTEN INTO FILE-   ILEN=',ilen
C
      NPACK=0
C
      GO TO 300
C     -----------------------------------------------------------------
C
 101  CONTINUE
C
      IF(IO.NE.0) THEN
      CALL PBCLOSE(IUNIT1,IRET)
      END IF
C
      END
