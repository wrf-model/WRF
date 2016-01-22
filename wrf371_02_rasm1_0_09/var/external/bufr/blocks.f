      SUBROUTINE BLOCKS(MBAY,MWRD)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BLOCKS
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2012-09-15
C
C ABSTRACT: BLOCKS WILL ADD IEEE FORTRAN TYPE RECORD CONTROL
C           WORDS TO A PURE BUFR RECORD PASSED FROM MSGWRT, IN
C           PREPARATION FOR OUTPUTING THE RECORD TO DISK.  THE
C           DEFAULT OUTPUT TYPE IS PURE (NO CONTROL WORDS), IN
C           WHICH CASE THIS ROUTINE DOES NOTHING.  AN APPLICATION
C           CAN SPECIFY THAT EITHER BIG OR LITTLE ENDIAN RECORD
C           CONTROL WORDS ARE TO BE APPENDED TO PURE BUFR RECORDS
C           VIA A PREVIOUS CALL TO SUBROUTINE SETBLOCK.
C
C THE FOLLOWING DIAGRAM ILLUSTRATES IEEE CONTROL WORDS FOUND
C IN AN UNFORMATTED FORTRAN RECORD CONRTAINING FOUR 4-BYTE WORDS
C
C     ctw1-wrd1-wrd2-wrd3-wrd4-ctw2
C     |    |    |    |    |    |
C     0016-aaaa-bbbb-cccc-dddd-0016
C
C CTW1 AND CTW2 CONTAIN A BYTE COUNT FOR THE DATA RECORD THAT 
C THEY ENCLOSE. THEY CAN BE STORED IN EITHER BIG OR LITTLE
C ENDIAN BYTE ORDERING (NOTE: CTWS ARE ALWAYS 4-BYTE WORDS)
C
C PROGRAM HISTORY LOG:
C 2012-09-15  J. WOOLLEN -- ORIGINAL AUTHOR
C
C USAGE:    CALL BLOCKS(MBAY,MWRD)
C   INPUT ARGUMENTS:
c     MBAY - INTEGER ARRAY CONTAINING PURE BUFR MESSAGE
c     MWRD - INTEGER WORD COUNT FOR MBAY
C
C   OUTPUT ARGUMENTS:
c     MBAY - INTEGER ARRAY CONTAINING INPUT BUFR MESSAGE, POSSIBLY
c            WITH CONTROL WORDS ADDED DEPENDING ON WHETHER SUBROUTINE
c            SETBLOCK WAS PREVIOUSLY CALLED
c     MWRD - INTEGER WORD COUNT FOR MBAY
C
C REMARKS:
C    THIS ROUTINE CALLS:   None 
C
C    THIS ROUTINE IS CALLED BY:  MSGWRT 
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      COMMON /HRDWRD/ NBYTW,NBITW,IORD(8)
      COMMON /ENDORD/ IBLOCK,IORDBE(4),IORDLE(4)

      INTEGER(4) MBAY(MWRD),IINT,JINT

      CHARACTER*1 CINT(4),DINT(4)
      EQUIVALENCE(CINT,IINT)
      EQUIVALENCE(DINT,JINT)

      DATA IFIRST/0/
      SAVE IFIRST

c----------------------------------------------------------------------
c----------------------------------------------------------------------

      if(iblock.eq.0) return

      if(ifirst.eq.0) then

c        Initialize some arrays for later use.  Note that Fortran
c        record control words are always 4 bytes.

         iint=0; cint(1)=char(1)
         do i=1,4 
         if(cint(1).eq.char(01)) then 
            iordbe(i)=4-i+1
            iordle(i)=i 
         else
            iordle(i)=4-i+1
            iordbe(i)=i 
         endif
         enddo
         ifirst=1
      endif

c  make room in mbay for control words - one at each end of the record
c  -------------------------------------------------------------------

      if(nbytw.eq.8) mwrd=mwrd*2

      do m=mwrd,1,-1
      mbay(m+1) = mbay(m)  
      enddo

c  store the endianized control word in bytes in dint/jint
c  -------------------------------------------------------

      iint=mwrd*4        

      do i=1,4         
      if(iblock.eq.+1) dint(i)=cint(iordbe(i))
      if(iblock.eq.-1) dint(i)=cint(iordle(i))
      enddo

c  increment mrwd and install the control words in their proper places
c  -------------------------------------------------------------------

      mwrd = mwrd+2
      mbay(1) = jint
      mbay(mwrd) = jint

      if(nbytw.eq.8) mwrd=mwrd/2

      return
      end
