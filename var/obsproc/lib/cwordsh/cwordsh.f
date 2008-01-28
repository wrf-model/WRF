C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C  
C MAIN PROGRAM:  CWORDSH
C   PRGMMR: J. ATOR          ORG: NP12        DATE: 2005-11-29
C
C ABSTRACT: CONVERTS BINARY BYTE STREAM BUFR FILES BACK AND FORTH
C   FROM A FORTRAN BLOCKED FORMAT.
C
C PROGRAM HISTORY LOG:
C 1999-08-19  J. WOOLLEN  ORIGINAL VERSION FOR IMPLEMENTATION
C 2004-03-19  D. KEYSER   INCREASED SIZE OF ARRAY MBAY FROM 3000 WORDS
C                         TO 5000 WORDS TO ALLOW IT TO PROCESS BUFR
C                         MESSAGES WITH UP TO 20K BYTES
C 2005-11-29  J. ATOR     REWRITTEN USING BUFRLIB C I/O LOGIC TO HANDLE
C                         ANY INPUT BUFR FILES (INCLUDING FILES WHICH
C                         CONTAIN EXTRANEOUS CHARACTERS (E.G. BULLETIN
C                         HEADERS) AND/OR WHICH PREVIOUSLY REQUIRED THE
C                         USE OF APPLICATION PROGRAM GRABBUFR) AND TO
C                         REMOVE DIRECT LINKS TO BUFRLIB COMMON BLOCKS
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT (OPERATION TYPE, INPUT FILENAME,
C                OUTPUT FILENAME)
C
C   OUTPUT FILES: 
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 51  - BLOCKED FORTRAN FILE OUTPUT FOR BLOCKING OPERATION
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       BUFRLIB  - CCBFL    COBFL    CRBMG    CWBMG    PADMSG
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  IBM SP
C
C$$$

      program cwordsh

      parameter(mxbufr=150000)
      parameter(mxbufrd4=mxbufr/4)

      character*80 bfile,ufile
      character*8  cword
      character*1  bufr(mxbufr)
      dimension    mbay(mxbufrd4)
      equivalence  (bufr(1),mbay(1))
 
c     Read and process the input arguments.
 
      read(5,'(a)') cword
      if(cword.eq.'block') then
         read(5,'(a)') ufile
         read(5,'(a)') bfile
      elseif(cword.eq.'unblk') then
         read(5,'(a)') bfile
         read(5,'(a)') ufile
      else
         print*,'cword must be block or unblk'
         stop
      endif
 
c     Open the input and output files.

      if(cword.eq.'block') then
         print*,'blocking from: ',ufile
         print*,'           to: ',bfile
         call cobfl(ufile,'r')
         open(51,file=bfile,form='unformatted')
      else
         print*,'unblocking from: ',bfile
         print*,'             to: ',ufile
         call cobfl(bfile,'r')
         call cobfl(ufile,'w')
      endif

c     Read the next message from the input file.

      write(6,*) 'mxbufr = ',mxbufr
      call crbmg(bufr,mxbufr,nbyt,ierr)
      write(6,*) 'mxbufr = ',mxbufr,' nbyt = ',nbyt
      write(6,*) 'ierr = ',ierr
      do while(ierr.ge.0)
        if(ierr.eq.0) then

c         Pad the end of the message with zeroed-out bytes up to the
c         next 8-byte boundary.

          call padmsg(mbay,mxbufrd4,npbyt)
          ntbyt = nbyt + npbyt

c         Write the message plus padding to the output file...

          if(cword.eq.'block') then

c           using a FORTRAN write.

	    write(6,*) 'ntbyt = ',ntbyt
            write(51) (bufr(i),i=1,ntbyt)
          else

c           using a C write.

            call cwbmg(bufr,ntbyt,ierw)
            if(ierw.ne.0) print*,'return value from cwbmg was ',ierw
          endif
        else
          print*,'return value from crbmg was ',ierr,
     .           '; message not written to output'
        endif
        call crbmg(bufr,mxbufr,nbyt,ierr)
      enddo

c     Close the input and output files.

      call ccbfl
      if(cword.eq.'block') close(51)
 
      stop
      end
