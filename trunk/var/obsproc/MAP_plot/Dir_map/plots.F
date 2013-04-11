      subroutine clproc (xcs,ycs,ncs,aid,gid,nid)

C*****************************************************************************C
C  clproc   - This is a MAPDRV routine                                        C
C  Section  - Fill                                                            C
C  Purpose  - This is the routine that does the color fill on countries,      C
C             states and continents.                                          C
C                                                                             C
C  On entry - XCS and YCS contain NCS  pairs which are x, y  coordinate pairs C
C             that describe a polygon to be filled.   AID  is an array of NID C
C             area identifiers for the polygon.  GID is an array of NID group C
C             identifiers for  the  polygon.   Color information for all non- C
C             water colors is passed in through the common block  FLINFO  and C
C             WACOLR, water color is passed in through common block FLWATR.   C
C                                                                             C
C  On exit  - The map in has been colored.                                    C
C                                                                             C
C  Assume   - GKS is open.  This routine is called by the AREAS utility.      C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             SFSGFA              SOFTFILL utility*                           C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C             This routine is not called by the MAPDRV utility, rather it  is C
C             called by the AREAS utility.                                    C
C                                                                             C
C  Author   - Jeremy Asbill        Date - July 7, 1990       for the MM4 club C
C*****************************************************************************C

C  Integer variables

      integer          aid(*),           ! area identifiers                (in)
     *                 gid(*),           ! group identifiers               (in)
     *                 nid,              ! dimension of AID,GID            (in)
     *                 ncs               ! dimension of XCS,YCS            (in)
      integer          fscolr,           ! for common block FLINFO
     *                 secolr,           ! for common block FLINFO
     *                 thcolr,           ! for common block FLINFO
     *                 frcolr,           ! for common block FLINFO
     *                 fvcolr,           ! for common block FLINFO
     *                 sicolr            ! for common block FLINFO
      integer          wacolr            ! for common block FLWATR
      integer          ind(200),         ! scratch array for SOFTFILL   (local)
     *                 index,            ! color index to use           (local)
     *                 itm               ! test value                   (local)

C  Real variables

      real             xcs(*),           ! point locations x direction     (in)
     *                 ycs(*)            ! point locations y direction     (in)
      real             dst(100)          ! scratch array                (local)

C  Common blocks

      common /flinfo/  fscolr,           ! first color
     *                 secolr,           ! second color
     *                 thcolr,           ! third color
     *                 frcolr,           ! fourth color
     *                 fvcolr,           ! fifth color
     *                 sicolr            ! sixth color
      common /flwatr/  wacolr            ! water color

C****************************  subroutine begin  *****************************C

C  Be certain that the current polygon is on the map

      if ((aid(1) .ge. 0) .and. (aid(2) .ge. 0)) then

C  If one is zero or negative and the other isn't test on the one that isn't

        itm = max0(aid(1),aid(2))
        if (itm .gt. 0) then

C  Determine proper color index, MAPACI is an EZMAPA utility function

          index = mapaci(itm)
          if (index .eq. 1) then
            index = wacolr
          else if (index .eq. 2) then
            index = fscolr
          else if (index .eq. 3) then
            index = secolr
          else if (index .eq. 4) then
            index = thcolr
          else if (index .eq. 5) then
            index = frcolr
          else if (index .eq. 6) then
            index = fvcolr
          else if (index .eq. 7) then
            index = sicolr
          end if
        else
          index = wacolr
        end if

C  Do color fill

        call sfsgfa (xcs,ycs,ncs-1,dst,100,ind,200,index)
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine crdrci (test,error,colind,defind,whline,i,name,
     *                   size,errsev,noplt,util)

C*****************************************************************************C
C  crdrci   - This is a CONDRV/MAPDRV routine                                 C
C  Section  - Tables                                                          C
C  Purpose  - To read in a single color index and check it for validity.      C
C                                                                             C
C  On entry - TEST is a logical that must be true for the index to be  parsed C
C             from the table.  ERROR is true if an error has occured in which C
C             case the index should not be parsed from the table.  DEFIND  is C
C             a default color index to use if needed.  WHLINE is a whole line C
C             taken from the current table.  I is the current place in WHLINE.C
C             NAME is the name of the color index to use when giving an error C
C             message.  SIZE is the number of characters in NAME.  ERRSEV in- C
C             dicates what severity of error should stop execution.   UTIL is C
C             is the name of the utility using this routine.                  C
C                                                                             C
C  On exit  - COLIND  contains  the value set to the  color index.   NOPLT is C
C             true if a non-correctable error has occured.  ERROR is true  if C
C             an error has occured and is false otherwise.                    C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             ERRHAN              CONDRV/MAPDRV utility                       C
C             GQCR                GKS                                         C
C             ----------------------------------------------------------------C
C                                                                             C
C  Assume   - Nothing                                                         C
C                                                                             C
C  Author   - Jeremy Asbill      Date - August 7, 1990      for the MM4 club  C
C*****************************************************************************C

C  Character variables

      character*80     whline            ! a line of text from table       (in)
      character*36     name              ! name of color index             (in)
      character*6      util              ! name of using utility           (in)
      character*60     ermes             ! error message string         (local)

C  Integer variables

      integer          defind,           ! default color index             (in)
     *                 i,                ! position in WHLINE              (in)
     *                 size,             ! number of chars in NAME         (in)
     *                 errsev            ! error severity comparitor       (in)
      integer          colind            ! color index                    (out)
      integer          j,                ! loop counter                 (local)
     *                 ier               ! error flag from GKS          (local)

C  Logical variables

      logical          test,             ! use the table ?                 (in)
     *                 error             ! has an error occured ?          (in)
      logical          noplt             ! will a plot be drawn ?         (out)

C  Real variables

      real             f1,f2,f3          ! junk fillers                 (local)

C****************************  Subroutine Begin  *****************************C

C  Check to see if we should use the table

      if ((test) .and. (.not. error)) then

C  Use the table to read the color index

        if ((whline(i:i) .eq. 'D') .or. (whline(i:i) .eq. 'd')) then
          colind = defind
        else if ((whline(i+1:i+1) .ne. ' ') .and.
     *           (whline(i+1:i+1) .ne. '|')) then
          read (whline(i:i+1),20,err=30) colind
        else
          read (whline(i:i),10,err=30) colind
        end if

C  Check that the color index is a valid one

        if (colind .ge. 0) then
          call gqcr (1,colind,0,ier,f1,f2,f3)
        end if
        if ((ier .eq. 87) .or. ((colind .lt. 0) .and.
     *      (colind .ne. defind)) .or. (colind .gt. 255) .or.
     *      (ier .eq. 86)) then
          ermes(1:size) = name(1:size)
          ermes(size+1:size+11) = ' Is Invalid'
          do 40 j = size+12,60
            ermes(j:j) = ' '
40        continue
          call errhan (util,1,ermes,errsev)
          colind = defind
          error = .true.
        else if (ier .ne. 0) then

C  If GKS returns another error, who knows what is wrong

          ermes(1:30)  = 'Non-Correctable Error Encounte'
          ermes(31:60) = 'red                           '
          call errhan (util,1,ermes,errsev)
          noplt = .true.
        else if (colind .ge. 100) then

C  Warn the user if the index is within a reserved region
C  100 - 199 is reserved for CONDRV
C  200 - 255 is reserved for MAPDRV

          ermes(1:size) = name(1:size)
          ermes(size+1:size+24) = ' Is In A Reserved Region'
          do 50 j = size+24,60
            ermes(j:j) = ' '
50        continue
          call errhan (util,0,ermes,errsev)
          colind = defind
        end if
        goto 60

C  If an error occured during the read tell the user

30      ermes(1:size) = name(1:size)
        ermes(size+1:size+17) = ' Input Conversion'
        call errhan (util,1,ermes,errsev)
        colind = defind
        error = .true.
      else
        colind = defind
      end if

C*****************************  subroutine end  ******************************C

C  Format statements begin ...

10    format (I1)
20    format (I2)

C  Format statements end.

60    return
      end
      subroutine errfil (flnum)

C*****************************************************************************C
C  errfil   - this is a MAPDRV routine                                        C
C  Section  - Error Handling                                                  C
C  Purpose  - To assign fill colors after an  error has  occured in the  fill C
C             table read.                                                     C
C                                                                             C
C  On entry - FLNUM is the number of the color indicies still needed.         C
C                                                                             C
C  On exit  - Those color indicies below  the  one erred upon,  and including C
C             the one erred upon have been set up with some valid guesses.    C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             GSCR                GKS                                         C
C             ----------------------------------------------------------------C
C                                                                             C
C  Assume   - GKS is open.  Color indicies above 200 are reserved for  MAPDRV C
C             use.                                                            C
C                                                                             C
C  Author   - Jeremy Asbill       Date - July 13, 1990       for the MM4 club C
C*****************************************************************************C

C  Integer variable

      integer          flnum             ! number of indicies needed       (in)
      integer          fscolr,           ! for common block FLINFO
     *                 secolr,           ! for common block FLINFO
     *                 thcolr,           ! for common block FLINFO
     *                 frcolr,           ! for common block FLINFO
     *                 fvcolr,           ! for common block FLINFO
     *                 sicolr            ! for common block FLINFO
      integer          wacolr            ! for common block FLWATR

C  Common blocks

      common /flinfo/  fscolr,           ! first color
     *                 secolr,           ! second color
     *                 thcolr,           ! third color
     *                 frcolr,           ! fourth color
     *                 fvcolr,           ! fifth color
     *                 sicolr            ! sixth color
      common /flwatr/  wacolr            ! water color

C****************************  subroutine begin  *****************************C

C  If execution did not stop in ERRHAN, make up some colors

      if (flnum .gt. 0) then
        call gscr (1,206,0.86,0.58,0.44)
        sicolr = 206                       ! Tan
      end if

      if (flnum .gt. 1) then
        call gscr (1,205,0.57,0.00,0.87)
        fvcolr = 205                       ! Purple
      end if

      if (flnum  .gt. 2) then
        call gscr (1,204,1.00,0.00,0.00)
        frcolr = 204                       ! Red
      end if

      if (flnum .gt. 3) then
        call gscr (1,203,0.14,0.56,0.14)
        thcolr = 203                       ! Forest Green
      end if

      if (flnum .gt. 4) then
        call gscr (1,202,1.00,1.00,0.00)
        secolr = 202                       ! Yellow
      end if

      if (flnum .gt. 5) then
        call gscr (1,201,1.00,0.00,1.00)
        fscolr = 201                       ! Magenta
      end if

      if (flnum .gt. 6) then
        call gscr (1,200,0.20,0.56,0.80)
        wacolr = 200                       ! Sky blue (A.K.A. Sea blue)
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine errhan (util,errwar,ermes,errsev)

C*****************************************************************************C
C  errhan   - this is a MAPDRV/CONDRV routine                                 C
C  Section  - Error Handling                                                  C
C  Purpose  - To deliver error  message  to  the user and determine  if  exe- C
C             execution should be halted.                                     C
C                                                                             C
C  On entry - UTIL contains the name of  the utility that has encountered the C
C             error.   ERMES  contains the string to be provided to the user. C
C             ERRWAR indicates a warning or a message.  ERRSEV indicates  the C
C             severity of an error at which execution should stop.   The cur- C
C             rent error count and warning count are passed in through common C
C             block ERRORS.                                                   C
C                                                                             C
C  On exit  - The message has been delivered.  The routine may or may not re- C
C             turn.                                                           C
C                                                                             C
C  Assume   - Nothing.                                                        C
C                                                                             C
C  Author   - Jeremy Asbill       Date - June 21, 1990       for the MM4 club C
C*****************************************************************************C

C  Character variables

      character*60     ermes             ! error message to deliver        (in)
      character*6      util              ! the utility with a problem      (in)
      character*27     fmstp             ! first part of stop message   (local)
      character*12     smstp             ! second part of stop message  (local)
      character*9      tmstp             ! third part of stop message   (local)

C  Integer variables

      integer          errwar,           ! severity of the error           (in)
     *                 errsev            ! execution stop level            (in)
      integer          error,            ! for common block ERRORS
     *                 warns             ! for common block ERRORS

C  Common blocks

      common /errors/  error,            ! current error count
     *                 warns             ! current warning count

C****************************  subroutine begin  *****************************C

C  Deliver the message
C  ERRWAR = 0  => A Warning Message
C  ERRWAR = 1  => An Error Message

      if (errwar .eq. 0) then
        warns = warns + 1
        write (6,20) util,ermes
      else
        error = error + 1
        write (6,10) util,ermes
      end if

C  Check to see if execution should be halted or not
C  ERRSEV = 1    => Nothing Halts Execution
C  ERRSEV = 0    => Error Halt Execution
C  ERRSEV = -1   => Errors and Warnings Halt Execution

      if (errwar .gt. errsev) then
        if ((warns .gt. 9) .and. (error .gt. 9)) then
          fmstp(1:27) = ' - Halting Execution After '
          smstp(1:12) = ' Errors And '
          tmstp(1:9) = ' Warnings'
          write (6,30) util,fmstp(1:27),error,smstp(1:12),
     *                 warns,tmstp(1:9)
        else if ((warns .gt. 9) .and. (error .eq. 1)) then
          fmstp(1:27) = ' - Halting Execution After '
          smstp(1:11) = ' Error And '
          tmstp(1:9) = ' Warnings'
          write (6,55) util,fmstp(1:27),error,smstp(1:11),
     *                 warns,tmstp(1:9)
        else if ((warns .gt. 9) .and.
     *           (((error .gt. 1) .and. (error .le. 9)) .or.
     *            (error .eq. 0))) then
          fmstp(1:27) = ' - Halting Execution After '
          smstp(1:12) = ' Errors And '
          tmstp(1:9) = ' Warnings'
          write (6,50) util,fmstp(1:27),error,smstp(1:12),
     *                 warns,tmstp(1:9)
        else if ((warns .eq. 1) .and. (error .gt. 9)) then
          fmstp(1:27) = ' - Halting Execution After '
          smstp(1:12) = ' Errors And '
          tmstp(1:8) = ' Warning'
          write (6,45) util,fmstp(1:27),error,smstp(1:12),
     *                 warns,tmstp(1:8)
        else if ((((warns .le. 9) .and. (warns .gt. 1)) .or.
     *            (warns .eq. 0)) .and. (error .gt. 9)) then
          fmstp(1:27) = ' - Halting Execution After '
          smstp(1:12) = ' Errors And '
          tmstp(1:9) = ' Warnings'
          write (6,40) util,fmstp(1:27),error,smstp(1:12),
     *                 warns,tmstp(1:9)
        else if ((warns .eq. 1) .and. (error .eq. 1)) then
          fmstp(1:27) = ' - Halting Execution After '
          smstp(1:11) = ' Error And '
          tmstp(1:8) = ' Warning'
          write (6,65) util,fmstp(1:27),error,smstp(1:11),
     *                 warns,tmstp(1:8)
        else if ((((warns .le. 9) .and. (warns .gt. 1)) .or.
     *            (warns .eq. 0)) .and. (error .eq. 1)) then
          fmstp(1:27) = ' - Halting Execution After '
          smstp(1:11) = ' Error And '
          tmstp(1:9) = ' Warnings'
          write (6,70) util,fmstp(1:27),error,smstp(1:11),
     *                 warns,tmstp(1:9)
        else if ((warns .eq. 1) .and. (((error .le. 9) .and.
     *           (error .gt. 1)) .or. (error .eq. 0))) then
          fmstp(1:27) = ' - Halting Execution After '
          smstp(1:12) = ' Errors And '
          tmstp(1:8) = ' Warning'
          write (6,75) util,fmstp(1:27),error,smstp(1:12),
     *                 warns,tmstp(1:8)
        else
          fmstp(1:27) = ' - Halting Execution After '
          smstp(1:12) = ' Errors And '
          tmstp(1:9) = ' Warnings'
          write (6,60) util,fmstp(1:27),error,smstp(1:12),
     *                 warns,tmstp(1:9)
        end if
        stop
      end if

C*****************************  subroutine end  ******************************C

C  Format statements begin ...

10    format (' ',A6,' - Error - ',A60)
20    format (' ',A6,' - Warning - ',A60)
30    format (' ',A6,A27,I2,A12,I2,A9)
40    format (' ',A6,A27,I2,A12,I1,A9)
45    format (' ',A6,A27,I2,A12,I1,A8)
50    format (' ',A6,A27,I1,A12,I2,A9)
55    format (' ',A6,A27,I1,A11,I2,A9)
60    format (' ',A6,A27,I1,A12,I1,A9)
65    format (' ',A6,A27,I1,A11,I1,A8)
70    format (' ',A6,A27,I1,A11,I1,A9)
75    format (' ',A6,A27,I1,A12,I1,A8)

C  Format statements end.

      return
      end
      subroutine gtreal (cval,rval,error)

C*****************************************************************************C
C  gtreal   - this is a MAPDRV/CONDRV routine                                 C
C  Section  - Tables                                                          C
C  Purpose  - To read from a character string a real, regardless of what      C
C             format it is in (that is, xxx.xxx or xxx.xxxExx).               C
C                                                                             C
C  On entry - CVAL  is  a character variable  of fixed size that contains the C
C             number to be converted to a real.  ERROR is false.              C
C                                                                             C
C  On exit  - RVAL  is the value in question.   If an error occured  ERROR is C
C             TRUE otherwise it is FALSE.                                     C
C                                                                             C
C  Assume   - Nothing                                                         C
C                                                                             C
C  Author   - Jeremy Asbill       Date - June 21, 1990      for the MM4 club  C
C*****************************************************************************C

C  Character variables

      character*20     cval              ! character version of number     (in)

C  Integer variables

      integer          unum              ! safe to use unit number      (local)

C  Logical variables

      logical          error             ! has an error occured ?         (out)
      logical          used              ! is a unit number in use ?    (local)

C  Real variables

      real             rval              ! real version of number          (in)

C****************************  subroutine begin  *****************************C

C  First make sure there are only numeric like chars in the string

      error = .false.
      do 50 i = 1,20
        if ((cval(i:i) .ne. ' ') .and. (cval(i:i) .ne. 'E') .and.
     *      (cval(i:i) .ne. 'e') .and. (cval(i:i) .ne. '.') .and.
     *      (cval(i:i) .ne. '+') .and. (cval(i:i) .ne. '-') .and.
     *      ((ichar(cval(i:i)) .lt. 48) .or.
     *       (ichar(cval(i:i)) .gt. 57))) error = .true.
50    continue
      if (.not. error) then

C  Determine a unit number that is safe to use

        unum = 7
40      unum = unum + 1
        inquire (unit=unum,opened=used)
        if (used) goto 40

C  Open a scratch file as unit number UNUM

        open (unum,file='CONDRV.SCR',status='unknown',err=20)

C  Write CVAL out to the scratch file

        write (unum,10,err=20) cval

C  Rewind and read the value back in as a real

        rewind (unum)
        read (unum,*,err=20) rval

C  Close the scratch file

        close (unum)
        goto 30

C  Handle the errors

20      continue
        error = .true.
      end if

C*****************************  subroutine end  ******************************C

C  Format statements ...

10    format (A20)

C  Format statements end.

30    return
      end
      subroutine interr (nomap,fsplat,ssplat,jmax,imax,xpa,ypa,xpb,ypb,
     *                   project,grds,cenlat,cenlon,buff,errsev)

C*****************************************************************************C
C  interr   - This is a MAPDRV routine                                        C
C  Section  - Error handling                                                  C
C  Purpose  - To check for a few obvious errors and  to  initialize the error C
C             handling variables.                                             C
C                                                                             C
C  On entry - JMAX is the x dimension of the entire domain grid.  IMAX is the C
C             y  dimension of the entire domain grid.    FSPLAT is the  first C
C             standard parallel if project = 'LC'.  SSPLAT is the second such C
C             parallel.  XPA, YPA form the lower left grid  point of the map. C
C             XPB, YPB the upper right  grid point of the map.  PROJECT is  2 C
C             characters denoting which projection is to be used. GRDS is the C
C             distance  between two grid  points.  CENLAT and CENLON are  the C
C             center latitude and longitude respectively.   ERRSEV  indicates C
C             what severity of error will halt execution of the program.   If C
C             BUFF  is negative, then no WISS workstation need be open.       C
C                                                                             C
C  On exit  - NOMAP is true if any errors occured.  The  counters in the com- C
C             mon block  ERRORS have been initialized correctly.              C
C                                                                             C
C  Assume   - Nothing.                                                        C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             ERRHAN              MAPDRV/CONDRV utility                       C
C             GQOPS               GKS                                         C
C             OPNGKS              SPPS*                                       C
C             GOPWK               GKS                                         C
C             GACWK               GKS                                         C
C             GQOPWK              GKS                                         C
C             GQCR                GKS                                         C
C             GSCR                GKS                                         C
C             GQWKC               GKS                                         C
C             GQWKCA              GKS                                         C
C             LCCONE              MAPDRV utility                              C
C             ----------------------------------------------------------------C
C                                                                             C
C  Author   - Jeremy Asbill       Date - July 14, 1990       for the MM4 club C
C*****************************************************************************C

C  Parameter

C  ECIRC is the circumference of the earth in kilometers

      parameter        (ecirc = 40023.8904)

C  Character varaibles

      character*2      project           ! specifies projection            (in)
      character*60     ermes             ! error message string         (local)

C  Integer variables

      integer          imax,             ! y dimension size of entire grid (in)
     *                 jmax,             ! x dimension size of entire grid (in)
     *                 buff,             ! GFLASH buffer number to use     (in)
     *                 errsev            ! error severity comparitor       (in)
      integer          error,            ! for common block ERRORS
     *                 warns             ! for common block ERRORS
      integer          ier,              ! error flag from GKS          (local)
     *                 nwk,              ! number of open workstations  (local)
     *                 num,              ! workstation identifier       (local)
     *                 temp,             ! junk filler                  (local)
     *                 wtype,            ! workstation type             (local)
     *                 categ,            ! workstation category         (local)
     *                 i,                ! loop counter                 (local)
     *                 unum,             ! free unit number             (local)
     *                 opst              ! GKS operating state          (local)

C  Logical variables

      logical          nomap             ! don't draw a map ?             (out)
      logical          awiss,            ! is a WISS workstation open ? (local)
     *                 used              ! is a unit number used ?      (local)

C  Real variables

      real             xpa,              ! lower left x grid pt. on map    (in)
     *                 xpb,              ! upper right x grid pt. on map   (in)
     *                 ypa,              ! lower left y grid pt. on map    (in)
     *                 ypb,              ! upper right y grid pt. on map   (in)
     *                 grds,             ! grid distance in kilometers     (in)
     *                 fsplat,           ! first standard parallel (LC)    (in)
     *                 ssplat,           ! second standard parallel (LC)   (in)
     *                 cenlat,           ! center lattitude of domain      (in)
     *                 cenlon            ! center longitude of domain      (in)
      real             red,              ! red component of color rep.  (local)
     *                 blue,             ! blue component of color rep. (local)
     *                 green             ! green component of color rep.(local)

C  Common blocks

      common /errors/  error,            ! error count
     *                 warns             ! warning count

C****************************  subroutine begin  *****************************C

C  Initialize error counters

      error = 0
      warns = 0

C  Initialize NOMAP

      nomap = .false.

C  Check for some obvious errors

C  Grid dimensions of the domain don't jive

      if ((imax .le. 0.0) .or.
     *    (jmax .le. 0.0)) then
        ermes(1:34)  = 'Grid Is Short A Dimension Or Two, '
        ermes(35:60) = 'Check JMAX and IMAX       '
        call errhan ('MAPDRV',1,ermes,errsev)
        nomap = .true.
      end if

C  Grid dimensions of the map don't jive

      if ((xpb .le. xpa) .or.
     *    (ypb .le. ypa)) then
        ermes(1:30)  = 'Map Cannot Be Inverted, xpb An'
        ermes(31:60) = 'd ypb Must Be > xpa And ypa   '
        call errhan ('MAPDRV',1,ermes,errsev)
        nomap = .true.
      end if

C  Projection is not supported by MAPDRV

      if ((project(1:2) .ne. 'ST') .and.
     *    (project(1:2) .ne. 'LC') .and.
     *    (project(1:2) .ne. 'ME') .and.
     *    (project(1:2) .ne. 'CE')) then
        ermes(1:25)  = 'Incapable Of Projection, '
        ermes(26:27) = project(1:2)
        ermes(28:60) = ', Choose From ST, LC, Or ME      '
        call errhan ('MAPDRV',1,ermes,errsev)
        nomap = .true.
      end if

C  If the projection is Lambert Conformal make sure the given 
C  standard parallels are valid.  If they are calculate the cone factor

      if (project(1:2) .eq. 'LC') then
        if ((abs(fsplat) .gt. 90.0) .or. (abs(ssplat) .gt. 90.0)) then
          if (cenlat .gt. 0.0) then
            call lccone (30.0,60.0,1)
          else
            call lccone (-30.0,-60.0,-1)
            fsplat = -91.0
          end if
        else if ((fsplat/ssplat) .lt. 0.0) then
          ermes(1:30)  = 'Lambert Projection Around The '
          ermes(31:60) = 'Equator, Use Mercator         '
          call errhan ('MAPDRV',1,ermes,errsev)
          nomap = .true.
        else
          if (cenlat .gt. 0.0) then
            call lccone (fsplat,ssplat,1)
          else
            call lccone (fsplat,ssplat,-1)
          end if
        end if

C  Make sure the given standard parallels are on the same half of the globe
C  as the center latitude

        if ((fsplat/cenlat) .lt. 0.0) then
          ermes(1:30)  = 'Lambert Parallels And Domain C'
          ermes(31:60) = 'enter In Opposite Hemispheres '
          call errhan ('MAPDRV',1,ermes,errsev)
          nomap = .true.
        end if

C  If the projection is Polar Stereographic make sure the FSPLAT parameter
C  is a valid true latitude.

      else if (project(1:2) .eq. 'ST') then

        if (abs(fsplat) .gt. 90.0) then
          if (cenlat .gt. 0.0) then
            call lccone (60.0,0.0,1)
          else
            call lccone (-60.0,0.0,-1)
            fsplat = -91.0
          end if
        else
          if (cenlat .gt. 0.0) then
            call lccone (fsplat,0.0,1)
          else
            call lccone (fsplat,0.0,-1)
          end if
        end if

C  Make sure the domain is in the same hemisphere as the projection is true in

        if ((fsplat/cenlat) .lt. 0.0) then
          ermes(1:30)  = 'Polar True Lat. And Domain Cen'
          ermes(31:60) = 'ter In Opposite Hemispheres   '
          call errhan ('MAPDRV',1,ermes,errsev)
          nomap = .true.
        end if
      end if

C  Make sure the subdomain is at least an improper subdomain of the domain

      if ((xpa .lt. 1.0) .or. (ypa .lt. 1.0) .or.
     *    (xpb .gt. float(jmax)) .or. (ypb .gt. float(imax))) then
        ermes(1:43)  = 'Subdomain Defined By xpa,ypa And xpb,ypb Is'
        ermes(44:60) = ' Not A Subdomain'
        call errhan ('MAPDRV',1,ermes,errsev)
        nomap = .true.
      end if

C  Check that the domain will even fit on the earth

      if ((jmax * nint(grds) .gt. ecirc) .or.
     *    (imax * nint(grds) .gt. ecirc)) then
        ermes(1:31)  = 'Domain Is Larger Than The Earth'
        ermes(32:60) = '                             '
        call errhan ('MAPDRV',1,ermes,errsev)
        nomap = .true.
      end if

C  Make sure the center lat and lon are withing reason

      if ((cenlat .gt. 90) .or. (cenlat .lt. -90) .or.
     *    (cenlon .gt. 180) .or. (cenlon .lt. -180)) then
        ermes(1:30)  = 'Center Latitude And Longitude '
        ermes(31:60) = 'Are Out Of This World         '
        call errhan ('MAPDRV',1,ermes,errsev)
        nomap = .true.
      end if

C  Make sure the state of GKS is proper

      call gqops (opst)
      if (opst .eq. 0) then
        ermes(1:30)  = 'GKS Is Not Open               '
        ermes(31:60) = '                              '
        call errhan ('MAPDRV',1,ermes,errsev)
        call opngks
        nomap = .true.
      else if (opst .eq. 1) then
        ermes(1:30)  = 'There Are No Open Workstations'
        ermes(31:60) = '                              '
        call errhan ('MAPDRV',1,ermes,errsev)
        call gopwk (1,2,1)
        call gacwk (1)
        nomap = .true.
      else if (opst .eq. 2) then
        ermes(1:30)  = 'There Are No Active Workstatio'
        ermes(31:60) = 'ns                            '
        call errhan ('MAPDRV',1,ermes,errsev)
        call gqopwk (1,ier,nwk,num)
        call gacwk (num)
        nomap = .true.
      end if

C  Make certain background and foreground color are defined

c "gqcr (1,index,0,ier,red,blue,green)" to get the 'red', 'blue',
c and 'green' for the 'index', here index=0:

      call gqcr (1,0,0,ier,red,blue,green)

C  Check for errors from GKS

      if ((ier .eq. 87) .or. (ier .eq. 93)) then
        ermes(1:30)  = 'Background Color Index Is Inva'
        ermes(31:60) = 'lid                           '
        call errhan ('MAPDRV',1,ermes,errsev)
        nomap = .true.
      else if ((ier .ne. 0) .and. (ier .ne. 94)) then
        ermes(1:30)  = 'Uncorrectable Error Encoutered'
        ermes(31:60) = '                              '
        call errhan ('MAPDRV',1,ermes,errsev)
        nomap = .true.
      end if

C  If Background color is not black warn the user that their maps may look
C  stupid

      if ((red .ne. 0.0) .and. (blue .ne. 0.0) .and.
     *    (green .ne. 0.0)) then
        ermes(1:30)  = 'Background Color Index Is Not '
        ermes(31:60) = 'Black, Background Color Reset '
        call errhan ('MAPDRV',0,ermes,errsev)

c All 'red', 'blue', and 'green' are not zero means the background 
c (index =0) is not black.

c To force the background (index =0) to be black:
#ifdef BBKG
       call gscr (1,0,0.00,0.00,0.00)
#endif
      end if

      call gqcr (1,1,0,ier,red,blue,green)

C  Check for errors from GKS

      if ((ier .eq. 87) .or. (ier .eq. 93)) then
        ermes(1:30)  = 'Foreground Color Index Is Inva'
        ermes(31:60) = 'lid                           '
        call errhan ('MAPDRV',1,ermes,errsev)
        nomap = .true.
      else if ((ier .ne. 0) .and. (ier .ne. 94)) then
        ermes(1:30)  = 'Uncorrectable Error Encoutered'
        ermes(31:60) = '                              '
        call errhan ('MAPDRV',1,ermes,errsev)
        nomap = .true.
      end if

C  If Foreground color is not white warn the user that their maps may look
C  stupid

      if ((red .lt. 0.8) .and. (blue .lt. 0.8) .and.
     *    (green .lt. 0.8)) then
        ermes(1:30)  = 'Foreground Color Index Is Not '
        ermes(31:60) = 'White, Foreground Color Reset '
        call errhan ('MAPDRV',0,ermes,errsev)

c All 'red', 'blue', and 'green' are less than 0.8 means the 
c foreground (index =1) is not white.

c To force the foreground (index =1) to be white:
#ifdef BBKG
       call gscr (1,1,0.80,0.80,0.80)
#endif
      end if

C  Check that a WISS workstation has been opened for GFLASH

      if (buff .ge. 0) then
        call gqopwk (1,ier,nwk,num)
        do 10 i = 1,nwk
          call gqopwk (i,ier,temp,num)
          call gqwkc (num,ier,temp,wtype)
          call gqwkca (wtype,ier,categ)
          if (categ .eq. 3) awiss = .true.
10      continue
        if (.not. awiss) then
          ermes(1:30)  = 'No WISS Workstation Open      '
          ermes(31:60) = '                              '
          call errhan ('MAPDRV',1,ermes,errsev)
          unum = 0
20        unum = unum + 1
          inquire (unit=unum,opened=used)
          if (used) goto 20
          call gopwk (2,unum,3)
          nomap = .true.
        end if
      end if

C*****************************  subroutine end *******************************C

      return
      end
      subroutine lccone (fsplat,ssplat,sign)

C*****************************************************************************C
C  lccone   - This is a MAPDRV routine                                        C
C  Section  - Design                                                          C
C  Purpose  - To calculate the cone factor to use in a Lambert Conformal Pro- C
C             jection.                                                        C
C                                                                             C
C  On entry - FSPLAT is the first standard parallel to use in the calculation C
C             of the cone factor.  SSPLAT is the second such parallel.   SIGN C
C             indicates what hemisphere the projection is in.                 C
C                                                                             C
C  On exit  - CONFAC is calculated and stored in the common block LAMSTF with C
C             FSPLAT and SSPLAT.                                              C
C                                                                             C
C  Assume   - Nothing.                                                        C
C                                                                             C
C  Author   - Jeremy Asbill       Date - October 10, 1990    for the MM4 club C
C*****************************************************************************C

C  Parameters

      parameter       (conv = 0.01745329251994)

C  Integer variables

      integer          sign              ! indicates which hemisphere      (in)

C  Real variables

      real             fsplat,           ! first standard parallel lat.    (in)
     *                 ssplat            ! second standard parallel lat.   (in)
      real             confac,           ! for common block LAMSTF
     *                 fsparl,           ! for common block LAMSTF
     *                 ssparl            ! for common block LAMSTF

C  Common blocks

      common /lamstf/  confac,           ! cone factor to be used
     *                 fsparl,           ! first standard parallel lat.
     *                 ssparl            ! second standard parallel lat.

C****************************  subroutine begin  *****************************C

C  Calculate CONFAC using input parameters

c For projection 'LC', abs(fsplat) is never equal to 90.0, but for projection 'ST', 
c the abs(fsplat) could be 90.0, and the confac calculation has difficulty 
c -- YRG (02/28/2005).

      if (abs(fsplat) == 90.0) then
         confac = 1.0
      else
      confac = alog10(cos(fsplat * conv)) - alog10(cos(ssplat * conv))
      confac = confac/(alog10(tan((45.0 - float(sign) * 
     *                             fsplat/2.0) * conv)) -
     *                 alog10(tan((45.0 - float(sign) *
     *                             ssplat/2.0) * conv)))
      endif
      print *,'in lccone: confac=',confac,'  fsplat=',fsplat

C  Stuff it all into the common block

      fsparl = fsplat
      ssparl = ssplat

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine llproc (xcs,ycs,ncs,aid,gid,nid)

C*****************************************************************************C
C   llproc  - This is a MAPDRV routine                                        C
C   Section - Lat/Lon Lines                                                   C
C   Purpose - To draw lat/lon  lines  for  all  patterns  except  publication C
C             style.                                                          C
C                                                                             C
C  On entry - XCS and YCS hold the x  and y  coordinates,  in  the fractional C
C             coordinate system, of NCS points defining a segment of the lat/ C
C             lon grid to be drawn. AID and GID contain NID pairs of area and C
C             group identifiers describing the polygon.   Common block LLLDET C
C             contains the detail information about where the lines go an how C
C             they should look.  Common block LLLCOL contains  the  color  to C
C             make them.  Common block MOCDET contains information about what C
C             outlines were used to draw and fill the map.   Finally,  common C
C             block MAPEDGE contains the fraction  coordinates  defining  the C
C             edge of the map.                                                C
C                                                                             C
C  On exit  - A portion of the grid has been drawn.   If the current  portion C
C             of the lat/lon grid intersected the edge of the map the approp- C
C             riate variables in common blocks LABNUM  and LABPOS  have  been C
C             updated.                                                        C
C                                                                             C
C  Assume   - GKS is open.  This routine is called by the EZMAPA utility.     C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             GQPLCI              GKS                                         C
C             GSPLCI              GKS                                         C
C             DASHDB              DASHLINE utility*                           C
C             CURVED              DASHLINE utility*                           C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C             This routine is not called by MAPDRV, it is called by the AREAS C
C             utility which is called by the EZMAPA utility routine MAPGRM.   C
C                                                                             C
C             Also note that the first part of this routine checks for  label C
C             positions for the MAPDRV style labels.                          C
C                                                                             C
C  Author   - Jeremy Asbill       Date - July 7, 1990        for the MM4 club C
C*****************************************************************************C

C  Parameters

      parameter        (tol = 0.0005)

C  Character variables

      character*2      wouts             ! for common block MOCDET

C  Integer variables

      integer          aid(*),           ! area identifiers                (in)
     *                 gid(*),           ! group identifiers               (in)
     *                 ncs,              ! dimension for XCS and YCS       (in)
     *                 nid               ! dimension for AID and GID       (in)
      integer          llplc,            ! for common block LLLDET
     *                 grdsh,            ! for common block LLLDET
     *                 llint             ! for common block LLLDET
      integer          llcolr            ! for common block LLLCOL
      integer          lfnum,            ! for common block LABNUM
     *                 rgnum,            ! for common block LABNUM
     *                 btnum,            ! for common block LABNUM
     *                 tpnum             ! for common block LABNUM
      integer          itm,              ! calculation variable         (local)
     *                 plsv,             ! integer save variable        (local)
     *                 ier               ! junk filler                  (local)

C  Real variables

      real             xcs(*),           ! the grid line x coordinates     (in)
     *                 ycs(*)            ! the grid line y coordinates     (in)
      real             left,             ! for common block MAPEDGE
     *                 right,            ! for common block MAPEDGE
     *                 bottom,           ! for common block MAPEDGE
     *                 top               ! for common block MAPEDGE
      real             lfpos(360,2),     ! for common block LABPOS
     *                 rgpos(360,2),     ! for common block LABPOS
     *                 btpos(360,2),     ! for common block LABPOS
     *                 tppos(360,2)      ! for common block LABPOS

C  Common blocks

      common /llldet/  llplc,            ! where do we draw lat/lon lines
     *                 grdsh,            ! lat/lon grid dash pattern
     *                 llint             ! not used
      common /lllcol/  llcolr            ! color of lat/lon lines
      common /mocdet/  wouts             ! which outlines were used
      common /mapedge/ left,             ! fractional coord. of left edge
     *                 right,            ! fractional coord. of right edge
     *                 bottom,           ! fractional coord. of bottom edge
     *                 top               ! fractional coord. of top edge
      common /labpos/  lfpos,            ! positions of labels along the left
     *                 rgpos,            ! positions of labels along the right
     *                 btpos,            ! positions of labels along the bottom
     *                 tppos             ! positions of labels along the top
      common /labnum/  lfnum,            ! # of positions in LFPOS
     *                 rgnum,            ! # of positions in RGPOS
     *                 btnum,            ! # of positions in BTPOS
     *                 tpnum             ! # of positions in TPPOS

C****************************  subroutine begin  ****************************C

C  Iteratively search for all intersections with the boundary
C  These intersections are locations for lat/lon line labels or MAPDRV
C  labels

      do 10 i = 1,ncs

C  First check for an intersection with the bottom

        if ((ycs(i) .ge. (bottom - tol))  .and.
     *      (ycs(i) .le. (bottom + tol))) then
          if (xcs(i) .ne. btpos(btnum,1)) then
            btnum = btnum + 1
            if (btnum .eq. 362) then
              btnum = btnum - 1
            else if (btnum .lt. 361) then
              btpos(btnum,1) = xcs(i)
              btpos(btnum,2) = ycs(i)
            end if
          end if

C  Second check for an intersection with the top

        else if ((ycs(i) .ge. (top - tol)) .and.
     *           (ycs(i) .le. (top + tol))) then
          if (xcs(i) .ne. tppos(tpnum,1)) then
            tpnum = tpnum + 1
            if (tpnum .eq. 362) then
              tpnum = tpnum - 1
            else if (tpnum .lt. 361) then
              tppos(tpnum,1) = xcs(i)
              tppos(tpnum,2) = ycs(i)
            end if
          end if

C  First check for intersection with the left

        else if ((xcs(i) .ge. (left - tol)) .and.
     *           (xcs(i) .le. (left + tol))) then
          if (ycs(i) .ne. lfpos(lfnum,2)) then
            lfnum = lfnum + 1
            if (lfnum .eq. 362) then
              lfnum = lfnum - 1
            else if (lfnum .lt. 361) then
              lfpos(lfnum,1) = xcs(i)
              lfpos(lfnum,2) = ycs(i)
            end if
          end if

C  Second check for an intersection with the right

        else if ((xcs(i) .ge. (right - tol)) .and.
     *           (xcs(i) .le. (right + tol))) then
          if (ycs(i) .ne. rgpos(rgnum,2)) then
            rgnum = rgnum + 1
            if (rgnum .eq. 362) then
              rgnum = rgnum - 1
            else if (rgnum .lt. 361) then
              rgpos(rgnum,1) = xcs(i)
              rgpos(rgnum,2) = ycs(i)
            end if
          endif
        end if
10    continue

C  Only draw lat/lon lines if some were requested
C  LLPLC = 0 means no lat/lon lines were requested
C  LLPLC = 0 means publication style lat/lon lines were requested

      if ((llplc .ne. 0) .and. (grdsh .ne. 0)) then

C  Get and save polyline color index

        call gqplci (ier,plsv)

C  Set up new index

        call gsplci (llcolr)

C  Set up correct dash pattern

        call dashdb (grdsh)

C  Make sure the polygon lies within the map

        if ((aid(1) .ge. 0) .and. (aid(2) .ge. 0)) then

C  Get the maximum of the area identifiers

          itm = max0(aid(1),aid(2))

C  If we want lat/lon lines over land then draw them when the suggested color
C  index is not 1.  The areas package suggests that oceans have color index 1.
C  MAPACI is an EZMAPA function.

          if ((llplc .eq. 1) .and. (wouts(1:2) .ne. 'NO')) then
            if ((mapaci(itm) .ne. 1) .and. (itm .ne. 223) .and.
     *          (itm .ne. 0))

C  Draw the line and do it using grdsh.

     *        call curved (xcs,ycs,ncs)
     
C  If we want lat/lon lines over water then draw them when the area is
C  identified as either ocean or no within the outlines
C  ITM = 2,1005 means the line is over the ocean
C  ITM = 223 means the line is outside the U.S. States
C  ITM = 0 means, even though outlines were requested there are none
C          in the map, so it is all water

          else if ((llplc .eq. -1) .and. (wouts(1:2) .ne. 'NO')) then
            if ((itm .eq. 2) .or. (itm .eq. 223) .or.
     *          (itm .eq. 1005) .or. (itm .eq. 0))

C  Draw the line and try to do it using grdsh.

     *        call curved (xcs,ycs,ncs)

C  If we want lat/lon lines over both land and water just draw them
C  Also if there are no outlines, just draw them

          else
            call curved (xcs,ycs,ncs)
          end if
        end if
      
C  Reset the polyline color index

        call gsplci (plsv)
      end if

C****************************  subroutine end  ******************************C

      return
      end
      subroutine mapdrv (project,fsplat,ssplat,cenlat,cenlon,grds,jmax,
     *                   imax,xpa,xpb,ypa,ypb,titline,titlen,buff,unum,
     *                   doset,errsev)

C*****************************************************************************C
C  mapdrv   - Map Driver                                                      C
C                                                                             C
C  Purpose  - To generate esthetically pleasing maps for use in analysis pro- C
C             gram.  It is the hopes of the programmer that this utility will C
C             be used in GRAPH, RAWINS, TERRAIN and other programs of the MM4 C
C             modelling system.  This utility is completely general and gives C
C             the user flexibility in map appearence.                         C
C                                                                             C
C  On entry - PROJECT  describes what projection is being  used.   FSPLAT and C
C             SSPLAT are only valid if PROJECT = 'LC' and then they represent C
C             the first and second standard parallels respectively to be used C
C             when calculating the projection cone factor.  CENLAT and CENLON C
C             is  the central latitude and longitude of the domain.  GRDS  is C
C             the grid distance of the grid the domain is described on.  JMAX C
C             and IMAX define the overall size of the grid.  XPA, YPA are the C
C             lower left hand grid point of the subdomain to be drawn.   XPB, C
C             YPB  are the upper right hand grid point of the subdomain to be C
C             drawn.  TITLINE is the title to go below the map. TITLEN is the C
C             number of characters in TITLINE.  BUFF is the GFLASH  buffer to C
C             store the map in and UNUM is the unit number to read the tables C
C             from.  If BUFF is negative then only an area map is to be  gen- C
C             erated for CONDRV to use.  If UNUM is negative there are no ta- C
C             bles and everything defaults.  DOSET  tells MAPDRV if it should C
C             do  its  own set call or use the set call the  user  has  made. C
C             ERRSEV  is either a negative positive or zero.                  C
C               ERRSEV > 0 means, nothing stops execution                     C
C               ERRSEV = 0 means, errors stop execution, warnings do not      C
C               ERRSEV < 0 means, both errors and warnings stop execution.    C
C                                                                             C
C  On exit  - The map has been stored in GFLASH buffer BUFF.                  C
C                                                                             C
C  Assume   - That a color table has been set up and GKS is open and active.  C
C             The defining grid begins at 1,1.                                C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             INTERR              MAPDRV utility                              C
C             MRDDET              MAPDRV utility                              C
C             MRDFIL              MAPDRV utility                              C
C             MRDCLT              MAPDRV utility                              C
C             SETWIN              MAPDRV utility                              C
C             SETPRO              MAPDRV utility                              C
C             MAPSTI              EZMAP utility*                              C
C             SETMAP              MAPDRV utility                              C
C             MPDRLL              MAPDRV utility                              C
C             MPDRCL              MAPDRV utility                              C
C             GSFAIS              GKS                                         C
C             MAPINT              EZMAP utility*                              C
C             GFLAS1              GFLASH utility*                             C
C             MPDROL              MAPDRV utility                              C
C             PERIM               GRIDAL utility*                             C
C             MPDREL              MAPDRV utility                              C
C             MPDRML              MAPDRV utility                              C
C             MPDRTL              MAPDRV utility                              C
C             GFLAS2              GFLASH utility*                             C
C             MAPFIL              MAPDRV utility                              C
C             GQCLIP              GKS                                         C
C             GSCLIP              GKS                                         C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C             If this is the domain ...                                       C
C                                                                             C
C        imax ----------------------------|--------------------------         C
C             |      ypb -------------------------------------      |         C
C             |          |                |                  |      |         C
C             |          |                |                  |      |         C
C             |          |                |                  |      |         C
C             |          |                |                  |      |         C
C             |          |                |                  |      |         C
C             |          |                |                  |      |         C
C             |          |                |                  |      |         C
C        cenlat----------|----------------|------------------|-------         C
C             |          |                |                  |      |         C
C             |      ypa -------------------------------------      |         C
C             |          xpa              |                xpb      |         C
C             |                           |                         |         C
C             |                           |                         |         C
C             |                         cenlon                      |         C
C             |                           |                         |         C
C             |                           |                         |         C
C             ----------------------------|--------------------------         C
C                                                                jmax         C
C                                                                             C
C  Author   - Jeremy Asbill        Date - July 6, 1990       for the MM4 club C
C*****************************************************************************C

C  Character variables

      character*80     titline           ! string containing the title     (in)
      character*2      project           ! specifies projection            (in)

C  Integer variables

      integer          imax,             ! y dimension size of entire grid (in)
     *                 jmax,             ! x dimension size of entire grid (in)
     *                 titlen,           ! # of characters in title string (in)
     *                 buff,             ! GFLASH buffer to use            (in)
     *                 unum,             ! unit number tables are on       (in)
     *                 errsev            ! error severity indicator        (in)
      integer          wlabs,            ! which labels do we want      (local)
     *                 cfsv,             ! clipping flag save variable  (local)
     *                 ier               ! junk filler                  (local)
     
C  Logical variables

      logical          doset             ! do our own set here ?           (in)
      logical          flmap,            ! is the map being filled ?    (local)
     *                 title,            ! put a title on the map ?     (local)
     *                 perm,             ! put a perimeter on the map ? (local)
     *                 nomap             ! don't draw a map ?           (local)

C  Real variables

      real             cenlat,           ! central latitude of the domain  (in)
     *                 cenlon,           ! central longitude of the domain (in)
     *                 fsplat,           ! first standard parallel (LC)    (in)
     *                 ssplat,           ! second standard parallel (LC)   (in)
     *                 grds,             ! grid distance in kilometers     (in)
     *                 xpa,              ! lower left x grid pt. on map    (in)
     *                 xpb,              ! upper right x grid pt. on map   (in)
     *                 ypa,              ! lower left y grid pt. on map    (in)
     *                 ypb               ! upper right y grid pt. on map   (in)
      real             junk(4)           ! junk filler                  (local)

C*********************** subroutine begin ************************************C

C  EZMAP internal parameters used in this routine are:
C  VS  - Vertical Slicing

C  Assume the map will not be filled until otherwise decided

      flmap = .false.

C  Do initial error checking

      call interr (nomap,fsplat,ssplat,jmax,imax,xpa,ypa,xpb,ypb,
     *             project,grds,cenlat,cenlon,buff,errsev)

C  Get map details

      call mrddet (unum,buff,llplc,wlabs,title,perm,errsev,nomap)

C  If an area map has been requested rather than a drawn map
C  skip the next two table reads

      if (buff .ge. 0) then

C  Get map fill information

        call mrdfil (unum,flmap,errsev,nomap)

C  Get map color information

        call mrdclt (unum,llplc,wlabs,title,errsev,nomap)

C  Save the users setting for clipping and then turn it off

        call gqclip (ier,cfsv,junk)
        call gsclip (0)
      end if

C  Do not design the map if NOMAP is set

      if (.not. nomap) then

C  Set up the proper projection

        call setpro (project,cenlat,cenlon)

C  Set the window up, making sure there is room above and below it

        call setwin (xpa,ypa,xpb,ypb,doset)
  
C  Set up the map window on the globe

        call setmap (cenlat,cenlon,project,grds,
     *               xpa,ypa,xpb,ypb,imax,jmax)

C  Set up the outline, outline colors, and text colors for the map

        call mpdrcl (perm,buff)
    
C  If color fill is requested, force solid fill

        if ((flmap) .and. (buff .ge. 0)) then
          call gsfais (1)

C  Use 20 vertical slice to stay within polygon size limits of devices

          call mapsti ('VS',20)
        end if

C  Initialize EZMAP

        call mapint
      end if

C  Start saving metacode instructions

      if (buff .ge. 0) call gflas1 (buff)

C  Do not draw the map if a non-correctable error occured

      if (.not. nomap) then

C  Call user subroutine

        call mpdrus

C  Prepare for and do color fill

        call mapfil (flmap,buff)

C  Draw the outlines

        if (buff .ge. 0) then
          call mpdrol

C  Draw in the lat/lon line grid

          call mpdrll (grds,flmap,wlabs,cenlon,xpa,xpb,project)

C  Draw a nice perimeter

          if (perm)
     *      call perim (0,nint(xpb - xpa),0,nint(ypb - ypa))

C  Set up and draw EZMAP labels

          call mpdrel (perm)

C  Set up and draw MAPDRV labels

          call mpdrml (xpa,ypa,xpb,ypb,project,grds,cenlat,cenlon,
     *                 jmax,imax,errsev)

C  Inform the user about there map only if one was made

          print *, 'MAPDRV - Map Successfully Completed'
        end if
      end if

C  Put on the title

      if (buff .ge. 0) then
        call mpdrtl (titline,titlen,nomap)

C  Restore the users clipping setting

        call gsclip (cfsv)

C  Save all this on the map background to be flashed

        call gflas2
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine mapfil (flmap,buff)

C*****************************************************************************C
C  mapfil   - This is a MAPDRV routine                                        C
C  Section  - Fill                                                            C
C  Purpose  - To do requested color fill, including filling the background if C
C             no geographical outlines are requested.                         C
C                                                                             C
C  On entry - FLMAP tells us if we should fill or not.  WOUTS in common block C
C             MOCDET tells us which geographical outlines are requested.   If C
C             BUFF is negative then the map should be put into the CONDRV ar- C
C             ea map and not filled to the buffer.                            C
C                                                                             C
C  On exit  - The map has been filled.                                        C
C                                                                             C
C  Assume   - GKS is open.                                                    C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             ARINAM              AREAS utility*                              C
C             MAPBLA              EZMAPA utility*                             C
C             ARPRAM              AREAS utility*                              C
C             ARSCAM              AREAS utility*                              C
C             GETSET              SPPS*                                       C
C             SFSGFA              SOFTFILL utility*                           C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill        Date - July 9, 1990       for the MM4 club C
C*****************************************************************************C

C  Character variables

      character*2      wouts             ! for common block MOCDET

C  Integer varaibles

      integer          buff              ! GFLASH buffer to use            (in)
      integer          amapf(255000)     ! for common block ARAMAP
      integer          cfmap(500000)     ! for common block CSCAMP
      integer          cdmap(100000)     ! for common block CLNAMP
      integer          wacolr            ! for common block FLWATR
      integer          binhr             ! for common block CONFLG
      integer          aid(10),          ! area idfiers array for AREAS (local)
     *                 gid(10),          ! group idfiers array , AREAS  (local)
     *                 ind(12),          ! scratch array for SOFTFILL   (local)
     *                 llsv              ! junk filler                  (local)

C  Logical variables

      logical          flmap             ! will we fill the map ?          (in)

C  Real variables

      real             xcs(5500),        ! x dimension array for AREAS  (local)
     *                 ycs(5500),        ! y dimension array for AREAS  (local)
     *                 xvp(4),           ! x coords for viewport        (local)
     *                 yvp(4),           ! y coords for viewport        (local)
     *                 dst(8),           ! scratch array for SOFTFILL   (local)
     *                 ulsv,             ! junk filler                  (local)
     *                 ursv,             ! junk filler                  (local)
     *                 utsv,             ! junk filler                  (local)
     *                 ubsv              ! junk filler                  (local)

C  Common blocks

      common /mocdet/  wouts             ! geographical outline indicator
      common /aramap/  amapf             ! area map for fill
      common /flwatr/  wacolr            ! background color
      common /cscamp/  cfmap             ! CONDRV area map for shade and color
      common /clnamp/  cdmap             ! CONDRV area map for drawing
      common /conflg/  binhr             ! 722 => we have been in here

C  External routines

      external         clproc            ! does actual filling of polygons

C****************************  subroutine begin  *****************************C

C  Do a regular map fill if a fill is requested and there are geographical
C  outlines to use

      if ((flmap) .and. (wouts(1:2) .ne. 'NO')) then

C  Initialize area map

        call arinam (amapf,255000)

C  Set the group identifiers to the EZMAPA defaults

        call mapsti ('G1',1)
        call mapsti ('G2',2)

C  Add edges to the area map

        call mapbla (amapf)

C  Preprocess the area map

        call arpram (amapf,0,0,0)

C  Fill the map

        call arscam (amapf,xcs,ycs,5500,aid,gid,10,clproc)

C  Fill the entire viewport if no outlines are request and fill is still
C  desired

      else if ((flmap) .and. (wouts(1:2) .eq. 'NO')) then

C  Define the polygon in an array

        call getset (ulsv,ursv,ubsv,utsv,xvp(1),xvp(3),yvp(1),yvp(2),
     *               llsv)
        xvp(2) = xvp(1)
        yvp(3) = yvp(2)
        xvp(4) = xvp(3)
        yvp(4) = yvp(1)

C  Fill the viewport

        call sfsgfa (xvp,yvp,4,dst,8,ind,12,wacolr)

C  Put the map into the CONDRV area map if BUFF is negative

      else if (buff .lt. 0) then

C  Let CONDRV know that we have been here

        binhr = 722

C  Initialize both area maps

        call arinam (cfmap,500000)
        call arinam (cdmap,100000)

C  Set the group identifiers to 6 and 7

        call mapsti ('G1',6)
        call mapsti ('G2',7)

C  Add the map to both area maps

        call mapbla (cfmap)
        call mapbla (cdmap)
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine mpdrcl (perm,buff)

C*****************************************************************************C
C   mpdrcl  - This is a MAPDRV routine                                        C
C   Section - Design                                                          C
C   Purpose - To set up those internal parameters, and a few other parameters C
C             that define the colors for the map.   To define the proper geo- C
C             graphical outlines for the map.                                 C
C                                                                             C
C  On entry - Common block OUTCOL contains the outline colors.   Common block C
C             PERCOL  contains the perimeter  color.   WOUTS  in common block C
C             MOCDET tells us which outline colors to set.   PERM tells  what C
C             kind of perimeter will be put on the map.   If BUFF is negative C
C             only set up the outline.                                        C
C                                                                             C
C  On exit  - The color for the map outlines have been set up.  The color for C
C             the perimeter has been set up.  The correct outlines have  been C
C             set up with EZMAP.                                              C
C                                                                             C
C  Assume   - GKS is open.                                                    C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             MAPSTI              EZMAP utility*                              C
C             GACOLR              GRIDAL utility*                             C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill        Date - July 7, 1990       for the MM4 club C
C*****************************************************************************C

C  Character variables

      character*2      wouts             ! for common block MOCDET

C  Integer variables

      integer          buff              ! GFLASH buffer number            (in)
      integer          cocolr,           ! for common block OUTCOL
     *                 uscolr,           ! for common block OUTCOL
     *                 cncolr            ! for common block OUTCOL
      integer          pecolr            ! for common block PERCOL

C  Logical variables

      logical          perm              ! what perimeter will be used ?   (in)

C  Common blocks

      common /outcol/  cocolr,           ! color of continents
     *                 uscolr,           ! color of states
     *                 cncolr            ! color of countries
      common /percol/  pecolr            ! color of perimeter
      common /mocdet/  wouts             ! desired outline indicator

C****************************  subroutine begin  ****************************C

C  EZMAP internal parameters used are:
C  C1  - Color of EZMAP perimeter
C  C5  - Color index for CO, continental boundaries
C  C6  - Color index for US, state boundaries
C  C7  - Color index for CN, country boundaries
C  OU  - geographical OUtlines

C  Only set up colors if a map is going to be drawn

      if (buff .ge. 0) then

C  Determine which outlines will be drawn and set those colors

        if (wouts(1:2) .eq. 'US') then
          call mapsti ('C6',uscolr)
        else if (wouts(1:2) .eq. 'PS') then
          call mapsti ('C6',uscolr)
          call mapsti ('C5',cocolr)
          call mapsti ('C7',cncolr)
        else if (wouts(1:2) .eq. 'PO') then
          call mapsti ('C5',cocolr)
          call mapsti ('C7',cncolr)
        else if (wouts(1:2) .eq. 'CO') then
          call mapsti ('C5',cocolr)
        end if

C  Set up the color of the perimeter

        if (perm) then
          call gacolr (pecolr,pecolr,pecolr,pecolr)
        else
          call mapsti ('C1',pecolr)
        end if
      end if

C  Setup the outline

      call mapstc ('OU',wouts(1:2))

C****************************  subroutine end  ******************************C

      return
      end
      subroutine mpdrel  (perm)

C*****************************************************************************C
C  mpdrel   - This is a MAPDRV routine                                        C
C  Section  - Labels                                                          C
C  Purpose  - To design and draw EZMAP labels.                                C
C                                                                             C
C  On entry - PERM indicates if the EZMAP perimeter should  be drawn with the C
C             labels.                                                         C
C                                                                             C
C  On exit  - The labels have been written to the GFLASH buffer.              C
C                                                                             C
C  Assume   - GKS is open.  The map itself has been drawn.                    C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             MAPSTI              EZMAP utility*                              C
C             MAPLBL              EZMAP utility*                              C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill        Date - July 7, 1990       for the MM4 club C
C*****************************************************************************C

C  Parameter

      parameter        (scale = 20.0)    ! scales LBSIZ for EZMAP

C  Integer variables

      integer          wlabs,            ! for common block MLBDET
     *                 lbqul(2)          ! for common block MLBDET
      integer          lacolr            ! for common block LABCOL

C  Logical variables

      logical          perm              ! put a perimeter on the map ?    (in)

C  Real variables

      real             lbsiz             ! for common block MLBDET
      real             temp              ! calculation variable         (local)

C  Common blocks

      common /mlbdet/  wlabs,            ! which labels do we want
     *                 lbsiz,            ! alternate label size
     *                 lbqul             ! not used
      common /labcol/  lacolr            ! label color

C****************************  subroutine begin  *****************************C

C  EZMAP internal parameters used in this routine are:
C  LS  - Label Size
C  C3  - Color index for EZMAP labels
C  PE  - PErimeter flag
C  LA  - LAbel flag

C  Set up EZMAP labels

      if ((wlabs .eq. 2) .or. (wlabs .eq. -1)) then

C  Turn on the EZMAP labels

        call mapstl ('LA',.true.)

C  Set up correct color for EZMAP labels

        call mapsti ('C3',lacolr)

C  Set up proper label size for EZMAP labels

        temp = scale * lbsiz
        temp = amod(temp,12.0)
        temp = temp/12
        if (temp .eq. 0.0) then
          temp = 1.0
        end if
        call mapsti ('LS',nint(temp))
      else

C  Turn of the EZMAP labels

        call mapstl ('LA',.false.)
      end if


C  Set up the EZMAP perimeter

      if (perm) then
        call mapstl ('PE',.false.)
      else
        call mapstl ('PE',.true.)
      end if

C  Draw the EZMAP labels ( and possibly the perimeter)

      if ((wlabs .eq. 2) .or. (wlabs .eq. -1) .or. (.not. perm))
     *  call maplbl

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine mpdrll (grds,flmap,wlabs,cenlon,xpa,xpb,project)

C*****************************************************************************C
C  mpdrll   - This is a MAPDRV routine                                        C
C  Section  - Lat/Lon Lines                                                   C
C  Purpose  - To set up a lat/lon line grid and draw it to the map.           C
C                                                                             C
C  On entry - The common block LLLDET contains the design information for the C
C             grid.  The common block LLLCOL contains the color index for the C
C             grid.   GRDS  conatins  the  grid distance  on the domain grid. C
C             FLMAP  tells us if the map was filled or not.   XPA  is  the  x C
C             coord of the first grid point in the map.   XPB  is the x coord C
C             of the last grid point  in  the map.   WLABS  tells what labels C
C             have been requested.  PROJECT contains the projection used.     C
C                                                                             C
C  On exit  - The grid has been drawn to the GFLASH buffer.                   C
C                                                                             C
C  Assume   - GKS is open. A color table has been set up.  The map itself has C
C             been drawn to the GFLASH buffer.                                C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             ARINAM              AREAS utility*                              C
C             MAPSTC              EZMAP utility*                              C
C             MAPBLA              EZMAPA utility*                             C
C             ARPRAM              AREAS utility*                              C
C             MAPSTI              EZMAP utility*                              C
C             MAPGRM              EZMAPA utility*                             C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C             This routine also  determines  the  locations  for  the  MAPDRV C
C             labels.   This  determination  occurs in the  external  routine C
C             LLPROC  which  is called by the  EZMAPA  routine  MAPGRM.  This C
C             routine saves some  necessary  information and then, even if no C
C             no lat/lon  lines are  requested  may have to call   MAPGRM  if C
C             MAPDRV labels have been requested.                              C
C                                                                             C
C  Author   - Jeremy Asbill        Date - July 7, 1990       for the MM4 club C
C*****************************************************************************C

C  Character variables

      character*2      project           ! projection used in map          (in)
      character*2      wouts             ! for common block MOCDET

C  Integer variables

      integer          wlabs             ! which labels do we want         (in)
      integer          llplc,            ! for common block LLLDET
     *                 grdsh,            ! for common block LLLDET
     *                 llint             ! for common block LLLDET
      integer          amapf(255000)     ! for common block ARAMAP
      integer          lfnum,            ! for common block LABNUM
     *                 rgnum,            ! for common block LABNUM
     *                 btnum,            ! for common block LABNUM
     *                 tpnum             ! for common block LABNUM
      integer          aid(10),          ! area identifiers for areas   (local)
     *                 gid(10),          ! group identifiers for areas  (local)
     *                 numdeg,           ! # of degs. lat/lon in domain (local)
     *                 numgrd,           ! grid interval to use         (local)
     *                 ngrd,             ! # of grid pts in map in x dir(local)
     *                 lltm              ! junk filler                  (local)

C  Logical variables

      logical          flmap             ! did we fill the map ?           (in)

C  Real variables

      real             grds,             ! grid distance in kilometers     (in)
     *                 xpa,              ! x coord of 1st grid in map      (in)
     *                 xpb,              ! x coord of last grid in map     (in)
     *                 cenlon            ! center longitude                (in)
      real             left,             ! for common block MAPEDGE
     *                 right,            ! for common block MAPEDGE
     *                 bottom,           ! for common block MAPEDGE
     *                 top               ! for common block MAPEDGE
      real             xcs(1000),        ! x coords for polys in areas  (local)
     *                 ycs(1000),        ! same as XCS for y coords     (local)
     *                 ultm,             ! junk filler                  (local)
     *                 urtm,             ! junk filler                  (local)
     *                 ubtm,             ! junk filler                  (local)
     *                 uttm              ! junk filler                  (local)

C  Common blocks

      common /llldet/  llplc,            ! where do we draw lat/lon lines
     *                 grdsh,            ! lat/lon grid dash pattern
     *                 llint             ! lat/lon grid interval in degrees
      common /mocdet/  wouts             ! geographical outline indicator
      common /aramap/  amapf             ! area map for lat/lon lines
      common /mapedge/ left,             ! fractional coord. of left edge
     *                 right,            ! fractional coord. of right edge
     *                 bottom,           ! fractional coord. of bottom edge
     *                 top               ! fractional coord. of top edge
      common /labnum/  lfnum,            ! # of label positions on left
     *                 rgnum,            ! # of label positions on right
     *                 btnum,            ! # of label positions on bottom
     *                 tpnum             ! # of label positions on top

C  External routines

      external         llproc            ! does actual drawing of lat/lon lines

C****************************  subroutine begin  *****************************C

C  EZMAP internal parrameters used in this routine are:
C  GR  - lat/lon GRid interval
C  DA  - lat/lon grid DAsh pattern
C  C2  - lat/lon grid Color index
C  OU  - geographical OUtline indicator
C  VS  - Vertical Slicing

C  Set vertical slicing to 1 slice so that dash patterns will be okay

      if (llplc .ne. 0)
     *  call mapsti ('VS',1)
  
C  See if grid needs to be masked
C  We donn't need the areamap if
C    a - no lat/lon lines requested and
C    b - no labels are requested

      if ((llplc .ne. 0) .or. (wlabs .eq. 1) .or.
     *    (wlabs .eq. 2)) then

C  Use the least time consuming outlines for the mask

        if ((wouts(1:2) .eq. 'PS') .or. (wouts(1:2) .eq. 'PO') .or.
     *      (wouts(1:2) .eq. 'NO'))
     *    call mapstc ('OU','CO')

C  Initialize area map

        call arinam (amapf,175000)

C  Also add the edges with which to mask

        call mapbla (amapf)

C  Preprocess the area map

        call arpram (amapf,0,0,0)

C  Get the fractional coordinates of the edges of the map

        call getset (left,right,bottom,top,
     *               ultm,urtm,ubtm,uttm,lltm)

C  Initialize the label counters

        lfnum = 0
        rgnum = 0
        btnum = 0
        tpnum = 0

C  If LLINT = 0 then choose a nice grid interval

        if (llint .eq. 0) then
          ngrd   = nint(xpb - xpa) + 1
          numdeg = int(grds * float(ngrd)/111.0)
          if (numdeg .lt. 10) then
            numgrd = 2
          else if (numdeg .lt. 20) then
            numgrd = 5
          else if (numdeg .lt. 90) then
            numgrd = 10
          else
            numgrd = 15
          end if
        else

C  If LLINT isn't 0 then use it as the grid interval

          numgrd = llint
        end if

C  Set up grid interval in EZMAP

        call mapsti ('GR',numgrd)

C  Draw the latitude/longitude grid

        if ((grdsh .eq. 0) .and. (llplc .ne. 0)) then
          call prodll (numgrd,cenlon,project)
        end if
        call mapgrm (amapf,xcs,ycs,1000,aid,gid,10,llproc)
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine mpdrml (xpa,ypa,xpb,ypb,project,grds,cenlat,cenlon,
     *                   jend,iend,errsev)

C*****************************************************************************C
C  mpdrml   - This is a MAPDRV routine                                        C
C  Section  - Labels                                                          C
C  Purpose  - To design and draw MAPDRV labels.                               C
C                                                                             C
C  On entry - XPA,vYPA forms the lower left grid point of the map.   XPB, YPB C
C             forms the upper right grid point of the map.  PROJECT indicates C
C             what projection  is  being used.   GRDS is the distance between C
C             grid points in km.   CENLAT  and CENLON are the center latitude C
C             and  longitude  respectively.   JEND, IEND form the upper right C
C             grid point  of  the domain.   ERRSEV indicates what severity of C
C             an error should halt execution.   Design information about  the C
C             labels particularly is passed in through common blocks.         C
C                                                                             C
C  On exit  - The labels have been drawn.                                     C
C                                                                             C
C  Assume   - GKS is open.  The map itself has been drawn.                    C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             MAPGTI              EZMAP utility*                              C
C             PCSETI              PLOTCHAR utility*                           C
C             XYTOLL              MAPDRV utility                              C
C             PLCHHQ              PLOTCHAR utility*                           C
C             GSTXCI              GKS                                         C
C             GSPLCI              GKS                                         C
C             ERRHAN              MAPDRV/CONDRV utility                       C
C             GETSET              SPPS*                                       C
C             SET                 SPPS*                                       C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C             This  routine  also draws  in  the  publication style perimeter C
C             ticks when they are requested.                                  C
C                                                                             C
C  Author   - Jeremy Asbill       Date - July 19, 1990       for the MM4 club C
C*****************************************************************************C

C  Parameter

      parameter        (ito = 1)         ! redundancy tolerance
      parameter        (pi  = 3.14159)   ! pi! you know pi, irrational guy
      parameter        (re  = 6370.0)    ! radius of the earth

C  Character variables

      character*2      project           ! projection indicator            (in)
      character*60     ermes             ! error message string         (local)
      character*5      str               ! label string                 (local)

C  Integer variables

      integer          jend,             ! right grid point value, domain  (in)
     *                 iend,             ! top grid point value, domain    (in)
     *                 errsev            ! error severity comparitor       (in)
      integer          wlabs,            ! for common block MLBDET
     *                 lbqul(2)          ! for common block MLBDET
      integer          imax,             ! for common block XYLLON
     *                 jmax              ! for common block XYLLON
      integer          lacolr            ! for common block LABCOL
      integer          lfnum,            ! for common block LABNUM
     *                 rgnum,            ! for common block LABNUM
     *                 btnum,            ! for common block LABNUM
     *                 tpnum             ! for common block LABNUM
      integer          llplc,            ! for common block LLLDET
     *                 grdsh,            ! for common block LLLDET
     *                 llint             ! for common block LLLDET
      integer          llcolr            ! for common block LLLCOL
      integer          nmgr,             ! lat/lon grid interval        (local)
     *                 llval(360,2),     ! lat/lon values per side      (local)
     *                 tst1,             ! test variable                (local)
     *                 tst2,             ! test variable                (local)
     *                 latcnt,           ! latitude counter             (local)
     *                 loncnt,           ! longitude counter            (local)
     *                 i,j,              ! loop counter/place keeper    (local)
     *                 xy,yz,            ! dimension picker             (local)
     *                 slen,             ! label string length          (local)
     *                 loop,             ! loop maximum                 (local)
     *                 llsv              ! save variable                (local)

C  Real variables

      real             xpa,              ! left grid point value, map      (in)
     *                 ypa,              ! bottom grid point value, map    (in)
     *                 xpb,              ! right grid point value, map     (in)
     *                 ypb,              ! top grid point value, map       (in)
     *                 grds,             ! grid distance in km             (in)
     *                 cenlat,           ! center latitude, domain         (in)
     *                 cenlon            ! center longitude, domain        (in)
      real             dds,              ! for common block XYLLON
     *                 xlat,             ! for common block XYLLON
     *                 xlon              ! for common block XYLLON
      real             lbsiz             ! for common block MLBDET
      real             left,             ! for common block MAPEDGE
     *                 right,            ! for common block MAPEDGE
     *                 bottom,           ! for common block MAPEDGE
     *                 top               ! for common block MAPEDGE
      real             lfpos(360,2),     ! for common block LABPOS
     *                 rgpos(360,2),     ! for common block LABPOS
     *                 btpos(360,2),     ! for common block LABPOS
     *                 tppos(360,2)      ! for common block LABPOS
      real             xn,               ! cone factor for projection   (local)
     *                 grx,              ! grid value x coord           (local)
     *                 gry,              ! grid value y coord           (local)
     *                 llx,              ! real latitude                (local)
     *                 lly,              ! real longitude               (local)
     *                 flsv,             ! save variable                (local)
     *                 frsv,             ! save variable                (local)
     *                 fbsv,             ! save variable                (local)
     *                 ftsv,             ! save variable                (local)
     *                 ulsv,             ! save variable                (local)
     *                 ursv,             ! save variable                (local)
     *                 ubsv,             ! save variable                (local)
     *                 utsv,             ! save variable                (local)
     *                 ang,              ! angle at which to draw tick  (local)
     *                 dcgtb,            ! dst from grd center to edge  (local)
     *                 dcltp,            ! dst from center to pole      (local)
     *                 tang              ! test angle                   (local)

C  Common blocks

      common /mlbdet/  wlabs,            ! which labels do we want
     *                 lbsiz,            ! alternate label size
     *                 lbqul             ! label quality
      common /xyllon/  dds,              ! grid distance in kilometers
     *                 xlat,             ! center latitude
     *                 xlon,             ! center longitude
     *                 imax,             ! maximum vertical grid point
     *                 jmax              ! maximum horizontal grid point
      common /labcol/  lacolr            ! label color
      common /labpos/  lfpos,            ! positions of labels along the left
     *                 rgpos,            ! positions of labels along the right
     *                 btpos,            ! positions of labels along the bottom
     *                 tppos             ! positions of labels along the top
      common /labnum/  lfnum,            ! # of positions in LFPOS
     *                 rgnum,            ! # of positions in RGPOS
     *                 btnum,            ! # of positions in BTPOS
     *                 tpnum             ! # of positions in TPPOS
      common /mapedge/ left,             ! fractional coord. of left edge
     *                 right,            ! fractional coord. of right edge
     *                 bottom,           ! fractional coord. of bottom edge
     *                 top               ! fractional coord. of top edge
      common /llldet/  llplc,            ! lat/lon line flag
     *                 grdsh,            ! lat/lon grid dash pattern
     *                 llint             ! not used
      common /lllcol/  llcolr            ! color of perimeter ticks

C****************************  subroutine begin  *****************************C

C  EZMAP internal parameters use in this routine are:
C  GR  - lat/lon GRid interval

C  PLOTCHAR internal parameters use in this routine are:
C  CD  - Complex or Duplex characters
C  QU  - QUality of charcters

C  Don't draw anything unless MAPDRV labels were requested or publication
C  style grid was used

      if (((wlabs .eq. 1) .or. (wlabs .eq. 2)) .or.
     *    ((grdsh .eq. 0) .and. (llplc .ne. 0))) then

C  Check right away for an error in locating them

        if (lfnum .eq. 361) then
          ermes(1:30)  = 'Too Many MAPDRV Style Labels -'
          ermes(31:60) = ' 360 Per Side Maximum         '
          call errhan ('MAPDRV',0,ermes,errsev)
          lfnum = 360
        end if

        if (rgnum .eq. 361) then
          ermes(1:30)  = 'Too Many MAPDRV Style Labels -'
          ermes(31:60) = ' 360 Per Side Maximum         '
          call errhan ('MAPDRV',0,ermes,errsev)
          rgnum = 360
        end if

        if (btnum .eq. 361) then
          ermes(1:30)  = 'Too Many MAPDRV Style Labels -'
          ermes(31:60) = ' 360 Per Side Maximum         '
          call errhan ('MAPDRV',0,ermes,errsev)
          btnum = 360
        end if

        if (tpnum .eq. 361) then
          ermes(1:30)  = 'Too Many MAPDRV Style Labels -'
          ermes(31:60) = ' 360 Per Side Maximum         '
          call errhan ('MAPDRV',0,ermes,errsev)
          tpnum = 360
        end if

C  Set up routine XYTOLL for use

        dds  = grds
        xlat = cenlat
        xlon = cenlon
        imax = iend
        jmax = jend

C  Determine cone factor

        if (project(1:2) .eq. 'LC') then
          xn = 0.716
        else if (project .eq. 'ST') then
          xn = 1.0
        else
          xn = 0.0
        end if

C  Set quality as the user requested in PLOTCHAR

        call pcseti ('CD',lbqul(1))
        call pcseti ('QU',lbqul(2))

C  Change user vieport to look like the grid so XYTOLL will be useful

        call getset (flsv,frsv,fbsv,ftsv,ulsv,ursv,ubsv,utsv,llsv)
        j = 0
60      j = j + 1
        call set    (left,right,bottom,top,
     *               xpa,xpb,ypa,ypb,1)
        latcnt = 0
        loncnt = 0

C  Retrieve the GRid interval

        call mapgti ('GR',nmgr)

C  First we will do the left, then the right, then the top,
C  and last the bottom

        if (j .eq. 1) then
          loop = lfnum
        else if (j .eq. 2) then
          loop = rgnum
        else if (j .eq. 3) then
          loop = tpnum
        else
          loop = btnum
        end if

C  First convert the positions to lat/lon values

        do 10 i = 1,loop

C  CFUX and CFUY are SPPS functions that convert from fractional coordinates
C  to user coordinates.  This gets the grid values of each point

          if (j .eq. 1) then
            grx = cfux(lfpos(i,1))
            gry = cfuy(lfpos(i,2))
          else if (j .eq. 2) then
            grx = cfux(rgpos(i,1))
            gry = cfuy(rgpos(i,2))
          else if (j .eq. 3) then
            grx = cfux(tppos(i,1))
            gry = cfuy(tppos(i,2))
          else
            grx = cfux(btpos(i,1))
            gry = cfuy(btpos(i,2))
          end if

C  Convert the grid values to lat/lon values

          call xytoll (grx,gry,llx,lly,project)
          llval(i,1) = nint(llx)
          llval(i,2) = nint(lly)

C  Check to see which (the lat or lon) value cause the line to be drawn

          tst1 = mod (llval(i,1),nmgr)
          tst2 = mod (llval(i,2),nmgr)
          if (tst1 .eq. 0) then
            if ((i .ne. 1) .and. (llval(i,1) .ne. llval(1,1))) then
              if ((llval(i,1) .ne. llval(i-1,1)) .or.
     *            (llval(i,2) .ne. llval(i-1,2)))
     *          latcnt = latcnt + 1
            else if (i .eq. 1) then
              latcnt = latcnt + 1
            end if
          end if
          if (tst2 .eq. 0) then
            if ((i .ne. 1) .and. (llval(i,2) .ne. llval(1,2))) then
              if (abs(llval(i,2)) .ne. 180) then
                if ((llval(i,1) .ne. llval(i-1,1)) .or.
     *              (llval(i,2) .ne. llval(i-1,2)))
     *            loncnt = loncnt + 1
              else
                if ((llval(i,1) .ne. llval(i-1,1)) .or.
     *              ((llval(i,2) .ne. llval(i-1,2)) .and.
     *               (llval(i,2) .ne. -llval(i-1,2))))
     *            loncnt = loncnt + 1
              end if
            else if (i .eq. 1) then
              loncnt = loncnt + 1
            end if
          end if
10      continue

C  Label the edge with the one which cause the most lines to be drawn

        if ((j .eq. 1) .or. (j .eq. 2)) then
          if (latcnt .ge. loncnt) then
            xy = 1
          else
            xy = 2
          end if
        else
          if (latcnt .gt. loncnt) then
            xy = 1
          else
            xy = 2
          end if
        end if

C  Make a pass through LLVAL checking for duplicate and redundant labels

        do 80 k = 1,loop
          do 70 i = 1,loop
            if (xy .eq. 1) then
              yz = 2
            else
              yz = 1
            end if
            if ((llval(k,xy) .eq. llval(i,xy)) .and. 
     *          (llval(k,yz) .le. (llval(i,yz) + ito)) .and.
     *          (llval(k,yz) .ge. (llval(i,yz) - ito)) .and.
     *          (k .ne. i) .and. (llval(k,xy) .ne. 400)) then
              llval(i,1) = 400
              llval(i,2) = 400
            end if
70        continue
80      continue

C  Set up the viewport to draw

        call set (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)

C  Make the string to be the label

        do 20 i = 1,loop
          if (llval(i,xy) .eq. 0) then                    ! Greenwich Meridian
            if (xy .eq. 1) then                           ! or Equator
              str(1:5) = 'EQ   '
            else
              str(1:5) = 'GM   '
            end if
            slen     = 2
          else if ((llval(i,xy) .eq. -180) .or.           ! International
     *             (llval(i,xy) .eq. 180)) then           ! Date Line
            str(1:5) = 'ID   '
            slen     = 2
          else if ((mod(llval(i,xy),nmgr) .eq. 0) .and.
     *             (llval(i,xy) .ne. 400)) then
            if (llval(i,xy) .ge. 100) then                ! [100 E,180)
              write (str(1:3),30) llval(i,xy)
              if (xy .eq. 1) then
                str(4:5) = ' N'
              else
                str(4:5) = ' E'
              end if
              slen     = 5
            else if (llval(i,xy) .ge. 10) then            ! [10 E,100 E)
              write (str(1:2),40) llval(i,xy)             ! [10 N,90 N]
              if (xy .eq. 1) then
                str(3:5) = ' N '
              else
                str(3:5) = ' E '
              end if
              slen     = 4
            else if (llval(i,xy) .gt. 0) then             ! (0,10 E)
              write (str(1:1),50) llval(i,xy)             ! (0,10 N)
              if (xy .eq. 1) then
                str(2:5) = ' N  '
              else
                str(2:5) = ' E  '
              end if
              slen     = 3
            else if (llval(i,xy) .gt. -10) then           ! (10 W,0)
              write (str(1:1),50) -1 * llval(i,xy)        ! (10 S,0)
              if (xy .eq. 1) then
                str(2:5) = ' S  '
              else
                str(2:5) = ' W  '
              end if
              slen     = 3
            else if (llval(i,xy) .gt. -100) then          ! (100 W,10 W]
              write (str(1:2),40) -1 * llval(i,xy)        ! [90 S,10 S]
              if (xy .eq. 1) then
                str(3:5) = ' S '
              else
                str(3:5) = ' W '
              end if
              slen     = 4
            else                                          ! (180,100W]
              write (str(1:3),30) -1 * llval(i,xy)
              if (xy .eq. 1) then
                str(4:5) = ' S'
              else
                str(4:5) = ' W'
              end if
              slen     = 5
            end if
          end if

C  Draw the label in

          if ((mod(llval(i,xy),nmgr) .eq. 0) .and.
     *        (llval(i,xy) .ne. 400)) then

C  Set up correct color for MAPDRV labels
C  To understand what the quality of the letters has to do with the color
C  read on page 2-14 in the NCAR Graphics Guide to New Utilities Version 3.00
C  under the heading of PLOTCHAR

            if ((lbqul(2) .eq. 0) .or. (lbqul(2) .eq. 1)) then
              call gsplci (lacolr)
            else
              call gstxci (lacolr)
            end if

C  Use PLOTCHAR to put the label up

            if (j .eq. 1) then
              call plchhq (lfpos(i,1)-lbsiz/85.0,lfpos(i,2),
     *                     str(1:slen),-lbsiz,0.0,1.0)
            else if (j .eq. 2) then
              call plchhq (rgpos(i,1)+lbsiz/85.0,rgpos(i,2),
     *                     str(1:slen),-lbsiz,0.0,-1.0)
            else if (j .eq. 3) then
              call plchhq (tppos(i,1),tppos(i,2)+lbsiz/55.0,
     *                     str(1:slen),-lbsiz,0.0,0.0)
            else
              call plchhq (btpos(i,1),btpos(i,2)-lbsiz/45.0,
     *                     str(1:slen),-lbsiz,0.0,0.0)
            end if

C  If publication style lat/lon grid was drawn, put in the tick marks

            if ((grdsh .eq. 0) .and. (llplc .ne. 0)) then

C  Set up correct color for publication style perimeter ticks

              if ((lbqul(2) .eq. 0) .or. (lbqul(2) .eq. 1)) then
                call gsplci (llcolr)
              else
                call gstxci (llcolr)
              end if

C  Calculate the angle at which to draw the tick

              ang = (float(llval(i,2)) - cenlon) * xn
              if ((j .eq. 1) .and. (xy .eq. 2))
     *          ang = ang + 90
              if ((j .eq. 2) .and. (xy .eq. 2))
     *          ang = ang - 90
              if ((j .eq. 3) .and. (xy .eq. 1) .and.
     *            (project(1:2) .eq. 'LC') .and.
     *            (llval(i,2) .lt. 0)) ang = ang + 90
              if ((j .eq. 3) .and. (xy .eq. 1) .and.
     *            (project(1:2) .eq. 'LC') .and.
     *            (llval(i,2) .gt. 0)) ang = ang - 90
              if ((j .eq. 3) .and. (xy .eq. 2) .and.
     *            (project(1:2) .eq. 'ST')) then
                if (cenlat .ge. 0.0) then
                  tang = 90.0 - cenlat
                else
                  tang = 90.0 + cenlat
                end if
                dcltp = 2 * pi * re * (tang/360.0)
                dcgtb = grds * iend * 0.5
                if (dcgtb .gt. dcltp) ang = ang + 180
              end if
              if ((j .eq. 3) .and. (xy .eq. 1) .and.
     *            (project(1:2) .eq. 'ST')) then
                if (tppos(i,1) .gt. 0.5) then
                  ang = ang - 90
                else if (tppos(i,1) .lt. 0.5) then
                  ang = ang + 90
                end if
              end if

C  Draw the tick as if it were a character.  Using the PLOTCHAR utility
C  allows easy specification of the angle at which to draw the tick.
C  For ticks extending from the right hand side, use a minus sign centered
C  on its right.  ...Extending from the left, use a minus sign centered on
C  its left.  ...Extending from the top, write down the screen instead of
C  across it and use a vertical bar centered on its top.  ...Extending from
C  the bottom, write down the screen instead of across it and use a vertical
C  bar centered on its bottom.

              if (j .eq. 1) then
                call plchhq (lfpos(i,1),lfpos(i,2),
     *                       '-',-lbsiz,ang,-1.0)
              else if (j .eq. 2) then
                call plchhq (rgpos(i,1),rgpos(i,2),
     *                       '-',-lbsiz,ang,1.0)
              else if (j .eq. 3) then
                call plchhq (tppos(i,1),tppos(i,2),
     *                       ':D:|',-0.667*lbsiz,ang,-1.0)
              else
                call plchhq (btpos(i,1),btpos(i,2),
     *                       ':D:|',-0.667*lbsiz,ang,1.0)
              end if
            end if
          end if
20      continue

C  If there are more sides to do, do them

        if (j .ne. 4) goto 60

C  Restore any disrupted set call

        call set (flsv,frsv,fbsv,ftsv,ulsv,ursv,ubsv,utsv,llsv)
      end if

C*****************************  subroutine end  ******************************C

C  Format statements begin ...

30    format (I3)
40    format (I2)
50    format (I1)

C  Format statements end.

      return
      end
      subroutine mpdrol

C*****************************************************************************C
C  mpdrol   - This is a MAPDRV routine                                        C
C  Section  - Draw                                                            C
C  Purpose  - To set the correct outline style and draw the outlines.         C
C                                                                             C
C  On entry - The common block  MOTDET  contains dotted line,  and line width C
C             information.                                                    C
C                                                                             C
C  On exit  - The outlines have been drawn.                                   C
C                                                                             C
C  Assume   - GKS is open.                                                    C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             MAPSTC              EZMAP utility*                              C
C             MAPSTI              EZMAP utility*                              C
C             MAPSTL              EZMAP utility*                              C
C             GETUSV              SPPS*                                       C
C             SETUSV              SPPS*                                       C
C             MAPLOT              EZMAP utility*                              C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill        Date - July 7, 1990       for the MM4 club C
C*****************************************************************************C

C  Integer variables

      integer          maplw,            ! for common block MOTDET
     *                 dtdsp             ! for common block MOTDET
      integer          lwsv              ! integer save variable

C  Logical variables

      logical          dtdmp             ! for common block MOTDET

C  Common blocks

      common /motdet/  dtdmp,            ! T => draw the map with dots ?
     *                 maplw,            ! line width for map outlines
     *                 dtdsp             ! dash spacing for map outlines

cdave
      common /hires/   ihires            ! ihires=1 do a hi res, else NCARG

C****************************  subroutine begin  *****************************C

C  EZMAP internal parameters use in this routine are:
C  DO  - DOtted outline flag
C  DD  - Distance between Dots

C  SPPS internal parameters used in this routine are
C  LW  - Line Width

C  Set up map outline form (dotted or solid)

      call mapstl ('DO',dtdmp)

C  Set up the proper line width for or dot spacing

      if (dtdmp) then
        call mapsti ('DD',dtdsp)
      else
        call getusv ('LW',lwsv)
        call setusv ('LW',maplw)
      end if

C  Draw the outlines
      
cdave
      if(ihires.eq.1) then
         call hiresmap(3)
      else
         call maplot
      endif

C  Reset the line width if need be

      if ((.not. dtdmp) .and. (lwsv .ne. maplw)) then
        call setusv ('LW',lwsv)
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine mpdrtl (titline,titlen,nomap)

C*****************************************************************************C
C  mpdrtl   - This is a MAPDRV routine                                        C
C  Section  - Labels                                                          C
C  Purpose  - To put the title of the map on the picture.                     C
C                                                                             C
C  On entry - TITLINE  contains the title string.   TITLEN  tells us how long C
C             the string is.   TLSIZ  in the common block MTLDET tells us how C
C             big to make the characters.  TLCOLR  in the common block TITCOL C
C             contains the color index for the title.  If NOMAP is true,  the C
C             routine delivers an error message to the plotting screen.       C
C                                                                             C
C  On exit  - The title has been drawn into the GFLASH buffer.                C
C                                                                             C
C  Assume   - GKS is open.                                                    C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             GETSET              SPPS*                                       C
C             SET                 SPPS*                                       C
C             PCSETI              PLOTCHAR utility*                           C
C             PLCHHQ              PLOTCHAR utility*                           C
C             GSTXCI              GKS                                         C
C             GSPLCI              GKS                                         C
C             ERRHAN              MAPDRV/CONDRV utility                       C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill        Date - July 8, 1990       for the MM4 club C
C*****************************************************************************C

C  Character variables

      character*80     titline           ! string containing the title     (in)

C  Integer variables

      integer          titlen            ! # of characters in title string (in)
      integer          tlqul(2)          ! for common block MTLDET
      integer          tlcolr            ! for common block TITCOL
      integer          llsv              ! viewport save variable       (local)

C  Logical variables

      logical          nomap             ! we didn't draw a map ?          (in)

C  Real variables

      real             tlsiz             ! for common block MTLDET
      real             flsv,             ! viewport save variable       (local)
     *                 frsv,             ! viewport save variable       (local)
     *                 fbsv,             ! viewport save variable       (local)
     *                 ftsv,             ! viewport save variable       (local)
     *                 ulsv,             ! viewport save variable       (local)
     *                 ursv,             ! viewport save variable       (local)
     *                 ubsv,             ! viewport save variable       (local)
     *                 utsv              ! viewport save variable       (local)

C  Common blocks

      common /mtldet/  tlsiz,            ! alternate title size
     *                 tlqul             ! title quality
      common /titcol/  tlcolr            ! title color

C****************************  subroutine begin  *****************************C

C  PLOTCHAR internal parameters used in this routine are:
C  CD  - Complex Duplex flag
C  QU  - QUality forcing flag

C  Make certian there is supposed to be a title plotted

      if ((nint(tlsiz) .ne. 0) .or. (nomap)) then

C  Get and save the current viewport

        call getset (flsv,frsv,fbsv,ftsv,ulsv,ursv,ubsv,utsv,llsv)

C  Set up viewport to be normalized

        call set (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
      end if
 
C  Set up the type of characters to use

      if ((nint(tlsiz) .ne. 0) .and. (.not. nomap)) then
        call pcseti ('CD',tlqul(1))

C  Force PLCHHQ to use the desired quality

        call pcseti ('QU',tlqul(2))

C  Set up the color properly
C  To understand how color and quality are related read on page 2-14 in the
C  NCAR Graphics Guide to New utilities Version 3.00 under the heading of
C  PLOTCHAR

        if ((tlqul(2) .eq. 0) .or. (tlqul(2) .eq. 1)) then
          call gsplci (tlcolr)
        else
          call gstxci (tlcolr)
        end if
      end if

C  If there was no map drawn put a title in the GFLASH buffer reflecting this

      if (nomap) then

C  Mske the title

        titline(1:43)  = 'No Map Drawn Due To A Non-Correctable Error'
        titlen = 43
        tlsiz = 1.0
      end if

C  Put on title

      if ((nint(tlsiz) .ne. 0) .or. (nomap)) then
        call plchhq (0.5,0.035,titline(1:titlen),-tlsiz,0.0,0.0)

C  Restore original viewport

        call set (flsv,frsv,fbsv,ftsv,ulsv,ursv,ubsv,utsv,llsv)
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine mpdrus

C*****************************************************************************C
C  mpdrus   - This is a MAPDRV routine                                        C
C  Section  - Design                                                          C
C  Purpose  - The default version of this routine does nothing.  It is called C
C             just before any  drawing takes place so the user can reset some C
C             things this way.                                                C
C                                                                             C
C  On entry - Nothing is input.                                               C
C                                                                             C
C  On exit  - Nothing is done.                                                C
C                                                                             C
C  Assume   - Nothing.                                                        C
C                                                                             C
C  Notes    - This is suppose to be a really well kept secret.                C
C                                                                             C
C  Author   - Jeremy Asbill       Date - July 11, 1990       for the MM4 club C
C*****************************************************************************C

C****************************  subroutine begin  *****************************C

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine mrdclt (unum,llplc,wlabs,title,errsev,nomap)

C*****************************************************************************C
C  mrdclt   - This is a MAPDRV routine                                        C
C  Section  - Tables                                                          C
C  Purpose  - To read in the map color table and assign  the map color  indi- C
C             cators appropriately.                                           C
C                                                                             C
C  On entry - UNUM  is the unit number  where to look for the  table.   LLPLC C
C             is the lat/lon line details flag.   WLABS  is the label details C
C             flag.   TITLE  is true if a title will be drawn and false if no C
C             title will be drawn.   ERRSEV  indicates the severity  of error C
C             that will cause execution to halt.   NOMAP  is  true  if a non- C
C             correctible error has occured  and no map is to be drawn and is C
C             false otherwise.                                                C
C                                                                             C
C  On exit  - The color indication variables in common blocks LLLCOL, LABCOL, C
C             OUTCOL, and PERCOL have been assigned correctly.  NOMAP is true C
C             if a  non-correctible  error has  occured  and no map  is to be C
C             drawn and is false otherwise.                                   C
C                                                                             C
C  Assume   - Nothing.                                                        C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             TBLLOK              MAPDRV/CONDRV utility                       C
C             CRDRCI              MAPDRV/CONDRV utility                       C
C             SEARCH              MAPDRV/CONDRV utility                       C
C             NEXT                MAPDRV/CONDRV utility                       C
C             ERRHAN              MAPDRV/CONDRV utility                       C
C             ----------------------------------------------------------------C
C             *NCAR Graphics Routine                                          C
C                                                                             C
C  Author   - Jeremy Asbill        Date - May 26, 1990       for the MM4 club C
C*****************************************************************************C

C  Character variables

      character*80     whline            ! line from map color table    (local)
      character*60     p,                ! SEARCH error message string  (local)
     *                 q,                ! NEXT error message string    (local)
     *                 ermes             ! general error message string (local)
      character*2      wouts             ! for common block MOCDET

C  Integer variables

      integer          llplc,            ! where do we want lat/lon lines  (in)
     *                 wlabs,            ! which labels do we want         (in)
     *                 unum,             ! unit number of table file       (in)
     *                 errsev            ! error severity indicator        (in)
      integer          llcolr            ! for common block LLLCOL
      integer          lacolr            ! for common block LABCOL
      integer          tlcolr            ! for common block TITCOL
      integer          cocolr,           ! for common block OUTCOL
     *                 uscolr,           ! for common block OUTCOL
     *                 cncolr            ! for common block OUTCOL
      integer          pecolr            ! for common block PERCOL
      integer          i                 ! loop counter/place keeper    (local)

C  Logical variables

      logical          title,            ! put a title on the map ?        (in)
     *                 nomap             ! don't draw a map ?              (in)
      logical          error,            ! has an error occured ?       (local)
     *                 found,            ! was the table found ?        (local)
     *                 test              ! is this true ?               (local)

C  Common blocks

      common /lllcol/  llcolr            ! color of lat lon lines
      common /labcol/  lacolr            ! color of labels
      common /titcol/  tlcolr            ! color of title
      common /outcol/  cocolr,           ! color of continents
     *                 uscolr,           ! color of states
     *                 cncolr            ! color of countries
      common /percol/  pecolr            ! color of perimeter
      common /mocdet/  wouts             ! desired outline indicator

C****************************  subroutine begin  *****************************C

C  If no map is to be drawn skip this routine

      if (nomap) goto 150

C  Initialize error flag

      error = .false.

C  Look for the table

      call tbllok (unum,'MAP COLORS',errsev,found,whline,'MAPDRV')

C  Parse table only if it was found

      if (found) then

C  Initialize the place keeper

        i = 1

C  Set up the error messages for SEARCH and NEXT errors

        p(1:23)  = 'Reading Colors Table,  '
        p(24:60) = 'Too Few Entries On Line              '
        q(1:23)  = p(1:23)
        q(24:60) = 'Entry Is Bizarre                     '

C  First in line is the lat/lon line color index, LLCOLR

        if (llplc .ne. 0) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('MAPDRV',1,p,errsev)
        end if

        if (llplc .ne. 0) then
          test = .true.
        else
          test = .false.
        end if
        call crdrci (test,error,llcolr,1,whline,i,
     *               'Lat/Lon Grid Color Index',24,errsev,
     *               nomap,'MAPDRV')
        if (nomap) goto 150

        if ((llplc .ne. 0) .and. (.not. error)) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('MAPDRV',1,q,errsev)
        end if

C  Second in line is the labels color index, LACOLR
      
        if ((wlabs .ne. 0) .and. (.not. error)) then  
          call search (whline,i,error)
          if (error)
     *      call errhan ('MAPDRV',1,p,errsev)
        end if

        if (wlabs .ne. 0) then
          test = .true.
        else
          test = .false.
        end if
        call crdrci (test,error,lacolr,1,whline,i,
     *               'Label Color Index',17,errsev,
     *               nomap,'MAPDRV')
        if (nomap) goto 150

        if ((wlabs .ne. 0) .and. (.not. error)) then  
          call next (whline,i,error)
          if (error)
     *      call errhan ('MAPDRV',1,q,errsev)
        end if

C  Third in line is the title color index, TLCOLR

        if ((title) .and. (.not. error)) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('MAPDRV',1,p,errsev)
        end if

        call crdrci (title,error,tlcolr,1,whline,i,
     *               'Title Color Index ',17,errsev,
     *               nomap,'MAPDRV')
        if (nomap) goto 150

        if ((title) .and. (.not. error)) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('MAPDRV',1,p,errsev)
        end if

C  Third in line is the U.S. States outline color index, USCOLR

        if (((wouts(1:2) .eq. 'PS') .or. (wouts(1:2) .eq. 'US')) .and.
     *      (.not. error)) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('MAPDRV',1,p,errsev)
        end if

        if ((wouts(1:2) .eq. 'PS') .or. (wouts(1:2) .eq. 'US')) then
          test = .true.
        else
          test = .false.
        end if
        call crdrci (test,error,uscolr,1,whline,i,
     *               'U.S. State Outline Color Index',30,errsev,
     *               nomap,'MAPDRV')
        if (nomap) goto 150

        if (((wouts(1:2) .eq. 'PS') .or. (wouts(1:2) .eq. 'US')) .and.
     *      (.not. error)) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('MAPDRV',1,q,errsev)
        end if

C  Fourth in line is the countries outline color index, CNCOLR

        if ((wouts(1:1) .eq. 'P') .and. (.not. error)) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('MAPDRV',1,p,errsev)
        end if

        if (wouts(1:1) .eq. 'P') then
          test = .true.
        else
          test = .false.
        end if
        call crdrci (test,error,cncolr,1,whline,i,
     *               'Political Outline Color Index',29,errsev,
     *               nomap,'MAPDRV')
        if (nomap) goto 150

        if ((wouts(1:1) .eq. 'P') .and. (.not. error)) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('MAPDRV',1,q,errsev)
        end if

C  Fifth in line is Continental outline color index, COCOLR

        if (((wouts(1:1) .eq. 'P') .or. (wouts(1:2) .eq. 'CO')) .and.
     *      (.not. error)) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('MAPDRV',1,p,errsev)
        end if

        if  ((wouts(1:1) .eq. 'P') .or. (wouts(1:2) .eq. 'CO')) then
          test = .true.
        else
          test = .false.
        end if
        call crdrci (test,error,cocolr,1,whline,i,
     *               'Continental Outline Color Index',31,errsev,
     *               nomap,'MAPDRV')
        if (nomap) goto 150

        if (((wouts(1:1) .eq. 'P') .or. (wouts(1:2) .eq. 'CO')) .and.
     *      (.not. error)) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('MAPDRV',1,q,errsev)
        end if

C  Sixth in line is the perimeter color index, PECOLR

        if (.not. error) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('MAPDRV',1,p,errsev)
        end if

        call crdrci (.true.,error,pecolr,1,whline,i,
     *               'Perimeter Color Index',21,errsev,
     *               nomap,'MAPDRV')
        if (nomap) goto 150

        if (.not. error) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('MAPDRV',0,q,errsev)
        end if

C  Check to see if there were too many entries in the table

        if (.not. error) then
          call search (whline,i,error)
          if (.not. error) then
            ermes(1:24)  = 'Reading Colors Table, To'
            ermes(25:50) = 'o Many Entries On Line    '
            ermes(51:60) = '          '
            call errhan ('MAPDRV',0,ermes,errsev)
          end if
        end if

C  Inform the user that things are at least okay

        print *, 'MAPDRV - Map Colors Set Up'
      else

C  The table was not found, assign the defaults

        print *, 'MAPDRV - Default Map Colors Used'
        llcolr = 1
        tlcolr = 1
        lacolr = 1
        uscolr = 1
        cncolr = 1
        cocolr = 1
        pecolr = 1
        goto 150
      end if


C*****************************  subroutine end  ******************************C

C  Format statements begin ...

30    format (I2)
40    format (I1)

C  Format statements end.

150   return
      end
      subroutine mrddet (unum,buff,ollplc,owlabs,title,perm,errsev,
     *                   nomap)

C*****************************************************************************C
C  mrddet   - This is a MAPDRV routine                                        C
C  Section  - Tables                                                          C
C  Purpose  - This routine reads in  the map detail table and assigns the map C
C             detail indicators appropriately.                                C
C                                                                             C
C  On entry - UNUM  is the unit number of the table file.   ERRSEV  indicates C
C             the error severity at which execution should be halted.   NOMAP C
C             is true if a non-correctable error has occured and no map is to C
C             be made and is false otherwise.                                 C
C                                                                             C
C  On exit  - The information  variables in  common  blocks  MOCDET,  MOTDET, C
C             LLLDET,  MLBDET, and MTLDET have been set up correctly.  OLLPLC C
C             tells the driver where  to put lat/lon  lines, OWLABS tells the C
C             driver what labels to use and TITLE tells the driver whether or C
C             not a title is to be.  PERM is true if the perimeter with  tick C
C             marks is to be drawn  and false if  the perimeter without  tick C
C             marks should be drawn. NOMAP is true if a non-correctable error C
C             has  occured and no map is to be made and is  false  otherwise. C
C             If BUFF is negative special defaults are used.                  C
C                                                                             C
C  Assume   - Nothing.                                                        C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             TBLLOK              MAPDRV/CONDRV utility                       C
C             SEARCH              MAPDRV/CONDRV utility                       C
C             NEXT                MAPDRV/CONDRV utility                       C
C             ERRHAN              MAPDRV/CONDRV utility                       C
C             GTREAL              MAPDRV/CONDRV utility                       C
C             ----------------------------------------------------------------C
C                                                                             C
C  Author   - Jeremy Asbill       Date - May 27, 1990        for the MM4 club C
C*****************************************************************************C

C  Parameters

      parameter        (scale = 20.0)    ! scales LBSIZ and TLSIZ

C  Character variables

      character*2      wouts             ! for common block MOCDET
      character*80     whline            ! line of info. from table     (local)
      character*60     p,                ! error message for SEARCH     (local)
     *                 q,                ! error message for NEXT       (local)
     *                 ermes             ! general error message        (local)
      character*20     gstrng            ! temporary string             (local)

C  Integer variables

      integer          unum,             ! unit number tables are on       (in)
     *                 buff,             ! GFLASH buffer number            (in)
     *                 errsev            ! error severity indicator        (in)
      integer          ollplc,           ! out version of LLPLC           (out)
     *                 owlabs            ! out version of WLABS           (out)
      integer          llplc,            ! for common block LLLDET
     *                 grdsh,            ! for common block LLLDET
     *                 llint             ! for common block LLLDET
      integer          wlabs,            ! for common block MLBDET
     *                 lbqul(2)          ! for common block MLBDET
      integer          tlqul(2)          ! for common block MTLDET
      integer          maplw,            ! for common block MOTDET
     *                 dtdsp             ! for common block MOTDET
      integer          i,                ! loop counter/place keeper    (local)
     *                 temp              ! temporary variable           (local)

C  Logical variables

      logical          nomap             ! don't draw a map ?              (in)
      logical          title,            ! draw a title to the map ?      (out)
     *                 perm              ! draw a perimeter ?             (out)
      logical          dtdmp             ! for common block MOTDET
      logical          error,            ! has an error occurred ?      (local)
     *                 found             ! is the table there ?         (local)

C  Real variables

      real             lbsiz             ! for common block MLBDET
      real             tlsiz             ! for common block MTLDET
      real             dumy              ! dumy variable                (local)

C  Common blocks

      common /llldet/  llplc,            ! where do we draw lat/lon lines
     *                 grdsh,            ! lat/lon grid dash pattern
     *                 llint             ! lat/lon grid interval in degrees
      common /mlbdet/  wlabs,            ! which labels do we want
     *                 lbsiz,            ! alternate label size
     *                 lbqul             ! label quality
      common /mtldet/  tlsiz,            ! alternate title size
     *                 tlqul             ! title quality
      common /mocdet/  wouts             ! desired outline indicator
      common /motdet/  dtdmp,            ! T => draw the map with dots ?
     *                 maplw,            ! line width for map outlines
     *                 dtdsp             ! dash spacing for map outlines

C****************************  subroutine begin  *****************************C

C  Check if we need to do this

      if (nomap) goto 250

C  Initialize the error flag

      error = .false.

C  Look for the table

      call tbllok (unum,'MAP DETAIL',errsev,found,whline,'MAPDRV')

C  Don't do reading if it wasn't there

      if ((found) .and. (buff .ge. 0)) then

C  Initialize the place keeper

        i = 1

C  Set up the error messages for SEARCH and NEXT errors

        p(1:23)  = 'Reading Details Table, '
        p(24:60) = 'Too Few Entries On Line              '
        q(1:23)  = p(1:23)
        q(24:60) = 'Entry Is Bizarre                     '

C  Parse the line starting with lat/lon grid information
C  First is LLPLC which may be
C    L                => LLPLC = 1  ; or over land only
C    W                => LLPLC = -1 ; or over water only
C    N                => LLPLC = 0  ; or no lat/lon lines
C    D,A,E            => LLPLC = 2  ; or over both land and water

        call search (whline,i,error)
        if (error) then
          call errhan ('MAPDRV',1,p,errsev)
          llplc      = 2
          grdsh      = 21845
          llint      = 0
          wlabs      = 2
          lbsiz      = 0.4
          lbqul(1)   = 0
          lbqul(2)   = 0
          tlsiz      = 0.6
          tlqul(1)   = 0
          tlqul(2)   = 0
          wouts(1:2) = 'PS'
          dtdmp      = .false.
          maplw      = 1000
          dtdsp      = 0
          perm       = .true.
        end if

        if (.not. error) then
          if ((whline(i:i) .eq. 'L') .or. (whline(i:i) .eq. 'l')) then
            llplc = 1
          else if ((whline(i:i) .eq. 'W') .or.
     *             (whline(i:i) .eq. 'w')) then
            llplc = -1
          else if ((whline(i:i) .eq. 'N') .or.
     *             (whline(i:i) .eq. 'n')) then
            llplc = 0
          else if ((whline(i:i) .eq. 'D') .or.
     *             (whline(i:i) .eq. 'd') .or.
     *             (whline(i:i) .eq. 'E') .or.
     *             (whline(i:i) .eq. 'e') .or.
     *             (whline(i:i) .eq. 'A') .or.
     *             (whline(i:i) .eq. 'a')) then
            llplc = 2
          else
            ermes(1:19)  = 'Lat/Lon Grid Flag, '
            ermes(20:20) = whline(i:i)
            ermes(21:60) = ', Unknown, Default Used                 '
            call errhan ('MAPDRV',0,ermes,errsev)
            llplc = 2
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,q,errsev)
            if (llplc .ne. 0) then
              grdsh    = 21845
            else
              grdsh    = 0
            end if
            llint      = 0
            wlabs      = 2
            lbsiz      = 0.4
            lbqul(1)   = 0
            lbqul(2)   = 0
            tlsiz      = 0.6
            tlqul(1)   = 0
            tlqul(2)   = 0
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
            perm       = .true.
          end if
        end if

C  Second is the lat/lon grid dash pattern, or GRDSH which may be
C    L  => GRDSH = 255   ; or 0000000011111111 ; or Large
C    M  => GRDSH = 3855  ; or 0000111100001111 ; or Medium
C    SM => GRDSH = 13107 ; or 0011001100110011 ; or SMall
C    T  => GRDSH = 21845 ; or 0101010101010101 ; or Tiny
C    SO => GRDSH = -1    ; or 1111111111111111 ; or SOlid
C    P  => GRDSH = 0     ; or publication style
C    D  => GRDSH = 21845 ; or 0101010101010101 ; or Default

        if ((.not. error) .and. (llplc .ne. 0)) then
          call search (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,p,errsev)
            if (llplc .ne. 0) then
              grdsh    = 21845
            else
              grdsh    = 0
            end if
            llint      = 0
            wlabs      = 2
            lbsiz      = 0.4
            lbqul(1)   = 0
            lbqul(2)   = 0
            tlsiz      = 0.6
            tlqul(1)   = 0
            tlqul(2)   = 0
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
            perm       = .true.
          end if
        end if

        if ((.not. error) .and. (llplc .ne. 0)) then
          if ((whline(i:i) .eq. 'L') .or. (whline(i:i) .eq. 'l')) then
            grdsh = 255
          else if ((whline(i:i) .eq. 'M') .or.
     *             (whline(i:i) .eq. 'm')) then
            grdsh = 3855
          else if ((whline(i:i+1) .eq. 'SM') .or.
     *             (whline(i:i+1) .eq. 'sm') .or.
     *             (whline(i:i+1) .eq. 'Sm') .or.
     *             (whline(i:i+1) .eq. 'sM')) then
            grdsh = 13107
          else if ((whline(i:i) .eq. 'T') .or.
     *             (whline(i:i) .eq. 't') .or.
     *             (whline(i:i) .eq. 'D') .or.
     *             (whline(i:i) .eq. 'd')) then
            grdsh = 21845
          else if ((whline(i:i+1) .eq. 'SO') .or.
     *             (whline(i:i+1) .eq. 'so') .or.
     *             (whline(i:i+1) .eq. 'So') .or.
     *             (whline(i:i+1) .eq. 'sO')) then
            grdsh = -1
          else if ((whline(i:i) .eq. 'P') .or.
     *             (whline(i:i) .eq. 'p')) then
            grdsh = 0
            if (llplc .ne. 2) then
              ermes(1:31)  = 'Publication Style Lat/Lon Grids'
              ermes(32:60) = ' Are Plotted Over Land & Water'
              call errhan ('MAPDRV',0,ermes,errrsev)
              llplc = 2
            end if
          else
            ermes(1:27)  = 'Lat/Lon Grid Dash Pattern, '
            ermes(28:29) = whline(i:i+1)
            ermes(30:60) = ', Unknown, Default Uesd        '
            call errhan ('MAPDRV',0,ermes,errsev)
            grdsh = 21845
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,q,errsev)
            llint      = 0
            wlabs      = 2
            lbsiz      = 0.4
            lbqul(1)   = 0
            lbqul(2)   = 0
            tlsiz      = 0.6
            tlqul(1)   = 0
            tlqul(2)   = 0
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
            perm       = .true.
          end if
        end if

C  Third is the lat/lon grid interval, or LLINT

        if ((.not. error) .and. (llplc .ne. 0)) then
          call search (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,p,errsev)
            llint      = 0
            wlabs      = 2
            lbsiz      = 0.4
            lbqul(1)   = 0
            lbqul(2)   = 0
            tlsiz      = 0.6
            tlqul(1)   = 0
            tlqul(2)   = 0
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
            perm       = .true.
          end if
        end if

        if ((.not. error) .and. (llplc .ne. 0)) then
          if ((whline(i:i) .eq. 'D') .or. (whline(i:i) .eq. 'd')) then
            llint = 0
          else
            if ((whline(i+1:i+1) .ne. ' ') .and.
     *          (whline(i+1:i+1) .ne. '|')) then
              read (whline(i:i+1),50,err=110) llint
            else
              read (whline(i:i),30,err=110) llint
            end if
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,q,errsev)
            wlabs      = 2
            lbsiz      = 0.4
            lbqul(1)   = 0
            lbqul(2)   = 0
            tlsiz      = 0.6
            tlqul(1)   = 0
            tlqul(2)   = 0
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
            perm       = .true.
          end if
        end if

C  Set up GRDSH and LLINT to be clean if no grid is desired

        if ((llplc .eq. 0) .and. (.not. error)) then
          grdsh = 0
          llint = 0
        end if

C  Read in label information
C  First is WLABS, which can be
C    E                => WLABS = -1 ; or only EZMAP labels
C    M                => WLABS = 1  ; or only MAPDRV labels
C    N                => WLABS = 0  ; or no labels at all
C    D,A              => WLABS = 2  ; or both EZMAP and MAPDRV labels

        if (.not. error) then
          call search (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,p,errsev)
            wlabs      = 2
            lbsiz      = 0.4
            lbqul(1)   = 0
            lbqul(2)   = 0
            tlsiz      = 0.6
            tlqul(1)   = 0
            tlqul(2)   = 0
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
            perm       = .true.
          end if
        end if

        if (.not. error) then
          if ((whline(i:i) .eq. 'E') .or.
     *        (whline(i:i) .eq. 'e')) then
            wlabs = -1
          else if ((whline(i:i) .eq. 'M') .or.
     *             (whline(i:i) .eq. 'm')) then
            wlabs = 1
          else if ((whline(i:i) .eq. 'N') .or.
     *             (whline(i:i) .eq. 'n')) then
            wlabs = 0
          else if ((whline(i:i) .eq. 'A') .or.
     *             (whline(i:i) .eq. 'a') .or.
     *             (whline(i:i) .eq. 'D') .or.
     *             (whline(i:i) .eq. 'd')) then
            wlabs = 2
          else
            ermes(1:12)  = 'Label Flag, '
            ermes(13:13) = whline(i:i)
            ermes(14:36) = ', Unknown, Default Used'
            ermes(37:60) = '                        '
            call errhan ('MAPDRV',0,ermes,errsev)
            wlabs = 2
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,q,errsev)
            if (wlabs .ne. 0) then
              lbsiz    = 0.4
            else
              lbsiz    = 0.0
            end if
            lbqul(1)   = 0
            lbqul(2)   = 0
            tlsiz      = 0.6
            tlqul(1)   = 0
            tlqul(2)   = 0
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
            perm       = .true.
          end if
        end if

C  Second is LBSIZ, or the label size

        if ((wlabs .ne. 0) .and. (.not. error)) then
          call search (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,p,errsev)
            if (wlabs .ne. 0) then
              lbsiz    = 0.4
            else
              lbsiz    = 0.0
            end if
            lbqul(1)   = 0
            lbqul(2)   = 0
            tlsiz      = 0.6
            tlqul(1)   = 0
            tlqul(2)   = 0
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
            perm       = .true.
          end if
        end if

        if ((wlabs .ne. 0) .and. (.not. error)) then
          if ((whline(i:i) .eq. 'D') .or. (whline(i:i) .eq. 'd')) then
            lbsiz = 0.4
          else
            if ((whline(i+1:i+1) .ne. ' ') .and.
     *          (whline(i+1:i+1) .ne. '|')) then
              read (whline(i:i+1),50,err=120) temp
            else
              read (whline(i:i),30,err=120) temp
            end if

C  Label Size should not be less than 1 and should not be greater than
C  25

            if (temp .lt. 1) then
              ermes(1:30)  = 'Label Size Is Too Freaking Sma'
              ermes(31:60) = 'll, 1 Assumed                 '
              call errhan ('MAPDRV',0,ermes,errsev)
              temp = 1
            end if

            if (temp .gt. 25) then
              ermes(1:30)  = 'Label Size Is Too Large, 25 As'
              ermes(31:60) = 'sumed                         '
              call errhan ('MAPDRV',0,ermes,errsev)
              temp = 25
            end if

            lbsiz = float(temp)/scale
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,q,errsev)
            lbqul(1)   = 0
            lbqul(2)   = 0
            tlsiz      = 0.6
            tlqul(1)   = 0
            tlqul(2)   = 0
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
            perm       = .true.
          end if
        end if

C  Third in line is the label quality, LBQUL specified by to consecutive
C  integers, combinations are
C  00 - Complex characters / High quality
C  01 - Complex characters / Medium quality
C  02 - Complex characters / Low quality
C  10 - Duplex characters  / High quality
C  11 - Duplex characters  / Medium quality
C  12 - Duplex characters  / Low quality
C  D  - Default = 11

        if ((wlabs .ne. 0) .and. (.not. error)) then
          call search (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,p,errsev)
            lbqul(1)   = 0
            lbqul(2)   = 0
            tlsiz      = 0.6
            tlqul(1)   = 0
            tlqul(2)   = 0
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
            perm       = .true.
          end if
        end if

        if ((wlabs .ne. 0) .and. (.not. error)) then
          if ((whline(i:i) .eq. 'D') .or. (whline(i:i) .eq. 'd')) then
            lbqul(1) = 1
            lbqul(2) = 1
          else
            read (whline(i:i),30,err=130)     lbqul(1)
            read (whline(i+1:i+1),30,err=130) lbqul(2)
          end if

C  If the specified qualities are none of the defined ones
C  give and error message

          if ((lbqul(1) .ne. 0) .and. (lbqul(1) .ne. 1)) then
            ermes(1:40) = 'Text Type Specified For Labels Unknown, '
            ermes(41:60) = '1 Used (Duplex)     '
            call errhan ('MAPDRV',0,ermes,errsev)
            lbqul(1) = 1
          end if
          if ((lbqul(2) .lt. 0) .or. (lbqul(2) .gt. 2)) then
            ermes(1:40) = 'Text Quality Specified For Labels Unknow'
            ermes(41:60) = 'n, 1 Used (Medium)  '
            call errhan ('MAPDRV',0,ermes,errsev)
            lbqul(2) = 1
          end if

          call next (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,q,errsev)
            tlsiz      = 0.6
            tlqul(1)   = 0
            tlqul(2)   = 0
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
            perm       = .true.
          end if
        end if

C  Set up label info to be clean if none were requested

        if ((wlabs .eq. 0) .and. (.not. error)) then
          lbsiz    = 0.0
          lbqul(1) = 1
          lbqul(2) = 1
        end if

C  Read in the perimeter flag, PERM
C  N   => Draw a line perimeter
C  Y   => Draw a perimeter with ticks

        if (((.not. error) .and. (grdsh .ne. 0)) .or.
     *      (llplc .eq. 0)) then
          call search (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,p,errsev)
            tlsiz      = 0.6
            tlqul(1)   = 0
            tlqul(2)   = 0
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
            perm       = .true.
          end if
        end if

        if (.not. error) then
          if ((grdsh .eq. 0) .and. (llplc .eq. 2)) then
            perm = .false.
          else
            if ((whline(i:i) .eq. 'N') .or. (whline(i:i) .eq. 'n')) then
              perm = .false.
            else if ((whline(i:i) .eq. 'Y') .or.
     *               (whline(i:i) .eq. 'y')) then
              perm = .true.
            else
              ermes(1:35)  = 'Perimeter Flag Can Only Be Y or N, '
              ermes(36:60) = 'Y Assumed                '
              call errhan ('MAPDRV',0,ermes,errsev)
              perm = .true.
            end if
            call next (whline,i,error)
            if (error) then
              call errhan ('MAPDRV',1,q,errsev)
              tlsiz      = 0.6
              tlqul(1)   = 0
              tlqul(2)   = 0
              wouts(1:2) = 'PS'
              dtdmp      = .false.
              maplw      = 1000
              dtdsp      = 0
            end if
          end if
        end if

C  Read in title information
C  First is the title flag
C    Y                   => read in TLSIZ and TLQUL
C    N                   => skip to outlines

        if (.not. error) then
          call search (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,p,errsev)
            tlsiz      = 0.6
            tlqul(1)   = 0
            tlqul(2)   = 0
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
          end if
        end if

        if (.not. error) then
          if ((whline(i:i) .eq. 'Y') .or. (whline(i:i) .eq. 'y')) then
            title = .true.
          else if ((whline(i:i) .eq. 'N') .or.
     *             (whline(i:i) .eq. 'n')) then
            title    = .false.
            tlsiz    = 0.0
            tlqul(1) = 0
            tlqul(2) = 0
          else
            ermes(1:31)  = 'Title Flag Can Only Be Y or N, '
            ermes(32:60) = 'Y Assumed                    '
            call errhan ('MAPDRV',0,ermes,errsev)
            title = .true.
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,q,errsev)
            if (.not. title) then
              tlsiz      = 0.0
            else
              tlsiz      = 0.6
            end if
            tlqul(1)   = 0
            tlqul(2)   = 0
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
          end if
        end if

C  Read in TLSIZ

        if ((title) .and. (.not. error)) then
          call search (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,p,errsev)
            tlsiz      = 0.6
            tlqul(1)   = 0
            tlqul(2)   = 0
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
          end if
        end if

        if ((title) .and. (.not. error)) then
          if ((whline(i:i) .eq. 'D') .or. (whline(i:i) .eq. 'd')) then
            tlsiz = 0.6
          else
            if ((whline(i+1:i+1) .ne. '|') .and.
     *          (whline(i+1:i+1) .ne. ' ')) then
              read (whline(i:i+1),50,err=140) temp
            else
              read (whline(i:i),30,err=140) temp
            end if

C  Title size entered must be between 1 and 25

            if (temp .lt. 1) then
              ermes(1:30)  = 'Title Size Is Too Freaking Sma'
              ermes(31:60) = 'll, 1 Assumed                 '
              call errhan ('MAPDRV',0,ermes,errsev)
              temp = 1
            end if

            if (temp .gt. 25) then
              ermes(1:30)  = 'Title Size Is Too Large, 25 As'
              ermes(31:60) = 'sumed                         '
              call errhan ('MAPDRV',0,ermes,errsev)
              temp = 25
            end if

            tlsiz = float(temp)/scale
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,q,errsev)
            tlqul(1)   = 0
            tlqul(2)   = 0
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
          end if
        end if

C  Read in TLQUL, which may be
C  00 - Complex characters / High quality
C  01 - Complex characters / Medium quality
C  02 - Complex characters / Low quality
C  10 - Duplex characters  / High quality
C  11 - Duplex characters  / Medium quality
C  12 - Duplex characters  / Low quality
C  D  - Default = 11

        if ((title) .and. (.not. error)) then
          call search (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,p,errsev)
            tlqul(1)   = 0
            tlqul(2)   = 0
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
          end if
        end if

        if ((title) .and. (.not. error)) then
          if ((whline(i:i) .eq. 'D') .or. (whline(i:i) .eq. 'd')) then
            tlqul(1) = 1
            tlqul(2) = 1
          else
            read (whline(i:i),30,err=150)     tlqul(1)
            read (whline(i+1:i+1),30,err=150) tlqul(2)
          end if

C  If the specified quality numbers are not defined, deliver an error message

          if ((tlqul(1) .ne. 0) .and. (tlqul(1) .ne. 1)) then
            ermes(1:40) = 'Text Type Specified For Title Unknown, 1'
            ermes(41:60) = ' Used (Duplex)      '
            call errhan ('MAPDRV',0,ermes,errsev)
            tlqul(1) = 1
          end if
          if ((tlqul(2) .lt. 0) .or. (tlqul(2) .gt. 2)) then
            ermes(1:40) = 'Text Quality Specified For Title Unknown'
            ermes(41:60) = ', 1 Used (Medium)   '
            call errhan ('MAPDRV',0,ermes,errsev)
            tlqul(2) = 1
          end if

          call next (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,q,errsev)
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
          end if
        end if 

C  Read in outline information, or WOUTS, which can be
C    NO => no outlines
C    CO => continental outlines only
C    US => U.S. State outlines only
C    PS => Continental + International + State outlines
C    PO => Continental + International outlines

        if (.not. error) then
          call search (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,p,errsev)
            wouts(1:2) = 'PS'
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
          end if
        end if

        if (.not. error) then
          wouts(1:2) = whline(i:i+1)

C  Make sure that WOUTS is given in all upper case

          if ((ichar(wouts(1:1)) .le. ichar('z')) .and.
     *        (ichar(wouts(1:1)) .ge. ichar('a')))
     *      wouts(1:1) = char(ichar(wouts(1:1)) - 32)
          if ((ichar(wouts(2:2)) .le. ichar('z')) .and.
     *        (ichar(wouts(2:2)) .ge. ichar('a')))
     *      wouts(2:2) = char(ichar(wouts(2:2)) - 32)

C  If WOUTS is not valid give an error

          if ((wouts(1:2) .ne. 'NO') .and.
     *        (wouts(1:2) .ne. 'CO') .and.
     *        (wouts(1:2) .ne. 'US') .and.
     *        (wouts(1:2) .ne. 'PS') .and.
     *        (wouts(1:2) .ne. 'PO')) then
            ermes(1:32)  = 'Geographical Outline Specifier, '
            ermes(33:34) = wouts(1:2)
            ermes(35:60) = ', Unknown, PS Used        '
            call errhan ('MAPDRV',0,ermes,errsev)
            wouts(1:2) = 'PS'
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,q,errsev)
            dtdmp      = .false.
            maplw      = 1000
            dtdsp      = 0
          end if
        end if

C  Read in outline details information
C  First is the dotted outline flag, or DTDMP, which may be
C    Y                   => outlines are to be drawn with dots;
C                           read in the dot spacing
C    N                   => outlines are solid lines;
C                           read in the outline line width

        if (wouts(1:2) .ne. 'NO') then
          if (.not. error) then
            call search (whline,i,error)
            if (error) then
              call errhan ('MAPDRV',1,p,errsev)
              dtdmp      = .false.
              maplw      = 1000
              dtdsp      = 0
            end if
          end if

          if (.not. error) then
            if ((whline(i:i) .eq. 'y') .or. (whline(i:i) .eq. 'Y')) then
              dtdmp = .true.
            else if ((whline(i:i) .eq. 'n') .or.
     *               (whline(i:i) .eq. 'N')) then
              dtdmp = .false.
            else
              ermes(1:40)  = 'Dotted Outline Flag Can Only Be Y or N, '
              ermes(41:60) = 'N Assumed           '
              call errhan ('MAPDRV',0,ermes,errsev)
              dtdmp = .false.
            end if
            call next (whline,i,error)
            if (error) then
              call errhan ('MAPDRV',1,q,errsev)
              maplw      = 1000
              dtdsp      = 0
            end if
          end if

C  Advance to next entry

          if (.not. error) then
            call search (whline,i,error)
            if (error) then
              call errhan ('MAPDRV',1,p,errsev)
              maplw      = 1000
              dtdsp      = 0
            end if
          end if

          if ((dtdmp) .and. (.not. error)) then

C  Read in the dot spacing

            if ((whline(i:i) .eq. 'D') .or. (whline(i:i) .eq. 'd')) then
              dtdsp = 12
            else
              if ((whline(i+1:i+1) .ne. ' ') .or.
     *            (whline(i+1:i+1) .ne. '|')) then
                read (whline(i:i+1),50,err=160) dtdsp
              else
                read (whline(i:i+1),30,err=160) dtdsp
              end if
            end if
            if (dtdsp .lt. 1) then
              ermes(1:38)  = 'Dot Spacing Must Be At Least 1, Change'
              ermes(39:60) = 'd To 1                '
              call errhan ('MAPDRV',0,ermes,errsev)
              dtdsp = 1
            end if
            if (dtdsp .gt. 36) then
              ermes(1:38)  = 'Dot Spacing Would Hardly Make A Map Re'
              ermes(39:60) = 'set To 36             '
              call errhan ('MAPDRV',0,ermes,errsev)
              dtdsp = 36
            end if
            maplw = 0
          else

C  Read in the outline line width

            if ((whline(i:i) .eq. 'D') .or. (whline(i:i) .eq. 'd')) then
              maplw = 1000
            else
              j = i
              call next (whline,i,error)
              if (.not. error) then
                gstrng(1:i-j) = whline(j:i-1)
                do 260 k = i-j+1,20
                  gstrng(k:k) = ' '
260             continue
                call gtreal (gstrng,dumy,error)
                if (error) then
                  ermes(1:30)  = 'Map Outline Line Width Input C'
                  ermes(31:60) = 'onversion                     '
                  call errhan ('MAPDRV',0,ermes,errsev)
                  maplw = 1000
                  error = .false.
                else
                  maplw = nint(dumy * 1000)
                end if
              else
                call errhan ('MAPDRV',0,q,errsev)
              end if
            end if
            dtdsp = 0
            if (maplw .lt. 1000) then
              ermes(1:38)  = 'Line Widths Of Less Than 1000 Will Be '
              ermes(39:60) = '1000                  '
              call errhan ('MAPDRV',0,ermes,errsev)
              maplw = 1000
            end if
            if (maplw .gt. 10000) then
              ermes(1:38)  = 'Line Widths Of Greater Than 10000 Will'
              ermes(39:60) = ' Be 10000             '
              call errhan ('MAPDRV',0,ermes,errsev)
              maplw = 10000
            end if
          end if
        else
          dtdmp      = .false.
          maplw      = 1000
          dtdsp      = 0
        end if

C  Warn user if errors exist

        if (.not. error) then
          call next (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',0,q,errsev)
          else
            call search (whline,i,error)
            if (.not. error) then
              ermes(1:24)  = 'Too Many Entries On Line'
              ermes(25:50) = ' - Extras Ignored         '
              ermes(51:60) = '          '
              call errhan ('MAPDRV',0,ermes,errsev)
            end if
          end if
        end if

C  Check for value errors
C  The lat/lon grid interval must be between 0 and 180

        if ((llint .lt. 0) .or. (llint .gt. 180)) then
          ermes(1:30)  = 'Lat/Lon Grid Interval Invalid,'
          ermes(31:60) = ' Default Used                 '
          call errhan ('MAPDRV',0,ermes,errsev)
          llint = 0
        end if

        goto 70
      else

        if (buff .ge. 0) then

C  The table is not there so use the defaults

          print *, 'MAPDRV - Defaults Used For Map Details'
          error      = .false.
          llplc      = 2
          grdsh      = 21845
          llint      = 0
          wlabs      = 2
          lbsiz      = 0.4
          lbqul(1)   = 0
          lbqul(2)   = 0
          tlsiz      = 0.6
          tlqul(1)   = 0
          tlqul(2)   = 0
          wouts(1:2) = 'PS'
          dtdmp      = .false.
          maplw      = 1000
          dtdsp      = 0
        else

C  The table may or may not be there, but only an area map has been requested
C  so special defaults are used

          print *, 'MAPDRV - Area Map Request Accepted'
          error      = .false.
          llplc      = 0
          grdsh      = 0
          llint      = 0
          wlabs      = 0
          lbsiz      = 0.0
          lbqul(1)   = 0
          lbqul(2)   = 0
          tlsiz      = 0.0
          tlqul(1)   = 0
          tlqul(2)   = 0
          wouts(1:2) = 'CO'
          dtdmp      = .false.
          maplw      = 1000
          dtdsp      = 0
        end if
        goto 90
      end if

C  Handle mismatch errors
C  First are lat/lon gird interval mismatch errors

110   ermes(1:37)  = 'Lat/Lon Grid Interval Type Mismatch  '
      ermes(38:60) = '                       '
      call errhan ('MAPDRV',1,ermes,errsev)
      llint      = 0
      wlabs      = 2
      lbsiz      = 0.4
      lbqul(1)   = 0
      lbqul(2)   = 0
      tlsiz      = 0.6
      tlqul(1)   = 0
      tlqul(2)   = 0
      wouts(1:2) = 'PS'
      dtdmp      = .false.
      maplw      = 1000
      dtdsp      = 0
      perm       = .true.
      goto 70

C  Second is the label size type mismatch

120   ermes(1:24)  = 'Label Size Type Mismatch'
      ermes(25:60) = '                                    '
      call errhan ('MAPDRV',1,ermes,errsev)
      lbsiz      = 0.4
      lbqul(1)   = 0
      lbqul(2)   = 0
      tlsiz      = 0.6
      tlqul(1)   = 0
      tlqul(2)   = 0
      wouts(1:2) = 'PS'
      dtdmp      = .false.
      maplw      = 1000
      dtdsp      = 0
      perm       = .true.
      goto 70

C  Third is label quality mismatch

130   ermes(1:27)  = 'Label Quality Type Mismatch'
      ermes(28:60) = '                                 '
      call errhan ('MAPDRV',1,ermes,errsev)
      lbqul(1)   = 0
      lbqul(2)   = 0
      tlsiz      = 0.6
      tlqul(1)   = 0
      tlqul(2)   = 0
      wouts(1:2) = 'PS'
      dtdmp      = .false.
      maplw      = 1000
      dtdsp      = 0
      perm       = .true.
      goto 70

C  Fourth is the title size type mismatch

140   ermes(1:24)  = 'Title Size Type Mismatch'
      ermes(25:60) = '                                    '
      call errhan ('MAPDRV',1,ermes,errsev)
      tlsiz      = 0.6
      tlqul(1)   = 0
      tlqul(2)   = 0
      wouts(1:2) = 'PS'
      dtdmp      = .false.
      maplw      = 1000
      dtdsp      = 0
      goto 70

C  Fifth is the title quality type mismatch

150   ermes(1:27)  = 'Title Quality Type Mismatch'
      ermes(28:60) = '                                 '
      call errhan ('MAPDRV',1,ermes,errsev)
      tlqul(1)   = 0
      tlqul(2)   = 0
      wouts(1:2) = 'PS'
      dtdmp      = .false.
      maplw      = 1000
      dtdsp      = 0
      goto 70

C  Sixth is the dot spacing type mismatch

160   ermes(1:25)  = 'Dot Spacing Type Mismatch'
      ermes(26:60) = '                                   '
      call errhan ('MAPDRV',1,ermes,errsev)
      dtdsp      = 12
      maplw      = 0
      goto 70

C  Last is the line width type mismatch

170   ermes(1:24)  = 'Line Width Type Mismatch'
      ermes(25:60) = '                                    '
      call errhan ('MAPDRV',1,ermes,errsev)
      dtdsp      = 0
      maplw      = 1000
      
70    print *, 'MAPDRV - Map Details Set Up'
90    owlabs = wlabs
      ollplc = llplc

C*****************************  subroutine end  ******************************C

C  Format statements begin ...

30    format (I1)
50    format (I2)
60    format (I4)

C  Format statements end.

250   return
      end
      subroutine mrdfil (unum,flmap,errsev,nomap)

C*****************************************************************************C
C  mrdfil   - This is a MAPDRV routine                                        C
C  Section  - Tables                                                          C
C  Purpose  - To read in the map fill table and assign the map fill colors.   C
C                                                                             C
C  On entry - UNUM  is the unit  number of the table file.   ERRSEV indicates C
C             severity of a error which will cause execution to halt.   NOMAP C
C             is true if a non-correctable  error has  occured and  is  false C
C             otherwise.                                                      C
C                                                                             C
C  On exit  - FLMAP is true if the table  was there,  and is false otherwise. C
C             The color indicies in common blocks FLINFO  and FLWATR have all C
C             been set up correctly.  If a non-correctable error has  occrued C
C             NOMAP is true and is false otherwise.                           C
C                                                                             C
C  Assume   - Nothing.                                                        C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             SEARCH              MAPDRV/CONDRV utility                       C
C             NEXT                MAPDRV/CONDRV utility                       C
C             ERRHAN              MAPDRV/CONDRV utility                       C
C             ERRFIL              MAPDRV utility                              C
C             CRDRCI              MAPDRV/CONDRV utility                       C
C             ----------------------------------------------------------------C
C                                                                             C
C  Author   - Jeremy Asbill        Date - May 27, 1990       for the MM4 club C
C*****************************************************************************C

C  Character variables

      character*80     whline            ! line of info. from table     (local)
      character*60     p,                ! error message for SEARCH     (local)
     *                 q,                ! error message for NEXT       (local)
     *                 ermes             ! general error message        (local)
      character*2      wouts             ! for common block MOCDET

C  Integer variables

      integer          unum,             ! unit number of table file       (in)
     *                 errsev            ! error severity indicator        (in)
      integer          fscolr,           ! for common block FLINFO
     *                 secolr,           ! for common block FLINFO
     *                 thcolr,           ! for common block FLINFO
     *                 frcolr,           ! for common block FLINFO
     *                 fvcolr,           ! for common block FLINFO
     *                 sicolr            ! for common block FLINFO
      integer          wacolr            ! for common block FLWATR
      integer          i                 ! loop counter/place keeper    (local)

C  Logical variables

      logical          flmap             ! color fill the map ?           (out)
      logical          nomap,            ! don't draw a map ?           (local)
     *                 error,            ! has an error occured ?       (local)
     *                 found             ! was the table found ?        (local)

C  Common blocks

      common /flinfo/  fscolr,           ! first color
     *                 secolr,           ! second color
     *                 thcolr,           ! third color
     *                 frcolr,           ! fourth color
     *                 fvcolr,           ! fifth color
     *                 sicolr            ! sixth color
      common /flwatr/  wacolr            ! water color
      common /mocdet/  wouts             ! geographical outline indicator

C****************************  subroutine begin  *****************************C

C  Check to see if we need to do this

      if (nomap) goto 150

C  Initialize the error flag

      error = .false.

C  Try to get the information line from the table

      call tbllok (unum,'MAP FILL  ',errsev,found,whline,'MAPDRV')

C  Only parse the information if the table was found

      if (found) then

C  Assume since the table was there that the map will be filled

        flmap = .true.

C  Set up the SEARCH and NEXT messages

        p(1:23)  = 'Reading Details Table, '
        p(24:60) = 'Too Few Entries On Line              '
        q(1:23)  = p(1:23)
        q(24:60) = 'Entry Is Bizarre                     '

C  Initialize the place keepers

        i = 1

C  First read in the water color index

        call search (whline,i,error)
        if (error)
     *    call errhan ('MAPDRV',1,p,errsev)

        call crdrci (.true.,error,wacolr,1,whline,i,
     *               'Water Color Index',17,errsev,
     *               nomap,'MAPDRV')
        if (error) call errfil (7)
        if (nomap) goto 150

        if (.not. error) then
          call next (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,q,errsev)
            call errfil (6)
          end if
        end if

C  After the water color there are six more color indices to read in
C  1

        if ((wouts(1:2) .ne. 'NO') .and. (.not. error)) then
          call search (whline,i,error)
          if (error) then
            call errhan ('MAPDRV',1,p,errsev)
            call errfil (6)
          end if

          if (.not. error) then
            call crdrci (.true.,error,fscolr,1,whline,i,
     *                   'First Color Index',17,errsev,
     *                   nomap,'MAPDRV')
            if (error) call errfil (6)
            if (nomap) goto 150
          end if

          if (.not. error) then
            call next (whline,i,error)
            if (error) then
              call errhan ('MAPDRV',1,q,errsev)
              call errfil (5)
            end if
          end if

C  2

          if (.not. error) then
            call search (whline,i,error)
            if (error) then
              call errhan ('MAPDRV',1,p,errsev)
              call errfil (5)
            end if
          end if

          if (.not. error) then
            call crdrci (.true.,error,secolr,1,whline,i,
     *                   'Second Color Index',18,errsev,
     *                   nomap,'MAPDRV')
            if (error) call errfil (5)
            if (nomap) goto 150
          end if

          if (.not. error) then
            call next (whline,i,error)
            if (error) then
              call errhan ('MAPDRV',1,q,errsev)
              call errfil (4)
            end if
          end if

C  3

          if (.not. error) then
            call search (whline,i,error)
            if (error) then
              call errhan ('MAPDRV',1,p,errsev)
              call errfil (4)
            end if
          end if

          if (.not. error) then
            call crdrci (.true.,error,thcolr,1,whline,i,
     *                   'Third Color Index',17,errsev,
     *                   nomap,'MAPDRV')
            if (error) call errfil (4)
            if (nomap) goto 150
          end if

          if (.not. error) then
            call next (whline,i,error)
            if (error) then
              call errhan ('MAPDRV',1,q,errsev)
              call errfil (3)
            end if
          end if

C  4

          if (.not. error) then
            call search (whline,i,error)
            if (error) then
              call errhan ('MAPDRV',1,p,errsev)
              call errfil (3)
            end if
          end if

          if (.not. error) then
            call crdrci (.true.,error,frcolr,1,whline,i,
     *                   'Fourth Color Index',18,errsev,
     *                   nomap,'MAPDRV')
            if (error) call errfil (3)
            if (nomap) goto 150
          end if

          if (.not. error) then
            call next (whline,i,error)
            if (error) then
              call errhan ('MAPDRV',1,q,errsev)
              call errfil (2)
            end if
          end if

C  5

          if (.not. error) then
            call search (whline,i,error)
            if (error) then
              call errhan ('MAPDRV',1,p,errsev)
              call errfil (2)
            end if
          end if

          if (.not. error) then
            call crdrci (.true.,error,fvcolr,1,whline,i,
     *                   'Fifth Color Index',17,errsev,
     *                   nomap,'MAPDRV')
            if (error) call errfil (2)
            if (nomap) goto 150
          end if

          if (.not. error) then
            call next (whline,i,error)
            if (error) then
              call errhan ('MAPDRV',1,q,errsev)
              call errfil (1)
            end if
          end if

C  6

          if (.not. error) then
            call search (whline,i,error)
            if (error) then
              call errhan ('MAPDRV',1,p,errsev)
              call errfil (1)
            end if
          end if

          if (.not. error) then
            call crdrci (.true.,error,sicolr,1,whline,i,
     *                   'Sixth Color Index',17,errsev,
     *                   nomap,'MAPDRV')
            if (error) call errfil (1)
            if (nomap) goto 150
          end if

          if (.not. error) then
            call next (whline,i,error)
            if (error) then
              call errhan ('MAPDRV',0,q,errsev)
            end if
          end if
        end if

C  Check to see if there were too many entries in the table

        if (.not. error) then
          call search (whline,i,error)
          if (.not. error) then
            ermes(1:24)  = 'Too Many Entries On Line'
            ermes(25:50) = ' - Extras Ignored         '
            ermes(51:60) = '          '
            call errhan ('MAPDRV',0,ermes,errsev)
          end if
        end if
        print *, 'MAPDRV - Map Fill Information Set Up'
      else

C  The table was not there, no fill is to be done

        flmap = .false.
        wacolr = 0
        fscolr = 0
        secolr = 0
        thcolr = 0
        frcolr = 0
        fvcolr = 0
        sicolr = 0
      end if

C*****************************  subroutine end  ******************************C

C  Format statements begin ...

30    format (I1)
50    format (I2)

C  Format statements end.

150   return
      end
      subroutine next (whline,place,error)

C*****************************************************************************C
C  next     - This is a CONDRV/MAPDRV routine                                 C
C  Section  - Tables                                                          C
C  Purpose  - To read to the next space or vertical bar in a table.           C
C                                                                             C
C  On entry - WHLINE contains a line (80 Characters) of a table.  PLACE  con- C
C             tains the location in WHLINE to start looking.  ERROR comes  in C
C             false.                                                          C
C                                                                             C
C  On exit  - PLACE contains the location of the the next space  or  vertical C
C             bar in WHLINE and ERROR is true if the  search went  beyond  80 C
C             characters and is false otherwise.                              C
C                                                                             C
C  Assume   - Nothing.                                                        C
C                                                                             C
C  Author   - Jeremy Asbill        Date - June 21, 1990      for the MM4 Club C
C*****************************************************************************C

C  Character variables

      character*80     whline            ! a line from current table       (in)

C  Integer variables

      integer          place             ! start parse here                (in)
      integer          i                 ! place keeper                 (local)

C  Logical variables

      logical          error             ! has an error occured ?          (in)

C****************************  subroutine begin  *****************************C

C  Use I and not PLACE

      i = place

C  Always increment at least one place in WHLINE

      i = i + 1

C  Test until we find the first space or vertical bar

10    continue
      if (i .le. 80) then
        if ((whline(i:i) .ne. ' ') .and. (whline(i:i) .ne. '|')) then
          i = i + 1
          goto 10
        end if
      end if

C  If no error occured reassign PLACE and exit

      if (i .le. 80) then
        place = i

C  If an error has occured let the calling routine know

      else
        error = .true.
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine prodll (nmgr,cenlon,project)

C*****************************************************************************C
C  prodll   - This is a MAPDRV routine                                        C
C  Section  - Lat/Lon Lines                                                   C
C  Purpose  - To make publication style lat/lon patterns on a map.            C
C                                                                             C
C  On entry - NMGR is the grid interval being used. CENLON is the center lon- C
C             longitude of the entire domain, even if this is a subset.       C
C                                                                             C
C  On exit  - The lat/lon grid has been marked with plus signs.               C
C                                                                             C
C  Assume   - GKS is open.                                                    C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             MAPGTI              EZMAP utility*                              C
C             MAPTRN              EZMAP utility*                              C
C             PLCHHQ              PLOTCHAR utility*                           C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill       Date - July 23, 1990       for the MM4 club C
C*****************************************************************************C

C  Character variables

      character*2      project           ! projection used in map          (in)
 
C  Integer variables

      integer          nmgr              ! grid interval to use            (in)
      integer          wlabs,            ! for common block MLBDET
     *                 lbqul(2)          ! for common block MLBDET
      integer          llcolr            ! for common block LLLCOL
      integer          i,j,              ! loop counters/place keepers  (local)
     *                 stri,             ! what lon. to start with      (local)
     *                 strj,             ! what lat. to start with      (local)
     *                 llsv              ! save variable                (local)

C  Real variables

      real             cenlon            ! center longitude                (in)
      real             lbsiz             ! for common block MLBDET
      real             xn,               ! cone factor for projection   (local)
     *                 x,                ! x position in grid of a pt.  (local)
     *                 y,                ! y position in grid of a pt.  (local)
     *                 flsv,             ! save variable                (local)
     *                 frsv,             ! save variable                (local)
     *                 fbsv,             ! save variable                (local)
     *                 ftsv,             ! save variable                (local)
     *                 ulsv,             ! left of map in user coords.  (local)
     *                 ursv,             ! right of map in user coords. (local)
     *                 ubsv,             ! bottom of map in user coords.(local)
     *                 utsv,             ! top of map in user coords.   (local)
     *                 ang               ! angle at which to draw plus  (local)

C  Common blocks

      common /mlbdet/  wlabs,            ! not used
     *                 lbsiz,            ! alternate label size
     *                 lbqul             ! not used
      common /lllcol/  llcolr            ! color of lat/lon lines

C****************************  subroutine begin  *****************************C

C  Set the user coords. up to mimic the grid

      call getset (flsv,frsv,fbsv,ftsv,ulsv,ursv,ubsv,utsv,llsv)

C  Set up correct color for the ticks
C  To understand what the quality of the letters has to do with the color
C  read on page 2-14 in the NCAR Graphics Guide to New Utilities Version 3.00
C  under the heading of PLOTCHAR

      call pcseti ('QU',0)
      call gsplci (llcolr)

C  Determin projection cone factor

      if (project(1:2) .eq. 'LC') then
        xn = 0.716
      else if (project(1:2) .eq. 'ST') then
        xn = 1.0
      else
        xn = 0.0
      end if

C  Determine which lat/lons to start with

      stri = -180 + nmgr
      strj = -90  + nmgr

C  Turn clipping on

      call gsclip (1)

C  Loop through every lat lon combo. with proper grid interval

      do 10 i = stri,180,nmgr
        do 20 j = strj,90,nmgr

C  Calculate the angle at which to draw the pluses

          ang = (float(i) - cenlon) * xn

C  Change current lat/lon combo. to user coordinates

          call maptrn (float(j),float(i),x,y)

C  Check to see if the point is within the viewport

          if ((x .ge. ulsv) .and. (x .le. ursv) .and. 
     *        (y .ge. ubsv) .and. (y .le. utsv)) then

C  Draw a correct plus

            call plchhq (x,y,'+',-lbsiz,ang,0)
          end if
20      continue
10    continue

C  Turn clipping off

      call gsclip (0)

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine search (whline,place,error)

C*****************************************************************************C
C  search   - This is a CONDRV/MAPDRV routine                                 C
C  Section  - Tables                                                          C
C  Purpose  - This is  a tool to  read to the next  meaningful character in a C
C             table.                                                          C
C                                                                             C
C  On entry - WHLINE contains a line of a table. PLACE contains the  position C
C             from where to start looking.                                    C
C                                                                             C
C  On exit  - PLACE contains the  location of the first  meaningful character C
C             within the string.  If no character was found ERROR is true.    C
C                                                                             C
C  Assume   - Nothing.                                                        C
C                                                                             C
C  Author   - Jeremy Asbill        Date - June 11, 1990      for the MM4 club C
C*****************************************************************************C

C  Character Variables

      character*80 whline      ! a line from a table                       (in)

C  Integer Variables

      integer      place       ! position within WHLINE                    (in)

C  Logical Variables

      logical      error       ! was there no character to find           (out)

C****************************  subroutine begin  *****************************C

C  Initialize ERROR

      error = .false.

C  Try to read to the first character

10    continue
      if ((whline(place:place) .eq. ' ') .or.
     *     (whline(place:place) .eq. '|')) then
        place = place + 1

C  Check to see if there is an error condition

        if (place .gt. 80) then
          error = .true.
          goto 20
        end if
        goto 10
      end if

C*****************************  subroutine end  ******************************C

20    return
      end
      subroutine setmap (cenlat,cenlon,project,grds,xpa,ypa,xpb,ypb,
     *                   iend,jend)

C*****************************************************************************C
C  setmap   - This is a MAPDRV routine                                        C
C  Section  - Design                                                          C
C  Purpose  - To match the grid and the  screen window with the domain on the C
C             globe.                                                          C
C                                                                             C
C  On entry - CENLAT is the center latitude of the whole domain.   CENLON  is C
C             the center longitude of the whole domain.  PROJECT is  the pro- C
C             jection indicator.  GRDS is the grid  distance  in  kilometers. C
C             XPA, YPA  are the lower  left grid point of the map.   XPB, YPB C
C             are the upper right grid point of the map. IEND is the value of C
C             the maximum grid point in the y direction.   JEND is the  value C
C             of the maximum grid point in the x direction.                   C
C                                                                             C
C  On exit  - The domain has been set up with EZMAP.                          C
C                                                                             C
C  Assume   - GKS is open.                                                    C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             MAPSET              EZMAP utility*                              C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill       Date - July 14, 1990       for the MM4 club C
C*****************************************************************************C

C  Character varaibles

      character*2      project           ! specifies projection            (in)

C  Integer variables

      integer          iend,             ! max. y value of grids           (in)
     *                 jend              ! max. x value of grids           (in)
      integer          imax,             ! for common block XYLLON
     *                 jmax              ! for common block XYLLON

C  Real variables

      real             cenlat,           ! center latitude                 (in)
     *                 cenlon,           ! center longitude                (in)
     *                 grds,             ! grid distance in kilometers     (in)
     *                 xpa,              ! x of lower left grid pt. in map (in)
     *                 ypa,              ! y of lower left grid pt. in map (in)
     *                 xpb,              ! x of upper right grd pt. in map (in)
     *                 ypb               ! y of upper right grd pt. in map (in)
      real             ds,               ! for common block XYLLON
     *                 xlatc,            ! for common block XYLLON
     *                 xlonc             ! for common block XYLLON
      real             latl,             ! lower left latitude          (local)
     *                 lonl,             ! lower left longitude         (local)
     *                 latu,             ! upper right latitude         (local)
     *                 lonu              ! upper right longitude        (local)

C  Common blocks

      common /xyllon/  ds,               ! grid distance in kilometers
     *                 xlatc,            ! center latitude
     *                 xlonc,            ! center longitude
     *                 imax,             ! maximum vertical gird point
     *                 jmax              ! maximum horizontal grid point

C****************************  subroutine begin  *****************************C

C  Set up to use XYTOLL

      ds    = grds
      xlatc = cenlat
      xlonc = cenlon
      imax  = iend
      jmax  = jend

C  Get the longitude and latitude

      call xytoll (xpa,ypa,latl,lonl,project)
      call xytoll (xpb,ypb,latu,lonu,project)

C  Set up the domain with EZMAP

      call mapset ('CO',latl,lonl,latu,lonu)

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine setpro (project,cenlat,cenlon)

C*****************************************************************************C
C  setpro   - This is a MAPDRV routine                                        C
C  Section  - Design                                                          C
C  Purpose  - To determine and set up the proper projection for the map.      C
C                                                                             C
C  On entry - PROJECT describes what  projection we are to use.   CENLAT  and C
C             CENLON are the central latitude and longitude respectively.     C
C                                                                             C
C  On exit  - The correct projection has been set up.   XN  contains  a calc- C
C             culation value needed by the routine  XYTOLL.                   C
C                                                                             C
C  Assume   - GKS is open.                                                    C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             MAPPROJ             EZMAP utility*                              C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill        Date - July 6, 1990       for the MM4 club C
C*****************************************************************************C

C  Character variables

      character*2      project           ! specifies projection to use     (in)

C  Real variables

      real             cenlat,           ! center latitude to use          (in)
     *                 cenlon            ! center longitude to use         (in)
      real             rotan,            ! rotation angle               (local)
     *                 polat             ! polar latitude               (local)
      real             confac,           ! for common block LAMSTF
     *                 fsparl,           ! for common block LAMSTF
     *                 ssparl            ! for common block LAMSTF

C  Common blocks

      common /lamstf/  confac,           ! not used
     *                 fsparl,           ! first standard parallel lat.
     *                 ssparl            ! second standard parallel lat.

C****************************  subroutine begin  *****************************C

C  Lambert Conformal Projection - These values are either set to defaults
C  or by the user, determined in INTERR.

      if (project .eq. 'LC') then
        rotan = fsparl
        polat = ssparl
        print *, 'MAPDRV - Lambert Conformal Projection'

C  Polar Stereographic Projection - The polar true latitude is either set to
C  the default by the user or determined in INTERR.

      else if (project .eq. 'ST') then
        rotan = 0.0
        if (cenlat .gt. 0.0) then
          polat = 90.0
        else
          polat = -90.0
        end if
        print *, 'MAPDRV - Polar Stereographic Projection'

C  Cylindrical Equidistant

      else if (project .eq. 'CE') then
        rotan = 0.0
        polat = 0.0
        print *, 'MAPDRV - Cylindrical Equidistant Projection'

C  MErcator

      else if (project .eq. 'ME') then
        rotan = 0.0
        polat = 0.0
        print *, 'MAPDRV - Mercator Projection'
      end if

C  Set up the projection

      call maproj (project,polat,cenlon,rotan)

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine setwin (xpa,ypa,xpb,ypb,doset)

C*****************************************************************************C
C  setwin   - This is a MAPDRV routine                                        C
C  Section  - Design                                                          C
C  Purpose  - To set the map in a nice window on the screen.  That is the map C
C             must allow room above and below it.                             C
C                                                                             C
C  On entry - XPA, YPA are the lower left hand corners of the map  within the C
C             domain grid.  XPB, YPB are the  upper  right hand corner of the C
C             map within the domain grid.  DOSET  indicates whether to make a C
C             set call here or whether the user did it.                       C
C                                                                             C
C  On exit  - The proper window has been set.                                 C
C                                                                             C
C  Assume   - GKS is open.                                                    C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             MAPPOS              EZMAP utility*                              C
C             GETSET              SPPS*                                       C
C             SET                 SPPS*                                       C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill       Date - July 11, 1990       for the MM4 club C
C*****************************************************************************C

C  Integer varaibles

      integer          llsv              ! junk filler                  (local)

C  Logical variables

      logical          doset             ! do a set call here ?            (in)

C  Real variables

      real             xpa,              ! left hand x coord of map        (in)
     *                 ypa,              ! lower y coord of map            (in)
     *                 xpb,              ! right hand x coord of map       (in)
     *                 ypb               ! upper y coord of map            (in)
      real             temp,             ! temporary test variable      (local)
     *                 dumy,             ! dummy test variable          (local)
     *                 test,             ! test variable                (local)
     *                 flsv,             ! left window edge             (local)
     *                 frsv,             ! right window edge            (local)
     *                 fbsv,             ! bottom window edge           (local)
     *                 ftsv,             ! top window edge              (local)
     *                 ulsv,             ! junk filler                  (local)
     *                 ursv,             ! junk filler                  (local)
     *                 ubsv,             ! junk filler                  (local)
     *                 utsv              ! junk filler                  (local)

C****************************  subroutine begin  *****************************C

C  Set up variables to test on
C  TEMP will represent the maps width
C  DUMY will represent the maps height

      temp = ypb - ypa + 1.0
      dumy = (xpb - xpa + 1.0) * 0.9

C  Check to see if we have control over the set call

      if (doset) then

C  If the map is almost sqare or is taller than it is wide, guarantee at least
C  10% of the scren on the top and bottom.
C  Otherwise, guarantee ourselves at least 5% on the top and bottom

        if (temp .ge. dumy) then
          call mappos (0.1,0.9,0.1,0.9)
        else
          call mappos (0.05,0.95,0.05,0.95)
        end if
      else

C  We don't have control over the set, do it like the user wants

        call getset (flsv,frsv,fbsv,ftsv,ursv,ulsv,ubsv,utsv,llsv)

C  Adjust to use the proper percentage of the domain by the same tests as
C  we would if a set call had not been made

        if (temp .ge. dumy) then
          test = 0.1 * (frsv - flsv)
          frsv = frsv - test
          flsv = flsv + test
          test = 0.1 * (ftsv - fbsv)
          ftsv = ftsv - test
          fbsv = fbsv + test
        else
          test = 0.05 * (frsv - flsv)
          frsv = frsv - test
          flsv = flsv + test
          test = 0.05 * (ftsv - fbsv)
          ftsv = ftsv - test
          fbsv = fbsv + test
        end if
      
C  Set up the users set call with EZMAP

        call set (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
        call mappos (flsv,frsv,fbsv,ftsv)
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine tbllok (unum,tabnam,errsev,there,whline,util)

C*****************************************************************************C
C  tbllok   - This is a MAPDRV/CONDRV routine                                 C
C  Section  - Tables                                                          C
C  Purpose  - To check and see if the requested table is there and place  the C
C             file pointer to the first information line in the table.        C
C                                                                             C
C  On entry - UNUM is the unit number of the table file.  TABNAM cantains the C
C             name of the table to check for.   ERRSEV  is the error severity C
C             indicator.                                                      C
C                                                                             C
C  On exit  - THERE tells the calling routine  whether the table is  there or C
C             is there or not.  WHLINE is the first information line from the C
C             table.                                                          C
C                                                                             C
C  Assume   - Nothing.                                                        C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             ERRHAN              MAPDRV/CONDRV utility                       C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill       Date - July 13, 1990       for the MM4 club C
C*****************************************************************************C

C  Character variables

      character*10     tabnam            ! name of the table to look for   (in)
      character*6      util              ! name of utility calling TBLLOK  (in)
      character*80     whline            ! line from the table            (out)
      character*60     ermes             ! error/warning message        (local)

C  Integer variables

      integer          unum,             ! unit number of table file       (in)
     *                 errsev            ! error severity indicator        (in)
      integer          start,            ! char. in ERMES to start      (local)
     *                 i                 ! loop counter/place keeper    (local)

C  Logical variables

      logical          there             ! is the table there ?           (out)

C****************************  subroutine begin  *****************************C

C  Prepare ERMES in case of an error

      if (tabnam(1:10) .eq. 'MAP DETAIL') then
        ermes(1:23) = 'Reading Details Table, '
        start = 24
      else if (tabnam(1:10) .eq. 'MAP FILL  ') then
        ermes(1:20) = 'Reading Fill Table, '
        start = 21
      else if (tabnam(1:10) .eq. 'CON DETAIL') then
        ermes(1:23) = 'Reading Details Table, '
        start = 24
      else
        ermes(1:21) = 'Reading Color Table, '
        start = 22
      end if

C  If UNUM is negative then it is assumed that the table is not there

      if (unum .lt. 0) then
        there = .false.
      else
C  It is expected that each table be preceded by three lines:
C  Line 1 - Anything
C  Line 2 - Table Title
C  Line 3 - Anything
C  Line 4 - Anything

C  Line 1 -
C  Try to read from the file,
C  if EOF the table is not there

        read (unum,10,end=20,err=30) whline(1:1)

C  Line 2 -
C  if EOF then give a warning

        read (unum,100,end=50,err=30) whline(1:80)

C  Check if this is the table we are looking for

        i = 1
70      continue
        if (whline(i:i) .eq. ' ') then
          i = i + 1
          if (i .gt. 73) then

C  No title was found on the line where it was expected

            ermes(start:start+33) = 'Table Title Expected But Not Found'
            do 80 i = start+34,60
              ermes(i:i) = ' '
80          continue
            call errhan (util,0,ermes,errsev)
            goto 20
          end if
          goto 70
        end if

C  A title was found on the line

        if (whline(i:i+9) .ne. tabnam(1:10)) then

C  The title found was not the one we wanted

          backspace (unum)
          backspace (unum)
          goto 20
        end if

C  Line 3 & 4 -
C  The title found was the one we wanted, read up to the infformation

        read (unum,10,end=50,err=30) whline(1:1)
        read (unum,10,end=50,err=30) whline(1:1)

C  Read in the first information line

        read (unum,100,end=50,err=30) whline(1:80)
      
C  Let the calling routine know the table is there

        there = .true.
        goto 90

C  Warnings

50      ermes(start:start+29) = 'Unexpected End Of File Reached'
        do 60 i = start+30,60
          ermes(i:i) = ' '
60      continue
        call errhan (util,0,ermes,errsev)
        goto 20

C  Errors

30      ermes(start:start+25) = 'Possibly A Bad Unit Number'
        do 40 i = start+26,60
          ermes(i:i) = ' '
40      continue
        call errhan (util,1,ermes,errsev)
20      there = .false.
      end if

C*****************************  subroutine end  ******************************C

C  Format statements begin ...

10    format (A1)
100   format (A80)

C  Format statements end.

90    return
      end
      subroutine xytoll (j,i,lat,lon,project)

C*****************************************************************************C
C  xytoll   - This is a MAPDRV routine                                        C
C  Section  - Labels                                                          C
C  Purpose  - To  transform  mesoscale gird point coordinates into  latitude, C
C             longitude coordinates.                                          C
C                                                                             C
C  On entry - J  and  I are an ordered pair representing a grid point in  the C
C             mesoscale grid.  XYLLON is a common block that contains the in- C
C             formation necessary for describing the domain.                  C
C                                                                             C
C  On exit  - LAT, LON contain  the latitude and longitude respectively  that C
C             resulted from the transformation.                               C
C                                                                             C
C  Assume   - Nothing.                                                        C
C                                                                             C
C  Notes    - The formula's used in this routine were taken from the  PROGRAM C
C             TERRAIN DOCUMENTATION AND USER'S GUIDE.                         C
C                                                                             C
C  Author   - Jeremy Asbill   Date - September 17, 1990      for the MM4 club C
C*****************************************************************************C

C  Parameters

      parameter       (pi = 3.14159265)  ! you know!  pi = 180 degrees
      parameter       (re = 6370.949)    ! the radius of the earth in km
      parameter       (ce = 40029.85315) ! the circumference of the earth in km

C  Character variables

      character*2      project           ! projection indicator            (in)

C  Integer variables

      integer          imax,             ! for common block XYLLON
     *                 jmax              ! for common block XYLLON

C  Real variables

      real             j,                ! x coord. to be changed          (in)
     *                 i                 ! y coord. to be changed          (in)
      real             lat,              ! resulting latitude             (out)
     *                 lon               ! resulting longitude            (out)
      real             grds,             ! for common block XYLLON
     *                 clat,             ! for common block XYLLON
     *                 clon              ! for common block XYLLON
      real             confac,           ! for common block LAMSTF
     *                 fsparl,           ! for common block LAMSTF
     *                 ssparl            ! for common block LAMSTF
      real             rcln,             ! center longitude in radians  (local)
     *                 rclt,             ! center latitude in radians   (local)
     *                 cj,               ! center x coord. for grid     (local)
     *                 ci,               ! center y coord. for grid     (local)
     *                 dj,               ! distance from the central
C                                          meridian to the point        (local)
     *                 di,               ! distance from pole to point  (local)
     *                 bm                ! calculation variable         (local)

C  Common blocks

      common /xyllon/  grds,             ! grid distance in kilometers
     *                 clat,             ! center latitude
     *                 clon,             ! center longitude
     *                 imax,             ! maximum vertical gird point
     *                 jmax              ! maximum horizontal grid point
      common /lamstf/  confac,           ! cone factor to be used
     *                 fsparl,           ! first standard parallel lat.
     *                 ssparl            ! second standard parallel lat.

C****************************  subroutine begin  *****************************C

C  Convert the center latitude and longitude of the domain to radians

      rclt = clat * pi/180.0
      rcln = clon * pi/180.0

C  Find the center values of the grid in mesoscale grid coordinates

      cj = float(jmax + 1) * 0.5
      ci = float(imax + 1) * 0.5

C  Calculate the distance from the vertical axis to (J,I)

      dj = (j - cj) * grds

C  The rest is figured out differently for each type of projection, so ...
C  If the projection is mercator ('ME') then ...

      if (project(1:2) .eq. 'ME') then

C  Calculate the distance the point in question is from the pole

        di = -re * log(cos(rclt)/(1 + sin(rclt))) +
     *       (i - ci) * grds

C  Calculate the latitude desired in radians

        lat = 2.0 * atan(exp(di/re)) - pi * 0.5

C  Calculate the longitude desired in radians

        lon = rcln + dj/re

C  If the projection is cylindrical equidistant ('CE') then ...

      else if (project(1:2) .eq. 'CE') then

C  Calculate the distance from the horizontal axis to (J,I)

        di = (i - ci) * grds

C  Determine the shift north-south

        lat = rclt + (pi * di/(ce * 0.5))

C  Determine the shift east-west

        lon = rcln + (2 * pi * dj/ce)

C  If the projection is lambert conic conformal ('LC') then ...

      else if (project(1:2) .eq. 'LC') then

C  Calculate the distance from the pole to J,I

        if (clat .ge. 0.0) then
          di = -re/confac * sin(pi * 0.5 - (fsparl * pi/180.0)) *
     *         (tan((pi * 0.5 - rclt) * 0.5) /
     *          tan((pi * 0.5 - (fsparl * pi/180.0)) * 0.5))**confac +
     *         (i - ci) * grds
        else
          di = -re/confac * sin(-pi * 0.5 - (fsparl * pi/180.0)) *
     *         (tan((-pi * 0.5 - rclt) * 0.5) /
     *          tan((-pi * 0.5 - (fsparl * pi/180.0)) * 0.5))**confac +
     *         (i - ci) * grds
        end if

C  Calculate out the Big Messy equation refered to as c1 in the document
C  from which this formula was taken

        bm = tan((pi * 0.5 - abs(fsparl * pi/180.0))/2.0) *
     *       (confac/re * sqrt(dj**2 + di**2) /
     *        sin(pi * 0.5 - abs(fsparl * pi/180.0)))**(1.0/confac)

C  Calculate the desired latitude in radians

        if (clat .ge. 0.0) then
          lat = pi * 0.5 - 2.0 * atan(bm)
        else
          lat = -pi * 0.5 + 2.0 * atan(bm)
        end if

C  Calculate the desired longitude in radians

        if (clat .ge. 0.0) then
          lon = rcln + (1.0/confac) * atan2(dj,-di)
        else
          lon = rcln + (1.0/confac) * atan2(dj,di)
        end if

C  If the projection is polar stereographic ('ST') then ...

      else if (project(1:2) .eq. 'ST') then

C  Calculate the distance J,I lies from the "true" point

        if (clat .gt. 0.0) then
          di = -re * sin(pi * 0.5 - rclt) *
     *         (1.0 + cos(pi * 0.5 - (fsparl * pi/180.0))) /
     *         (1.0 + cos(pi * 0.5 - rclt)) +
     *         (i - ci) * grds
        else
          di = -re * sin(-pi * 0.5 - rclt) *
     *         (1.0 + cos(-pi * 0.5 - (fsparl * pi/180.0))) /
     *         (1.0 + cos(-pi * 0.5 - rclt)) +
     *         (i - ci) * grds
        end if

C  Calculate the Big Messy quantity as would be done, for lambert conformal
C  projections.  This quantity is different in value, same in purpose of
C  BM above

        if (clat .ge. 0.0) then
          bm = (1/re) * sqrt(dj**2 + di**2) /
     *         (1.0 + cos(pi * 0.5 - (fsparl * pi/180.0)))
        else
          bm = (1/re) * sqrt(dj**2 + di**2) /
     *         (1.0 + cos(-pi * 0.5 - (fsparl * pi/180.0)))
        end if

C  Calculate the desired latitude in radians

        if (clat .ge. 0.0) then
          lat = pi * 0.5 - 2.0 * atan(bm)
        else
          lat = -pi * 0.5 + 2.0 * atan(bm)
        end if

C  Calculate the desired longitude in radians

        if (clat .ge. 0.0) then
          lon = rcln + atan2(dj,-di)
        else
          lon = rcln + atan2(dj,di)
        end if
      end if

C  Convert the calculated lat,lon pair into degrees

      lat = lat * 180.0/pi
      lon = lon * 180.0/pi

C  Make sure no values are greater than 180 degrees and none
C  are less than -180 degrees

      if (lon .gt. 180.0)  lon = lon - 360.0
      if (lon .lt. -180.0) lon = lon + 360.0

C*****************************  Subroutine End  ******************************C

      return
      end
      subroutine condrv (indata,xdim,ydim,xstr,ystr,xend,yend,pnum,
     *                   lmeth,levels,zl,mask,scale,title,tsize,unum,
     *                   doset,errsev)

C*****************************************************************************C
C  condrv   - Contour Driver                                                  C
C                                                                             C
C  Purpose  - This utility is intended as an interface to the CONPACK utility C
C             in NCAR Graphics.   For reference to that utility look to  NCAR C
C             Graphics Guide to New Utilities in the Contours section.   This C
C             utility will allow the user to access all of the  power  of the C
C             CONPACK utility through one call to this routine.  Data direct- C
C             ives for this routine are passed in as  parameters and graphics C
C             directives are read in from a table file.  This utility  should C
C             be one hundred percent general and completely portable.         C
C                                                                             C
C  On entry - INDATA contains the data to be plotted.   XDIM and YDIM are the C
C             x and y dimensions of  INDATA.   XSTR and  YSTR are the x and y C
C             grid point at which to start plotting (lower left).    XEND and C
C             YEND are the x and y grid point at which to quit plotting  (up- C
C             per right).   PNUM contains the number that denotes which over- C
C             this plot is.  LMETH specifies the method to use when determin- C
C             ing contour levels.  Choices for LMETH are:                     C
C             LMETH = 0  : CONDRV picks everything, defaults.   LEVELS is ig- C
C               nored.                                                        C
C             LMETH = -1 : A contour interval is given to use between a given C
C               contour minimum and a given contour maximum. LEVELS(1) is the C
C               contour  interval.    LEVELS(2)  is the contour  maximum  and C
C               LEVELS(3) is the contour minimum.                             C
C             LMETH = -2 : A contour interval is specified, CONDRV picks  the C
C               contour minimum and maximum.   LEVELS(1) is that contour  in- C
C               terval.                                                       C
C             LMETH > 0  : LMETH is the number of levels the user wants.      C
C               LEVELS is ignored.                                            C
C             LMETH < -2 : ABS(LMETH) - 2 is  the  number of  levels the user C
C               wants.  Each level is specified individually in LEVELS.       C
C             ZL indicates whether a zero line should appear  on the plot  or C
C             not.  SCALE is a scale factor to use when labeling the contours C
C             and  TITLE  is the title string for the picture.   TSIZE is the C
C             number of characters in TITLE.  If  TSIZE  is negative the con- C
C             tour minimum, maximum and interval will be used as a title.  If C
C             TSIZE is zero no title will be drawn.  UNUM is the unit  number C
C             where the information tables may be found if UNUM is  positive. C
C             If UNUM is negative, then no tables are to be used and defaults C
C             are used.  If UNUM is 0 then the settings made by the last read C
C             through tables should be used.  DOSET = 1 if CONDRV should make C
C             the set call for the plot and is 0  if it should use the user's C
C             set call and is -1  if it should make the set call  considering C
C             cross points.   ERRSEV indicates what severity of  error should C
C             halt CONDRV execution.                                          C
C               ERRSEV > 0 means, nothing stops execution                     C
C               ERRSEV = 0 means, errors stop execution, warnings do not      C
C               ERRSEV < 0 means, both errors and warnings stop execution.    C
C             MASK indicates if a map mask is to be made when contouring.  In C
C             other words, if MASK is :                                       C
C               NO - the contouring will be done like normal.                 C
C               LO - contouring will be done over the land only               C
C               LL - contouring will be done over the land and lakes          C
C               OO - contouring will be done over the oceans only             C
C               OL - contouring will be done over all water bodies.           C
C             If MASK is not NO the MAPDRV must be called before CONDRV.      C
C                                                                             C
C  On exit  - A nice contour plot has been drawn to a CGM file including, all C
C             requested labels and a title.                                   C
C                                                                             C
C  Assume   - GKS is open.  A color table has been defined.                   C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             INTERC              CONDRV utility                              C
C             CRDDET              CONDRV utility                              C
C             CRDCLT              CONDRV utility                              C
C             CRDPRT              CONDRV utility                              C
C             CSETWN              CONDRV utility                              C
C             SETCON              CONDRV utility                              C
C             SUBCON              CONDRV utility                              C
C             CSETCL              CONDRV utility                              C
C             CPSETR              CONPACK utility*                            C
C             CONFIL              CONDRV utility                              C
C             CONDRW              CONDRV utility                              C
C             SETLIN              CONDRV utility                              C
C             CONLBL              CONDRV utility                              C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill       Date - June 5, 1990       for the MM4 club  C
C*****************************************************************************C

C  Character variables

      character*120    title             ! title string for plot           (in)
      character*2      mask              ! map masking indicator           (in)
      character*2      cmask             ! changeable version of MASK   (local)

C  Integer variables

      integer          xdim,             ! the x dimension of INDATA       (in)
     *                 xend,             ! x coord. of last grid point     (in)
     *                 xstr,             ! x coord. of first grid point    (in)
     *                 ydim,             ! the y dimension of INDATA       (in)
     *                 yend,             ! y coord. of last grid point     (in)
     *                 ystr,             ! y coord. of first grid point    (in)
     *                 pnum,             ! number of overlays current      (in)
     *                 lmeth,            ! method of level specification   (in)
     *                 tsize,            ! # of characters in TITLE        (in)
     *                 unum,             ! unit number of info. files      (in)
     *                 doset,            ! set call indicator              (in)
     *                 errsev            ! error severity comparitor       (in)
      integer          cmeth,            ! coloring method indicator    (local)
     *                 ctsiz             ! changeable version of TSIZE  (local)

C  Logical variables

      logical          zl                ! draw in the zero line ?         (in)
      logical          noplt             ! for common block NOPLOT
      logical          color,            ! is this a color plot ?       (local)
     *                 lover             ! draw lines over the plot ?   (local)

C  Real variables

      real             indata(xdim,ydim),! data array                      (in)
     *                 levels(100),      ! individually specified levels   (in)
     *                 scale             ! scaling factor for labeling     (in)

C  Common blocks

      common /noplot/  noplt             ! don't draw anything ?

C****************************  subroutine begin  *****************************C

C  Initialize changeables

      ctsiz      = tsize
      cmask(1:2) = mask(1:2)

C  Check for any errors that may exist before starting

      call interc (xstr,ystr,xend,yend,xdim,ydim,lmeth,pnum,
     *             ctsiz,cmask,errsev)

C  Set up detail variables

      if (unum .ne. 0) then
        call crddet (unum,errsev,pnum,hfilb,tfilb,lfilb)

C  Set up color variables with correct color indices

        call crdclt (unum,errsev,hfilb,tfilb,lfilb,zl,cmeth)

C  Set up any contouring partitions that might be needed

        call crdprt (unum,errsev,cmeth,scale)
      end if

C  If an non-correctable error has occurred skip all Design and Draw
C  routines

      if (.not. noplt) then

C  Set up the window to use when plotting

        call csetwn (xstr,ystr,xend,yend,doset)

C  Set up contouring method, including setting up the contour levels

        call setcon (lmeth,cmeth,levels,errsev)

C  Set up the scaling factor

        call cpsetr ('SFS',scale)

C  Set up the correct subset of the data to be plotted
C  and initialize CONPACK

        call subcon (indata,xdim,ydim,xstr,ystr,xend,yend)

C  Set up the line width, dash pattern and the zero line

        call setlin (zl)
      end if

C  Title will be drawn even if no plot is to be made
C  Set up colors and labels and title

      call csetcl (ctsiz,title,pnum,scale,errsev)

C  If requested fill the plot (shade or solid fill)

      if (.not. noplt) then
        call confil (lover,cmask)

C  Draw contour lines

        if (lover) call condrw (cmask)

C  Draw in the labels

        call conlbl (xstr,ystr,xend,yend)
        print *, 'CONDRV - Plot Successfully Completed'
      end if

C*****************************  suroutine end  *******************************C

      return
      end
      subroutine condrw (mask)

C*****************************************************************************C
C  condrw   - This is a CONDRV routine                                        C
C  Section  - Contour Lines                                                   C
C  Purpose  - To draw contour lines to a plot.                                C
C                                                                             C
C  On entry - Needed information is passed in through common blocks. MASK in- C
C             dicates how to draw the contours with respect to a map.         C
C                                                                             C
C  On exit  - The contour lines have been drawn.                              C
C                                                                             C
C  Assume   - GKS is open and everything is set up with CONPACK.              C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             ARINAM              AREAS utility*                              C
C             CPLBAM              CONPACK utility*                            C
C             CPCLDM              CONPACK utility*                            C
C             CPCLDR              CONPACK utility*                            C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill      Date - August 12, 1990     for the MM4 club  C
C*****************************************************************************C

C  Character variables

      character*2      mask              ! map masking indicator           (in)
      character*2      cmask             ! for common block MAPFLI

C  Integer variables

      integer          lputl,            ! for common block QLBDET
     *                 tputl             ! for common block QLBDET
      integer          iwork(1000)       ! for common block DATAKP
      integer          amapl(100000)     ! for common block CLNAMP
      integer          aid(20),          ! area identifiers             (local)
     *                 gid(20)           ! group identifiers            (local)

C  Logical variables

      logical          hputl             ! for common block QLBDET
      logical          hputb             ! for common block HLBDET

C  Real variables

      real             mywork(1000,1000),! for common block DATAKP
     *                 rwork(5000)       ! for common block DATAKP
      real             xscm(2000),       ! work space for AREAS         (local)
     *                 yscm(2000)        ! work space for AREAS         (local)

C  Common block

      common /qlbdet/  hputl,            ! draw in high/low labels ?
     *                 lputl,            ! draw in line labels
     *                 tputl             ! draw in the title
      common /hlbdet/  hputb             ! draw a box around high/low labels ?
      common /datakp/  mywork,           ! array of data to plot
     *                 iwork,            ! integer work space for CONPACK
     *                 rwork             ! real work space for CONPACK
      common /clnamp/  amapl             ! area map for line drawing
      common /mapfli/  cmask             ! common block version of MASK

C  External routines

      external         drawcl            ! draws contour lines masked

C****************************  Subroutine Begin  *****************************C

C  Initialize CMASK

      cmask(1:2) = mask(1:2)

C  Initialize the area map

      if ((mask(1:1) .eq. 'N') .or. (mask(1:1) .eq. 'n'))
     *  call arinam (amapl,100000)

C  Put the label boxes in the area map

      if ((hputb) .or. (lputl .gt. 0) .or.
     *    ((mask(1:1) .ne. 'N') .and. (mask(1:1) .ne. 'n'))) then
        call cplbam (mywork,rwork,iwork,amapl)

C  Use area map to mask high/lows in drawing

        call cpcldm (mywork,rwork,iwork,amapl,drawcl)
      else

C  Do not mask high/lows

        call cpcldr (mywork,rwork,iwork)
      end if

C*****************************  Subroutine End  ******************************C

      return
      end
      subroutine confil (lover,mask)

C*****************************************************************************C
C  confil   - This is a CONDRV routine                                        C
C  Section  - Fill                                                            C
C  Purpose  - To direct the color and shade fill for the plot.                C
C                                                                             C
C  On entry - The data to be plotted is in common block DATAKP.  The fill in- C
C             formation is in common block FILDET.  MASK indicated how the a- C
C             rea map should be used with respect to a map.                   C
C                                                                             C
C  On exit  - LOVER is true if lines should be drawn over a filled plot  & is C
C             true if the plot was not filled and is false otherwise.         C
C                                                                             C
C  Assume   - GKS is open.                                                    C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             ARINAM              AREAS utility*                              C
C             CPCLAM              CONPACK utility*                            C
C             CPLBAM              CONPACK utility*                            C
C             SFSETP              SOFTFILL utility*                           C
C             SFSETI              SOFTFILL utility*                           C
C             ARSCAM              AREAS utility*                              C
C             GSFAIS              GKS                                         C
C             MKFCOL              CONDRV utility                              C
C             ----------------------------------------------------------------C
C             * NCAR Graphics Routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill      Date - August 12, 1990     for the MM4 club  C
C*****************************************************************************C

C  Character variables

      character*2      mask              ! map masking indicator           (in)
      character*2      cmask             ! for common block MAPFLI

C  Integer variables

      integer          lputl,            ! for common block QLBDET
     *                 tputl             ! for common block QLBDET
      integer          iwork(1000)       ! for common block DATAKP
      integer          amapf(500000)     ! for common block CSCAMP
      integer          aid(20),          ! area identifiers for AREAS   (local)
     *                 gid(20),          ! group identifiers for AREAS  (local)
     *                 pat(8,8),         ! dot pattern for shading      (local)
     *                 i,j               ! loop counters                (local)

C  Logical variables

      logical          lover             ! should lines be drawn ?        (out)
      logical          fill,             ! for common block FILDET
     *                 lshd,             ! for common block FILDET
     *                 color             ! for common block FILDET
      logical          hputl             ! for common block QLBDET
      logical          hputb             ! for common block HLBDET

C  Real variables

      real             mywork(1000,1000),! for common block DATAKP
     *                 rwork(5000)       ! for common block DATAKP
      real             xscam(200000),    ! work space for ARSCAM        (local)
     *                 yscam(200000)     ! work space for ARSCAM        (local)

C  Common blocks

      common /fildet/  fill,             ! will the plot be filled ?
     *                 lshd,             ! draw contour lines over a fill ?
     *                 color             ! make the plot in color ?
      common /qlbdet/  hputl,            ! draw in high/low labels ?
     *                 lputl,            ! draw in line labels
     *                 tputl             ! draw in the title
      common /hlbdet/  hputb             ! draw a box around high/low labels ?
      common /datakp/  mywork,           ! array of data to plot
     *                 iwork,            ! integer work space for CONPACK
     *                 rwork             ! real work space for CONPACK
      common /cscamp/  amapf             ! area map for shade and color
      common /mapfli/  cmask             ! common block version of MASK

C  External routines

      external         shadem            ! does shade filling
      external         fillem            ! does color filling

C****************************  Subroutine Begin  *****************************C

C  SOFTFILL internal parameters used in this routine are :
C  TY  - TYpe of fill
C  DO  - DOtted fill flag

C  If no filling was requested, do nothing

      if (fill) then

C  Initialize CMASK

        cmask(1:2) = mask(1:2)

C  Initialize the area map

        if ((mask(1:1) .eq. 'N') .or. (mask(1:1) .eq. 'n'))
     *    call arinam (amapf,500000)

C  And set up the area identifiers

        if (color) call mkfcol

C  Put contour lines to the area map

        call cpclam (mywork,rwork,iwork,amapf)

C  Put the label boxes in the area map

        if ((hputb) .or. (lputl .gt. 0))
     *    call cplbam (mywork,rwork,iwork,amapf)

C  Shade the plot if requested

        if (.not. color) then

C  If a label bar was made all should be set up

          if (tputl .ne. 0) then

C  Make the fill a pattern fill

            call gsfais (0)

C  Set up a constant dot pattern

            do 10 i = 1,8
              do 20 j = 1,8
                pat(i,j) = 1
20            continue
10          continue

C  Set up the dot pattern with SOFTFILL

            call sfsetp (pat)

C  Tell SOFTFILL to use dots in shading

            call sfseti ('TY',1)
            call sfseti ('DO',1)
          end if
          call arscam (amapf,xscam,yscam,200000,aid,gid,20,shadem)
        end if

C  Force a solid fill if not shading

        if (color) then
          call gsfais (1)

C  Fill the plot if requested

          call arscam (amapf,xscam,yscam,200000,aid,gid,20,fillem)
        end if
      end if

C  Determine if lines should be drawn

      if ((fill) .and. (lshd)) then
        lover = .true.
      else if (.not. fill) then
        lover = .true.
      else
        lover = .false.
      end if

C*****************************  Subroutine End  ******************************C

      return
      end
      subroutine conlbl (xstr,ystr,xend,yend)

C*****************************************************************************C
C  conlbl   - This is a CONDRV routine                                        C
C  Section  - Labels                                                          C
C  Purpose  - To draw in, high/low labels,  line labels,  the title,  and the C
C             perimeter.                                                      C
C                                                                             C
C  On entry - Needed information is passed in through common blocks.   Common C
C             block  PERDET has the flag that indicates if a perimeter should C
C             should be drawn.  XSTR & YSTR represent the first point of  the C
C             grid to be plotted. XEND & YEND represent the last point of the C
C             the grid to be plotted.                                         C
C                                                                             C
C  On exit  - The labels have been drawn.                                     C
C                                                                             C
C  Assume   - GKS is open.  The plot has been completed except for the labels C
C             and they are all set up with CONPACK.                           C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             GSFAIS              GKS                                         C
C             CPLBDR              CONPACK utility*                            C
C             DRWTTL              CONDRV utility                              C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill      Date - August 12, 1990     for the MM4 club  C
C*****************************************************************************C

C  Integer variables

      integer          xstr,             ! x coord. of first plotted point (in)
     *                 ystr,             ! y coord. of first plotted point (in)
     *                 xend,             ! x coord. of last plotted point  (in)
     *                 yend              ! y coord. of last plotted point  (in)
      integer          lputl,            ! for common block QLBDET
     *                 tputl             ! for common block QLBDET
      integer          iwork(1000)       ! for common block DATAKP

C  Logical variables

      logical          hputl             ! for common block QLBDET
      logical          prput             ! for common block PERDET

C  Real variables

      real             mywork(1000,1000),! for common block DATAKP
     *                 rwork(5000)       ! for common block DATAKP

C  Common blocks

      common /qlbdet/  hputl,            ! draw in high/low labels ?
     *                 lputl,            ! draw in line labels
     *                 tputl             ! draw in the title
      common /datakp/  mywork,           ! array of data to plot
     *                 iwork,            ! integer work space for CONPACK
     *                 rwork             ! real work space for CONPACK
      common /perdet/  prput             ! put in a perimeter ?

C****************************  Subroutine Begin  *****************************C

C  Force a solid fill here for label box fills

      call gsfais (1)

C  If the information label is suppose to be drawn, do so

      if (tputl .gt. 0) call drwttl

C  If no labels are to be drawn do nothing

      if ((lputl .gt. 0) .or. (hputl)) then

C  Fill in the labels

        call cplbdr (mywork,rwork,iwork)
      end if

C  Draw a perimeter now if one was requested

      if (prput) call perim (0,xend-xstr,0,yend-ystr)

C*****************************  Subroutine End  ******************************C

      return
      end
      subroutine connum (value,number,length)

C*****************************************************************************C
C  connum   - This is a CONDRV routine                                        C
C  Section  - Design                                                          C
C  Purpose  - To convert real numbers into nice strings for PLOTCHAR use      C
C                                                                             C
C  On entry - VALUE is the number to convert                                  C
C                                                                             C
C  On exit  - NUMBER is the string representing VALUE.  LENGTH is the number  C
C             of characters the conversion used.                              C
C                                                                             C
C  Assume   - Then string is to be for PLOTCHAR.                              C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             PCGETC              PLOTCHAR utility*                           C
C             ----------------------------------------------------------------C
C             * NCAR Graphics Routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill      Date - October 31, 1990     for the MM4 club C
C*****************************************************************************C

C  Character variables

      character          cchar           ! PLOTCHAR command delimeter   (local)
      character*20       number          ! the converted number           (out)

C  Integer variables

      integer            length          ! the number of characters used  (out)
      integer            adjust,         ! for counting exponents       (local)
     *                   expon,          ! the numbers exponent         (local)
     *                   start,          ! string place keeper          (local)
     *                   i               ! loop counter                 (local)

C  Real variables

      real               value           ! the number to convert           (in)
      real               divis,          ! divisor for finding exponent (local)
     *                   manti,          ! the numbers mantissa         (local)
     *                   test            ! calculation variable         (local)

C****************************  Subroutine Begin  *****************************C

C  The following PLOTCHAR internal parameters are used:
C  FC  - Function Code delimeter character

C  Retrieve the current PLOTCHAR command character

      call pcgetc ('FC',cchar)

C  Determine the divisor for finding the exponent

      if ((abs(value) .lt. 1.0) .and. (abs(value) .gt. 0.0)) then
        divis  = 0.1
        adjust = -1
      else
        divis  = 10.0
        adjust = 1
      end if

C  Determine the exponent

      expon = 0
      test  = abs(value)
10    continue
      if (((test .lt. 1.0) .or. (test .ge. 10.0)) .and.
     *    (test .ne. 0.0)) then
        test  = test/divis
        expon = expon + adjust
        goto 10
      end if

C  Get the mantissa from the previous loop

      if (value .lt. 0.0) then
        manti = -1.0 * test
      else
        manti = test
      end if

C  Check to see if number should be drawn in exponential format

      if ((expon .gt. 4) .or. (expon .lt. -3)) then

C  Write the mantissa into the string

        if (manti .lt. 0.0) then
          write (number(1:6),20) manti
          start = 7
        else
          write (number(1:5),30) manti
          start = 6
        end if

C  Clean trailing zeros

        i = start - 1
5       continue
        if ((number(i:i) .eq. '0') .or. (number(i:i) .eq. ' ') .or.
     *      (number(i:i) .eq. '.')) then
          i = i - 1
          goto 5
        end if
        start                   = i + 1
        number(start:start+4)   = ' x 10'
        number(start+5:start+5) = cchar
        number(start+6:start+6) = 'S'
        start = start + 7

C  Write the exponent into the string

        if (expon .lt. 0) then
          if (expon .lt. -9) then
            number(start:start)     = '3'
            number(start+1:start+1) = cchar
            write (number(start+2:start+4),40) expon
            length = start + 4
          else
            number(start:start)     = '2'
            number(start+1:start+1) = cchar
            write (number(start+2:start+3),50) expon
            length = start + 3
          end if
        else
          if (expon .gt. 9) then
            number(start:start)     = '2'
            number(start+1:start+1) = cchar
            write (number(start+2:start+3),50) expon
            length = start + 3
          else
            number(start:start)     = '1'
            number(start+1:start+1) = cchar
            write (number(start+2:start+2),60) expon
            length = start + 2
          end if
        end if

C  If not exponent format write it normal

      else
        if (value .lt. 0) then
          write (number(1:10),70) value
          i = 10
        else
          write (number(1:9),80) value
          i = 9
        end if

C  Clean trailing zeros and spaces

15      continue
        if ((number(i:i) .eq. '0') .or. (number(i:i) .eq. ' ') .or.
     *      (number(i:i) .eq. '.')) then
          i = i - 1
          goto 15
        end if
        length = i
      end if

C  Clean leading blanks from the string

      if (length .eq. 0) then
        number(1:1) = '0'
        length      = 1
      else
        i = 1
25      continue
        if ((number(i:i) .eq. '0') .or. (number(i:i) .eq. ' ')) then
          i = i + 1
          goto 25
        end if
        number (1:length - i + 1) = number (i:length)
        length = length - i + 1
      end if

C*****************************  Subroutine End  ******************************C

C  Format statements begin ...

20    format (F6.3)
30    format (F5.3)
40    format (I3)
50    format (I2)
60    format (I1)
70    format (F10.3)
80    format (F9.3)

C  Format statements end

      return
      end
        subroutine cpchhl (flag)

C*****************************************************************************C
C  cpchhl   - This is for CONDRV                                              C
C  Section  - CONPACK change routines                                         C
C  Purpose  - To alter the character quality and the box fill color of the    C
C             high low labels.                                                C
C                                                                             C
C  On entry - HQUAL in the common block HLQDET contains the necessary infor-  C
C             maiton for character quality.  The common block BCOLORS has     C
C             the box fill colors for the high low labels.                    C
C                                                                             C
C  On exit  - The PLOTCHAR internal parameters CD and QU have been set up     C
C             properly.  The PLOTCHAR utility is documented in the NCAR       C
C             Graphics Guide to New Utilities Version 3.00.                   C
C             CD - Use Complex or Duplex characters                           C
C             QU - High Low or Medium Quality                                 C
C             CONPACK internal parameter LBC has been set up to fill high/low C
C             label boxes with the right color.                               C
C                                                                             C
C  Assume   - GKS is open and either CPLBDR or CPLBDM has been called and is  C
C             calling this routine.                                           C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             PCSETI              PLOTCHAR utility*                           C
C             GSFACI              GKS                                         C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C             This is a CONPACK change routine and is called by the CONPACK   C
C             utility and NOT the CONDRV utility.                             C
C                                                                             C
C  Author   - Jeremy Asbill     Date - June 15, 1990 for the MM4 club.        C 
C*****************************************************************************C

C  Integer variables

      integer          flag              ! indicates action in hi/lo draw  (in)
      integer          hqual(2)          ! for common block HLQDET
      integer          hcol(2),          ! for common block HLCOLS
     *                 lcol(2)           ! for common block HLCOLS

C  Common blocks

      common /hlqdet/  hqual             ! quality of characters
      common /hlcols/  hcol,             ! high label box fill color
     *                 lcol              ! low label box fill color

C****************************  subroutine begin  *****************************C

C  Test on flag, if flag is:
C       2 then a box for a high is about to be filled
C       3 then a label for a high is about to be drawn
C       6 then a box for a low is about to be filled
C       7 then a label for a low is about to be drawn

      if ((flag .eq. 3) .or. (flag .eq. 7)) then

C  Set the character quality

        call pcseti ('CD',hqual(1))
        call pcseti ('QU',hqual(2))
      else if (flag .eq. 2) then

C  Set up high label box fill color index

        call gsfaci (hcol(2))
      else if (flag .eq. 6) then

C  Set up low label box fill color index

        call gsfaci (lcol(2))
      end if

C*****************************  subroutine end  ******************************C

      return
      end
        subroutine cpchll (flag)

C*****************************************************************************C
C  cpchhl   - This is for CONDRV                                              C
C  Section  - CONPACK change routines                                         C
C  Purpose  - To alter the character quality of the line labels.  To properly C
C             color the fills for line labels.                                C
C                                                                             C
C  On entry - LQUAL contains the information as to how the user wants to      C
C             change the labels.  MFCOLOR and ZFCOLOR and MLCOLOR contain     C
C             the indicies to use when coloring the labels.                   C
C                                                                             C
C  On exit  - The PLOTCHAR internal parameters CD and QU have been set up     C
C             properly.  The PLOTCHAR utility is documented in the NCAR       C
C             Graphics Guide to New Utilities Version 3.00.                   C
C             CD - Use Complex or Duplex characters                           C
C             QU - High Low or Medium Quality                                 C
C             The proper indicies have been set up to color the fill boxes.   C
C                                                                             C
C  Assume   - GKS is open and either CPLBDR or CPLBDM has been called and is  C
C             calling this routine.                                           C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             PCSETI              PLOTCHAR utility*                           C
C             GSFACI              GKS                                         C
C             CPGETR              CONPACK utility*                            C
C             CPSETI              CONPACK utility*                            C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C             This is a CONPACK change routine and is called by the CONPACK   C
C             utility and NOT the CONDRV utility.                             C
C                                                                             C
C  Author   - Jeremy Asbill     Date - June 15, 1990 for the MM4 club.        C 
C*****************************************************************************C

C  Parameter

      parameter       (idcsp = -1)       ! indicates default color specified

C  Integer variables

      integer          flag              ! indicates action label drawing  (in)
      integer          lqual(2)          ! for common block LBQDET
      integer          lbco(3)           ! for common block LBCOLS
      integer          zcol(3)           ! for common block ZLCOLS
      integer          linco             ! contour line color index     (local)

C  Logical variables

      logical          hghlt,            ! for common block LBCOLS
     *                 same,             ! for common block LBCOLS
     *                 revrs             ! for common block LBCOLS

C  Real variables

      real             clev              ! contour level value          (local)

C  Common blocks

      common /lbqdet/  lqual             ! quality of characters
      common /lbcols/  hghlt,            ! highlighted labeled lines ?
     *                 same,             ! line same color as label ?
     *                 revrs,            ! text and fill reverse after zero ?
     *                 lbco              ! line label colors
      common /zrcols/  zcol              ! zero line colors


C****************************  subroutine begin  *****************************C

C  Test on flag, if flag is:
C       2 then a box is about to be filled
C       3 then a label is about to be drawn

      if (flag .eq. 3) then

C  Set the character quality

        call pcseti ('CD',lqual(1))
        call pcseti ('QU',lqual(2))

      else if (flag .eq. 2) then

C  Watch for a box to be filled

        call cpgetr ('CLV',clev)
        if (revrs) then
          if (clev .lt. 0.0) then
            call gsfaci (lbco(2))
          else if (clev .eq. 0.0) then
            call gsfaci (zcol(2))
          else if (clev .gt. 0.0) then
            call gsfaci (lbco(3))
          end if
        else
          call gsfaci (lbco(2))
        end if
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine cramps

C*****************************************************************************C
C  cramps   - This is a CONDRV routine                                        C
C  Section  - Colors                                                          C
C  Purpose  - To  calculate out ramps of colors.  The ramps change  gradually C
C             from  one  color to another and are specified by the  partition C
C             colors.                                                         C
C                                                                             C
C  On entry - All neccesary information is passed in through  common  blocks. C
C                                                                             C
C  On exit  - The variables NRMPS and  RAMPS have been set up in common block C
C             RAMPSC.  NRMPS is a list of levels/partition. RAMPS is a set of C
C             color ramps to use on the partitions.                           C
C                                                                             C
C  Assume   - GKS is open.  CONPACK has been initialized. Contour levels have C
C             been set up.                                                    C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             CPGETI              CONPACK utility*                            C
C             CPSETI              CONPACK utility*                            C
C             CPGETR              CONPACK utility*                            C
C             GQCR                GKS                                         C
C             GSCR                GKS                                         C
C             ----------------------------------------------------------------C
C             * NCAR Graphics Routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill       Date - June 8, 1990       for the MM4 club  C
C*****************************************************************************C

C  Parameters

      parameter       (imin = -1)        ! since before the first contour level
      parameter       (imax = 101)       ! until after the last contour level

C  Integer variables

      integer          pcolor(100,2)     ! for common block PARCOL
      integer          nprt,             ! for common block PARINF
     *                 iprts(100,2)      ! for common block PARINF
      integer          cmeth,            ! for common block COLIND
     *                 bckco,            ! for common block COLIND
     *                 rmeth             ! for common block COLIND
      integer          nrmps(100),       ! for common block RAMPSC
     *                 ramps(100,100)    ! for common block RAMPSC
      integer          nclv,             ! # of contour levels total    (local)
     *                 i,j,              ! loop counters/place keepers  (local)
     *                 dum,temp,         ! temporary dummy values       (local)
     *                 base              ! calculation variable         (local)

C  Logical variables

      logical          ints              ! for common block PARINF

C  Real variables

      real             rprts(100,2)      ! for common block PARINF
      real             clev,             ! temporary contour level value(local)
     *                 rindo,            ! COLONE's red portion         (local)
     *                 rindt,            ! COLTWO's red portion         (local)
     *                 rindn,            ! new color's red portion      (local)
     *                 gindo,            ! COLONE's green portion       (local)
     *                 gindt,            ! COLTWO's grren portion       (local)
     *                 gindn,            ! new color's green portion    (local)
     *                 bindo,            ! COLONE's blue portion        (local)
     *                 bindt,            ! COLTWO's blue portion        (local)
     *                 bindn,            ! new color's blue portion     (local)
     *                 mpy,              ! calculation variable         (local)
     *                 root,             ! calculation variable         (local)
     *                 divn              ! calculation variable         (local)
      
C  Comon blocks

      common /colind/  cmeth,            ! method of color plot
     *                 bckco,            ! not used
     *                 rmeth             ! method of ramping colors
      common /parinf/  nprt,             ! number of partitions
     *                 iprts,            ! integer partitions
     *                 rprts,            ! real partitions
     *                 ints              ! are the partitions integers ?
      common /parcol/  pcolor            ! colors for each partition
      common /rampsc/  nrmps,            ! # or levels/partition
     *                 ramps             ! color ramps

C****************************  subroutine begin  *****************************C

C  CONPACK internal parameters used are:
C  NCL - Number of Contour Levels
C  PAI - Parameter Array Index
C  CLV - Contour LeVels

C  Get the number of levels that are in the plot

      call cpgeti ('NCL',nclv)

C  Ramps only need to be built if the coloring method is 4,5,6 or 7
C  and there are contours

      if ((cmeth .ge. 4) .and. (cmeth .le. 7) .and. (nclv .ne. 0)) then

C  Initialize the BASE of the color ramps

        base = 100

C  Initialize partition levels counts

        do 10 i = 1,nprt
          nrmps(i) = 0
10      continue

C  Determine how many of levels exist within each partition

        if ((.not. ints) .and. (cmeth .lt. 6)) then
          do 20 i = 1,nclv
            call cpseti ('PAI',i)
            call cpgetr ('CLV',clev)
            do 30 j = 1,nprt
              if ((clev .ge. rprts(j,1)) .and.
     *            (clev .lt. rprts(j,2))) then
                nrmps(j) = nrmps(j) + 1
              end if
30          continue
20        continue
        else if ((ints) .and. (cmeth .lt. 6)) then
          do 40 i = 1,nprt
            if (iprts(i,1) .eq. imin) then
              temp = 1
            else
              temp = iprts(i,1)
            end if
            if (iprts(i,2) .eq. imax) then
              dum  = nclv + 1
            else
              dum  = iprts(i,2)
            end if
            nrmps(i) = dum - temp
40        continue
        else if (cmeth .eq. 6) then
          nprt = 5
          dum  = mod(nclv,5)
          temp = nclv/5
          nrmps(1) = temp
          nrmps(2) = temp
          nrmps(3) = temp
          nrmps(4) = temp
          nrmps(5) = temp
          do 90 i = 1,dum
            nrmps(i) = nrmps(i) + 1
90        continue
        else if (cmeth .eq. 7) then
          nprt = 5
          nrmps(1) = 20
          nrmps(2) = 20
          nrmps(3) = 20
          nrmps(4) = 20
          nrmps(5) = 20
        end if

C  Loop through the partitions, building, for each, individual ramps

        do 50 i = 1,nprt

C  Retrieve from GKS the color representations specified

          if (cmeth .lt. 6) then
            call gqcr (1,pcolor(i,1),0,ier,rindo,gindo,bindo)
            call gqcr (1,pcolor(i,2),0,ier,rindt,gindt,bindt)
            ramps(i,1) = pcolor(i,1)
          else
            if (i .eq. 1) then
              call gscr (1,250,0.0,0.0,1.0)
              ramps(i,1) = 250
              rindo      = 0.0
              gindo      = 0.0
              bindo      = 1.0
              rindt      = 0.0
              gindt      = 1.0
              bindt      = 1.0
            else if (i .eq. 2) then
              call gscr (1,251,0.0,1.0,1.0)
              ramps(i,1) = 251
              rindo      = 0.0
              gindo      = 1.0
              bindo      = 1.0
              rindt      = 0.0
              gindt      = 1.0
              bindt      = 0.0
            else if (i .eq. 3) then
              call gscr (1,252,0.0,1.0,0.0)
              ramps(i,1) = 252
              rindo      = 0.0
              gindo      = 1.0
              bindo      = 0.0
              rindt      = 1.0
              gindt      = 1.0
              bindt      = 0.0
            else if (i .eq. 4) then
              call gscr (1,253,1.0,1.0,0.0)
              ramps(i,1) = 253
              rindo      = 1.0
              gindo      = 1.0
              bindo      = 0.0
              rindt      = 1.0
              gindt      = 0.5
              bindt      = 0.0
            else
              call gscr (1,254,1.0,0.5,0.0)
              ramps(i,1) = 254
              rindo      = 1.0
              gindo      = 0.5
              bindo      = 0.0
              rindt      = 1.0
              gindt      = 0.0
              bindt      = 0.0
            end if
          end if

C  Determine the function on which to vary the colors

          if (rmeth .gt. 0) then

C  RMETH > 0 means use a sine function

            divn = 3.14159/(float(nrmps(i)) + 1)
            do 60 j = 2,nrmps(i)
              if (mod(i,2) .eq. 0) then

C  If the current partition is even go down the ramp instead of up it

                root  = (3.14159 * 0.5) + float(j) * divn
                mpy   = sin(root)
                mpy   = (1.0 - mpy)/2.0
              else

C  If the current partition is odd go up the ramp

                root  = (3.14159 * 3.0/2.0) + float(j) * divn
                mpy   = sin(root)
                mpy   = (mpy + 1.0)/2.0
              end if

C  Determine the red, green and blue components of the new color

              rindn = rindo + (rindt - rindo) * mpy
              gindn = gindo + (gindt - gindo) * mpy
              bindn = bindo + (bindt - bindo) * mpy

C  Define the color in GKS

              call gscr (1,base+j-2,rindn,gindn,bindn)

C  Add the color to the ramp

              ramps(i,j) = base + j - 2
60          continue
          else if (rmeth .lt. 0) then

C  RMETH < 0 means use an exponential function

            divn = 1.0/(float(nrmps(i)) + 1)
            do 70 j = 2,nrmps(i)
              if (mod(i,2) .eq. 0) then

C  If the current partition is even go down the ramp instead of up it

                root  = 1.0 - float(j) * divn
                mpy   = root**2

C  Calculate the red, green and blue components for the new color

                rindn = rindt + (rindo - rindt) * mpy
                gindn = gindt + (gindo - gindt) * mpy
                bindn = bindt + (bindo - bindt) * mpy
              else
          
C  If the curent partition is odd go down the ramp instead of up it

                root  = float(j) * divn
                mpy   = root**2

C  Calculate the red, green and blue components for the new color

                rindn = rindo + (rindt - rindo) * mpy
                gindn = gindo + (gindt - gindo) * mpy
                bindn = bindo + (bindt - bindo) * mpy
              end if

C  Define the new color with GKS

              call gscr (1,base+j-2,rindn,gindn,bindn)

C  Add the new color to the ramp

              ramps(i,j) = base + j - 2
70          continue
          else

C  RMETH = 0 means use a linear function

            divn = 1.0/(float(nrmps(i)) + 1)
            do 80 j = 2,nrmps(i)
              mpy   = float(j) * divn

C  Calculate the red, green and blue color components of the new color

              rindn = rindo + (rindt - rindo) * mpy
              gindn = gindo + (gindt - gindo) * mpy
              bindn = bindo + (bindt - bindo) * mpy

C  Define the new color with GKS

              call gscr (1,base+j-2,rindn,gindn,bindn)

C  Add the new color to the ramps

              ramps(i,j) = base + j - 2
80          continue
          end if

C  Increment BASE to be the first free color index after the last one in the
C  last ramp

          if ((ramps(i,nrmps(i)) .ge. 100) .and.
     *        (ramps(i,nrmps(i)) .lt. 200)) base = ramps(i,nrmps(i)) + 1
50      continue
        print *, 'CONDRV - Color Ramps Created'
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine crdclt (unum,errsev,hfilb,tfilb,lfilb,zl,ometh)

C*****************************************************************************C
C  crdclt   - This is a CONDRV routine                                        C
C  Section  - Tables                                                          C
C  Purpose  - To read in the proper color indexes to be used in a contour     C
C             plot.                                                           C
C                                                                             C
C  On entry - COLOR indicates whether to look for the CON COLORS table.  UNUM C
C             is the unit number on  which to look for  the table. ERRSEV in- C
C             dicates what severity of error should halt execution.    HFILB, C
C             TFILB  and  LFILB are logicals indicating if the high/low label C
C             boxes, the title boxes and the line  label  boxes  respectively C
C             should be fill.  ZL is true if a zero line will be drawn in the C
C             plot.                                                           C
C                                                                             C
C  On exit  - OMETH  contains the  coloring method to use.   All variables in C
C             common blocks HLCOLS, ZLCOLS, TLCOLS,  LBCOLS and  COLIND  have C
C             been set up accordingly with the table.   See listing of  those C
C             common blocks for more information.                             C
C                                                                             C
C  Assume   - Nothing                                                         C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             SEARCH              CONDRV/MAPDRV utility                       C
C             NEXT                CONDRV/MAPDRV utility                       C
C             GTREAL              CONDRV/MAPDRV utility                       C
C             ERRHAN              CONDRV/MAPDRV utility                       C
C             TBLLOK              CONDRV/MAPDRV utility                       C
C             CRDRCI              CONDRV/MAPDRV utility                       C
C             ----------------------------------------------------------------C
C                                                                             C
C  Author   - Jeremy Asbill       Date - June 5, 1990       for the MM4 club  C
C*****************************************************************************C

C  Parameters

      parameter       (idcsp = -1)       ! color index for defaults

C  Character variables

      character*80     whline            ! a whole line from the table  (local)
      character*60     ermes,            ! error message string,general (local)
     *                 p,                ! error message string,SEARCH  (local)
     *                 q                 ! error message string,NEXT    (local)

C  Integer variables

      integer          errsev,           ! error severity comparitor       (in)
     *                 unum              ! unit number of table file       (in)
      integer          ometh             ! out version of CMETH           (out)
      integer          cmeth,            ! for common block COLIND
     *                 bckco,            ! for common block COLIND
     *                 rmeth             ! for common block COLIND
      integer          lputl,            ! for common block QLBDET
     *                 tputl             ! for common block QLBDET
      integer          hcol(2),          ! for common block HLCOLS
     *                 lcol(2)           ! for common block HLCOLS
      integer          zcol(3)           ! for common block ZLCOLS
      integer          tcol(2)           ! for common block TLCOLS
      integer          lbco(3)           ! for common block LBCOLS
      integer          pcol              ! for common block PERCOL
      integer          i                 ! place keeper                 (local)

C  Logical variables

      logical          hfilb,            ! fill high/low label boxes ?     (in)
     *                 tfilb,            ! fill title boxes ?              (in)
     *                 lfilb,            ! fill line label boxes ?         (in)
     *                 zl                ! draw the zero line ?            (in)
      logical          noplt             ! for common block NOPLOT
      logical          hputl             ! for common block QLBDET
      logical          hghlt,            ! for common block LBCOLS
     *                 same,             ! for common block LBCOLS
     *                 revrs             ! for common block LBCOLS
      logical          fill,             ! for common block FILDET
     *                 lshd,             ! for common block FILDET
     *                 color             ! for common block FILDET
      logical          prput             ! for common block PERDET
      logical          error,            ! has an error occured ?       (local)
     *                 found,            ! was the table found ?        (local)
     *                 test              ! is this true ?               (local)

C  Common blocks

      common /colind/  cmeth,            ! method of color plot
     *                 bckco,            ! backup color index
     *                 rmeth             ! method of ramping colors
      common /qlbdet/  hputl,            ! draw in high/low labels ?
     *                 lputl,            ! draw in line labels
     *                 tputl             ! draw in the title
      common /hlcols/  hcol,             ! high label colors
     *                 lcol              ! low label colors
      common /zrcols/  zcol              ! zero line colors
      common /tlcols/  tcol              ! title colors
      common /lbcols/  hghlt,            ! highlighted labeled lines ?
     *                 same,             ! line same color as label ?
     *                 revrs,            ! text and fill reverse after zero ?
     *                 lbco              ! line label colors
      common /perdet/  prput             ! put in a perimeter ?
      common /percol/  pcol              ! color index for perimeter
      common /fildet/  fill,             ! will the plot be filled ?
     *                 lshd,             ! draw contour lines over a fill ?
     *                 color             ! make the plot in color ?
      common /noplot/  noplt             ! has a non-correctable erro occured ?

C****************************  subroutine begin  *****************************C

C  If a non-correctable error has occured, don't bother reading the table

      if (noplt) goto 90

C  Assume the table is not there

      found = .false.

C  The table should only be there if this is a color plot, we should only
C  parse it if this is a color plot

      if (color)

C  Look for the CON COLORS table

     *  call tbllok (unum,'CON COLORS',errsev,found,whline,'CONDRV')

C  Initialize the error flag

      error = .false.

C  If the table was there to be read, parse through it

      if (found) then

C  Initialize the place keeper

        i = 1

C  Set up SEARCH and NEXT error message strings

        p(1:22)  = 'Reading Colors Table, '
        p(23:60) = 'Too Few Entries On Line               '
        q(1:22)  = p(1:22)
        q(23:60) = 'Entry Is Bizarre                      '

C  CMETH is the first item in the color table
C  0   => Use all the defaults
C  1   => A single color to color all contour lines
C  2   => User will specify a group of partitions with one color
C         used per partition; partitions specified by level values
C  3   => User will specify a group of partitions with one color
C         used per partition; partitions specified by level numbers
C  4   => User will specify a group of partitions; color ramping will
C         be done between two colors for each partition; partitons
C         specified by level value
C  5   => User will specify a group of partitions; color ramping will
C         be done between two colors for each partition; partitions
C         specified by level number
C  6   => CONDRV chooses four partions and ramp between :
C         partition 1 : Blue to Cyan
C         partition 2 : Cyan to Green
C         partition 3 : Green to Yellow
C         partition 4 : Yellow to Red
C  7   => CONDRV will force 100 contour levels, select four partitions and
C         ramp as above
C  anything else will cause a warning message and CMETH = 1 will be assumed

        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          found = .false.
          cmeth = -1
        end if

        if (.not. error) then
          if ((whline(i:i) .eq. 'D') .or. (whline(i:i) .eq. 'd')) then
            cmeth = 1
          else
            read (whline(i:i),10,err=30) cmeth
          end if
          if ((cmeth .lt. 0) .or. (cmeth .gt. 7)) then
            ermes(1:30)  = 'Color Method Value Is Invalid,'
            ermes(31:60) = ' Options Are 0 Thru 7         '
            call errhan ('CONDRV',0,ermes,errsev)
            cmeth = 1
          end if

C  CMETH = 0 Means all the defaults should be used, FOUND as false and 
C  ERROR as true will cause execution to lead right to the defaults assignments

          if (cmeth .eq. 0) then
            found = .false.
            call next (whline,i,error)
            if (error) then
              call errhan ('CONDRV',1,q,errsev)
            else
              error = .true.
            end if
          end if
          goto 35

C  If there was an error during read give an error message

30        ermes(1:30)  = 'Color Method Value Input Conve'
          ermes(31:60) = 'rsion                         '
          call errhan ('CONDRV',1,ermes,errsev)
          cmeth = 1
          bckco = 1

          error = .true.

35        if (.not. error) then
            call next (whline,i,error)
            if (error) then
              call errhan ('CONDRV',1,q,errsev)
              bckco = 1
              rmeth = 0
            end if
          end if
        end if

C  Next in line should always be BCKCO, which is the "BACKUP COLOR"
C  BCKCO is used to color all contours when CMETH = 1
C  It is also used if any colors are accidentally left out of the table
C  and it is used for any place that has a D or a d for the color

        if (.not. error) then
          call search (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,p,errsev)
            bckco = 1
            rmeth = 0
          end if
        end if

        call crdrci (.true.,error,bckco,1,whline,i,
     *               'Backup Color Index',18,errsev,noplt,
     *               'CONDRV')
        if (error) rmeth = 0
        if (noplt) goto 90

        if (.not. error) then
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            rmeth = 0
          end if
        end if

C  Next item will be the ramping method if CMETH was specified as
C  either 4 or 5

        if (((cmeth .eq. 4) .or. (cmeth .eq. 5)) .and.
     *      (.not. error)) then
          call search (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,p,errsev)
            rmeth = 0
          end if
        end if

C  L         => Linear Ramping
C  E         => Exponential Ramping
C  S         => Sine Wavular Ramping Dude
C  D         => Linear Ramping
C  anything else results in a warning message and linear ramping

        if (((cmeth .eq. 4) .or. (cmeth .eq. 5)) .and.
     *      (.not. error)) then
          if ((whline(i:i) .eq. 'D') .or. (whline(i:i) .eq. 'd') .or.
     *        (whline(i:i) .eq. 'L') .or. (whline(i:i) .eq. 'l')) then
            rmeth = 0
          else if ((whline(i:i) .eq. 'E') .or.
     *             (whline(i:i) .eq. 'e')) then
            rmeth = -1
          else if ((whline(i:i) .eq. 'S') .or.
     *             (whline(i:i) .eq. 's')) then
            rmeth = 1
          else
            ermes(1:30)  = 'Ramping Method Flag Is Inconcl'
            ermes(31:60) = 'usive, L Assumed              '
            call errhan ('CONDRV',0,ermes,errsev)
            rmeth = 0
          end if

          call next (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,q,errsev)
        else
          rmeth = 0
        end if

C  High label text and perimeter color is next
C  D,d => use color index 1 (white)

        if ((hputl) .and. (.not. error)) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,p,errsev)
        end if

        call crdrci (hputl,error,hcol(1),1,whline,i,
     *               'High Label Color Index',22,errsev,noplt,
     *               'CONDRV')
        if (noplt) goto 90

        if ((hputl) .and. (.not. error)) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,q,errsev)
        end if

C  If there was a box requested around the high labels and it is to be filled
C  there should be a high fill color

        if ((hfilb) .and. (.not. error)) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,p,errsev)
        end if

        call crdrci (hfilb,error,hcol(2),0,whline,i,
     *               'High Label Box Fill Color Index',32,errsev,
     *               noplt,'CONDRV')
        if (noplt) goto 90

        if ((hfilb) .and. (.not. error)) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,q,errsev)
        end if

C  Low label text and perimeter color is next
C  D,d => use color index 1 (white)

        if ((hputl) .and. (.not. error)) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,p,errsev)
        end if

        call crdrci (hputl,error,lcol(1),1,whline,i,
     *               'Low Label Color Index',21,errsev,noplt,
     *               'CONDRV')
        if (noplt) goto 90

        if ((hputl) .and. (.not. error)) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,q,errsev)
        end if

C  If there was a box requested around the low labels and it is to be filled
C  there should be a low fill color

        if ((hfilb) .and. (.not. error)) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,p,errsev)
        end if

        call crdrci (hfilb,error,lcol(2),0,whline,i,
     *               'Low Label Box Fill Color Index',31,errsev,
     *               noplt,'CONDRV')
        if (noplt) goto 90

        if ((hfilb) .and. (.not. error)) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,q,errsev)
        end if

C  The zero line has three points in question
C  First - Zero Line Label text and perimeter color index
C          D,d  => Use color of zero line

        if ((zl) .and. (lputl .eq. 1) .and. (.not. error)) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,p,errsev)
        end if
  
        if ((zl) .and. (lputl .eq. 1)) then
          test = .true.
        else
          test = .false.
        end if
        call crdrci (test,error,zcol(1),idcsp,whline,i,
     *               'Zero Line Label Color Index',27,errsev,
     *               noplt,'CONDRV')
        if (noplt) goto 90

        if ((zl) .and. (lputl .eq. 1) .and. (.not. error)) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,q,errsev)
        end if

C  Second - Zero Line Label Box Fill Color index
C           D,d  => Use black

        if ((zl) .and. (lfilb) .and. (.not. error)) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,p,errsev)
        end if
  
        if ((zl) .and. (lfilb)) then
          test = .true.
        else
          test = .false.
        end if
        call crdrci (test,error,zcol(2),0,whline,i,
     *               'Zero Line Label Box Fill Color Index',36,
     *               errsev,noplt,'CONDRV')
        if (noplt) goto 90

        if ((lfilb) .and. (zl) .and. (.not. error)) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,q,errsev)
        end if

C  Third - Zero Line color index
C          D,d  => Treat the zero line as any other line

        if ((zl) .and. (.not. error) .and. (((fill) .and. (lshd)) .or.
     *      (.not. fill))) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,p,errsev)
        end if
  
        if ((zl) .and. (((fill) .and. (lshd)) .or.
     *      (.not. fill))) then
          test = .true.
        else
          test = .false.
        end if
        call crdrci (test,error,zcol(3),idcsp,whline,i,
     *               'Zero Line Color Index',21,errsev,noplt,
     *               'CONDRV')
        if (noplt) goto 90

        if ((zl) .and. (.not. error) .and. (((fill) .and. (lshd)) .or.
     *      (.not. fill))) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,q,errsev)
        end if

C  Title text and perimeter color is next
C  D,d => use color index 1 (white)

        if ((tputl .ge. 0) .and. (.not. error)) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,p,errsev)
        end if

        if (tputl .ge. 0) then
          test = .true.
        else
          test = .false.
        end if
        call crdrci (test,error,tcol(1),1,whline,i,
     *               'Title Color Index',17,errsev,noplt,
     *               'CONDRV')
        if (noplt) goto 90

        if ((tputl .ge. 0) .and. (.not. error)) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,q,errsev)
        end if

C  If there was a box requested around the title and it is to be filled
C  there should be a title fill color

        if (((tfilb) .or. (tputl .eq. 0)) .and. (.not. error)) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,p,errsev)
        end if

        if (((tfilb) .or. (tputl .eq. 0)) .and. (.not. error)) then
          test = .true.
        else
          test = .false.
        end if
        call crdrci (test,error,tcol(2),0,whline,i,
     *               'Title Box Fill Color Index',26,errsev,
     *               noplt,'CONDRV')
        if (noplt) goto 90

        if (((tfilb) .or. (tputl .eq. 0)) .and. (.not. error)) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,q,errsev)
        end if

C  Last Group - The Line Label Things
C  Initialize variables

        hghlt = .false.
        same  = .false.
        revrs = .false.

C  First - The Line Label Flag, It may be :
C  H  => Special Highlighted Lines and Labels, Don't read third
C  S  => Line color is given by the label color which is given third
C  D  => Label color is the same as the line color which is given third
C  R  => Label Text/Perimeter color and Box Fill color should be flipped
C        at the zero line, lines default and text/perimeter color is given
C        third
C  #  => the color index for the label text/perimeter

        if ((lputl .ge. 0) .and. (.not. error)) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,p,errsev)
        end if

        if ((lputl .ge. 0) .and. (.not. error)) then
          if ((whline(i:i) .eq. 'H') .or. (whline(i:i) .eq. 'h')) then
            hghlt   = .true.
            lbco(1) = 0
          else if ((whline(i:i) .eq. 'S') .or.
     *             (whline(i:i) .eq. 's')) then
            same    = .true.
            lbco(1) = 0
          else if ((whline(i:i) .eq. 'R') .or.
     *             (whline(i:i) .eq. 'r')) then
            revrs   = .true.
            lbco(1) = 0
          else
            call crdrci (.true.,error,lbco(1),idcsp,whline,i,
     *                   'Line Label Color',16,errsev,noplt,
     *                   'CONDRV')
            if (noplt) goto 90
          end if
        else
          lbco(1) = 0
        end if

C  If the user requested zero line reversal and conrec style line labels
C  tell then where they are wistling

        if ((lputl .eq. 0) .and. (revrs)) then
          ermes(1:30)  = 'Zero Line Reversal And Conrec '
          ermes(31:60) = 'Style Labeling Do Not Mix     '
          call errhan ('CONDRV',0,ermes,errsev)
          revrs = .false.
        end if

        if ((lputl .ge. 0) .and. (.not. error)) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,q,errsev)
        end if

C  Second - Line Label Box Fill color index
C           D,d  => fill in black

        if ((lfilb) .and. (.not. error)) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,p,errsev)
        end if

        call crdrci (lfilb,error,lbco(2),0,whline,i,
     *               'Line Label Fill Color Index',27,errsev,
     *               noplt,'CONDRV')
        if (noplt) goto 90

        if ((lfilb) .and. (.not. error)) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,q,errsev)
        end if

C  Third - Labeled Line Color Index
C          D,d  => treat it as any other contour line

        if ((lputl .ge. 0) .and. (.not. hghlt) .and.
     *      (.not. error)) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,p,errsev)
        end if

        if ((lputl .ge. 0) .and. (.not. hghlt)) then
          test = .true.
        else
          test = .false.
        end if
        call crdrci (test,error,lbco(3),idcsp,whline,i,
     *               'Labeled Line Color Index',24,errsev,
     *               noplt,'CONDRV')
        if (noplt) goto 90

        if ((lputl .ge. 0) .and. (.not. error)) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,q,errsev)
        end if

C  If the user requested two different colors for the label and the line
C  and requested CONREC style labels, tell them it ain't gonna work

        if ((lputl .eq. 0) .and. (lbco(1) .ne. lbco(3)) .and.
     *      (.not. same) .and. (.not. hghlt)) then
          ermes(1:30)  = 'Line And Labels Cannot Be Diff'
          ermes(31:60) = 'erent Colors With CONREC Style'
          call errhan ('CONDRV',0,ermes,errsev)
          lbco(1) = -1
        end if

C  Perimeter color is next
C  D,d => use color index 1 (white)

        if ((prput) .and. (.not. error)) then
          call search (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',1,p,errsev)
        end if

        call crdrci (prput,error,pcol,1,whline,i,
     *               'Perimeter Color Index',21,errsev,noplt,
     *               'CONDRV')
        if (noplt) goto 90

        if ((prput) .and. (.not. error)) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',0,q,errsev)
        end if

C  Check for extra entries at the end of the line

        if ((.not. error) .or. ((.not. found) .and.
     *      (cmeth .eq. 0))) then
          call search (whline,i,error)
          if (.not. error) then
            ermes(1:30)  = 'Reading Color Table, Too Many '
            ermes(31:60) = 'Entries On Line               '
            call errhan ('CONDRV',0,ermes,errsev)
          end if
        end if

C  Inform the user all is at least okay

        if (cmeth .ne. 0) print *, 'CONDRV - Contour Colors Set Up'
      end if

C  If the table was not found, assign the defaults

      if (.not. found) then
        print *, 'CONDRV - Default Contour Colors Used'
        cmeth   = 0
        bckco   = 1
        rmeth   = 0
        hcol(1) = 1
        hcol(2) = 0
        lcol(1) = 1
        lcol(2) = 0
        zcol(1) = 1
        zcol(2) = 0
        zcol(3) = 1
        tcol(1) = 1
        tcol(2) = 0
        hghlt   = .false.
        same    = .false.
        revrs   = .false.
        lbco(1) = 1
        lbco(2) = 0
        lbco(3) = 1
        pcol    = 1
      end if

C  Set up output variable

      ometh = cmeth

C*****************************  subroutine end  ******************************C

C  Format statements begin ...

10    format (I1)
20    format (I2)

C  Format statements end.

90    return
      end

      subroutine crddet (unum,errsev,pnum,hfilb,tfilb,lfilb)

C*****************************************************************************C
C  crddet   - this is a CONDRV routine                                        C
C  Section  - Tables                                                          C
C  Purpose  - To read in the entire details table including:  Color, shading, C
C             line width and dash pattern characteristics; High/Low label in- C
C             formation; Line Label information; and Title details.           C
C                                                                             C
C  On entry - UNUM is the unit number from which to read the table.    ERRSEV C
C             indicates at what severity of error execution should halt. PNUM C
C             is the number of call this was to CONDRV within one frame.      C
C                                                                             C
C  On exit  - All CONDRV variables associated with the aforementioned details C
C             have been set up.  See common blocks in this routine and in the C
C             routines CRDHLO, CRDLAB, CRDTTL for more information. The three C
C             variables, HFILB, TFILB and LFILB indicate whether boxes around C
C             the high/low labels, the title and the line labels respectively C
C             are to be filled.                                               C
C                                                                             C
C  Assume   - Nothing                                                         C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             SEARCH              CONDRV/MAPDRV utility                       C
C             NEXT                CONDRV/MAPDRV utility                       C
C             GTREAL              CONDRV/MAPDRV utility                       C
C             CRDHLO              CONDRV utility                              C
C             CRDLAB              CONDRV utility                              C
C             CRDTTL              CONDRV utility                              C
C             ERRHAN              CONDRV/MAPDRV utility                       C
C             TBLLOK              CONDRV/MAPDRV utility                       C
C             ----------------------------------------------------------------C
C                                                                             C
C  Author   - Jeremy Asbill      Date - June 6, 1990        for the MM4 club  C
C*****************************************************************************C

C  Parameter

      parameter       (wltwo = 2.0)      ! line width for even overlays

C  Character variables

      character*80     whline            ! a whole line from the table  (local)
      character*60     ermes,            ! error message string         (local)
     *                 p,                ! error message string, SEARCH (local)
     *                 q                 ! error message string, NEXT   (local)
      character*20     tstrg             ! temporary string             (local)

C  Integer variables

      integer          errsev,           ! error severity comparitor       (in)
     *                 unum,             ! unit number of info. file       (in)
     *                 pnum              ! overlay indicator               (in)
      integer          ddpv(3)           ! for common block LWDPDT
      integer          lputl,            ! for common block QLBDET
     *                 tputl             ! for common block QLBDET
      integer          dlwi,             ! line width partition indic.  (local)
     *                 ddpi,             ! dash pat. partition indic.   (local)
     *                 i,j,k,n           ! loop counter/place keepers   (local)

C  Logical variables

      logical          hfilb,            ! fill high/low label boxes ?    (out)
     *                 lfilb,            ! fill line label boxes ?        (out)
     *                 tfilb             ! fill title box ?               (out)
      logical          noplt             ! for common block NOPLOT
      logical          fshd              ! for common block SHDDIR
      logical          hputl             ! for common block QLBDET
      logical          fill,             ! for common block FILDET
     *                 lshd,             ! for common block FILDET
     *                 color             ! for common block FILDET
      logical          prput             ! for common block PERDET
      logical          error,            ! error reading table ?        (local)
     *                 found,            ! is the table there ?         (local)
     *                 done,             ! loop test flag ?             (local)
     *                 dash              ! has dash pattern been read ? (local)

C  Real variables

      real             dlwv(3)           ! for common block LWDPDT
      real             temp              ! temporary storage            (local)

C  Common blocks

      common /shddir/  fshd              ! should shading go from low to high ?
      common /fildet/  fill,             ! will the plot be filled ?
     *                 lshd,             ! draw contour lines over a fill ?
     *                 color             ! make the plot in color ?
      common /qlbdet/  hputl,            ! draw in high/low labels ?
     *                 lputl,            ! draw in line labels
     *                 tputl             ! draw in the title
      common /lwdpdt/  dlwv,             ! details line width values
     *                 ddpv              ! details dash pattern values
      common /perdet/  prput             ! put in a perimeter ?
      common /noplot/  noplt             ! has a non-correctable erro occured ?

C****************************  subroutine begin  *****************************C

C  Check if we need to read in the table at all

      if (noplt) goto 130

C  Initialze ERROR and output flags

      error = .false.
      lfilb = .false.
      hfilb = .false.
      tfilb = .false.      
                            
C  Look for the table

      call tbllok (unum,'CON DETAIL',errsev,found,whline,'CONDRV')

C  If the table was there parse the input

      if (found) then

C  Initialize the place keeper

        i = 1

C  Set up error messages got errors in SEARCH and NEXT

        p(1:23)  = 'Reading Details Table, '
        p(24:60) = 'Too Few Entries On Line              '
        q(1:23)  = p(1:23)
        q(24:60) = 'Entry Is Bizarre                     '

C  Parse the line starting with the color flag
C  Y             => the plot will be color
C  N             => the plot will not be color
C  anything else => gives a waring

        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          color   = .false.
          fill    = .false.
          hputl   = .true.
          lputl   = 0
          tputl   = 1
          prput   = .false.
        end if

        if (.not. error) then
          if ((whline(i:i) .eq. 'Y') .or. (whline(i:i) .eq. 'y')) then
            color = .true.
          else if ((whline(i:i) .eq. 'N') .or.
     *             (whline(i:i) .eq. 'n')) then
            color = .false.
          else
            ermes(1:30)  = 'Color Flag Entry Is Inconclusi'
            ermes(31:60) = 've, N Assumed                 '
            call errhan ('CONDRV',0,ermes,errsev)
            color = .false.
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            fill    = .false.
            hputl   = .true.
            lputl   = 0
            tputl   = 1
            prput   = .false.
          end if
        end if

C  Check to see if the plot will be filled
C  Y             => the plot will be filled
C  N             => the plot will not be filled
C  anything else => gives a warning message

        if (.not. error) then
          call search (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,p,errsev)
            fill    = .false.
            hputl   = .true.
            lputl   = 0
            tputl   = 1
            prput   = .false.
          end if
        end if

        if (.not. error) then
          if ((whline(i:i) .eq. 'Y') .or. (whline(i:i) .eq. 'y')) then
            fill = .true.
          else if ((whline(i:i) .eq. 'N') .or.
     *             (whline(i:i) .eq. 'n')) then
            fill = .false.
          else
            ermes(1:30)  = 'Fill Flag Entry Is Inconclusiv'
            ermes(31:60) = 'e, N Assumed                  '
            call errhan ('CONDRV',0,ermes,errsev)
            fill = .false.
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            hputl   = .true.
            lputl   = 0
            tputl   = 1
            prput   = .false.
          end if
        end if

C  If COLOR is false and FILL is true then the plot will be shaded and
C  we will need to know if we should shade from low to high or high to
C  low
C  This flag is only here if COLOR is false and FILL is true
C  H             => high gets highest shading intensity
C  L             => low gets highest shading intensity
C  anything else => give a warning message

        if ((.not. color) .and. (fill) .and. (.not. error)) then
          call search (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,p,errsev)
            hputl  = .true.
            lputl  = 0
            tputl  = 1
            prput   = .false.
          end if
        end if

        if ((.not. color) .and. (fill) .and. (.not. error)) then
          if ((whline(i:i) .eq. 'H') .or. (whline(i:i) .eq. 'h')) then
            fshd = .false.
          else if ((whline(i:i) .eq. 'L') .or.
     *             (whline(i:i) .eq. 'l')) then
            fshd = .true.
          else
            ermes(1:30)  = 'Shading Direction Indicator Is'
            ermes(31:60) = 'Missing Or Invalid, H Used    '
            call errhan ('CONDRV',0,ermes,errsev)
            fshd = .false.
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            hputl   = .true.
            lputl   = 0
            tputl   = 1
            prput   = .false.
          end if
        else
          fshd = .false.
        end if

C  If FILL is true then there will be a flag saying whether the lines
C  should be drawn in at all
C  Y             => draw in the lines
C  N             => don't draw in the lines
C  anything else => give a warning message
C  If the plot will be color filled and this flag is Y the backup color will
C  be used to draw the lines

        if ((fill) .and. (.not. error)) then
          call search (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,p,errsev)
            hputl   = .true.
            lputl   = 0
            tputl   = 1
            prput   = .false.
          end if
        end if

        if ((fill) .and. (.not. error)) then
          if ((whline(i:i) .eq. 'Y') .or. (whline(i:i) .eq. 'y')) then
            lshd = .true.
          else if ((whline(i:i) .eq. 'N') .or.
     *             (whline(i:i) .eq. 'n')) then
            lshd = .false.
          else
            ermes(1:30)  = 'Draw Fill Line Flag Is Inconcl'
            ermes(31:60) = 'usive, N Assumed              '
            call errhan ('CONDRV',0,ermes,errsev)
            lshd = .false.
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            hputl   = .true.
            lputl   = 0
            tputl   = 1
            prput   = .false.
          end if
        else
          lshd = .false.
        end if

C  Parse through in the high/low label flag
C  Y             => include the high/low labels
C  N             => don't include the high/low labels
C  anything else => give a warning message

        if (.not. error) then
          call search (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,p,errsev)
            hputl   = .true.
            lputl   = 0
            tputl   = 1
            prput   = .false.
          end if
        end if

        if (.not. error) then
          if ((whline(i:i) .eq. 'N') .or. (whline(i:i) .eq. 'n')) then
            hputl = .false.
          else if ((whline(i:i) .eq. 'Y') .or.
     *             (whline(i:i) .eq. 'y')) then
            hputl = .true.
          else
            ermes(1:30)  = 'High/Low Label Flag Is Inconcl'
            ermes(31:60) = 'usive, Y Assumed              '
            call errhan ('CONDRV',0,ermes,errsev)
            hputl = .true.
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            lputl   = 0
            tputl   = 1
            prput   = .false.
          end if
        end if

C  Parse through in the line label flag
C  Y             => include the line labels
C  N             => don't include the line labels
C  anything else => give a warning message

        if (.not. error) then
          call search (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,p,errsev)
            lputl   = 0
            tputl   = 1
            prput   = .false.
          end if
        end if

        if (.not. error) then
          if ((whline(i:i) .eq. 'N') .or. (whline(i:i) .eq. 'n')) then
            lputl = -1
          else if ((whline(i:i) .eq. 'Y') .or.
     *             (whline(i:i) .eq. 'y')) then
            lputl = 1
          else if ((whline(i:i) .eq. 'C') .or.
     *             (whline(i:i) .eq. 'c')) then
            lputl = 0
          else
            ermes(1:30)  = 'Line Label Flag Is Inconclusiv'
            ermes(31:60) = 'e, C Assumed                  '
            call errhan ('CONDRV',0,ermes,errsev)
            lputl = 0
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            tputl   = 1
            prput   = .false.
          end if
        end if

C  Parse through in the title flag
C  Y             => include the title
C  N             => don't include the title
C  anything else => give a warning message

        if (.not. error) then
          call search (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,p,errsev)
            tputl   = 1
            prput   = .false.
          end if
        end if

        if (.not. error) then
          if ((whline(i:i) .eq. 'N') .or. (whline(i:i) .eq. 'n')) then
            tputl = -1
          else if ((whline(i:i) .eq. 'Y') .or.
     *             (whline(i:i) .eq. 'y')) then
            tputl = 1
          else if ((whline(i:i) .eq. 'L') .or.
     *             (whline(i:i) .eq. 'l')) then
            tputl = 0
          else
            ermes(1:30)  = 'Title Flag Is Inconclusive, Y '
            ermes(31:60) = 'Assumed                       '
            call errhan ('CONDRV',0,ermes,errsev)
            tputl = 1
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            prput = .false.
          end if
        end if

C  Parse through in the perimeter flag
C  Y             => include the perimeter
C  N             => don't include the perimeter
C  anything else => give a warning message

        if (.not. error) then
          call search (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,p,errsev)
            prput = .false.
          end if
        end if

        if (.not. error) then
          if ((whline(i:i) .eq. 'N') .or. (whline(i:i) .eq. 'n')) then
            prput = .false.
          else if ((whline(i:i) .eq. 'Y') .or.
     *             (whline(i:i) .eq. 'y')) then
            prput = .true.
          else
            ermes(1:30)  = 'Perimeter Flag Is Inconclusive'
            ermes(31:60) = ', N Assumed                   '
            call errhan ('CONDRV',0,ermes,errsev)
            prput = .false.
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
          end if
        end if

C  Check to see if the line width or dash pattern is here

        dlwv(1) = -1.0
        dlwv(2) = -1.0
        dlwv(3) = -1.0
        ddpv(1) = 0
        ddpv(2) = 0
        ddpv(3) = 0
        if ((.not. error) .and. ((lshd) .or. (.not. fill))) then
          dash    = .false.
50        call search (whline,i,error)
          if (.not. error) then

C  One or both of them are here, figure out which

            if ((whline(i:i) .eq. 'L') .or.
     *          (whline(i:i) .eq. 'l')) then

C  This is line width

              i = i + 1
              dlwv(1) = 1.0
              dlwv(2) = 1.0
              dlwv(3) = 1.0
              n = 1
              done = .false.
30            continue

C  Get the detail line width indicator
C  +             => only positive contours are affected by the following value
C  -             => only negative contours are affected by the following value
C  @             => only the zero line is affected by the following value
C  =             => all contours are affected by the following value
C  D,d           => all contours will be normal line width
C  anything else => give a warning message

                if (whline(i:i) .eq. '+') then
                  dlwi = 1
                else if (whline(i:i) .eq. '-') then
                  dlwi = -1
                else if (whline(i:i) .eq. '@') then
                  dlwi = 0
                else if (whline(i:i) .eq. '=') then
                  dlwi = 2
                else if ((whline(i:i) .eq. 'd') .or.
     *                   (whline(i:i) .eq. 'D')) then
                  done    = .true.
                else
                  ermes(1:30)  = 'Detail Line Width Indicator Is'
                  ermes(31:60) = ' Invalid, Options: +-=@dD     '
                  call errhan ('CONDRV',0,ermes,errsev)
                  done    = .true.
                end if

C  Parse out the line width multiplier
C  any multiplier less than 1.0 is ineffective

                if (.not. done) then
                  j = i
                  i = i + 1
10                j = j + 1
                  if ((whline(j:j) .ne. '+') .and.
     *                (whline(j:j) .ne. '-') .and.
     *                (whline(j:j) .ne. '=') .and.
     *                (whline(j:j) .ne. '@') .and.
     *                (whline(j:j) .ne. ' ') .and.
     *                (whline(j:j) .ne. '|') .and.
     *                (whline(j:j) .ne. 'D') .and.
     *                (whline(j:j) .ne. 'd'))
     *              goto 10
                  tstrg(1:j-i) = whline(i:j-1)
                  if ((tstrg(1:j-i) .eq. 'D') .or.
     *                (tstrg(1:j-i) .eq. 'd')) then
                    temp = 1.0
                  else
                    do 40 k = j-i+1,20
                      tstrg(k:k) = ' '
40                  continue
                    call gtreal (tstrg(1:20),temp,error)
                    if (error) then
                      ermes(1:30)  = 'Could Not Convert Line Width M'
                      ermes(31:60) = 'ultiplier To A Real Number    '
                      call errhan ('CONDRV',0,ermes,errsev)
                      temp = 1.0
                    end if

C  Line width multipliers must be between 1 and 10 inclusive

                    if (temp .lt. 1.0) then
                      ermes(1:30)  = 'Line Width Multipliers Less Th'
                      ermes(31:60) = 'an One, Have No Effect        '
                      call errhan ('CONDRV',0,ermes,errsev)
                      temp = 1.0
                    end if
                    if (temp .gt. 10.0) then
                      ermes(1:30)  = 'Line Width Multipliers Greater'
                      ermes(31:60) = ' Than 10 Are Invalid          '
                      call errhan ('CONDRV',0,ermes,errsev)
                      temp = 10.0
                    end if
                  end if
                  i = j
                  if (dlwi .eq. 1) then
                    dlwv(1) = temp
                  else if (dlwi .eq. -1) then
                    dlwv(3) = temp
                  else if (dlwi .eq. 0) then
                    dlwv(2) = temp
                  else
                    dlwv(1) = temp
                    dlwv(2) = temp
                    dlwv(3) = temp
                    done = .true.
                  end if
                end if

C  Determine if the entire line width string has been parsed, if not
C  continue parsing otherwise go on.  There can never be more than three
C  linwe width entries

                if ((whline(i:i) .eq. ' ') .or.
     *              (whline(i:i) .eq. '|') .or.
     *              (n .eq. 3)) then
                  done = .true.
                else
                  n = n + 1
                end if
              if (.not. done) goto 30
            else if ((whline(i:i) .eq. 'D') .or.
     *               (whline(i:i) .eq. 'd')) then
              dash = .true.

C  This is dash pattern

              i = i + 1
              ddpv(1) = -1
              ddpv(2) = -1
              ddpv(3) = 21845
              n = 1
              done = .false.
60            continue

C  Get the detail dash pattern indicator
C  +             => only positive contours are affected by the following value
C  -             => only negative contours are affected by the following value
C  @             => only the zero line is affected by the following value
C  =             => all contours are affected by the following value
C  D,d           => all contours will be normal line width
C  anything else => give a warning message

                if (whline(i:i) .eq. '+') then
                  ddpi = 1
                else if (whline(i:i) .eq. '-') then
                  ddpi = -1
                else if (whline(i:i) .eq. '@') then
                  ddpi = 0
                else if (whline(i:i) .eq. '=') then
                  ddpi = 2
                else if ((whline(i:i) .eq. 'd') .or.
     *                   (whline(i:i) .eq. 'D')) then
                  done    = .true.
                else
                  ermes(1:30)  = 'Detail Dash Pattern Indicator '
                  ermes(31:60) = 'Is Invalid, Options: +-=@dD   '
                  call errhan ('CONDRV',0,ermes,errsev)
                  done    = .true.
                end if

C  Parse out the dash pattern
C  L  => DDPV = 255   ; or 0000000011111111 ; or Large
C  M  => DDPV = 3855  ; or 0000111100001111 ; or Medium
C  SM => DDPV = 13107 ; or 0011001100110011 ; or SMall
C  T  => DDPV = 21845 ; or 0101010101010101 ; or Tiny
C  SO => DDPV = -1    ; or 1111111111111111 ; or SOlid
C  D  => DDPV = -1    ; or 1111111111111111 ; or Default for non negatives
C  D  => DDPV = 21845 ; or 0101010101010101 ; or Default for negatives

                if (.not. done) then
                  i = i + 1
                  if ((whline(i:i) .eq. 'L') .or.
     *                (whline(i:i) .eq. 'l')) then
                    j = 255
                  else if ((whline(i:i) .eq. 'M') .or.
     *                     (whline(i:i) .eq. 'm')) then
                    j = 3855
                  else if ((whline(i:i+1) .eq. 'SM') .or.
     *                     (whline(i:i+1) .eq. 'Sm') .or.
     *                     (whline(i:i+1) .eq. 'sM') .or.
     *                     (whline(i:i+1) .eq. 'sm')) then
                    j = 13107
                  else if ((whline(i:i) .eq. 'T') .or.
     *                     (whline(i:i) .eq. 't')) then
                    j = 21845
                  else if ((whline(i:i+1) .eq. 'SO') .or.
     *                     (whline(i:i+1) .eq. 'So') .or.
     *                     (whline(i:i+1) .eq. 'sO') .or.
     *                     (whline(i:i+1) .eq. 'so')) then
                    j = -1
                  else if (((whline(i:i) .eq. 'D') .or.
     *                      (whline(i:i) .eq. 'd')) .and.
     *                     (ddpi .eq. -1)) then
                    j = 21845
                  else if (((whline(i:i) .eq. 'D') .or.
     *                      (whline(i:i) .eq. 'd')) .and.
     *                     (ddpi .ne. -1)) then
                    j = -1
                  else
                    ermes(1:30)  = 'Dash Pattern Entry Is In Error'
                    ermes(31:60) = ', Options: LG ME SM TI SO D   '
                    call errhan ('CONDRV',0,ermes,errsev)
                    j = -1
                  end if
                  if (ddpi .eq. 1) then
                    ddpv(1) = j
                  else if (ddpi .eq. -1) then
                    ddpv(3) = j
                  else if (ddpi .eq. 0) then
                    ddpv(2) = j
                  else
                    ddpv(1) = j
                    ddpv(2) = j
                    ddpv(3) = j
                    done = .true.
                  end if
                end if

C  Look for the next indicator

70              i = i + 1
                if ((whline(i:i) .ne. '+') .and.
     *              (whline(i:i) .ne. '-') .and.
     *              (whline(i:i) .ne. '=') .and.
     *              (whline(i:i) .ne. '@') .and.
     *              (whline(i:i) .ne. ' ') .and.
     *              (whline(i:i) .ne. '|') .and.
     *              (whline(i:i) .ne. 'D') .and.
     *              (whline(i:i) .ne. 'd'))
     *            goto 70

C  Determine if the entire dash pattern string has been parsed, if not
C  continue parsing otherwise go on.  There can never be more than three
C  dash pattern entries

                if ((whline(i:i) .eq. ' ') .or.
     *              (whline(i:i) .eq. '|') .or.
     *              (n .eq. 3)) then
                  done = .true.
                else
                  n = n + 1
                end if
              if (.not. done) goto 60
            else
              ermes(1:30)  = 'Expecting Line Width Or Dash P'
              ermes(31:60) = 'attern Information            '
              call errhan ('CONDRV',0,ermes,errsev)
              dash = .true.
            end if
            call next (whline,i,error)
          else
            dash  = .true.
            error = .false.
            i     = 80
          end if
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
          else if (.not. dash) then
            goto 50
          end if
        else

C  No lines are going to be drawn so no line width and dash pattern
C  is necessary

          ddpv(1) = -1
          ddpv(2) = -1
          ddpv(3) = -1
          dlwv(1) = 1.0
          dlwv(2) = 1.0
          dlwv(3) = 1.0
        end if

C  Check to see that everything is hunky dory at the end of the line

        call search (whline,i,error)
        if (.not. error) then
          ermes(1:30)  = 'Reading First Line Of Details '
          ermes(31:60) = 'Table, Too Many Entries       '
          call errhan ('CONDRV',0,ermes,errsev)
        end if

C  If high/low labels were requested read in high/low information from
C  the next line of information in the table

        if (hputl) then
          read (unum,90,end=80,err=80) whline(1:1)
          read (unum,90,end=80,err=80) whline(1:1)
          read (unum,90,end=80,err=80) whline(1:1)
          read (unum,100,end=80,err=80) whline(1:80)

C  Read in all the high/low information

          call crdhlo (whline,errsev,hfilb)
          goto 85

C  If there was an error in the read, give an error message, then
C  default the high/low label information

80        ermes(1:30)   = 'Could Not Read In High/Low Lab'
          ermes(31:60)  = 'el Information Line           '
          call errhan ('CONDRV',1,ermes,errsev)
          whline(1:40)  = '                                        '
          whline(41:80) = '                                        '
          call crdhlo (whline,errsev,hfilb)
85        continue
        end if

C  If line labels were requested read in the line label information from
C  the next line of information in the table

        if (lputl .eq. 1) then
          read (unum,90,end=110,err=110) whline(1:1)
          read (unum,90,end=110,err=110) whline(1:1)
          read (unum,90,end=110,err=110) whline(1:1)
          read (unum,100,end=110,err=110) whline(1:80)

C  Read in all the label information

          call crdlab (whline,errsev,lfilb)
          goto 115

C  If there was an error reading in the information, give an error
C  message and default the line label information

110       ermes(1:30)   = 'Could Not Read In Line Label I'
          ermes(31:60)  = 'nformation Line               '
          call errhan ('CONDRV',1,ermes,errsev)
          whline(1:40)  = '                                        '
          whline(41:80) = '                                        '
          call crdlab (whline,errsev,lfilb)
115       continue
        end if

C  If a title is to be drawn read the information concerning it from the
C  next line of information in the table

        if (tputl .ge. 0) then
          read (unum,90,end=120,err=120) whline(1:1)
          read (unum,90,end=120,err=120) whline(1:1)
          read (unum,90,end=120,err=120) whline(1:1)
          read (unum,100,end=120,err=120) whline(1:80)

C  Read in all the title information

          call crdttl (whline,errsev,tputl,tfilb)
          goto 125

C  If there was an error in the read, give an error message and default
C  the title information

120       ermes(1:30)   = 'Could Not Read In Title Inform'
          ermes(31:60)  = 'ation Line                    '
          call errhan ('CONDRV',1,ermes,errsev)
          whline(1:40)  = '                                        '
          whline(41:80) = '                                        '
          call crdttl (whline,errsev,tputl,tfilb)
125       continue
        end if

C  Tell the user the details are set up, even if an error occured but
C  didn't stop execution

        print *, 'CONDRV - Contour Details Set Up'
      else

C  Assign defaults if the table is not there

        print *, 'CONDRV - Default Contour Details Used'
        color       = .false.
        fill        = .false.
        hputl       = .true.
        lputl       = 0
        lshd        = .false.
        fshd        = .false.
        prput       = .false.
        tputl       = 1
        if (mod(pnum,2) .eq. 0) then
          ddpv(1) = -1
          ddpv(2) = -1
          ddpv(3) = -1
          dlwv(1) = wltwo
          dlwv(2) = wltwo
          dlwv(3) = wltwo
        else
          ddpv(1) = -1
          ddpv(2) = -1
          ddpv(3) = 21845
          dlwv(1) = 1.0
          dlwv(2) = 1.0
          dlwv(3) = 1.0
        end if
        whline(1:40)  = '                                        '
        whline(41:80) = '                                        '
        call crdhlo (whline,errsev,hfilb)
        call crdttl (whline,errsev,tputl,tfilb)
      end if

C*****************************  subroutine end  ******************************C

C  Format statements begin ...

90    format (A1)
100   format (A80)

C  Format statements end.


130   return
      end
      subroutine crdhlo (whline,errsev,ofilb)

C*****************************************************************************C
C  crdhlo   - this is a CONDRV routine                                        C
C  Section  - Tables                                                          C
C  Purpose  - This routine determines high/low  label information as was des- C
C             scribed by the user in the DETAILS table.                       C
C                                                                             C
C  On entry - WHLINE contains the line of the DETAILS table containing all of C
C             the high/low label information.  ERRSEV indicates at  what sev- C
C             erity or error execution should halt.                           C
C                                                                             C
C  On exit  - All variables in common block HLODET have been properly set up. C
C             The variables in common block HLQDET are set up.  OFILB says if C
C             the high/low label boxes should be filled or not.               C
C                                                                             C
C  Assume   - Nothing.                                                        C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             SEARCH              CONDRV/MAPDRV utility                       C
C             NEXT                CONDRV/MAPDRV utility                       C
C             ERRHAN              CONDRV/MAPDRV utility                       C
C             GTREAL              CONDRV/MAPDRV utility                       C
C             ----------------------------------------------------------------C
C                                                                             C
C  Author   - Jeremy Asbill       Date - June 11, 1990      for the MM4 club  C
C*****************************************************************************C

C  Character variables

      character*80     whline            ! a line from the table           (in)
      character*60     ermes,            ! error message string,general (local)
     *                 p,                ! error message string,SEARCH  (local)
     *                 q                 ! error message string,NEXT    (local)
      character*20     gstrng            ! temporary string             (local)

C  Integer variables

      integer          errsev            ! error severity comparitor       (in)
      integer          hstyl(2),         ! for common block HLODET
     *                 hsize,            ! for common block HLODET
     *                 hangl             ! for common block HLODET
      integer          hqual(2)          ! for common block HLQDET
      integer          i                 ! loop counter                 (local)

C  Logical variables

      logical          ofilb             ! out version of HFILB           (out)
      logical          hputb             ! for common block HLBDET
      logical          hputp,            ! for common block HLODET
     *                 hfilb,            ! for common block HLODET
     *                 hfilt             ! for common block HLODET
      logical          error             ! has an error been detected ? (local)

C  Real variables

      real             hprlw             ! for common block HLODET

C  Common blocks

      common /hlbdet/  hputb             ! draw boxes around highs and lows ?
      common /hlodet/  hputp,            ! darw in perimeter on boxes ?
     *                 hfilb,            ! fill in the the box ?
     *                 hprlw,            ! line width for box perimeter
     *                 hstyl,            ! high/low style indicator
     *                 hsize,            ! character size for highs and lows
     *                 hfilt,            ! use an overlap filter ?
     *                 hangl             ! angle for horiz. to draw highs/lows
      common /hlqdet/  hqual             ! character quality

C****************************  subroutine begin  *****************************C

C  Set up I to use as a counter

      i = 1

C  Initialize the error flag

      error = .false.

C  Set up SEARCH and NEXT error strings

      p(1:23)  = 'Reading High/Low Inform'
      p(24:60) = 'ation Line, Too Few Entries On Line  '
      q(1:23)  = p(1:23)
      q(24:60) = 'ation Line, Entry Is Bizzare         '

C  Do we want to draw in boxes

      call search (whline,i,error)
      if (error) then
        hputb    = .false.
        hputb    = .false.
        hfilb    = .false.
        hstyl(1) = 13
        hstyl(2) = 13
        hsize    = 12
        hfilt    = .false.
        hangl    = 0
        hqual(1) = 1
        hqual(2) = 1
      end if

      if (.not. error) then
        if ((whline(i:i) .eq. 'Y') .or. (whline(i:i) .eq. 'y')) then
          hputb = .true.
        else if ((whline(i:i) .eq. 'N') .or.
     *           (whline(i:i) .eq. 'n')) then
          hputb = .false.
        else
          ermes(1:30)  = 'High/Low Label Box Flag Is Inc'
          ermes(31:60) = 'conclusive, N Assumed         '
          call errhan ('CONDRV',0,ermes,errsev)
          hputb = .false.
        end if
        call next (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,q,errsev)
          hstyl(1) = 13
          hstyl(2) = 13
          hsize    = 12
          hfilt    = .false.
          hangl    = 0
          hqual(1) = 1
          hqual(2) = 1
        end if
      end if

C  Do we want to draw in the perimeter on the boxes

      if ((hputb) .and. (.not. error)) then
        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          hputp    = .false.
          hfilb    = .false.
          hprlw    = 0.0
          hstyl(1) = 13
          hstyl(2) = 13
          hsize    = 12
          hfilt    = .false.
          hangl    = 0
          hqual(1) = 1
          hqual(2) = 1
        end if

        if (.not. error) then
          if ((whline(i:i) .eq. 'N') .or. (whline(i:i) .eq. 'n')) then
            hputp = .false.
          else if ((whline(i:i) .eq. 'Y') .or.
     *             (whline(i:i) .eq. 'y')) then
            hputp = .true.
          else
            ermes(1:30)  = 'High/Low Label Box Perimeter F'
            ermes(31:60) = 'lag Is Inconclusive, Y Assumed'
            call errhan ('CONDRV',0,ermes,errsev)
            hputp = .true.
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            hfilb    = .false.
            if (hputp) then
              hprlw  = 1.0
            else
              hprlw  = 0.0
            end if
            hstyl(1) = 13
            hstyl(2) = 13
            hsize    = 12
            hfilt    = .false.
            hangl    = 0
            hqual(1) = 1
            hqual(2) = 1
          end if
        end if

C  Do we want to fill in the boxes

        if (.not. error) then
          call search (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,p,errsev)
            hfilb    = .false.
            if (hputp) then
              hprlw  = 1.0
            else
              hprlw  = 0.0
            end if
            hstyl(1) = 13
            hstyl(2) = 13
            hsize    = 12
            hfilt    = .false.
            hangl    = 0
            hqual(1) = 1
            hqual(2) = 1
          end if
        end if

        if (.not. error) then
          if ((whline(i:i) .eq. 'Y') .or. (whline(i:i) .eq. 'y')) then
            hfilb = .true.
          else if ((whline(i:i) .eq. 'N') .or.
     *             (whline(i:i) .eq. 'n')) then
            hfilb = .false.
          else
            ermes (1:30)  = 'High/Low Label Box Fill Flag I'
            ermes (1:30)  = 's Inconclusive, N Assumed     '
            call errhan ('CONDRV',0,ermes,errsev)
            hfilb = .false.
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            if (hputp) then
              hprlw  = 1.0
            else
              hprlw  = 0.0
            end if
            hstyl(1) = 13
            hstyl(2) = 13
            hsize    = 12
            hfilt    = .false.
            hangl    = 0
            hqual(1) = 1
            hqual(2) = 1
          end if
        end if

C  What line width should the box perimeter have?
C  "d" indicates lw = 1000

        if ((hputp) .and. (.not. error)) then
          call search (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,p,errsev)
            if (hputp) then
              hprlw  = 1.0
            else
              hprlw  = 0.0
            end if
            hstyl(1) = 13
            hstyl(2) = 13
            hsize    = 12
            hfilt    = .false.
            hangl    = 0
            hqual(1) = 1
            hqual(2) = 1
          end if
        end if

        if ((hputp) .and. (.not. error)) then
          if ((whline(i:i) .eq. 'd') .or. (whline(i:i) .eq. 'D')) then
            hprlw = 1.0
            call next (whline,i,error)
          else
            j = i
            call next (whline,i,error)
            if (.not. error) then
              gstrng(1:i-j) = whline(j:i-1)
              do 10 k = i-j+1,20
                gstrng(k:k) = ' '
10            continue
              call gtreal (gstrng,hprlw,error)
              if (error) then
                ermes(1:30)  = 'Could Not Read High/Low Label '
                ermes(31:60) = 'Box Perimeter Width, 1.0 Used '
                call errhan ('CONDRV',0,ermes,errsev)
                hprlw = 1.0
                error = .false.
              end if
            end if
          end if
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            hstyl(1) = 13
            hstyl(2) = 13
            hsize    = 12
            hfilt    = .false.
            hangl    = 0
            hqual(1) = 1
            hqual(2) = 1
          end if            
        end if
      else
        hfilb = .false.
        hputp = .false.
        hprlw  = 0.0
      end if

C  What kind of marker to we want for the highs
C  There are 13 options, defined in the routine SETHLO
C  D,d  => style # 13 is used

      if (.not. error) then
        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          hstyl(1) = 13
          hstyl(2) = 13
          hsize    = 12
          hfilt    = .false.
          hangl    = 0
          hqual(1) = 1
          hqual(2) = 1
        end if
      end if

      if (.not. error) then
        if ((whline(i:i) .eq. 'D') .or. (whline(i:i) .eq. 'd')) then
          hstyl(1) = 13
        else if ((whline(i+1:i+1) .ne. ' ') .and.
     *           (whline(i+1:i+1) .ne. '|')) then
          read  (whline(i:i+1),20,err=40) hstyl(1)
        else
          read (whline(i:i),30,err=40) hstyl(1)
        end if
        goto 45

C  Inform the user of an error in the read in

40      ermes(1:30)  = 'High Label Style Number Input '
        ermes(31:60) = 'Conversion                    '
        call errhan ('CONDRV',1,ermes,errsev)
        hstyl(1) = 13
        hstyl(2) = 13
        hsize    = 12
        hfilt    = .false.
        hangl    = 0
        hqual(1) = 1
        hqual(2) = 1
        error = .true.
        
45      if (.not. error) then
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            hstyl(2) = 13
            hsize    = 12
            hfilt    = .false.
            hangl    = 0
            hqual(1) = 1
            hqual(2) = 1
          end if
        end if
      end if

C  What kind of marker to we want for the lows
C  There are 13 options, defined in the routine SETHLO
C  D, d  => style # 13 is used

      if (.not. error) then
        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          hstyl(2) = 13
          hsize    = 12
          hfilt    = .false.
          hangl    = 0
          hqual(1) = 1
          hqual(2) = 1
        end if
      end if

      if (.not. error) then
        if ((whline(i:i) .eq. 'D') .or. (whline(i:i) .eq. 'd')) then
          hstyl(2) = 13
        else if ((whline(i+1:i+1) .ne. ' ') .and.
     *           (whline(i+1:i+1) .ne. '|')) then
          read  (whline(i:i+1),20,err=50) hstyl(2)
        else
          read (whline(i:i),30,err=50) hstyl(2)
        end if
        goto 55

C  Inform the user of an error in the read in

50      ermes(1:30)  = 'Low Label Style Number Input C'
        ermes(31:60) = 'onversion                     '
        call errhan ('CONDRV',1,ermes,errsev)
        hstyl(2) = 13
        hsize    = 12
        hfilt    = .false.
        hangl    = 0
        hqual(1) = 1
        hqual(2) = 1
        error = .true.
        
55      if (.not. error) then
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            hsize    = 12
            hfilt    = .false.
            hangl    = 0
            hqual(1) = 1
            hqual(2) = 1
          end if
        end if
      end if

C  How big should the highs and lows be
C  This is specified in plotter coordinates

      if (.not. error) then
        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          hsize    = 12
          hfilt    = .false.
          hangl    = 0
          hqual(1) = 1
          hqual(2) = 1
        end if
      end if

      if (.not. error) then
        if ((whline(i:i) .eq. 'D') .or. (whline(i:i) .eq. 'd')) then
          hsize = 12
        else if ((whline(i+1:i+1) .ne. ' ') .and.
     *           (whline(i+1:i+1) .ne. '|')) then
          read  (whline(i:i+1),20,err=60) hsize
        else
          read (whline(i:i),30,err=60) hsize
        end if
        goto 65

C  Inform the user of an error if here

60      ermes(1:30)  = 'High/Low Label Size Input Conv'
        ermes(31:60) = 'ersion                        '
        call errhan ('CONDRV',1,ermes,errsev)
        hsize    = 12
        hfilt    = .false.
        hangl    = 0
        hqual(1) = 1
        hqual(2) = 1
        error = .true.

65      if (.not. error) then
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            hfilt    = .false.
            hangl    = 0
            hqual(1) = 1
            hqual(2) = 1
          end if
        end if
      end if

C  Check to see if the overlap filter has been requested

      if (.not. error) then
        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          hfilt    = .false.
          hangl    = 0
          hqual(1) = 1
          hqual(2) = 1
        end if
      end if

      if (.not. error) then
        if ((whline(i:i) .eq. 'N') .or. (whline(i:i) .eq. 'n')) then
          hfilt = .false.
        else if ((whline(i:i) .eq. 'Y') .or.
     *           (whline(i:i) .eq. 'y')) then
          hfilt = .true.
        else
          ermes(1:30)  = 'High/Low Label Overlap Filter '
          ermes(31:60) = 'Flag Is Inconclusive, Y Used  '
          call errhan ('CONDRV',0,ermes,errsev)
          hfilt = .true.
        end if
        call next (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,q,errsev)
          hangl    = 0
          hqual(1) = 1
          hqual(2) = 1
        end if
      end if

C  Get the angle from the horizontal of the highs and lows

      if (.not. error) then
        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          hangl    = 0
          hqual(1) = 1
          hqual(2) = 1
        end if
      end if

      if (.not. error) then
        if ((whline(i:i) .eq. 'D') .or. (whline(i:i) .eq. 'd')) then
          hangl = 0
        else if ((whline(i+1:i+1) .ne. ' ') .and.
     *           (whline(i+1:i+1) .ne. '|')) then
          if ((whline(i+2:i+2) .ne. ' ') .and.
     *        (whline(i+2:i+2) .ne. '|')) then
            read (whline(i:i+2),80,err=70) hangl
          else
            read  (whline(i:i+1),20,err=70) hangl
          end if
        else
          read (whline(i:i),30,err=70) hangl
        end if
        goto 75

C  If there was an error in the read inform the user

70      ermes(1:30)  = 'High/Low Label Angle Input Con'
        ermes(31:60) = 'version                       '
        call errhan ('CONDRV',1,ermes,errsev)
        hangl    = 0
        hqual(1) = 1
        hqual(2) = 1
        error = .false.

75      if (.not. error) then
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            hqual(1) = 1
            hqual(2) = 1
          end if
        end if
      end if

C  Read in the letter quality to be used in the high low labels

      if (.not. error) then
        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          hqual(1) = 1
          hqual(2) = 1
        end if
      end if

      if (.not. error) then  
        hqual(2) = -1
        if ((whline(i:i) .eq. 'd') .or. (whline(i:i) .eq. 'D')) then
          hqual(1) = 1
          hqual(2) = 1
        else if (whline(i:i) .eq. '0') then
          hqual(1) = 0
        else if (whline(i:i) .eq. '1') then
          hqual(1) = 1
        else
          ermes(1:30)  = 'High/Low Label Text Type Is In'
          ermes(31:60) = 'valid, 1 Used (Duplex)        '
          call errhan ('CONDRV',0,ermes,errsev)
          hqual(1) = 1
        end if
        if (hqual(2) .eq. -1) then
          i = i + 1
          if ((whline(i:i) .eq. 'd') .or. (whline(i:i) .eq. 'D')) then
            hqual(2) = 1
          else if (whline(i:i) .eq. '0') then
            hqual(2) = 0
          else if (whline(i:i) .eq. '1') then
            hqual(2) = 1
          else if (whline(i:i) .eq. '2') then
            hqual(2) = 2
          else
            ermes(1:30)  = 'High/Low Label Text Quality Is'
            ermes(31:60) = ' Invalid, 1 Used (Medium)     '
            call errhan ('CONDRV',0,ermes,errsev)
            hqual(2) = 1
          end if
        end if
        call next (whline,i,error)
        if (error) then
          call errhan ('CONDRV',0,q,errsev)
        else
          call search (whline,i,error)
          if (.not. error) then
            ermes(1:30)  = 'Reading High/Low Information L'
            ermes(31:60) = 'ine, Too Many Entries On Line '
            call errhan ('CONDRV',0,ermes,errsev)
          end if
        end if
      end if
    
C  Check for value errors in the set up numbers
C  Label Box Perimeter Line Width Needs to Be between 1 and 10

      if (hprlw .gt. 10.0) then
        ermes(1:30)  = 'High/Low Label Box Perimeter L'
        ermes(31:60) = 'ine Width Is Too Big, 10 Used '
        call errhan ('CONDRV',0,ermes,errsev)
        hprlw = 10.0
      end if

      if ((hprlw .lt. 1.0) .and. (hputp)) then
        ermes(1:30)  = 'High/Low Label Box Perimeter L'
        ermes(31:60) = 'ine Width Is Too Small, 1 Used'
        call errhan ('CONDRV',0,ermes,errsev)
        hprlw = 10.0
      end if

C  Styles range from 1 to 13

      if ((hstyl(1) .lt. 1) .or. (hstyl(1) .gt. 13)) then
        ermes(1:30)  = 'High Label Style Is Invalid, S'
        ermes(31:60) = 'tyle 13 Used                  '
        call errhan ('CONDRV',0,ermes,errsev)
        hstyl(1) = 13
      end if

      if ((hstyl(2) .lt. 1) .or. (hstyl(2) .gt. 13)) then
        ermes(1:30)  = 'Low Label Style Is Invalid, St'
        ermes(31:60) = 'yle 13 Used                   '
        call errhan ('CONDRV',0,ermes,errsev)
        hstyl(2) = 13
      end if

C  Size can be no less than 1 and no more than 25

      if (hsize .lt. 1) then
        ermes(1:30)  = 'High/Low Label Size Is Too Sma'
        ermes(31:60) = 'll, 1 Used                    '
        call errhan ('CONDRV',0,ermes,errsev)
        hsize = 1
      end if

      if (hsize .gt. 25) then
        ermes(1:30)  = 'High/Low Label Size Is Too Big'
        ermes(31:60) = ', 25 Used                     '
        call errhan ('CONDRV',0,ermes,errsev)
        hsize = 25
      end if

C  Angle can only be given between 0 and 360 degrees inclusive

      if ((hangl .lt. 0) .or. (hangl .gt. 360)) then
        ermes(1:30)  = 'High/Low Label Angle Is Invali'
        ermes(31:60) = 'd, 0 Degrees Used             '
        call errhan ('CONDRV',0,ermes,errsev)
        hangl = 0
      end if

C  Quality, the first number is the type of characters
C    0 - Complex
C    1 - Duplex
C  the second number is the quality of characters
C    0 - High Quality
C    1 - Medium Quality
C    2 - Low Quality

      if ((hqual(1) .ne. 0) .and. (hqual(1) .ne. 1)) then
        ermes(1:30)  = 'High/Low Label Text Type Is In'
        ermes(31:60) = 'valid, 1 Assumed (Duplex)     '
        call errhan ('CONDRV',0,ermes,errsev)
        hqual(1) = 1
      end if

      if ((hqual(2) .gt. 2) .or. (hqual(2) .lt. 0)) then
        ermes(1:30)  = 'High/Low Label Text Quality Is'
        ermes(31:60) = ' Invalid, 1 Assumed (Medium)  '
        call errhan ('CONDRV',0,ermes,errsev)
        hqual(2) = 1
      end if

C  Assign output variable

      ofilb = hfilb

C*****************************  subroutine end  ******************************C

C  Format statements begin ...

20    format (I2)
30    format (I1)
80    format (I3)

C  Format statements end.

      return
      end
      subroutine crdlab (whline,errsev,ofilb)

C*****************************************************************************C
C  crdlab   - This is a CONDRV routine                                        C
C  Section  - Tables                                                          C
C  Purpose  - To determine line label information as specified by the user in C
C             the CON DETAILS table.                                          C
C                                                                             C
C  On entry - WHLINE  contains  the line from  the table  which holds all the C
C             needed information.  ERRSEV indicates what severity of an error C
C             will cause execution to cease.                                  C
C                                                                             C
C  On exit  - All of the values in common block LABDET have been set up.  The C
C             variables in LBQDET too have been set up correctly.  OFILB says C
C             if the line label boxes will be filled or not.                  C
C                                                                             C
C  Assume   - Nothing.                                                        C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             SEARCH              CONDRV/MAPDRV utility                       C
C             NEXT                CONDRV/MAPDRV utility                       C
C             ERRHAN              CONDRV/MAPDRV utility                       C
C             GTREAL              CONDRV/MAPDRV utility                       C
C             ----------------------------------------------------------------C
C                                                                             C
C  Author   - Jeremy Asbill       Date - June 11, 1990      for the MM4 club  C
C*****************************************************************************C

C  Character variables

      character*80     whline            ! the line from the table         (in)
      character*60     p,                ! error message string, SEARCH (local)
     *                 q,                ! error message string, NEXT   (local)
     *                 ermes             ! error message string, general(local)
      character*20     gstrng            ! temporary string             (local)

C  Integer variables

      integer          errsev            ! error severity comparitor       (in)
      integer          lqual(2)          ! for common block LBQDET
      integer          lsize,            ! for common block LABDET
     *                 lortn,            ! for common block LABDET
     *                 langl,            ! for common block LABDET
     *                 lintv             ! for common block LABDET

C  Logical varaibles

      logical          ofilb             ! out version of LFILB           (out)
      logical          lputb,            ! for common block LABDET
     *                 lputp,            ! for common block LABDET
     *                 lfilb             ! for common block LABDET
      logical          error             ! has an error occured ?       (local)

C  Real variables

      real             lprlw             ! for common block LABDET

C  Common blocks

      common /labdet/  lputb,            ! put boxes on the line labels ?
     *                 lputp,            ! put perimeter on label boxes ?
     *                 lfilb,            ! fill label boxes ?
     *                 lprlw,            ! label box perimeter line width
     *                 lsize,            ! line label character size
     *                 lintv,            ! line label placement per line
     *                 langl,            ! line label angle
     *                 lortn             ! line label orientation
      common /lbqdet/  lqual             ! line label character quality

C****************************  subroutine begin  *****************************C

C  Set up I to use as a counter

      i = 1

C  Initialize the error flag

      error = .false.

C  Set up the SEARCH and NEXT error message strings

      p(1:23)  = 'Reading Line Label Info'
      p(24:60) = 'rmation Line, Too Few Entries On Line'
      q(1:23)  = p(1:23)
      q(24:60) = 'rmation Line, Entry Is Bizarre       '

C  The first search is erred upon if WHLINE is blank, if this is the case
C  CONDRV does not consider that an error but an instruction to default all
C  line label information

      call search (whline,i,error)
      if (error) then
        lputb    = .false.
        lputp    = .false.
        lfilb    = .false.
        lsize    = 8
        lintv    = 4
        langl    = 0
        lortn    = 1
        lqual(1) = 1
        lqual(2) = 1
      end if

C  Parse out the line label box flag, first
C  Y             => Do draw a box around the line labels
C  N             => Don't draw a box around the line labels
C  anything else => Give a warning message

      if (.not. error) then
        if ((whline(i:i) .eq. 'Y') .or. (whline(i:i) .eq. 'y')) then
          lputb = .true.
        else if ((whline(i:i) .eq. 'N') .or.
     *           (whline(i:i) .eq. 'n')) then
          lputb = .false.
        else
          ermes(1:30)  = 'Line Label Box Flag Is Inconcl'
          ermes(31:60) = 'usive, N Assumed              '
          call errhan ('CONDRV',0,ermes,errsev)
          lputb = .false.
        end if
        call next (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,q,errsev)
          lsize    = 8
          lintv    = 4
          langl    = 0
          lortn    = 1
          lqual(1) = 1
          lqual(2) = 1
        end if
      end if

C  Parse through the label line box detail information if a box has been
C  requested

      if ((lputb) .and. (.not. error)) then

C  First is the box perimeter flag
C  Y             => Do draw a perimeter around the line label box
C  N             => Don't draw a perimeter around the line label box
C  anything else => Give a warning message

        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          lputp    = .false.
          lfilb    = .false.
          lprlw    = 0.0
          lsize    = 8
          lintv    = 4
          langl    = 0
          lortn    = 1
          lqual(1) = 1
          lqual(2) = 1
        end if

        if (.not. error) then
          if ((whline(i:i) .eq. 'N') .or. (whline(i:i) .eq. 'n')) then
            lputp = .false.
          else if ((whline(i:i) .eq. 'Y') .or.
     *             (whline(i:i) .eq. 'y')) then
            lputp = .true.
          else
            ermes(1:30)  = 'Line Label Box Perimeter Flag '
            ermes(31:60) = 'Is Inconclusive, N Assumed    '
            call errhan ('CONDRV',0,ermes,errsev)
            lputp = .false.
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            lfilb    = .false.
            if (lputp) then
              lprlw    = 1.0
            else
              lprlw    = 0.0
            end if
            lsize    = 8
            lintv    = 4
            langl    = 0
            lortn    = 1
            lqual(1) = 1
            lqual(2) = 1
          end if
        end if

C  Next is the line label box fill flag
C  Y              => Fill the box
C  N              => Leave the box hollow
C  Anything else  => Give Warning message

        if (.not. error) then
          call search (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,p,errsev)
            lfilb    = .false.
            if (lputp) then
              lprlw    = 1.0
            else
              lprlw    = 0.0
            end if
            lsize    = 8
            lintv    = 4
            langl    = 0
            lortn    = 1
            lqual(1) = 1
            lqual(2) = 1
          end if
        end if

        if (.not. error) then
          if ((whline(i:i) .eq. 'Y') .or. (whline(i:i) .eq. 'y')) then
            lfilb = .true.
          else if ((whline(i:i) .eq. 'N') .or.
     *             (whline(i:i) .eq. 'n')) then
            lfilb = .false.
          else
            ermes(1:30)  = 'Line Label Box Fill Flag Is In'
            ermes(31:60) = 'conclusive, N Assumed         '
            call errhan ('CONDRV',0,ermes,errsev)
            lfilb = .false.
          end if
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            if (lputp) then
              lprlw    = 1.0
            else
              lprlw    = 0.0
            end if
            lsize    = 8
            lintv    = 4
            langl    = 0
            lortn    = 1
            lqual(1) = 1
            lqual(2) = 1
          end if
        end if

C  What line width should the box perimeter have?  This value is a mulitplier
C  by the normal line width, a "D" or a "d" indicates normal line width

        if ((lputp) .and. (.not. error)) then
          call search (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,p,errsev)
            if (lputp) then
              lprlw    = 1.0
            else
              lprlw    = 0.0
            end if
            lsize    = 8
            lintv    = 4
            langl    = 0
            lortn    = 1
            lqual(1) = 1
            lqual(2) = 1
          end if
        end if

        if ((lputp) .and. (.not. error)) then
          if ((whline(i:i) .eq. 'd') .or. (whline(i:i) .eq. 'D')) then
            lprlw = 1.0
            call next (whline,i,error)
          else
            j = 1
            call next (whline,i,error)
            if (.not. error) then
              gstrng(1:i-j) = whline(j:i-1)
              do 10 k = i-j+1,20
                gstrng(k:k) = ' '
10            continue
              call gtreal (gstrng,lprlw,error)
              if (error) then
                ermes(1:30)  = 'Could Not Read Line Label Box '
                ermes(31:60) = 'Perimeter Width, 1.0 Used     '
                call errhan ('CONDRV',0,ermes,errsev)
                lprlw = 1.0
                error = .false.
              end if
            end if
          end if
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            lsize    = 8
            lintv    = 4
            langl    = 0
            lortn    = 1
            lqual(1) = 1
            lqual(2) = 1
          end if
        end if
      else
        lfilb = .false.
        lputp = .false.
        lprlw  = 0.0
      end if

C  Parse out character quality
C  0 In space 1          => Complex Characters will be used
C  1 In space 1          => Duplex Characters will be used
C  0 In space 2          => Characters are of high quality
C  1 In space 2          => Medium Quality
C  2 In space 2          => Low Quality
C  Anything Else         => Give a Warning Message

      if (.not. error) then
        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          lsize    = 8
          lintv    = 4
          langl    = 0
          lortn    = 1
          lqual(1) = 1
          lqual(2) = 1
        end if
      end if

      if (.not. error) then
        lqual(2) = -1
        if ((whline(i:i) .eq. 'd') .or. (whline(i:i) .eq. 'D')) then
          lqual(1) = 1
          lqual(2) = 1
        else if (whline(i:i) .eq. '0') then
          lqual(1) = 0
        else if (whline(i:i) .eq. '1') then
          lqual(1) = 1
        else
          ermes(1:30)  = 'Line Label Text Type Is Invali'
          ermes(31:60) = 'd, 1 Used (Duplex)            '
          call errhan ('CONDRV',0,ermes,errsev)
          lqual(1) = 1
        end if
        if (lqual(2) .eq. -1) then
          i = i + 1
          if ((whline(i:i) .eq. 'd') .or. (whline(i:i) .eq. 'D')) then
            lqual(2) = 1
          else if (whline(i:i) .eq. '0') then
            lqual(2) = 0
          else if (whline(i:i) .eq. '1') then
            lqual(2) = 1
          else if (whline(i:i) .eq. '2') then
            lqual(2) = 2
          else
            ermes(1:30)  = 'Line Label Text Quality Is Inv'
            ermes(31:60) = 'alid, 1 Used (Medium)         '
            call errhan ('CONDRV',0,ermes,errsev)
            lqual(2) = 1
          end if
        end if
        call next (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,q,errsev)
          lsize    = 8
          lintv    = 4
          langl    = 0
          lortn    = 1
        end if
      end if

C  How big should line labels be
C  This is specified in plotter coordinates

      if (.not. error) then
        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          lsize    = 8
          lintv    = 4
          langl    = 0
          lortn    = 1
        end if
      end if

      if (.not. error) then
        if ((whline(i:i) .eq. 'D') .or. (whline(i:i) .eq. 'd')) then
          lsize = 8
        else if ((whline(i+1:i+1) .ne. ' ') .and.
     *           (whline(i+1:i+1) .ne. '|')) then
          read  (whline(i:i+1),20,err=40) lsize
        else
          read (whline(i:i),30,err=40) lsize
        end if
        goto 45

C  Inform the user of an error if here

40      ermes(1:30)  = 'Line Label Size Input Conversi'
        ermes(31:60) = 'on                            '
        call errhan ('CONDRV',1,ermes,errsev)
        lsize    = 8
        langl    = 0
        lintv    = 4
        lortn    = 1
        error = .true.

45      if (.not. error) then
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            lintv    = 4
            langl    = 0
            lortn    = 1
          end if
        end if
      end if

C  Check to see how the user wants their labels oriented
C  D,d           => drawn along with line
C  360 > x > 0   => rotated x degrees from horizontal
C  = 0           => horizontal

      if (.not. error) then
        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          lintv    = 4
          langl    = 0
          lortn    = 1
        end if
      end if

      if (.not. error) then
        if ((whline(i:i) .eq. 'd') .or. (whline(i:i) .eq. 'D')) then
          lortn = 1
          langl = 0
        else
          lortn = 0
          if ((whline(i+1:i+1) .ne. ' ') .and.
     *        (whline(i+1:i+1) .ne. '|')) then
            if ((whline(i+2:i+2) .ne. ' ') .and.
     *          (whline(i+2:i+2) .ne. '|')) then
              read (whline(i:i+2),60,err=50) langl
            else
              read (whline(i:i+1),20,err=50) langl
            end if
          else
            read (whline(i:i),30,err=50) langl
          end if
        end if
        goto 55

C  Inform user of error if here

50      ermes(1:30)  = 'Line Label Angle Input Convers'
        ermes(31:60) = 'ion                           '
        call errhan ('CONDRV',1,ermes,errsev)
        langl    = 0
        lintv    = 4
        lortn    = 1
        error = .true.

55      if (.not. error) then
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',1,q,errsev)
            lintv = 4
          end if
        end if
      end if

C  Get the interval of lines at which to place line labels

      if (.not. error) then
        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          lintv = 4
        end if
      end if

      if (.not. error) then
        if ((whline(i:i) .eq. 'D') .or. (whline(i:i) .eq. 'd')) then
          lintv = 4
        else if ((whline(i+1:i+1) .ne. ' ') .and.
     *           (whline(i+1:i+1) .ne. '|')) then
          if ((whline(i+2:i+2) .ne. ' ') .and.
     *        (whline(i+2:i+2) .ne. '|')) then
            read (whline(i:i+2),60,err=70) lintv
          else
            read (whline(i:i+1),20,err=70) lintv
          end if
        else
          read (whline(i:i),30,err=70) lintv
        end if
        goto 75

C  Inform user of an error if here

70      ermes(1:30)  = 'Line Label Placement Interval '
        ermes(31:60) = 'Input Conversion              '
        call errhan ('CONDRV',1,ermes,errsev)
        lintv    = 4
        error = .true.

75      if (.not. error) then
          call next (whline,i,error)
          if (error) then
            call errhan ('CONDRV',0,q,errsev)
          else
            call search (whline,i,error)
            if (.not. error) then
              ermes(1:30)  = 'Reading Line Labels Informatio'
              ermes(31:60) = 'n, Too Many Entries On Line   '
              call errhan ('CONDRV',0,ermes,errsev)
            end if
          end if
        end if
      end if


C  Check for some value errors
C  LPRLW must be between 1 and 10 inclusive if the perimeter is going to
C  be drawn around boxes

      if ((lputp) .and. (lprlw .lt. 1.0)) then
        ermes(1:30)  = 'Line Label Box Perimeter Width'
        ermes(31:60) = 's Less Than 1 Are Ineffective '
        call errhan ('CONDRV',0,ermes,errsev)
        lprlw = 1.0
      end if
      if ((lputp) .and. (lprlw .gt. 10.0)) then
        ermes(1:30)  = 'Line Label Box Perimeter Width'
        ermes(31:60) = 's Greater Than 10 Are Too Big '
        call errhan ('CONDRV',0,ermes,errsev)
        lprlw = 10.0
      end if

C  Maximum Label Size is 25 and the minimum is 1

      if (lsize .lt. 1) then
        ermes(1:30)  = 'Line Label Size Is Too Small, '
        ermes(31:60) = 'Changed To 1                  '
        call errhan ('CONDRV',0,ermes,errsev)
        lsize = 1
      end if
      if (lsize .gt. 25) then
        ermes(1:30)  = 'Line Label Size Is Too Large, '
        ermes(31:60) = 'Changed To 25                 '
        call errhan ('CONDRV',0,ermes,errsev)
        lsize = 25
      end if

C  Label angle must be between 0 and 360

      if ((langl .lt. 0) .or. (langl .gt. 360)) then
        ermes(1:30)  = 'Line Label Angle Is Invalid, L'
        ermes(31:60) = 'abels Will Be Along Lines     '
        call errhan ('CONDRV',0,ermes,errsev)
        langl = 0
        lortn = 1
      end if

C  Label Interval must be at least 1 and at most 100

      if (lintv .lt. 1) then
        ermes(1:30)  = 'Line Label Placement Interval '
        ermes(31:60) = 'Too Small, Changed to 1       '
        call errhan ('CONDRV',0,ermes,errsev)
        lintv = 1
      end if
      if (lintv .gt. 100) then
        ermes(1:30)  = 'Line Label Placement Interval '
        ermes(31:60) = 'Too Large, Changed to 100     '
        call errhan ('CONDRV',0,ermes,errsev)
        lintv = 100
      end if

C  Assign output variable

      ofilb = lfilb

C*****************************  subroutine end  ******************************C

C  Format statements begin ...

20    format (I2)
30    format (I1)
60    format (I3)

C  Format statements end.

      return
      end
      subroutine crdprt (unum,errsev,cmeth,scale)

C*****************************************************************************C
C  crdprt   - This is a CONDRV routine                                        C
C  Section  - Tables                                                          C
C  Purpose  - To read in contouring partitions indicated by the user.         C
C                                                                             C
C  On entry - UNUM is the unit number of the file containing the  CON  PARTI- C
C             TIONS table.   ERRSEV indicated what  severity of error  should C
C             halt execution.   CMETH is the  coloring  method to be used  in C
C             the  picture.   SCALE is a scaling factor that will be used  in C
C             labeling the plot.                                              C
C                                                                             C
C  On exit  - Variables in common blocks for partitions,  color,  line width, C
C             and dash pattern have been set up.  Remember not  all of  these C
C             need to be set up all the time.                                 C
C                                                                             C
C  Assume   - Nothing                                                         C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             SEARCH              CONDRV/MAPDRV utility                       C
C             NEXT                CONDRV/MAPDRV utility                       C
C             GTREAL              CONDRV/MAPDRV utility                       C
C             ERRHAN              CONDRV/MAPDRV utility                       C
C             TBLLOK              CONDRV/MAPDRV utility                       C
C             CRDRCI              CONDRV/MAPDRV utility                       C
C             ----------------------------------------------------------------C
C                                                                             C
C  Author   - Jeremy Asbill      Date - August 9, 1990      for the MM4 club  C
C*****************************************************************************C

C  Parameters

      parameter       (imin = -1)        ! since before the first contour level
      parameter       (imax = 101)       ! until after the last contour level
      parameter       (rmin = -1.0E36)   ! the smallest value in the data
      parameter       (rmax = 1.0E36)    ! the largest value in the data

C  Character variables

      character*80     whline            ! a whole line from the table  (local)
      character*60     ermes,            ! error message string,general (local)
     *                 p,                ! error message string,SEARCH  (local)
     *                 q                 ! error message string,NEXT    (local)
      character*36     cermes            ! color index error message    (local)
      character*20     gstrng            ! string for GTREAL            (local)

C  Integer variables

      integer          errsev,           ! error severity comparitor       (in)
     *                 unum,             ! unit number of table file       (in)
     *                 cmeth             ! coloring method to use          (in)
      integer          ddpv(3)           ! for common block LWDPDT
      integer          nprt,             ! for common block PARINF
     *                 iprts(100,2)      ! for common block PARINF
      integer          pcolor(100,2)     ! for common block PARCOL
      integer          pdpv(100)         ! for common block LWDPPR
      integer          cmln,             ! number of chars in CERMES    (local)
     *                 i,j               ! loop counter/place keeper    (local)

C  Logical variables

      logical          noplt             ! don't make any plot ?           (in)
      logical          ints              ! for common block PARINF
      logical          found,            ! was the table found ?        (local)
     *                 error,            ! has an error occured ?       (local)
     *                 done              ! are all partitions in ?      (local)

C  Real variables

      real             scale             ! scaling factor                  (in)
      real             dlwv(3)           ! for common block LWDPDT
      real             rprts(100,2)      ! for common block PARINF
      real             plwv(100)         ! for common block LWDPPR

C  Common blocks

      common /lwdpdt/  dlwv,             ! details line width values
     *                 ddpv              ! details dash pattern values
      common /parinf/  nprt,             ! number of partitions
     *                 iprts,            ! integer partitions
     *                 rprts,            ! real partitions
     *                 ints              ! are the partitions integers ?
      common /parcol/  pcolor            ! colors for each partition
      common /lwdppr/  plwv,             ! partition line width values
     *                 pdpv              ! partition dash pattern values
      common /noplot/  noplt             ! don't draw anything ?

C****************************  Subroutine Begin  *****************************C

C  If no picture will be made don't bother parsing the table

      if (noplt) goto 110

C  Only read the table if the prior tables indicated that we should do so
C  Those indications are :
C    (1) There was no line width given before (DLWV(x) < 0)
C    (2) There was no dash pattern given before (DDPV(x) = 0)
C    (3) The coloring method includes user specified partitions
C        (CMETH = 2,3,4,5)

      if (((cmeth .ge. 2) .and. (cmeth .le. 5)) .or.
     *    (dlwv(1) .lt. 0.0) .or. (ddpv(1) .eq. 0)) then

C  The table should be there, since we are here, try to read in the first line

        call tbllok (unum,'CON PARTIT',errsev,found,whline,'CONDRV')

C  If the table was found, parse it

        if (found) then

C  Initialize all parsing tools and place keepers

          i        = 1
          p(1:23)  = 'Reading Partition Table'
          p(24:60) = ', Too Few Entries On Line            '
          q(1:23)  = p(1:23)
          q(24:60) = ', Entry Is Bizarre                   '
          error    = .false.
          nprt     = 0
          done     = .false.

C  Get to the first entry in the table

          call search (whline,i,error)
          if (error) then
            found = .false.
            done  = .true.
          end if

C  The partition specification should always be there
C  If CMETH = 3,5 then the specifications will be integers
C  If CMETH = 2,4 then the specifications will be reals
C  If CMETH is not in either of those groups assume the partitions are
C    reals

          if ((cmeth .eq. 3) .or. (cmeth .eq. 5)) then
            ints = .true.
          else
            ints = .false.
          end if

C  This is the begining of the loop that is iterated once per line of the
C  table

10        nprt = nprt + 1

          if ((ints) .and. (.not. error)) then

C  An M,m in the minimum column means that that partition starts with the
C  first level in the plot

            if ((whline(i:i) .eq. 'M') .or.
     *          (whline(i:i) .eq. 'm')) then
              iprts(nprt,1) = imin
            else if ((whline(i+1:i+1) .ne. ' ') .and.
     *               (whline(i+1:i+1) .ne. '|')) then
              read (whline(i:i+1),20,err=40) iprts(nprt,1)
            else
              read (whline(i:i),30,err=40) iprts(nprt,1)
            end if
            goto 45

C  If there is an error in reading the value, inform the user here

40          ermes(1:30)  = 'Partition Minimum Boundary Inp'
            ermes(31:60) = 'ut Conversion                 '
            call errhan ('CONDRV',1,ermes,errsev)
            found = .false.
            done  = .true.
            error = .true.

C  Goto maximum column

45          if (.not. error) then
              call next (whline,i,error)
              if (error) then
                call errhan ('CONDRV',1,q,errsev)
                found = .false.
                done  = .true.
              end if
            end if

            if (.not. error) then
              call search (whline,i,error)
              if (error) then
                call errhan ('CONDRV',1,p,errsev)
                found = .false.
                done  = .true.
              end if
            end if

C  An M,m in the maximum column means that that partition ends with the
C  last level in the plot

            if ((whline(i:i) .eq. 'M') .or.
     *          (whline(i:i) .eq. 'm')) then
              iprts(nprt,2) = imax
              done = .true.
            else if ((whline(i+1:i+1) .ne. ' ') .and.
     *               (whline(i+1:i+1) .ne. '|')) then
              read (whline(i:i+1),20,err=60) iprts(nprt,2)
            else
              read (whline(i:i),30,err=60) iprts(nprt,2)
            end if
            goto 65

C  If there is an error in reading the value, inform the user here

60          ermes(1:30)  = 'Partition Maximum Boundary Inp'
            ermes(31:60) = 'ut Conversion                 '
            call errhan ('CONDRV',1,ermes,errsev)
            found = .false.
            done  = .true.
            error = .true.
65          continue
          else if (.not. error) then

C  An M,m in the minimum column means that that partition starts with the
C  smallest value in the data

            if ((whline(i:i) .eq. 'M') .or.
     *          (whline(i:i) .eq. 'm')) then
              rprts(nprt,1) = rmin
            else
              j = i
              call next (whline,i,error)
              gstrng(1:i-j) = whline(j:i-1)
              do 50 k = i-j+1,20
                gstrng(k:k) = ' '
50            continue
              call gtreal (gstrng,rprts(nprt,1),error)
              if (error) then
                ermes(1:30)  = 'Partition Minimum Boundary Inp'
                ermes(31:60) = 'ut Conversion                 '
                call errhan ('CONDRV',1,ermes,errsev)
                found = .false.
                done  = .true.
              end if
              rprts(nprt,1) = rprts(nprt,1) * scale
            end if

C  Goto maximum column

            if (.not. error) then
              call next (whline,i,error)
              if (error) then
                call errhan ('CONDRV',1,q,errsev)
                found = .false.
                done  = .true.
              end if
            end if

            if (.not. error) then
              call search (whline,i,error)
              if (error) then
                call errhan ('CONDRV',1,p,errsev)
                found = .false.
                done  = .true.
              end if
            end if

C  An M,m in the maximum column means that that partition ends with the
C  largest value in the data

            if ((whline(i:i) .eq. 'M') .or.
     *          (whline(i:i) .eq. 'm')) then
              rprts(nprt,2) = rmax
              done = .true.
            else
              j = i
              call next (whline,i,error)
              gstrng(1:i-j) = whline(j:i-1)
              do 70 k = i-j+1,20
                gstrng(k:k) = ' '
70            continue
              call gtreal (gstrng,rprts(nprt,2),error)
              if (error) then
                ermes(1:30)  = 'Partition Maximum Boundary Inp'
                ermes(31:60) = 'ut Conversion                 '
                call errhan ('CONDRV',1,ermes,errsev)
                found = .false.
                done  = .true.
              end if
              rprts(nprt,2) = rprts(nprt,2) * scale
            end if
          end if

          if (.not. error) then
            call next (whline,i,error)
            if (error) then
              call errhan ('CONDRV',1,q,errsev)
              found = .false.
              done  = .true.
            end if
          end if

C  The next column in the table is for COLOR 1.  COLOR 1 is the color to use
C  on the partition if CMETH = 2,3 and is the color to ramp FROM if CMETH = 4,5

          if ((cmeth .ge. 2) .and. (cmeth .le. 5) .and.
     *        (.not. error)) then
            call search (whline,i,error)
            if (error) then
              call errhan ('CONDRV',1,p,errsev)
              found = .false.
              done  = .true.
            end if
          end if

          if ((cmeth .ge. 2) .and. (cmeth .le. 5) .and.
     *        (.not. error)) then
            cermes(1:16) = 'Color 1 On Line '
            if (nprt .le. 9) then
              write (cermes(17:17),30) nprt
              cermes (18:36) = ' Of The Partitions'
              cmln = 35
            else
              write (cermes(17:18),20) nprt
              cermes (19:36) = ' Of The Partitions'
              cmln = 36
            end if
            call crdrci (.true.,error,pcolor(nprt,1),0,whline,i,
     *                   cermes,cmln,errsev,noplt,'CONDRV')
            if (error) then
              found = .false.
              done  = .true.
            end if
            if (noplt) goto 110

            if (.not. error) then
              call next (whline,i,error)
              if (error) then
                if (((cmeth .eq. 2) .or. (cmeth .eq. 3)) .and.
     *              (dlwv(1) .ge. 0.0) .and. (ddpv(1) .ne. 0)) then
                  call errhan ('CONDRV',0,q,errsev)
                else
                  call errhan ('CONDRV',1,q,errsev)
                  found = .false.
                  done  = .true.
                end if
              end if
            end if
          end if

C  The next column in the table is for COLOR 1.  COLOR 2 is the color to
C  ramp TO if CMETH = 4,5

          if ((cmeth .eq. 4) .or. (cmeth .eq. 5) .and.
     *        (.not. error)) then
            call search (whline,i,error)
            if (error) then
              call errhan ('CONDRV',1,p,errsev)
              found = .false.
              done  = .true.
            end if
          end if

          if ((cmeth .eq. 4) .or. (cmeth .eq. 5) .and.
     *        (.not. error)) then
            cermes(1:16) = 'Color 2 On Line '
            if (nprt .le. 9) then
              write (cermes(17:17),30) nprt
              cermes (18:36) = ' Of The Partitions'
              cmln = 35
            else
              write (cermes(17:18),20) nprt
              cermes (19:36) = ' Of The Partitions'
              cmln = 36
            end if
            call crdrci (.true.,error,pcolor(nprt,2),1,whline,i,
     *                   cermes,cmln,errsev,noplt,'CONDRV')
            if (error) then
              found = .false.
              done  = .true.
            end if
            if (noplt) goto 110

            if (.not. error) then
              call next (whline,i,error)
              if (error) then
                if ((dlwv(1) .ge. 0.0) .and. (ddpv(1) .ne. 0)) then
                  call errhan ('CONDRV',0,q,errsev)
                else
                  call errhan ('CONDRV',1,q,errsev)
                  found = .false.
                  done  = .true.
                end if
              end if
            end if
          end if

C  The next column in the table is for line width if it is needed

          if (dlwv(1) .lt. 0.0) then
            call search (whline,i,error)
            if (error) then
              call errhan ('CONDRV',1,p,errsev)
              found = .false.
              done  = .true.
            end if
          end if

          if (dlwv(1) .lt. 0.0) then
            if ((whline(i:i) .eq. 'D') .or.
     *          (whline(i:i) .eq. 'd')) then
              plwv(nprt) = 1.0
            else
              j = i
              call next (whline,i,error)
              gstrng(1:i-j) = whline(j:i-1)
              do 80 k = i-j+1,20
                gstrng(k:k) = ' '
80            continue
              call gtreal (gstrng,plwv(nprt),error)
              if (error) then
                ermes(1:30)  = 'Line Width Multiplier On Line '
                if (nprt .le. 9) then
                  write (ermes(31:31),30) nprt
                  ermes(32:60) = ' Input Conversion            '
                else
                  write (ermes(31:32),20) nprt
                  ermes(33:60) = ' Input Conversion           '
                end if
                call errhan ('CONDRV',1,ermes,errsev)
                found = .false.
                done  = .true.
              end if

C  Make sure the given value is within reason

              if (plwv(nprt) .lt. 1.0) then
                ermes(1:27)  = 'Line Width Entered On Line '
                if (nprt .le. 9) then
                  write (ermes(28:28),30) nprt
                  ermes(29:60) = ' Is Too Small, 1 Used           '
                else
                  write (ermes(28:29),20) nprt
                  ermes(30:60) = ' Is Too Small, 1 Used          '
                end if
                call errhan ('CONDRV',0,ermes,errsev)
                plwv(nprt) = 1.0
              else if (plwv(nprt) .gt. 10.0) then
                ermes(1:27)  = 'Line Width Entered On Line '
                if (nprt .le. 9) then
                  write (ermes(28:28),30) nprt
                  ermes(29:60) = ' Is Too Large, 10 Used          '
                else
                  write (ermes(28:29),20) nprt
                  ermes(30:60) = ' Is Too Large, 10 Used         '
                end if
                call errhan ('CONDRV',0,ermes,errsev)
                plwv(nprt) = 10.0
              end if
            end if

            if (.not. error) then
              call next (whline,i,error)
              if (error) then
                if (ddpv(1) .ne. 0) then
                  call errhan ('CONDRV',0,q,errsev)
                else
                  call errhan ('CONDRV',1,q,errsev)
                  found = .false.
                  done  = .true.
                end if
              end if
            end if
          end if

C  The next column in the table is for dash pattern if it is needed

          if (ddpv(1) .eq. 0) then
            call search (whline,i,error)
            if (error) then
              call errhan ('CONDRV',1,p,errsev)
              found = .false.
              done  = .true.
            end if
          end if

          if (ddpv(1) .eq. 0) then
            if ((whline(i:i) .eq. 'D') .or.
     *          (whline(i:i) .eq. 'd') .or.
     *          (whline(i:i+1) .eq. 'SO') .or.
     *          (whline(i:i+1) .eq. 'So') .or.
     *          (whline(i:i+1) .eq. 'so') .or.
     *          (whline(i:i+1) .eq. 'sO')) then
              pdpv(nprt) = -1
            else if ((whline(i:i) .eq. 'L') .or.
     *               (whline(i:i) .eq. 'l')) then
              pdpv(nprt) = 255
            else if ((whline(i:i) .eq. 'M') .or.
     *               (whline(i:i) .eq. 'm')) then
              pdpv(nprt) = 3855
            else if ((whline(i:i+1) .eq. 'SM') .or.
     *               (whline(i:i+1) .eq. 'Sm') .or.
     *               (whline(i:i+1) .eq. 'sM') .or.
     *               (whline(i:i+1) .eq. 'sm')) then
              pdpv(nprt) = 13107
            else if ((whline(i:i) .eq. 'T') .or.
     *               (whline(i:i) .eq. 't')) then
              pdpv(nprt) = 21845
            else
              ermes(1:27)  = 'Dash Pattern Given On Line '
              if (nprt .le. 9) then 
                write (ermes(28:28),30) nprt
                ermes(29:60) = ' Is Unknown, SO Used            '
              else
                write (ermes(28:29),20) nprt
                ermes(30:60) = ' Is Unknown, SO Used           '
              end if
              call errhan ('CONDRV',0,ermes,errsev)
              pdpv(nprt) = -1
            end if

            if (.not. error) then
              call next (whline,i,error)
              if (error) then
                call errhan ('CONDRV',0,q,errsev)
              end if
            end if
          end if

C  Check for extra entries on the line

          if (.not. error) then
            call search (whline,i,error)
            if (.not. error) then
              ermes(1:25)  = 'Too Many Entries On Line '
              if (nprt .le. 9) then
                write (ermes(26:26),30) nprt
                ermes(27:60) = ' Of The Partitions Table          '
              else
                write (ermes(26:27),20) nprt
                ermes(28:60) = ' Of The Partitions Table         '
              end if
              call errhan ('CONDRV',0,ermes,errsev)
            end if
          end if

C  Go back and read the next line if that was not the last one

          if (.not. done) then
            read (unum,90,err=100,end=100) whline(1:80)
            i = 1
            call search (whline,i,error)
            if (error) then
              ermes(1:30)  = 'More Lines Expected In Partiti'
              ermes(31:60) = 'ons Table                     '
            else
              goto 10
            end if
            goto 105
100         ermes(1:30)  = 'Could Not All Of The Partition'
            ermes(31:60) = 's Table                       '
105         call errhan ('CONDRV',1,ermes,errsev)
            found = .false.
          else
            if (found) print *, 'CONDRV - Contour Partitions Set Up'
          end if
        end if

C  If the table was not found, deliver an error message

        if (.not. found) then
          if (nprt .le. 1) then
            ermes(1:30)  = 'Partition Table Expected But N'
            ermes(31:60) = 'ot Found                      '
            call errhan ('CONDRV',1,ermes,errsev)
            nprt        = 1
            iprts(1,1)  = imin
            iprts(1,2)  = imax
            ints        = .true.
            pcolor(1,1) = 0
            pcolor(1,1) = 1
            plwv(1)    = 1.0
            pdpv(1)    = -1
          else

C  If the table was found bu not complete give a different error message

            ermes(1:30)  = 'Partition Table Not Complete  '
            ermes(31:60) = '                              '
            call errhan ('CONDRV',1,ermes,errsev)
            if (ints) then
              iprts(nprt,1)  = iprts(nprt-1,2)
              iprts(nprt,2)  = imax
            else
              rprts(nprt,1)  = rprts(nprt-1,2)
              rprts(nprt,2)  = rmax
            end if
            pcolor(nprt,1)   = 0
            pcolor(nprt,1)   = 1
            plwv(nprt)      = 1.0
            pdpv(nprt)      = -1
          end if
        end if
      end if

C*****************************  Subroutine End  *****************************C

C  Format statements begin ...

20    format (I2)
30    format (I1)
90    format (A80)

C  Format statements end.

110   return
      end
      subroutine crdttl (whline,errsev,tputl,ofilb)

C*****************************************************************************C
C  crdttl   - This is for CONDRV                                              C
C  Section  - Tables                                                          C
C  Purpose  - To parse out  the title  information  given in the  CON DETAILS C
C             table.                                                          C
C                                                                             C
C  On entry - WHLINE contains the line from the table giving all of the title C
C             information.  ERRSEV indicates what severity of error will halt C
C             execution.   TPUTL is 0  if label bar information is to be read C
C             and is 1 if title information is to be read.                    C
C                                                                             C
C  On exit  - All variables in the common block TITDET have been properly set C
C             up.  OFILB is true if the title box will be filled and is false C
C             otherwise.                                                      C
C                                                                             C
C  Assume   - Nothing                                                         C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             SEARCH              CONDRV/MAPDRV utility                       C
C             NEXT                CONDRV/MAPDRV utility                       C
C             ERRHAN              CONDRV/MAPDRV utility                       C
C             GTREAL              CONDRV/MAPDRV utility                       C
C             ----------------------------------------------------------------C
C                                                                             C
C  Author   - Jeremy Asbill       Date - June 29, 1990      for the MM4 club  C
C*****************************************************************************C

C  Character variables

      character*80     whline            ! a whole line from the table     (in)
      character*60     ermes,            ! error mess. string, general  (local)
     *                 p,                ! error message string, SEARCH (local)
     *                 q                 ! error message string, NEXT   (local)
      character*20     gstrng            ! temporary, skimpy string     (local)

C  Integer variables

      integer          errsev,           ! error severity comparitor       (in)
     *                 tputl             ! label bar/title indicator       (in)
      integer          tsize             ! for common block TITDET
      integer          tqual(2)          ! for common block TIQDET
      integer          i,j,k             ! loop counters/place keepers  (local)

C  Logical variables

      logical          ofilb             ! out version of TFILB           (out)
      logical          tputb,            ! for common block TITDET
     *                 tputp,            ! for common block TITDET
     *                 tfilb             ! for common block TITDET
      logical          error             ! has an error occured ?       (local)

C  Real variables

      real             tprlw             ! for common block TITDET

C  Common blocks

      common /titdet/  tputb,            ! put a box around the title ?
     *                 tputp,            ! draw the perimeter of the box ?
     *                 tfilb,            ! fill the box ?
     *                 tprlw,            ! title box perim. line width
     *                 tsize             ! title character size
      common /tiqdet/  tqual             ! title character quality

C****************************  subroutine begin  *****************************C

C  Set up I to use as a counter

      i = 1

C  Initialize the error flag

      error = .false.

C  Set up SEARCH and NEXT error strings

      p(1:23)  = 'Reading Line Label Info'
      p(24:60) = 'rmation Line, Too Few Entries On Line'
      q(1:23)  = p(1:23)
      q(24:60) = 'rmation Line, Entry Is Bizarre       '

C  Whline being passed in as blanks is the same as assigning the defaults

      call search (whline,i,error)
      if ((error) .and. (tputl .eq. 1)) then
        tputb    = .false.
        tputp    = .false.
        tfilb    = .false.
        tsize    = 10
        tqual(1) = 1
        tqual(2) = 1
      else if ((error) .and. (tputl .eq. 0)) then
        tputb    = .true.
        tsize    = 0
        tqual(1) = 0
        tqual(2) = 0
      end if

C  The title box flag should be read in whether this is a label bar
C  or a regular title
C  Y & Label Bar => make label bar one big bar
C  N & Label Bar => break label bar into individual blocks
C  Y & Title     => put a box around the title
C  N & Title     => title should not be boxed
C  anything else => give a warning message

      if (.not. error) then
        if ((whline(i:i) .eq. 'Y') .or. (whline(i:i) .eq. 'y')) then
          tputb = .true.
        else if ((whline(i:i) .eq. 'N') .or.
     *           (whline(i:i) .eq. 'n')) then
          tputb = .false.
        else if (tputl .eq. 1) then
          ermes(1:30)  = 'Title Box Flag Is Inconclusive'
          ermes(31:60) = ', N Assumed                   '
          call errhan ('CONDRV',0,ermes,errsev)
          tputb = .false.
        else if (tputl .eq. 0) then
          ermes(1:30)  = 'Label Bar Box Flag Is Inconclu'
          ermes(31:60) = 'sive, Y Assumed               '
          call errhan ('CONDRV',0,ermes,errsev)
          tputb = .true.
        end if
        call next (whline,i,error)
        if ((error) .and. (tputl .eq. 1)) then
          call errhan ('CONDRV',1,q,errsev)
          tsize    = 10
          tqual(1) = 1
          tqual(2) = 1
        else if ((error) .and. (tputl .eq. 0)) then
          call errhan ('CONDRV',1,q,errsev)
          tsize    = 0
          tqual(1) = 0
          tqual(2) = 0
        end if
      end if

C  If TPUTL indicates a title and a box was requested,  parse out the box
C  perimeter flag and the box fill flag

      if ((tputb) .and. (tputl .eq. 1) .and. (.not. error)) then
        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          tputp    = .false.
          tsize    = 10
          tqual(1) = 1
          tqual(2) = 1
        end if
      end if

      if ((tputb) .and. (tputl .eq. 1) .and. (.not. error)) then
        if ((whline(i:i) .eq. 'N') .or. (whline(i:i) .eq. 'n')) then
          tputp = .false.
        else if ((whline(i:i) .eq. 'Y') .or.
     *           (whline(i:i) .eq. 'y')) then
          tputp = .true.
        else
          ermes(1:30)  = 'Title Box Perimeter Flag Is In'
          ermes(31:60) = 'conclusive, N Assumed         '
          call errhan ('CONDRV',0,ermes,errsev)
          tputp = .false.
        end if
        call next (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,q,errsev)
          tsize    = 10
          tqual(1) = 1
          tqual(2) = 1
        end if
      else
        tputp = .false.
      end if

C  If TPUTL indicates a title and a box was requested, parse to see if the
C  box should be filled
C  Y             => Fill the box
C  N             => Leave the box hollow
C  anything else => give a warning message

      
      if ((tputb) .and. (tputl .eq. 1) .and. (.not. error)) then
        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          tfilb    = .false.
          tsize    = 10
          tqual(1) = 1
          tqual(2) = 1
        end if
      end if

      if ((tputb) .and. (tputl .eq. 1) .and. (.not. error)) then
        if ((whline(i:i) .eq. 'Y') .or. (whline(i:i) .eq. 'y')) then
          tfilb = .true.
        else if ((whline(i:i) .eq. 'N') .or.
     *           (whline(i:i) .eq. 'n')) then
          tfilb = .false.
        else
          ermes(1:30)  = 'Title Box Fill Flag Is Inconcl'
          ermes(31:60) = 'usive, N Assumed              '
          call errhan ('CONDRV',0,ermes,errsev)
          tfilb = .false.
        end if
        call next (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,q,errsev)
          tsize    = 10
          tqual(1) = 1
          tqual(2) = 1
        end if
      else
        tfilb = .false.
      end if

C  Parse the perimeter line with for a label bar or a title
C  D,d    => use normal line width

      if ((((tputb) .and. (tputp)) .or. (tputl .eq. 0)) .and.
     *    (.not. error)) then
        call search (whline,i,error)
        if ((error) .and. (tputl .eq. 1)) then
          call errhan ('CONDRV',1,p,errsev)
          tsize    = 10
          tqual(1) = 1
          tqual(2) = 1
        else if ((error) .and. (tputl .eq. 0)) then
          call errhan ('CONDRV',1,p,errsev)
          tsize    = 0
          tqual(1) = 0
          tqual(2) = 0
        end if
      end if

      if ((((tputb) .and. (tputp)) .or. (tputl .eq. 0)) .and.
     *    (.not. error)) then
        if ((whline(i:i) .eq. 'd') .or. (whline(i:i) .eq. 'D')) then
          tprlw = 1.0
          call next (whline,i,error)
        else
          j = i
          call next (whline,i,error)
          if (.not. error) then
            gstrng(1:i-j) = whline(j:i-1)
            do 10 k = i-j+1,20
              gstrng(k:k) = ' '
10          continue
            call gtreal (gstrng,tprlw,error)
            if ((error) .and. (tputl .eq. 1)) then
              ermes(1:30)  = 'Could Not Read Title Box Perim'
              ermes(31:60) = 'eter Width, 1.0 Used          '
              call errhan ('CONDRV',0,ermes,errsev)
              tprlw = 1.0
              error = .false.
            else if ((error) .and. (tputl .eq. 0)) then
              ermes(1:30)  = 'Could Not Read Label Bar Perim'
              ermes(31:60) = 'eter Width, 1.0 Used          '
              call errhan ('CONDRV',0,ermes,errsev)
              tprlw = 1.0
              error = .false.
            end if
          end if
        end if
        if ((error) .and. (tputl .eq. 1)) then
          call errhan ('CONDRV',1,q,errsev)
          tsize    = 10
          tqual(1) = 1
          tqual(2) = 1
        else if ((error) .and. (tputl .eq. 0)) then
          call errhan ('CONDRV',1,q,errsev)
          tsize    = 0
          tqual(1) = 0
          tqual(2) = 0
        end if
      else
        tprlw  = 0.0
      end if
      
C  If TPUTL implies a title, then parse character quality
C  0 in space 1 => complex character set should be used
C  1 in space 1 => duplex character set should be used
C  0 in space 2 => high quality characters
C  1 in space 2 => medium quality
C  1 in space 3 => low quality

      if ((.not. error) .and. (tputl .eq. 1)) then
        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          tsize    = 10
          tqual(1) = 1
          tqual(2) = 1
        end if
      end if

      if ((.not. error) .and. (tputl .eq. 1)) then
        tqual(2) = -1
        if ((whline(i:i) .eq. 'd') .or. (whline(i:i) .eq. 'D')) then
          tqual(1) = 1
          tqual(2) = 1
        else if (whline(i:i) .eq. '0') then
          tqual(1) = 0
        else if (whline(i:i) .eq. '1') then
          tqual(1) = 1
        else
          ermes(1:30)  = 'Title Text Type Is Invalid, 1 '
          ermes(31:60) = 'Used (Duplex)                 '
          call errhan ('CONDRV',0,ermes,errsev)
          tqual(1) = 1
        end if
        if (tqual(2) .eq. -1) then
          i = i + 1
          if ((whline(i:i) .eq. 'd') .or. (whline(i:i) .eq. 'D')) then
            tqual(2) = 1
          else if (whline(i:i) .eq. '0') then
            tqual(2) = 0
          else if (whline(i:i) .eq. '1') then
            tqual(2) = 1
          else if (whline(i:i) .eq. '2') then
            tqual(2) = 2
          else
            ermes(1:30)  = 'Title Text Quality Is Invalid,'
            ermes(31:60) = ' 1 Used (Medium)              '
            call errhan ('CONDRV',0,ermes,errsev)
            tqual(2) = 1
          end if
        end if
        call next (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,q,errsev)
          tsize    = 10
        end if
      end if
          
C  If TPUTL implies a title, parse out the title character size
C  This should be specified in plotter coordinates

      if ((.not. error) .and. (tputl .eq. 1)) then
        call search (whline,i,error)
        if (error) then
          call errhan ('CONDRV',1,p,errsev)
          tsize = 10
        end if
      end if

      if ((.not. error) .and. (tputl .eq. 1)) then
        if ((whline(i:i) .eq. 'D') .or. (whline(i:i) .eq. 'd')) then
          tsize = 10
        else if ((whline(i+1:i+1) .ne. ' ') .and.
     *           (whline(i+1:i+1) .ne. '|')) then
          read  (whline(i:i+1),20,err=40) tsize
        else
          read (whline(i:i),30,err=40) tsize
        end if
        goto 45

C  Inform the user of the error if here

40      ermes(1:30)  = 'Title Character Size Input Con'
        ermes(31:60) = 'version                       '
        call errhan ('CONDRV',1,ermes,errsev)
        tsize = 10
        error = .true.

45      if (.not. error) then
          call next (whline,i,error)
          if (error)
     *      call errhan ('CONDRV',0,q,errsev)
        end if
      end if

C  Check for extra entries on the end of the line

      if (.not. error) then
        call search (whline,i,error)
        if ((.not. error) .and. (tputl .eq. 1)) then
          ermes(1:30)  = 'Reading Title Information Line'
          ermes(31:60) = ', Too Many Entries On Line    '
          call errhan ('CONDRV',0,ermes,errsev)
        else if ((.not. error) .and. (tputl .eq. 0)) then
          ermes(1:30)  = 'Reading Label Bar Information '
          ermes(31:60) = 'Line, Too Many Entries On Line'
          call errhan ('CONDRV',0,ermes,errsev)
        end if
      end if

C  Check for value errors
C  TPRLW must be between 1.0 and 10.0 inclusive

      if ((tprlw .lt. 1.0) .and. ((tputl .eq. 0) .or. (tputp))) then
        if (tputl .eq. 1) then
          ermes(1:30)  = 'Title Box Perimeter Line Width'
          ermes(1:30)  = 'Is Too Small, 1 Used          '
        else
          ermes(1:30)  = 'Label Bar Perimeter Line Width'
          ermes(1:30)  = 'Is Too Small, 1 Used          '
        end if
        call errhan ('CONDRV',0,ermes,errsev)
        tprlw = 1.0
      end if
      if ((tprlw .gt. 10.0) .and. ((tputl .eq. 0) .or. (tputp))) then
        if (tputl .eq. 1) then
          ermes(1:30)  = 'Title Box Perimeter Line Width'
          ermes(1:30)  = 'Is Too Large, 10 Used         '
        else
          ermes(1:30)  = 'Label Bar Perimeter Line Width'
          ermes(1:30)  = 'Is Too Large, 10 Used         '
        end if
        call errhan ('CONDRV',0,ermes,errsev)
        tprlw = 10.0
      end if

C  TSIZE must be between 1 and 25 inclusive

      if ((tsize .lt. 1) .and. (tputl .eq. 1)) then
        ermes(1:30)  = 'Title Character Size Is Too Sm'
        ermes(1:30)  = 'all, 1 Used                   '
        call errhan ('CONDRV',0,ermes,errsev)
        tsize = 1
      end if
      if ((tsize .gt. 25) .and. (tputl .eq. 1)) then
        ermes(1:30)  = 'Title Character Size Is Too La'
        ermes(1:30)  = 'rge, 1 Used                   '
        call errhan ('CONDRV',0,ermes,errsev)
        tsize = 25
      end if

C  Assign output variables

      ofilb = tfilb

C*****************************  subroutine end  ******************************C

C  Format statements begin ...

20    format (I2)
30    format (I1)

C  Format statements end.

      return
      end

      subroutine csetcl (tsize,title,pnum,scale,errsev)

C*****************************************************************************C
C  csetcl   - This is a CONDRV routine                                        C
C  Section  - Design                                                          C
C  Purpose  - To set up the color of contour levels,  style and placement  of C
C             line labels, style of high/low labels and the color of the pre- C
C             viously mentioned labels.  To set the color of the perimeter if C
C             one was requested.                                              C
C                                                                             C
C  On entry - TITLE is the title string to draw with the plot.   TSIZE is the C
C             number of characters in TITLE.   PNUM is the number of overlays C
C             done previous to this one plus two.  SCALE is the scale  factor C
C             to  be used when labeling.   ERRSEV indicates at what  severity C
C             or an error, execution should halt.                             C
C                                                                             C
C  On exit  - Color and label information for the entire plot has been set.   C
C                                                                             C
C  Assume   - GKS is open.  CONPACK has been initialized.                     C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             SETTTL              CONDRV utility                              C
C             CPSETI              CONPACK utility*                            C
C             SETHLO              CONDRV utility                              C
C             CPSETC              CONPACK utility                             C
C             CRAMPS              CONDRV utility                              C
C             SETCOL              CONDRV utility                              C
C             SETLAB              CONDRV utility                              C
C             MKLBAR              CONDRV utility                              C
C             ----------------------------------------------------------------C
C             * NCAR Graphics Routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill     Date - August 10, 1990      for the MM4 club  C
C*****************************************************************************C

C  Character variables

      character*120    title             ! title string                    (in)

C  Integer variables

      integer          tsize,            ! # of chars in TITLE             (in)
     *                 pnum,             ! indicates what overlay this is  (in)
     *                 errsev            ! error severity comparitor       (in)
      integer          lputl,            ! for common block QLBDET
     *                 tputl             ! for common block QLBDET
      integer          pcol              ! for common block PERCOL

C  Logical variables

      logical          hputl             ! for common block QLBDET
      logical          prput             ! for common block PERDET
      logical          noplt             ! for common block NOPLOT

C  Real variables

      real             scale             ! scale factor to use             (in)

C  Common blocks

      common /qlbdet/  hputl,            ! draw in high/low labels ?
     *                 lputl,            ! draw in line labels
     *                 tputl             ! draw in the title
      common /perdet/  prput             ! put in a perimeter ?
      common /percol/  pcol              ! color index for perimeter
      common /noplot/  noplt             ! has a non-correctable erro occured ?

C****************************  Subroutine Begin  *****************************C

C  CONPACK internal parameters used in this routine are :
C  LBC - Label Box fill Color index
C  HLT - High/Low label Text
C  LLP - Line Label Positioning flag
C  ILT - Information Label Text

C  Set up the title information, if no plot will be drawn or a title is
C  requested

      if ((noplt) .or. ((tputl .eq. 1) .and. (tsize .ne. 0))) then
        call setttl (tsize,title,pnum,scale)
      else
        call cpsetc ('ILT',' ')
      end if

C  If there should be no plot drawn, don't bother with setting up the colors

      if (.not. noplt) then

C  Tell CONPACK to use the current fill color index to fill all types
C  of label boxes

        call cpseti ('LBC',-1)

C  Set up the high/low information, if a plot will be drawn and they
C  are requested

        if (hputl) then
          call sethlo
        else
          call cpsetc ('HLT',' '' ')
        end if

C  Set up the color ramps requested by the user

        call cramps

C  Set up the colors as they relate to individual lines

        call setcol

C  Set the color of the perimeter if a perimeter was requested

        if (prput) call gacolr (pcol,pcol,pcol,pcol)

C  Set up all the line label information, if line labels were requested

        if (lputl .ge. 0) then
          call setlab (lputl,errsev)
        else
          call cpseti ('LLP',0)
        end if
      end if

C  If a label bar was requested, make it here

      if ((.not. noplt) .and. (tputl .eq. 0)) call mklbar
      
C*****************************  Subroutine End  ******************************C

      return
      end
      subroutine csetwn (xstr,ystr,xend,yend,doset)

C*****************************************************************************C
C  csetwn   - This is a CONDRV routine                                        C
C  Section  - Design                                                          C
C  Purpose  - To set up the proper viewport in which to draw the contours.    C
C                                                                             C
C  On entry - XSTR, YSTR are the first point in the grid to be plotted.  XEND C
C             and YEND are the last point in the  grid to be plotted.   These C
C             two points should correspond to the lower left and  upper right C
C             respecitvely, of a map on which the picture is overlayed. DOSET C
C             is 1 if CONDRV should make a standard  set call and is 0 if  it C
C             should use the users last call to the SPPS routine SET.  If DO- C
C             set is negative CONDRV  makes the set call  considering a cross C
C             point grid.                                                     C
C                                                                             C
C  On exit  - The proper window has been set.                                 C
C                                                                             C
C  Assume   - GKS is open                                                     C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             CPSETR              CONPACK utility*                            C
C             CPSETI              CONPACK utility*                            C
C             GETSET              SPPS*                                       C
C             SET                 SPPS*                                       C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill      Date - August 9, 1990      for the MM4 club  C
C*****************************************************************************C

C  Integer variables

      integer          xstr,             ! x coord. of first grid point    (in)
     *                 ystr,             ! y coord. of first grid point    (in)
     *                 xend,             ! x coord. of last grid point     (in)
     *                 yend,             ! y coord. of last grid point     (in)
     *                 doset             ! set call type indicator         (in)
      integer          lltp              ! junk filler                  (local)

C  Real variables

      real             temp,             ! temporary test variable      (local)
     *                 dumy,             ! dummy test variable          (local)
     *                 right,            ! right side of view port      (local)
     *                 left,             ! left side of view port       (local)
     *                 top,              ! top of view port             (local)
     *                 bottom,           ! bottom of view port          (local)
     *                 test,             ! calculation variable         (local)
     *                 junk,             ! calculation variable         (local)
     *                 ultp,urtp,        ! junk filler                  (local)
     *                 ubtp,uttp         ! junk filler                  (local)

C****************************  Subroutine Begin  *****************************C

C  CONPACK internal parameters used in this routine are:
C  VPB - View Port Bottom
C  VPT - View Port Top
C  VPL - View Port Left
C  VPR - View Port Right
C  SET - do-SET-call flag

C  Set up variables to test on
C  TEMP will represent the maps width
C  DUMY will represent the maps height

      if (doset .lt. 0) then
        temp = float(yend - ystr + 2)
        dumy = float(xend - xstr + 2) * 0.9
      else
        temp = float(yend - ystr + 1)
        dumy = float(xend - xstr + 1) * 0.9
      end if

C  Ckeck to see if a standard set should be done

      if (doset .gt. 0) then

C  If the plot is somewhere between almost square and being taller than
C  it is wide, then use 80% of the screen in the plots largest direction
C  otherwise use 90%.

        if (temp .ge. dumy) then
          call cpsetr ('VPB',0.10)
          call cpsetr ('VPL',0.10)
          call cpsetr ('VPT',0.90)
          call cpsetr ('VPR',0.90)
        else
          call cpsetr ('VPB',0.05)
          call cpsetr ('VPL',0.05)
          call cpsetr ('VPT',0.95)
          call cpsetr ('VPR',0.95)
        end if
        call cpseti ('SET',1)
      else if (doset .eq. 0) then

C  A standard set should not be done if execution gets here
C  Use the users last call to the SPPS routine SET

        call getset (left,right,bottom,top,ultp,urtp,ubtp,uttp,lltp)

C  Adjust the users set call, that is if the plot is somewhere between
C  being almost as tall as it is wide and being taller than it is wide
C  only use 80% of the users view port, else use 90% of the users view-
C  port

        if (temp .ge. dumy) then
          test   = 0.1 * (right - left)
          right  = right - test
          left   = left + test
          test   = 0.1 * (top - bottom)
          top    = top - test
          bottom = bottom + test
        else
          test   = 0.05 * (right - left)
          right  = right - test
          left   = left + test
          test   = 0.05 * (top - bottom)
          top    = top - test
          bottom = bottom + test
        end if

C  Set up the users set call with CONPACK

        call set (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)
        call cpsetr ('VPB',bottom)
        call cpsetr ('VPL',left)
        call cpsetr ('VPT',top)
        call cpsetr ('VPR',right)
        call cpseti ('SET',1)
      else

C  A standard set call is to be made except considering cross points
C  rather than dot points
C  First determine how much graphics space is one-half grid

        if (temp .ge. dumy) then
          temp = float(yend - ystr + 1)
          dumy = float(xend - xstr + 1)
          if (temp .ge. dumy) then
            test   = 0.80 * 0.5/(yend - ystr + 1)
            bottom = 0.1 + test
            top    = 0.9 - test
          else
            test   = 0.80 * 0.5/(xend - xstr + 1)
            left   = 0.1 + test
            right  = 0.9 - test
          end if
        else
          test = 0.90 * 0.5/(xend - xstr + 1)
          left   = 0.05 + test
          right  = 0.95 - test
        end if
        if (temp .ge. dumy) then
          junk     = (1.0 - (2.0 * test * (xend - xstr + 1))) * 0.5
          right    = 1.0 - junk - test
          left     = 0.0 + junk + test
        else
          junk     = (1.0 - (2.0 * test * (yend - ystr + 1))) * 0.5
          top      = 1.0 - junk - test
          bottom   = 0.0 + junk + test
        end if
        call set (left,right,bottom,top,
     *            1.0,xend-xstr+1.0,1.0,yend-ystr+1.0,1)
        call cpseti ('SET',0)
      end if

C*****************************  Subroutine End  ******************************C

      return
      end
      subroutine drawcl (xcs,ycs,ncs,aid,gid,nid)

C*****************************************************************************C
C  drawcl   - This is a CONDRV routine                                        C
C  Section  - Contour Lines                                                   C
C  Purpose  - To draw in the contour lines when masking for label boxes.      C
C                                                                             C
C  On entry - XCS and YCS contain NCS coordinate pairs that describe a  piece C
C             of a contour line.   GID contains NID group identifiers of  the C
C             polygon in which the piece of line exists.   AID  contains  NID C
C             area identifiers associated with each of the NID group identif- C
C             iers in GID that tell where the polygon is.                     C
C                                                                             C
C  On exit  - If that piece of a line was inside  the plot viewport but  out- C
C             side of any label boxes it was drawn.                           C
C                                                                             C
C  Assume   - GKS is open.  The proper line width, color and dash pattern are C
C             already assigned.                                               C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             CURVED              DASHLINE utility*                           C
C             ----------------------------------------------------------------C
C             * NCAR Graphics Routine                                         C
C                                                                             C
C             There is a bug in CONPACK.  This routine is not always 100% ac- C
C             curate do to the information CONPACK sends it.                  C
C                                                                             C
C  Author   - Jeremy Asbill      Date - August 14, 1990     for the MM4 club  C
C*****************************************************************************C

C  Character variables

      character*2      mask          ! for common block MAPFLI

C  Integer variables

      integer          aid(*),       ! area identifiers for the polygon    (in)
     *                 gid(*),       ! group identifiers for the polygon   (in)
     *                 nid,          ! dimension of identifier arrays      (in)
     *                 ncs           ! number of points in XCS and YCS     (in)
      integer          i,            ! loop counter                     (local)
     *                 idmp          ! map area identifier              (local)

C  Logical variables

      logical          dodr          ! draw the line ?                  (local)

C  Real variables

      real             xcs(*),       ! x coord. of points on the line      (in)
     *                 ycs(*)        ! y coord. of points on the line      (in)

C  Common blocks

      common /mapfli/  mask          ! map masking indicator

C****************************  Subroutine Begin  *****************************C

C  Initialize the draw flag

      dodr = .false.

C  Determine the map area identifier that lies under the line in question

      do 10 i = 1,nid
        if (gid(i) .eq. 6) idmp = aid(i)

C  There will always only be one element in AID and GID
C  GID(1) will be 3 and AID(1) will either be -1 or 0
C  If AID(1) is zero then we are outside of any label boxes and inside of
C  the plot viewport
C  If AID(1) is anything else we are either outside of the viewport or
C  inside of a label box

        if ((gid(i) .eq. 3) .and. (aid(i) .eq. 0)) dodr = .true.
10    continue

      if (dodr) then

C  Determine if the map masks out the line

        if ((mask(1:2) .eq. 'LO') .or. (mask(1:2) .eq. 'lo') .or.
     *      (mask(1:2) .eq. 'Lo') .or. (mask(1:2) .eq. 'lO')) then
          if (mapaci(idmp) .eq. 1) dodr = .false.
        else if ((mask(1:2) .eq. 'LL') .or. (mask(1:2) .eq. 'll') .or.
     *           (mask(1:2) .eq. 'Ll') .or. (mask(1:2) .eq. 'lL')) then
          if (idmp .eq. 2) dodr = .false.
        else if ((mask(1:2) .eq. 'OO') .or. (mask(1:2) .eq. 'oo') .or.
     *           (mask(1:2) .eq. 'Oo') .or. (mask(1:2) .eq. 'oO')) then
          if (idmp .ne. 2) dodr = .false.
        else if ((mask(1:2) .eq. 'OL') .or. (mask(1:2) .eq. 'ol') .or.
     *           (mask(1:2) .eq. 'Ol') .or. (mask(1:2) .eq. 'oL')) then
          if (mapaci(idmp) .ne. 1) dodr = .false.
        end if

C  Draw the line if it wasn't masked out anywhere

        if (dodr) call curved (xcs,ycs,ncs)
      end if

C*****************************  Subroutine End  ******************************C

      return
      end
      subroutine drwttl

C*****************************************************************************C
C  drwttl   - This is a CONDRV routine                                        C
C  Section  - Labels                                                          C
C  Purpose  - To draw in the title, information label, on the plot.           C
C                                                                             C
C  On entry - Locations, sizes and other needed information is passed in  via C
C             the common block TLOCAT and TITDET. The string itself is in the C
C             common block TSTRNG.                                            C
C                                                                             C
C  On exit  - The title, information label, has been drawn.                   C
C                                                                             C
C  Assume   - GKS is open.                                                    C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             SFSGFA              SOFTFILL utility*                           C
C             SETUSV              SPPS*                                       C
C             GSPLCI              GKS                                         C
C             LINE                SPPS*                                       C
C             PCSETI              PLOTCHAR utility*                           C
C             GSTXCI              GKS                                         C
C             PLCHHQ              PLOTCHAR utility*                           C
C             ----------------------------------------------------------------C
C             * NCAR Graphics Routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill    Date - November 2, 1990      for the MM4 club  C
C*****************************************************************************C

C  Character variables

      character*120    title             ! for common block TSTRNG

C  Integer variables

      integer          tlen              ! for common block TLOCAT
      integer          tcol(2)           ! for common block TLCOLS
      integer          tsize             ! for common block TITDET
      integer          tqual(2)          ! for common block TIQDET
      integer          ind(12)           ! work array for SOFTFILL      (local)

C  Logical variables

      logical          tputb,            ! for common block TITDET
     *                 tputp,            ! for common block TITDET
     *                 tfilb             ! for common block TITDET

C  Real variables

      real             tprlw             ! for common block TITDET
      real             csiz,             ! for common block TLOCAT
     *                 boxx(4),          ! for common block TLOCAT
     *                 boxy(4),          ! for common block TLOCAT
     *                 xpos,             ! for common block TLOCAT
     *                 ypos              ! for common block TLOCAT
      real             dst(8)            ! work array for SOFTFILL      (local)

C  Common blocks

      common /titdet/  tputb,            ! put a box around the title ?
     *                 tputp,            ! draw the perimeter of the box ?
     *                 tfilb,            ! fill the box ?
     *                 tprlw,            ! title box perim. line width
     *                 tsize             ! title character size
      common /tlocat/  xpos,             ! horizontal center of label
     *                 ypos,             ! vertical center of label
     *                 boxx,             ! four x coords of text extent box
     *                 boxy,             ! four y coords of text extent box
     *                 csiz,             ! character size to use
     *                 tlen              ! final title string length
      common /tstrng/  title             ! final title string
      common /tiqdet/  tqual             ! quality of characters
      common /tlcols/  tcol              ! title colors

C****************************  subroutine begin  *****************************C

C  PLOTCHAR internal parameters used are:

C  TPUTB is true if there should be a box around the title

      if (tputb) then

C  TFILB is true if the box should be filled in

        if (tfilb)
     *    call sfsgfa (boxx,boxy,4,dst,8,ind,12,tcol(2))

C  TPUTP is true if a perimeter should be drawn on the box

        if (tputp) then
          call setusv ('LW',nint(1000.0 * tprlw))
          call gsplci (tcol(1))
          call line (cfux(boxx(1)),cfuy(boxy(1)),cfux(boxx(2)),
     *               cfuy(boxy(2)))
          call line (cfux(boxx(2)),cfuy(boxy(2)),cfux(boxx(3)),
     *               cfuy(boxy(3)))
          call line (cfux(boxx(3)),cfuy(boxy(3)),cfux(boxx(4)),
     *               cfuy(boxy(4)))
          call line (cfux(boxx(4)),cfuy(boxy(4)),cfux(boxx(1)),
     *               cfuy(boxy(1)))
          call setusv ('LW',1000)
        end if
      end if

C  Set up the character quality for the title

      call pcseti ('CD',tqual(1))
      call pcseti ('QU',tqual(2))

C  Set up correct color for the information label
C  To understand what the quality of the letters has to do with the color
C  read on page 2-14 in the NCAR Graphics Guide to New Utilities Version 3.00
C  under the heading of PLOTCHAR

      if ((tqual(2) .eq. 0) .or. (tqual(2) .eq. 1)) then
        call gsplci (tcol(1))
      else
        call gstxci (tcol(1))
      end if

C  Draw the title

      call plchhq (xpos,ypos,title(1:tlen),csiz,0.0,0.0)

C****************************  subroutine end  *******************************C

      return
      end
      subroutine fillem (xpoly,ypoly,nep,aid,gid,nid)

C*****************************************************************************C
C  fillem   - This is a CONDRV routine                                        C
C  Section  - Fill                                                            C
C  Purpose  - To color in the contour levels being plotted by CONPACK.        C
C                                                                             C
C  On entry - XPOLY,YPOLY,NEP define a polygon to be filled.  AID,GID,NID     C
C             allow the routine to know when and how to shade.                C
C                                                                             C
C  On exit  - The incoming polygon has been colored the appropriate color.    C
C                                                                             C
C  Assume   - GKS is open.                                                    C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             SFSGFA              SOFTFILL utility*                           C
C             ----------------------------------------------------------------C
C             * NCAR Graphics Routine                                         C
C                                                                             C
C             This routine is called by the AREAS routine ARSCAM.             C
C                                                                             C
C  Author   - Jeremy Asbill       Date - June 12, 1990      for the MM4 club  C
C*****************************************************************************C

C  Character variables

      character*2      mask          ! for common block MAPFLI

C  Integer variables

      integer          aid(*),       ! area identifiers for the polygon    (in)
     *                 gid(*),       ! group identifiers for the polygon   (in)
     *                 nep,          ! number of points defining polygon   (in)
     *                 nid           ! dimension of identifier arrays      (in)
      integer          ind(1200),    ! work array for SOFTFILL          (local)
     *                 color,        ! color index of fill color        (local)
     *                 idmp          ! map area identifier              (local)

C  Logical variables

      logical          fill          ! fill the polygon ?               (local)

C  Real variables

      real             xpoly(*),     ! x coords. of polygon points         (in)
     *                 ypoly(*)      ! y coords. of polygon points         (in)
      real             dst(1100)     ! work array for SOFTFILL          (local)

C  Common blocks

      common /mapfli/  mask          ! map masking indicator

C****************************  subroutine begin  *****************************C

C  Assume the polygon will be filled

      fill = .true.

C  If any area identifier is negative then don't fill the polygon

      do 10 i = 1,nid
        if (aid(i) .lt. 0) fill = .false.
10    continue

C  The color to fill the polygon is implied in AID(i) when GID(i) is
C  equal to 3

      if (fill) then
        do 20 i = 1,nid
          if (gid(i) .eq. 3) color = aid(i)
          if (gid(i) .eq. 6) idmp  = aid(i)
20      continue

C  Determine if the map will mask out the area

        if ((mask(1:2) .eq. 'LO') .or. (mask(1:2) .eq. 'lo') .or.
     *      (mask(1:2) .eq. 'Lo') .or. (mask(1:2) .eq. 'lO')) then
          if (mapaci(idmp) .eq. 1) fill = .false.
        else if ((mask(1:2) .eq. 'LL') .or. (mask(1:2) .eq. 'll') .or.
     *           (mask(1:2) .eq. 'Ll') .or. (mask(1:2) .eq. 'lL')) then
          if (idmp .eq. 2) fill = .false.
        else if ((mask(1:2) .eq. 'OO') .or. (mask(1:2) .eq. 'oo') .or.
     *           (mask(1:2) .eq. 'Oo') .or. (mask(1:2) .eq. 'oO')) then
          if (idmp .ne. 2) fill = .false.
        else if ((mask(1:2) .eq. 'OL') .or. (mask(1:2) .eq. 'ol') .or.
     *           (mask(1:2) .eq. 'Ol') .or. (mask(1:2) .eq. 'oL')) then
          if (mapaci(idmp) .ne. 1) fill = .false.
        end if

C  Fill the polygon

        if (fill) call sfsgfa (xpoly,ypoly,nep,dst,1100,ind,1200,color)
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine getspc (color,errsev)

C*****************************************************************************C
C  getspc   - This is a CONDRV routine                                        C
C  Section  - Colors                                                          C
C  Purpose  - To determine a color to use in highlighted labeling.            C
C                                                                             C
C  On entry - COLOR  contains the color index of which we need  a  variation. C
C             ERRSEV indicates what severity of error will halt execution.    C
C                                                                             C
C  On exit  - COLOR exits as the new color index.                             C
C                                                                             C
C  Assume   - GKS is open.                                                    C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             GQCR                GKS                                         C
C             GSCR                GKS                                         C
C             ----------------------------------------------------------------C
C                                                                             C
C  Author   - Jeremy Asbill       Date - June 23, 1990      for the MM4 club  C
C*****************************************************************************C

C  Character variables

      character*60     ermes             ! error message string         (local)

C  Integer variables

      integer          color,            ! index of original color         (in)
     *                 errsev            ! error severity comparitor       (in)
      integer          ier,              ! error indicator for GQCR     (local)
     *                 i                 ! loop counter/place keeper    (local)

C  Logical variables

      logical          noplt             ! for common block NOPLT
      logical          bright            ! make a brighter color ?      (local)

C  Real variables

      real             red,              ! red component of a color     (local)
     *                 green,            ! green component of a color   (local)
     *                 blue,             ! blue component of a color    (local)
     *                 temp,             ! calculation variable         (local)
     *                 rt,gt,bt          ! junk fillers                 (local)

C  Common blocks

      common /noplot/  noplt             ! is no picture to be made ?

C*****************************************************************************C

C  Retrieve definition of COLOR

      call gqcr (1,color,0,ier,red,green,blue)

C  Decide if we should make a brighter color or a darker color

      if ((red .gt. 0.5) .and. (green .gt. 0.5) .and.
     *    (blue .gt. 0.5)) then
        bright = .false.
      else if ((red .gt. 0.5) .and. (green .gt. 0.5) .and.
     *    (blue .le. 0.5)) then
        bright = .false.
      else if ((red .gt. 0.5) .and. (green .le. 0.5) .and.
     *    (blue .gt. 0.5)) then
        bright = .false.
      else if ((red .le. 0.5) .and. (green .gt. 0.5) .and.
     *    (blue .gt. 0.5)) then
        bright = .false.
      else
        bright = .true.
      end if

C  If we want a darker color

      if (.not. bright) then

C  Determine a good factor to subtract to all of the components of the color
C  to make that darker color

        if (red .le. 0.1) then

C  Work with blue and green only

          temp  = min(blue,green) * 0.5
          blue  = blue - temp
          green = green - temp
        else if (blue .le. 0.1) then

C  Work with red and green only

          temp  = min(red,green) * 0.5
          red   = red - temp
          green = green - temp
        else if (green .le. 0.1) then

C  Work with blue and red only

          temp  = min(blue,red) * 0.5
          blue  = blue - temp
          red   = red - temp
        else

C  Work with all three

          temp  = min(blue,green,red) * 0.5
          blue  = blue - temp
          green = green - temp
          red   = red - temp
        end if
      else

C  If we want a lighter color, determine a good factor to add to all of
C  the components of the color to make that lighter color

        if (red .ge. 0.9) then

C  Work with blue and green only

          temp  = (1 - max(blue,green)) * 0.5
          blue  = blue + temp
          green = green + temp
        else if (blue .ge. 0.9) then

C  Work with red and green only

          temp  = (1 - max(red,green)) * 0.5
          red   = red + temp
          green = green + temp
        else if (green .ge. 0.9) then

C  Work with blue and red only

          temp  = (1 - max(blue,red)) * 0.5
          blue  = blue + temp
          red   = red + temp
        else

C  Work with all three

          temp  = (1 - max(blue,green,red)) * 0.5
          blue  = blue + temp
          green = green + temp
          red   = red + temp
        end if
      end if

C  Determine the next color index

      i = 1
10    i = i + 1
      if (i .eq. 256) then
        ermes(1:30)  = 'Color Index Space Depleted, Fe'
        ermes(31:60) = 'wer Line Labels Suggested     '
        call errhan ('CONDRV',1,ermes,errsev)
        noplt = .true.
      else
        call gqcr (1,i,0,ier,rt,gt,bt)
        if (ier .ne. 87) goto 10
        color = i
      end if

C  Make the new components define the next free color index

      call gscr (1,color,red,green,blue)

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine interc (xstr,ystr,xend,yend,xdim,ydim,lmeth,pnum,
     *                   tsize,mask,errsev)

C*****************************************************************************C
C  interc   - This is a CONDRV routine                                        C
C  Section  - Error handling                                                  C
C  Purpose  - To check for a few obvious errors and  to  initialize the error C
C             handling variables.                                             C
C                                                                             C
C  On entry - XSTR and YSTR are the first point in the grid that is to be in- C
C             cluded in the plot.   XEND  and  YEND are the last point in the C
C             grid that is to be included in the plot.  XDIM and YDIM are the C
C             dimensions of the grid.  LMETH specifies  what type of  contour C
C             level specification to use.  PNUM tells how many plot this plot C
C             will overlay.   ERRSEV  indicates what severity of error should C
C             halt execution.  TSIZE is the number of characters declared for C
C             the title length.  MASK indicates what type of masking will  be C
C             used in regards to a map previously drawn.                      C
C                                                                             C
C  On exit  - NOPLT is true if any errors occured.  The  counters in the com- C
C             mon block ERRORS have been initialized correctly.               C
C                                                                             C
C  Assume   - Nothing.                                                        C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             ERRHAN              MAPDRV/CONDRV utility                       C
C             GQOPS               GKS                                         C
C             OPNGKS              SPPS*                                       C
C             GOPWK               GKS                                         C
C             GACWK               GKS                                         C
C             GQOPWK              GKS                                         C
C             GQCR                GKS                                         C
C             GSCR                GKS                                         C
C             ----------------------------------------------------------------C
C                                                                             C
C  Author   - Jeremy Asbill      Date - August 10, 1990      for the MM4 club C
C*****************************************************************************C

C  Character varaibles

      character*2      mask              ! map masking indicator           (in)
      character*60     ermes             ! error message string         (local)

C  Integer variables

      integer          xstr,             ! x coord. of first grid point    (in)
     *                 xend,             ! x coord. of last grid point     (in)
     *                 ystr,             ! y coord. of first grid point    (in)
     *                 yend,             ! y coord. of last grid point     (in)
     *                 xdim,             ! the x dimension of the data     (in)
     *                 ydim,             ! the y dimension of the data     (in)
     *                 lmeth,            ! level specification indicator   (in)
     *                 pnum,             ! number of the plot              (in) 
     *                 tsize,            ! length of title string          (in)
     *                 errsev            ! error severity comparitor       (in)
      integer          canbe             ! for common block CONFLG
      integer          opst,             ! GKS operation state          (local)
     *                 ier,              ! GKS error flag               (local)
     *                 nwk,              ! number of open workstations  (local)
     *                 num               ! work station identifier      (local)

C  Logical variables

      logical          noplt             ! for common block NOPLOT

C  Real variables

      real             red,              ! red component of color       (local)
     *                 blue,             ! blue component of color      (local)
     *                 green             ! green component of color     (local)

C  Common blocks

      common /errors/  error,            ! error count
     *                 warns             ! warning count
      common /noplot/  noplt             ! has a non-correctable erro occured ?
      common /conflg/  canbe             ! map masking can be done if 722

C****************************  subroutine begin  *****************************C

C  Initialize error counters

      error = 0
      warns = 0

C  Initialize NOPLT

      noplt = .false.

C  Check for some obvious errors
C  Grid dimensions of the wanted data don't jive

      if ((xend .le. xstr) .or.
     *    (yend .le. ystr)) then
        ermes(1:30)  = 'Desired Data Is Not Defined, x'
        ermes(31:60) = 'end, yend Must Be > xstr, ystr'
        call errhan ('CONDRV',1,ermes,errsev)
        noplt = .true.
      end if

C  Make sure the subdomain is at least an improper subdomain of the domain

      if ((xstr .lt. 1) .or. (ystr .lt. 1) .or.
     *    (xend .gt. xdim) .or. (yend .gt. ydim)) then
        ermes(1:43)  = 'Subset Of Data Specified Is Not Within The '
        ermes(44:60) = 'Entire Set      '
        call errhan ('CONDRV',1,ermes,errsev)
        noplt = .true.
      end if

C  See if the user requested too many levels

      if ((lmeth .gt. 100) .or. (lmeth .lt. -102)) then
        ermes(1:30)  = 'Too Many Contour Levels Reques'
        ermes(31:60) = 'ted, 100 Maximum              '
        call errhan ('CONDRV',1,ermes,errsev)
        noplt = .true.
      end if

C  Make sure we have a valid plot number

      if (pnum .lt. 1) then
        ermes(1:30)  = 'The Plot Number Given Is Usele'
        ermes(31:60) = 'ss                            '
        call errhan ('CONDRV',1,ermes,errsev)
        noplt = .true.
      end if

C  Make sure the state of GKS is proper

      call gqops (opst)
      if (opst .eq. 0) then
        ermes(1:30)  = 'GKS Is Not Open               '
        ermes(31:60) = '                              '
        call errhan ('CONDRV',1,ermes,errsev)
        call opngks
        noplt = .true.
      else if (opst .eq. 1) then
        ermes(1:30)  = 'There Are No Open Workstations'
        ermes(31:60) = '                              '
        call errhan ('CONDRV',1,ermes,errsev)
        call gopwk (1,2,1)
        call gacwk (1)
        noplt = .true.
      else if (opst .eq. 2) then
        ermes(1:30)  = 'There Are No Active Workstatio'
        ermes(31:60) = 'ns                            '
        call errhan ('CONDRV',1,ermes,errsev)
        call gqopwk (1,ier,nwk,num)
        call gacwk (num)
        noplt = .true.
      end if

C  Make certain background and foreground color are defined

      call gqcr (1,0,0,ier,red,blue,green)

C  Check for errors from GKS

      if ((ier .eq. 87) .or. (ier .eq. 93)) then
        ermes(1:30)  = 'Background Color Index Is Inva'
        ermes(31:60) = 'lid                           '
        call errhan ('CONDRV',1,ermes,errsev)
        noplt = .true.
      else if ((ier .ne. 0) .and. (ier .ne. 94)) then
        ermes(1:30)  = 'Uncorrectable Error Encoutered'
        ermes(31:60) = '                              '
        call errhan ('CONDRV',1,ermes,errsev)
        noplt = .true.
      end if

C  If Background color is not black warn the user that their maps may look
C  stupid

      if ((red .ne. 0.0) .and. (blue .ne. 0.0) .and.
     *    (green .ne. 0.0)) then
        ermes(1:30)  = 'Background Color Index Is Not '
        ermes(31:60) = 'Black, Background Color Reset '
        call errhan ('CONDRV',0,ermes,errsev)
        call gscr (1,0,0.00,0.00,0.00)
      end if

      call gqcr (1,1,0,ier,red,blue,green)

C  Check for errors from GKS

      if ((ier .eq. 87) .or. (ier .eq. 93)) then
        ermes(1:30)  = 'Foreground Color Index Is Inva'
        ermes(31:60) = 'lid                           '
        call errhan ('CONDRV',1,ermes,errsev)
        noplt = .true.
      else if ((ier .ne. 0) .and. (ier .ne. 94)) then
        ermes(1:30)  = 'Uncorrectable Error Encoutered'
        ermes(31:60) = '                              '
        call errhan ('CONDRV',1,ermes,errsev)
        noplt = .true.
      end if

C  If Background color is not black warn the user that their maps may look
C  stupid

      if ((red .lt. 0.8) .and. (blue .lt. 0.8) .and.
     *    (green .lt. 0.8)) then
        ermes(1:30)  = 'Foreground Color Index Is Not '
        ermes(31:60) = 'White, Foreground Color Reset '
        call errhan ('CONDRV',0,ermes,errsev)
        call gscr (1,1,0.80,0.80,0.80)
      end if

C  If the title string length is too long warn the user

      if (tsize .gt. 120) then
        ermes(1:30)  = 'Title String Length Is Too Lar'
        ermes(31:60) = 'ge, Truncated To 120          '
        call errhan ('CONDRV',0,ermes,errsev)
        tsize = 120
      end if

C  Check that the given MASK is valid

      if ((mask(1:1) .ne. 'l') .and. (mask(1:1) .ne. 'L') .and.
     *    (mask(1:1) .ne. 'o') .and. (mask(1:1) .ne. 'O') .and.
     *    (mask(1:1) .ne. 'n') .and. (mask(1:1) .ne. 'N')) then
        ermes(1:30)  = 'Map Masking Indicator Is Inval'
        ermes(31:60) = 'id, NO Assumed                '
        call errhan ('CONDRV',0,ermes,errsev)
        mask(1:2) = 'NO'
      else if ((mask(2:2) .ne. 'l') .and. (mask(2:2) .ne. 'L') .and.
     *         (mask(2:2) .ne. 'o') .and. (mask(2:2) .ne. 'O')) then
        ermes(1:30)  = 'Map Masking Indicator Is Inval'
        ermes(31:60) = 'id, NO Assumed                '
        call errhan ('CONDRV',0,ermes,errsev)
        mask(1:2) = 'NO'
      end if

      if ((mask(1:1) .ne. 'n') .and. (mask(1:1) .ne. 'N')) then
        if (canbe .ne. 722) then
          ermes(1:30)  = 'MAPDRV Must Be Called To Fill '
          ermes(31:60) = 'CONDRV Area Map Before Masking'
          call errhan ('CONDRV',1,ermes,errsev)
          mask(1:2) = 'NO'
        else
          canbe = 0
        end if
      end if

C*****************************  subroutine end *******************************C

      return
      end
      subroutine lbfill (iftp,xcra,ycra,ncra,indx)

C*****************************************************************************C
C  lbfill   - This is a CONDRV routine                                        C
C  Section  - Labels                                                          C
C  Purpose  - To fill in a shaded label bar.                                  C
C                                                                             C
C  On entry - IFTP is 1.   XCRA and YCRA contain NCRA points that define  the C
C             current box as a polygon to be filled. INDX is the index to use C
C             with the dot spacing array SPACE which is passed in through co- C
C             mmon block DOTSPC.                                              C
C                                                                             C
C  On exit  - The current box in the label bar has been shaded.               C
C                                                                             C
C  Assume   - This routine was called by LBLBAR.                              C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             SFSETR              SOFTFILL utility*                           C
C             SFSGFA              SOFTFILL utility*                           C
C             ----------------------------------------------------------------C
C             * NCAR Graphics Routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill      Date - August 14, 1990     for the MM4 club  C
C*****************************************************************************C

C  Integer variables

      integer          iftp,             ! the value 1                     (in)
     *                 ncra,             ! dimension of XCRA and YCRA      (in)
     *                 indx              ! index to use with SPACE         (in)
      integer          ind(1200)         ! work space for SOFTFILL      (local)

C  Real variables

      real             xcra(*),          ! x coords. describing box        (in)
     *                 ycra(*)           ! y coords. describing box        (in)
      real             space(30)         ! for common block DOTSPC
      real             dst(1100)         ! work space for SFOTFILL      (local)

C  Common blocks

      common /dotspc/  space             ! dot spacings for shading

C****************************  Subroutine Begin  *****************************C

C  SOFTFILL internal parameters used in this routine are :
C  SP  - line SPacing

C  Set correct dot spacing with SOFTFILL

      call sfsetr ('SP',space(indx))

C  Shade the area

      call sfsgfa (xcra,ycra,ncra,dst,1100,ind,1200,1)

C*****************************  Subroutine End  ******************************C

      return
      end
      subroutine mkfcol

C*****************************************************************************C
C  mkfcol   - This is for CONDRV                                              C
C  Section  - Fill                                                            C
C  Purpose  - To set up the colors for color fill.                            C
C                                                                             C
C  On entry - Needed input is passed in through common blocks.                C
C                                                                             C
C  On exit  - CONPACK area identifiers have been set up to indicate the color C
C             to be used in coloring that particular level.                   C
C                                                                             C
C  Assume   - GKS is open.                                                    C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             CPGETI              CONPACK utility*                            C
C             CPSETI              CONPACK utility*                            C
C             GSCR                GKS                                         C
C             ----------------------------------------------------------------C
C             * NCAR Graphics Routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill   Date - July 6, 1990  for the MM4 club.          C
C*****************************************************************************C

C  Integer variables

      integer          cmeth,            ! for common block COLIND
     *                 bckco,            ! for common block COLIND
     *                 rmeth             ! for common block COLIND
      integer          nprt,             ! for common block PARINF
     *                 iprts(100,2)      ! for common block PARINF
      integer          pcolor(100,2)     ! for common block PARCOL
      integer          nmlev,            ! number of levels             (local)
     *                 color,            ! a color index                (local)
     *                 i                 ! loop counter                 (local)

C  Logical variables

      logical          ints              ! for common block PARINF

C  Real variables

      real             rprts(100,2)      ! for common block PARINF
      real             clev              ! a contour level value        (local)

C  Common blocks

      common /colind/  cmeth,            ! method of color plot
     *                 bckco,            ! backup color index
     *                 rmeth             ! not used
      common /parinf/  nprt,             ! number of partitions
     *                 iprts,            ! not used
     *                 rprts,            ! not used
     *                 ints              ! not used
      common /parcol/  pcolor            ! colors for each partition

C****************************  subroutine begin  *****************************C

C  CONPACK internal parameters used in this routine are:
C  AIA - Area Identifier Above contour level
C  AIB - Area Identifier Below contour level
C  NCL - Number of Contour Levels
C  CLC - Contour Level Color index
C  PAI - Parameter Array Index
C  CLV - Contour Level Values

C  Get the number of contour levels from CONPACK

      call cpgeti ('NCL',nmlev)

C  Loop through all the levels

      do 10 i = 1,nmlev

C  Select the contour level

        call cpseti ('PAI',i)

C  Get the color at that level

        call cpgeti ('CLC',color)

C  Assign that color to be below the line

        call cpseti ('AIB',color)

C  Assign the contour level color to be BCKCO

        call cpseti ('CLC',bckco)
10    continue

C  Now go back through and assign all of the area identifiers above the
C  levels to match those below the level

      do 20 i = 1,nmlev

C  On the last line, we have no color to go above
C  Thus we have to construe one

        if (i .eq. nmlev) then
          call cpseti ('PAI',nmlev)
          if (cmeth .le. 1) then
            call cpseti ('AIA',bckco)
          else if ((cmeth .eq. 2) .or. (cmeth .eq. 3)) then
            call cpgeti ('AIB',color)
            call cpseti ('AIA',color)
          else if (cmeth .eq. 4) then
            call cpgetr ('CLV',clev)
            do 30 j = 1,nprt
              if ((clev .ge. rprts(j,1)) .and.
     *            (clev .le. rprts(j,2)))
     *          call cpseti ('AIA',pcolor(j,2))
30          continue
          else if (cmeth .ge. 6) then
            call gscr (1,255,1.0,0.0,0.0)
            call cpseti ('AIA',255)
          end if

C  Otherwise we have the above color

        else
          call cpseti ('PAI',i+1)
          call cpgeti ('AIB',color)
          call cpseti ('PAI',i)
          call cpseti ('AIA',color)
        end if
20    continue

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine mklbar

C*****************************************************************************C
C  mklbar   - This is a CONDRV routine                                        C
C  Section  - Labels                                                          C
C  Purpose  - To  create a label bar according to the colors and contour  le- C
C             vels already set up in CONPACK internal parameters.             C
C                                                                             C
C  On entry - Needed information is passed in through common blocks.          C
C                                                                             C
C  On exit  - A label bar has been drawn.                                     C
C                                                                             C
C  Assume   - GKS is open.   Contour levels and colors for those levels  have C
C             been set up already.                                            C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             CPGETR              CONPACK utility*                            C
C             CPGETI              CONPACK utility*                            C
C             CPSETI              CONPACK utility*                            C
C             CPSETR              CONPACK utility*                            C
C             CPGETC              CONPACK utility*                            C
C             LBLBAR              LABELBAR utility*                           C
C             SFSETI              SOFTFILL utility*                           C
C             LBSETI              LABELBAR utility*                           C
C             LBSETR              LABELBAR utility*                           C
C             GETSET              SPPS*                                       C
C             ----------------------------------------------------------------C
C             * NCAR Graphics Routine                                         C
C                                                                             C
C  Author - Jeremy Asbill        Date - July 3, 1990        for the MM4 club  C
C*****************************************************************************C

C  Parameter

      parameter      (base = 0.0005)
      parameter      (smax = 0.01)

C  Character variables

      character*20     llab(100),        ! array of labels for LABELBAR (local)
     *                 minlab            ! minimum value label tester   (local)

C  Integer variables

      integer          tsize             ! for common block TITDET
      integer          tcol(2)           ! for common block TLCOLS
      integer          nmbox,            ! number of boxes in label bar (local)
     *                 nmlab,            ! number of labels for bar     (local)
     *                 lcol(100),        ! array of colors for LABELBAR (local)
     *                 lorn,             ! 0 => horiz. ; 1 => vertical  (local)
     *                 skip,             ! number of values to skip     (local)
     *                 i,j,k,            ! loop counters/place keepers  (local)
     *                 pat(8,8)          ! dot pattern for shading      (local)

C  Logical variables

      logical          tputb,            ! for common block TITDET
     *                 tputp,            ! for common block TITDET
     *                 tfilb             ! for common block TITDET
      logical          fill,             ! for common block FILDET
     *                 fshd,             ! for common block FILDET
     *                 color             ! for common block FILDET
      logical          lhohl             ! for common block SHDDIR

C  Real variables

      real             space(30)         ! for common block DOTSPC
      real             rlev,             ! contour level value          (local)
     *                 lled,             ! left edge of bar <0.0 to 1.0>(local)
     *                 lred,             ! right edge - bar <0.0 to 1.0>(local)
     *                 lbed,             ! bottom edge -bar <0.0 to 1.0>(local)
     *                 lted,             ! top  edge of bar <0.0 to 1.0>(local)
     *                 part,             ! what x amount is boxes       (local)
     *                 touch,            ! what y amount is boxes       (local)
     *                 flsv,             ! trash filler                 (local)
     *                 frsv,             ! right side of CONPACK window (local)
     *                 fbsv,             ! bottom of CONPACK window     (local)
     *                 ftsv,             ! trash filler                 (local)
     *                 ulsv,ursv,        ! trash filler                 (local)
     *                 ubsv,utsv,        ! trash filler                 (local)
     *                 incr,             ! shading increment            (local)
     *                 llsv              ! trash filler                 (local)

C  Common blocks

      common /titdet/  tputb,            ! connect the boxes of the label bar ?
     *                 tputp,            ! not used
     *                 tfilb,            ! not used
     *                 tprlw,            ! label bar perim. line width
     *                 tsize             ! not used
      common /tlcols/  tcol              ! title colors
      common /fildet/  fill,             ! will the plot be filled ?
     *                 lshd,             ! draw contour lines over a fill ?
     *                 color             ! make the plot in color ?
      common /shddir/  lhohl             ! shade for high to low or visa versa?
      common /dotspc/  space             ! dot spacing for shading      (local)

C****************************  subroutine begin  *****************************C

C  The CONPACK internal parameters used in this routine are:
C  CLV - Contour LeVels
C  CLC - Contour Level Color index
C  PAI - Parameter Array Index
C  NCL - Number of Contour Levels
C  ZDV - Z Data Value

C  The LABELBAR internal parameters used in this routine are:
C  CLB - Color index for LaBels
C  CBL - Color index for Box Lines
C  WBL - Width of the Box Lines

C  The SOFTFILL internal parameters used in this routine are:
C  TY  - TYpe of fill pattern

C  Determine how many boxes will be needed

      call cpgeti ('NCL',nmbox)

C  If shading rather coloring, calculate the shading increment

      if ((.not. color) .and. (fill))
     *  incr = (smax - base)/(nmbox - 1)

C  Make sure there will not bee too many boxes

      if (nmbox .ge. 90) then
        skip = 5
      else if (nmbox .ge. 80) then
        skip = 4
      else if (nmbox .ge. 60) then
        skip = 3
      else if (nmbox .ge. 30) then
        skip = 2
      else
        skip = 1
      end if

C  Fill an array with the contour levels to scale and the
C  color indexes they are associated with if coloring
C  and fill an array with dot spacings if shading

      j = 0
      do 10 i = 1,nmbox-1,skip
        j = j + 1
        call cpseti ('PAI',i)            ! set internal array index
        call cpgetr ('CLV',rlev)         ! get contour level value
        call cpsetr ('ZDV',rlev)         ! give that value to a converter
        call cpgetc ('ZDV',llab(j))      ! get a string of that value back
        if ((.not. color) .and. (fill)) then
          if (lhohl) then
            space(j) = smax - (i - 1) * incr
          else
            space(j) = (i - 1) * incr + base
          end if
          lcol(j) = j
        else
          call cpgeti ('CLC',lcol(j))
        end if
10    continue

C  Adjust NMBOX according to the number of values skipped
C  Start a new place keeper for the labels, NMLAB

      nmbox = j
      nmlab = nmbox

C  Bump everything in LLAB up one element and put the minimum value
C  in place one and the maximum value in place two if this is solid
C  label bar

      if (tputb) then
        call cpgetr ('ZMN',rlev)
        call cpsetr ('ZDV',rlev)
        call cpgetc ('ZDV',minlab)
        if (minlab(1:20) .ne. llab(1)(1:20)) then
          do 40 i = nmlab,2,-1
            llab(i)(1:20) = llab(i-1)(1:20)
40        continue
          llab(1)(1:20) = minlab(1:20)
        end if
        nmlab = nmlab + 1
        call cpgetr ('ZMX',rlev)
        call cpsetr ('ZDV',rlev)
        call cpgetc ('ZDV',llab(nmlab))
      end if

C  Set up the proper label color

      call lbseti ('CLB',tcol(1))

C  Set up the proper perimeter color

      if (color) then
        call lbseti ('CBL',tcol(2))
      else
        call lbseti ('CBL',1)
      end if

C  Set up the proper perimeter line width

      call lbsetr ('WBL',tprlw)

C  Determine exactly where the CONPACK window sits

      call getset (flsv,frsv,fbsv,ftsv,ulsv,ursv,ubsv,utsv,llsv)

C  Determine if the bar should be horizontal or vertical

      if (frsv-flsv .ge. ftsv-fbsv) then
        lorn = 0                         ! horizontal

C  Place the bar appropriately

        lbed = fbsv * 0.5 - 0.0375
        lted = lbed + 0.075
        lled = 0.05
        lred = 0.95

C  Design the label bar

        part  = 0.37
        if (tputb) then
          touch = 1.0
        else
          touch = 0.8
        end if
      else
        lorn = 1                         ! vertical

C  Place the bar appropriately

        lled = (1.0 - frsv) * 0.5 + frsv - 0.0375
        lred = lled + 0.075
        lbed = 0.05
        lted = 0.95

C  Design the label bar

        touch = 0.37
        if (tputb) then
          part = 1.0
        else
          part = 0.8
        end if
      end if

C  Force solid fill if coloring

      if ((color) .or. (.not. fill)) then
        call gsfais (1)

C  Set up the proper pattern for filling

        call sfseti ('TY',0)

      else

C  Set up a constant dot pattern if shading

        call gsfais (0)
        do 30 i = 1,8
          do 20 j = 1,8
            pat(i,j) = 1
20        continue
30      continue

C  Set up the dot pattern with SOFTFILL

        call sfsetp (pat)

C  Tell SOFTFILL to use dots in shading

        call sfseti ('TY',1)
        call sfseti ('DO',1)
      end if

C  Draw the label bar

      if ((.not. color) .and. (fill)) then
        call lblbar (lorn,lled,lred,lbed,lted,nmbox,touch,part,lcol,
     *               1,llab,nmlab,1)
      else
        call lblbar (lorn,lled,lred,lbed,lted,nmbox,touch,part,lcol,
     *               0,llab,nmlab,1)
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine prettl (tlen,title,scale,httl)

C*****************************************************************************C
C  prettl   - This is a CONDRV routine                                        C
C  Section  - Desgin                                                          C
C  Purpose  - To preprocess the title for special CONDRV strings.             C
C                                                                             C
C  On entry - TLEN  is  the number of characters in the user specified  title C
C             TITLE is that title string.                                     C
C                                                                             C
C  On exit  - If  TITLE  contained any special CONDRV strings they have  been C
C             converted  to the user  desired string.  Otherwise this routine C
C             does nothing.  The converted or unchanged string will be trans- C
C             ferred to httl.  SCALE is the scaling factor used in the plot.  C
C                                                                             C
C  Assume   - GKS is open.                                                    C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             CPGETR              CONPACK utility*                            C
C             CONNUM              CONDRV utility                              C
C             CPGETI              CONPACK utility*                            C
C             CPSETI              CONPACK utility*                            C
C             ----------------------------------------------------------------C
C             * NCAR Graphics Routine                                         C
C                                                                             C
C             $ContourInterval$ - Will be replaced by the contour interval.   C
C             $Contour Maximum$ - Will be replaced by the contour maximum.    C
C             $Contour Minimum$ - Will be replaced by the contour minimum.    C
C             $Scaling  Factor$ - Will be replaced by the scaling factor.     C
C                                                                             C
C  Author   - Jeremy Asbill     Date - November 1, 1990     for the MM4 club  C
C*****************************************************************************C

C  Character variables

      character*120    title             ! title string                    (in)
      character*120    httl              ! title string                 (local)
      character*20     number            ! a real converted to a string (local)

C  Integer variables

      integer          tlen              ! # of characters in TITLE        (in)
      integer          i,j,              ! loop counters                (local)
     *                 length            ! # of characters in NUMBER    (local)

C  Logical variables

      logical          middle            ! in between dollar signs?     (local)

C  Real variables

      real             scale             ! scale factor for the plot       (in)
      real             cval,             ! contour information value    (local)
     *                 test              ! temporary test value         (local)

C****************************  subroutine begin  *****************************C

C  The following CONPACK internal parameters are used:
C  CIU - Contour Interval Used
C  NCL - Number of Contour Levels
C  PAI - Parameter Array Index
C  CLV - Contour Level Values

C  Search TITLE for '$', transfer to HTTL as we search

      middle = .false.
      do 10 i = 1,tlen
        if ((title(i:i) .ne. '$') .and. (.not. middle)) then
          httl(i:i) = title(i:i)
        else if ((title(i:i) .eq. '$') .and. (middle)) then
          middle    = .false.
          httl(i:i) = ' '
        else if ((title(i:i) .eq. '$') .and. (.not. middle)) then

C  Check to see if this is a CONDRV special value, requesting the contour
C  interval

          if (title(i:i+16) .eq. '$ContourInterval$') then

C  Retrieve the interval from CONPACK

            call cpgetr ('CIU',cval)

C  Substitute the contour interval value into HTTL where the old request
C  string was

            call connum (cval,number,length)
            httl(i:i + length - 1) = number(1:length)
            do 20 j = i + length,i + 15
              httl(j:j) = ' '
20          continue
            middle = .true.

C  Check to see if this is a CONDRV special value, requesting the contour
C  minimum

          else if (title(i:i+16) .eq. '$Contour Minimum$') then

C  Retrieve the contour mimimum from CONPACK

            call cpgeti ('NCL',nlev)
            cval = 1.0E36
            do 30 j = 1,nlev
              call cpseti ('PAI',j)
              call cpgetr ('CLV',test)
              if (test .lt. cval) cval = test
30          continue

C  Substitute the contour minimum value into HTTL where the old request
C  string was

            call connum (cval,number,length)
            httl(i:i + length - 1) = number(1:length)
            do 40 j = i + length,i + 15
              httl(j:j) = ' '
40          continue
            middle = .true.

C  Check to see if this is a CONDRV special value, requesting the contour
C  maximum

          else if (title(i:i+16) .eq. '$Contour Maximum$') then

C  Retrieve the contour maximum from CONPACK

            call cpgeti ('NCL',nlev)
            cval = -1.0E36
            do 50 j = 1,nlev
              call cpseti ('PAI',j)
              call cpgetr ('CLV',test)
              if (test .gt. cval) cval = test
50          continue

C  Substitute the contour maximum value into HTTL where the old request
C  string was

            call connum (cval,number,length)
            httl(i:i + length - 1) = number(1:length)
            do 60 j = i + length,i + 15
              httl(j:j) = ' '
60          continue
            middle = .true.

C  Check to see if this is a CONDRV special value, requesting the scaling
C  factor

          else if (title(i:i+16) .eq. '$Scaling  Factor$') then
            cval = scale

C  Substitute the scaling factor value into HTTL where the old request
C  string was

            call connum (cval,number,length)
            httl(i:i + length - 1) = number(1:length)
            do 70 j = i + length,i + 15
              httl(j:j) = ' '
70          continue
            middle = .true.

C  If it was not a CONDRV special value string, simply transfer it

          else
            httl(i:i) = title(i:i)
          end if
        end if
10    continue

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine setcol

C*****************************************************************************C
C  setcol   - This is a CONDRV routine                                        C
C  Section  - Colors                                                          C
C  Purpose  - This routine sets the parameters for the color of contour level C
C             lines and fills.                                                C
C                                                                             C
C  On entry - The  necessary information is passed in through common  blocks. C
C                                                                             C
C  On exit  - The color indexes for each contour level to be in the plot have C
C             been assigned correctly not considering line labels.            C
C                                                                             C
C  Assume   - GKS is open.   CONPACK  has been initialized and contour levels C
C             have been chosen and set up.                                    C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             CPGETI              CONPACK utility*                            C
C             CPSETI              CONPACK utility*                            C
C             CPGETR              CONPACK utility*                            C
C             ----------------------------------------------------------------C
C             * NCAR Graphics Routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill         Date - June 8, 1990     for the MM4 club  C
C*****************************************************************************C

C  Parameters

      parameter       (idcsp = -1)       ! color index for defaults

C  Integer variables

      integer          nprt,             ! for common block PARINF
     *                 iprts(100,2)      ! for common block PARINF
      integer          cmeth,            ! for common block COLIND
     *                 bckco,            ! for common block COLIND
     *                 rmeth             ! for common block COLIND
      integer          nrmps(100),       ! for common block RAMPSC
     *                 ramps(100,100)    ! for common block RAMPSC
      integer          pcolor(100,2)     ! for common block PARCOL
      integer          zcol(3)           ! for common block ZLCOLS
      integer          nclv              ! number of contour levels     (local)

C  Logical variables

      logical          ints              ! for common block PARINF

C  Real variables

      real             rprts(100,2)      ! for common block PARINF
      real             clev,             ! contour level value          (local)
     *                 test,             ! partition test value 1       (local)
     *                 chek              ! partition test value 2       (local)

C  Common blocks

      common /colind/  cmeth,            ! method of color plot
     *                 bckco,            ! not used
     *                 rmeth             ! method of ramping colors
      common /parinf/  nprt,             ! number of partitions
     *                 iprts,            ! integer partitions
     *                 rprts,            ! real partitions
     *                 ints              ! are the partitions integers ?
      common /rampsc/  nrmps,            ! # or levels/partition
     *                 ramps             ! color ramps
      common /parcol/  pcolor            ! colors for each partition
      common /zrcols/  zcol              ! zero line colors

C****************************  subroutine begin  *****************************C

C  The following CONPACK internal parameters are used
C  NCL - Number of Contour Levels
C  PAI - Parameter Array Index
C  CLV - Contour LeVels
C  CLC - Contour Level Color index

C  Get the number of lines to be drawn

      call cpgeti ('NCL',nclv)

C  Loop through all contours first and set them to the backup color

      do 10 i = 1,nclv
        call cpseti ('PAI',i)
        call cpseti ('CLC',bckco)
10    continue

C  If ramps were made, use them

      if ((cmeth .eq. 4) .or. (cmeth .eq. 5)) then

C  Loop through all of the partitions and set colors according to the
C  constructed ramps, that way if the list of partitions is incomplete
C  the backup color fills the gaps

        do 20 i = 1,nprt

C  Loop through the contour levels and set the correct color index
C  for the entire partition before even looking at other partitions

          k = 1
          do 30 j = 1,nclv
            call cpseti ('PAI',j)
            if (.not. ints) then
              call cpgetr ('CLV',clev)
              if ((clev .ge. rprts(i,1)) .and.
     *            (clev .lt. rprts(i,2))) then
                call cpseti ('CLC',ramps(i,k))
                k = k + 1
              end if
            else
              if ((j .ge. iprts(i,1)) .and.
     *            (j .lt. iprts(i,2))) then
                call cpseti ('CLC',ramps(i,k))
                k = k + 1
              end if
            end if
30        continue
20      continue

      else if ((cmeth .eq. 6) .or. (cmeth .eq. 7)) then

C  In this case IPRTS and RPRTS are not defined, but NRMPS was defined
C  in CRAMPS to be capable of taking IPRTS place

        k = 1
        do 40 j = 1,nclv
          call cpseti ('PAI',j)
          if (j .le. nrmps(1)) then
            i = 1
            call cpseti ('CLC',ramps(1,k))
            k = k + 1
          else if (j .le. nrmps(1) + nrmps(2)) then
            k = 1
            call cpseti ('CLC',ramps(2,i))
            i = i + 1
          else if (j .le. nrmps(1) + nrmps(2) + nrmps(3)) then
            i = 1
            call cpseti ('CLC',ramps(3,k))
            k = k + 1
          else
     *      if (j .le. nrmps(1) + nrmps(2) + nrmps(3) + nrmps(4)) then
            k = 1
            call cpseti ('CLC',ramps(4,i))
            i = i + 1
          else
     *      if (j .le. nrmps(1) + nrmps(2) + nrmps(3) + 
     *                 nrmps(4) + nrmps(5)) then
            call cpseti ('CLC',ramps(5,k))
            k = k + 1
          end if
40      continue

      else if ((cmeth .eq. 2) .or. (cmeth .eq. 3)) then

C  No ramps were specified but partitions should still be colored differently

        do 50 i=1,nclv

C  Set the correct parameter array index and retrieve the value for
C  that line

          call cpseti ('PAI',i)

C  Loop through the number of partitions and set the color appropriately
C  if the value of the current contour is within a partition

          do 60 j = 1,nprt
            if (.not. ints) then
              call cpgetr ('CLV',clev)
              if ((clev .ge. rprts(j,1)) .and.
     *            (clev .lt. rprts(j,2)))
     *          call cpseti ('CLC',pcolor(j,1))
            else
              if ((i .ge. iprts(j,1)) .and.
     *            (i .lt. iprts(j,2)))
     *          call cpseti ('CLC',pcolor(j,1))
            end if
60        continue
50      continue
      end if

C  If the zero line should have its own color, make it so

      if (zcol(3) .ne. idcsp) then
        do 70 i = 1,nclv
          call cpseti ('PAI',i)
          call cpgetr ('CLV',clev)
          if (clev .eq. 0.0) call cpseti ('CLC',zcol(3))
70      continue
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine setcon (lmeth,cmeth,levels,errsev)

C*****************************************************************************C
C  setcon   - This a CONDRV routine                                           C
C  Section  - Contour Levels                                                  C
C  Purpose  - To set up the proper  contour levels  according to the  contour C
C             levels specification method.  This includes telling CONPACK all C
C             of what levels we want.                                         C
C                                                                             C
C  On entry - LMETH is indicates what method to use when specifying the  con- C
C             tour levels.  LEVELS contains either the contour interval,  the C
C             contour interval, contour minimum and contour maximum or it has C
C             a list of specific levels in it.  ERRSEV indicates what  sever- C
C             ity of error to use.   CMETH indicates which coloring method is C
C             going to be used.                                               C
C                                                                             C
C  On exit  - The proper contour levels have been set up.                     C
C                                                                             C
C  Assume   - That GKS is open                                                C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             CPSETR              CONPACK utility*                            C
C             CPSETI              CONPACK utility*                            C
C             ERRHAN              CONDRV/MAPDRV utility                       C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill      Date - August 10, 1990     for the MM4 Club  C
C*****************************************************************************C

C  Character variables

      character*60     ermes             ! error message string         (local)

C  Integer variables

      integer          lmeth,            ! contour level spec. method ind. (in)
     *                 cmeth,            ! coloring method inicator        (in)
     *                 errsev            ! error severity comparitor       (in)

C  Real variables

      real             levels(100)       ! values used in level spec.      (in)

C****************************  subroutine begin  *****************************C

C  The following CONPACK internal parameters are used in this routine :
C  NCL - Number of contour levels
C  CIS - Contour Interval Specifier
C  CIU - Contour Interval Used
C  CLS - Contour Level Selection flag
C  CLU - Contour Level Use flag
C  CLV - Contour Level Values
C  CMN - Contour Minimum
C  CMX - Contour Maximum

C  CMETH = 7 indicates that the special coloring method has been chosen
C            and we should ignore lmeth and force 100 contours

      if (cmeth .eq. 7) then

        call cpseti ('CLS',-100)
        call cpseti ('NCL',100)
      else

        if (lmeth .eq. -2) then

C  LMETH = -2 means a contour interval is given but the contour minimum and
C             maximums are at the default.
C             The contour interval should be in LEVELS(1)

          call cpsetr ('CIS',levels(1))
          call cpsetr ('CIU',levels(1))
          call cpsetr ('CMN',1.0)
          call cpsetr ('CMX',0.0)
          call cpseti ('CLS',16)

        else if (lmeth .gt. 0) then

C  LMETH > 0 means the user has specified a number of contour levels to have
C            LMETH is that number

          call cpseti ('CLS',-lmeth)
          call cpseti ('NCL',lmeth)

        else if (lmeth .eq. -1) then

C LMETH = -1 means the user has specified a number of levels, a contour
C            interval, a maximum and a minimum to use
C            The contour min, max and int are given in LEVELS(3),LEVELS(2)
C            and LEVELS(1) respectively

          call cpsetr ('CIS',levels(1))
          call cpsetr ('CIU',levels(1))
          if (levels(3) .gt. levels(2)) then
            ermes(1:30)  = 'Contour Minimum Must Be Less T'
            ermes(31:60) = 'han The Contour Maximum       '
            call errhan ('CONDRV',1,ermes,errsev)
            temp      = levels(3)
            levels(3) = levels(2)
            levels(2) = temp
          end if
          call cpsetr ('CMN',levels(3))
          call cpsetr ('CMX',levels(2))
          call cpseti ('CLS',16)

        else if (lmeth .lt. -2) then

C LMETH < -2 means that the user has specified a certain list of contours
C            that they want to see.

          call cpseti ('NCL',abs(lmeth) - 2)
          do 10 i = 1,abs(lmeth) - 2
            call cpseti ('PAI',i)
            call cpsetr ('CLV',levels(i))
10        continue
          call cpseti ('CLS',0)

        else if (lmeth .eq. 0) then

C LMETH = 0 means that the default set up for CONPACK to choose
C           contour levels for us should be used

          call cpsetr ('CIS',0.0)
          call cpseti ('CLS',16)
        end if
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine sethlo

C*****************************************************************************C
C  sethlo   - This is a CONDRV routine                                        C
C  Section  - Design                                                          C
C  Purpose  - To set up what the high/low labels are suppose to look  like in C
C             the plot.                                                       C
C                                                                             C
C  On entry - Needed information is passed in through common block HLODET and C
C             common block HLCOLS.                                            C
C                                                                             C
C  On exit  - Internal Parameters of CONPACK specific to high/low labels have C
C             been set up except for box fill color and the text quality.     C 
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             CPSETI              CONPACK utility*                            C
C             CPSETR              CONPACK utility*                            C
C             CPSETC              CONPACK utility*                            C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Assume   - That GKS is open.                                               C
C                                                                             C
C  Author   - Jeremy Asbill       Date - June 14, 1990      for the MM4 club  C
C*****************************************************************************C

C  Integer variables

      integer          hstyl(2),         ! for common block HLODET
     *                 hsize,            ! for common block HLODET
     *                 hangl             ! for common block HLODET
      integer          hcol(2),          ! for common block HLCOLS
     *                 lcol(2)           ! for common block HLCOLS

C  Logical variables

      logical          hputb             ! for common block HLBDET
      logical          hputp,            ! for common block HLODET
     *                 hfilb,            ! for common block HLODET
     *                 hfilt             ! for common block HLODET

C  Real variables

      real             hprlw             ! for common block HLODET
      real             csiz              ! real conversion of HSIZE     (local)

C  Common blocks

      common /hlbdet/  hputb             ! draw boxes around highs and lows ?
      common /hlodet/  hputp,            ! darw in perimeter on boxes ?
     *                 hfilb,            ! fill in the the box ?
     *                 hprlw,            ! line width for box perimeter
     *                 hstyl,            ! high/low style indicator
     *                 hsize,            ! character size for highs and lows
     *                 hfilt,            ! use an overlap filter ?
     *                 hangl             ! angle for horiz. to draw highs/lows
      common /hlcols/  hcol,             ! high label colors
     *                 lcol              ! low label colors

C****************************  subroutine begin  *****************************C

C  The CONPACK internal parameters used are:
C  HIC - HIgh label Color Index
C  HIT - HIgh label Text string
C  HLA - High/Low label Angle
C  HLB - High/Low label Box flag
C  HLC - High/Low label Color index
C  HLO - High/Low label Overlap flag
C  HLS - High/Low label Size
C  HLL - High/Low Line width
C  HLT - High/Low label Text strings
C  LOC - LOw label Color index
C  LOT - LOw label Text string

C  HPUTB is true if there should be a box masked around the high/low labels
C  and is false otherwise

      if (hputb) then

C  HPUTP is true if we want to draw the aforementioned box into the plot as
C  a line going around its perimeter and is false if no line should be drawn

        if (hputp) then

C  HFILB is true if the aforementioned box should be filled in

          if (hfilb) then
            call cpseti ('HLB',3)
          else
            call cpseti ('HLB',1)
          end if

C  Since a line should be drawn to show the box, set its line width
C  HPRLW is a multiplier, that is the line width will be
C  HPRLW * normal line width

          call cpsetr ('HLL',hprlw)
        else
          if (hfilb) then
            call cpseti ('HLB',2)
          else
            call cpseti ('HLB',0)
          end if
        end if
      else
        call cpseti ('HLB',0)
      end if
        
C  HSTYL = 0 means only put an 'h' or and 'l'

      if (hstyl(1) .eq. 0) then

C  If the two styles are the same we can set them both here

        if (hstyl(1) .eq. hstyl(2)) then
          call cpsetc ('HLT',':L:H'':L:L')

C  Otherwise just set the high labels

        else
          call cpsetc ('HIT',':L:H')
        end if

C  HSTYL = 1 means only put an 'H' or an 'L'

      else if (hstyl(1) .eq. 1) then
        if (hstyl(1) .eq. hstyl(2)) then
          call cpsetc ('HLT','H''L')
        else
          call cpsetc ('HIT','H')
        end if            

C  HSTYL = 2 means to put either 'hi' or 'lo'

      else if (hstyl(1) .eq. 2) then
        if (hstyl(1) .eq. hstyl(2)) then
          call cpsetc ('HLT',':L2:HI'':L2:LO')          
        else
          call cpsetc ('HIT',':L2:HI')
        end if

C  HSTYL = 3 means to put either 'HI' or 'LO'

      else if (hstyl(1) .eq. 3) then
        if (hstyl(1) .eq. hstyl(2)) then
          call cpsetc ('HLT','HI''LO')
        else
          call cpsetc ('HIT','HI')
        end if

C  HSTYL = 4 means to put either 'high' or 'low'

      else if (hstyl(1) .eq. 4) then
        if (hstyl(1) .eq. hstyl(2)) then
          call cpsetc ('HLT',':L4:HIGH'':L3:LOW')
        else
          call cpsetc ('HIT',':L4:HIGH')
        end if

C  HSTYL = 5 means to put either 'HIGH' or 'LOW'

      else if (hstyl(1) .eq. 5) then
        if (hstyl(1) .eq. hstyl(2)) then
          call cpsetc ('HLT','HIGH''LOW')          
        else
          call cpsetc ('HIT','HIGH')          
        end if

C  HSTYL = 6 means to put either 'Hi' or 'Lo'

      else if (hstyl(1) .eq. 6) then
        if (hstyl(1) .eq. hstyl(2)) then
          call cpsetc ('HLT','H:L:I''L:L:O')          
        else
          call cpsetc ('HIT','H:L:I')          
        end if

C  HSTYL = 7 means to put either 'High' or 'Low'

      else if (hstyl(1) .eq. 7) then
        if (hstyl(1) .eq. hstyl(2)) then
          call cpsetc ('HLT','H:L3:IGH''L:L2:OW')          
        else
          call cpsetc ('HIT','H:L3:IGH')          
        end if
          
C  HSTYL = 8 means to put just the value at the high/low

      else if (hstyl(1) .eq. 8) then
        if (hstyl(1) .eq. hstyl(2)) then
          call cpsetc ('HLT','$ZDV$')          
        else
          call cpsetc ('HIT','$ZDV$')          
        end if
C  HSTYL = 9 means to put an 'h' or an 'l' with the value in parens

      else if (hstyl(1) .eq. 9) then
        if (hstyl(1) .eq. hstyl(2)) then
          call cpsetc ('HLT',':L:H($ZDV$)'':L:L($ZDV$)')          
        else
          call cpsetc ('HIT',':L:H($ZDV$)')          
        end if

C  HSTYL = 10 is the same as 7 with capitals

      else if (hstyl(1) .eq. 10) then
        if (hstyl(1) .eq. hstyl(2)) then
          call cpsetc ('HLT','H($ZDV$)''L($ZDV$)')          
        else
          call cpsetc ('HIT','H($ZDV$)')          
        end if
C  HSTYL = 11 means to put an 'h' or an 'l' with a subscripted value

      else if (hstyl(1) .eq. 11) then
        if (hstyl(1) .eq. hstyl(2)) then
          call cpsetc ('HLT',':L:H:B:$ZDV$:E:'':L:L:B:$ZDV$:E:')          
        else
          call cpsetc ('HIT',':L:H:B:$ZDV$:E:')          
        end if

C  HSTYL = 12 means to put an 'H' or an 'L' with a subscripted value

      else if (hstyl(1) .eq. 12) then
        if (hstyl(1) .eq. hstyl(2)) then
          call cpsetc ('HLT','H:B:$ZDV$:E:''L:B:$ZDV$:E:')
        else
          call cpsetc ('HIT','H:B:$ZDV$:E:')
        end if

C  HSTYL = 13 means do it just like conrec

      else if (hstyl(1) .eq. 13) then
        if (hstyl(1) .eq. hstyl(2)) then
          call cpsetc ('HLT','H:V-1Q H-60:$ZDV$''L:V-1Q H-60:$ZDV$')
        else
          call cpsetc ('HIT','H:V-1Q H-60:$ZDV$')
        end if
      end if

C  If Highs and Lows are to be labeled differently set up the lows now

      if (hstyl(1) .ne. hstyl(2)) then
        if (hstyl(2) .eq. 0) then
          call cpsetc ('LOT',':L:L')
        else if (hstyl(2) .eq. 1) then
          call cpsetc ('LOT','L')
        else if (hstyl(2) .eq. 2) then
          call cpsetc ('LOT',':L2:LO')
        else if (hstyl(2) .eq. 3) then
          call cpsetc ('LOT','LO')
        else if (hstyl(2) .eq. 4) then
          call cpsetc ('LOT',':L3:LOW')
        else if (hstyl(2) .eq. 5) then
          call cpsetc ('LOT','LOW')          
        else if (hstyl(2) .eq. 6) then
          call cpsetc ('LOT','L:L:O')          
        else if (hstyl(2) .eq. 7) then
          call cpsetc ('LOT','L:L2:OW')          
        else if (hstyl(2) .eq. 8) then
          call cpsetc ('LOT','$ZDV$')          
        else if (hstyl(2) .eq. 9) then
          call cpsetc ('LOT',':L:L($ZDV$)')          
        else if (hstyl(2) .eq. 10) then
          call cpsetc ('LOT','L($ZDV$)')          
        else if (hstyl(2) .eq. 11) then
          call cpsetc ('LOT',':L:L:B:$ZDV$:E:')          
        else if (hstyl(2) .eq. 12) then
          call cpsetc ('LOT','L:B:$ZDV$:E:')
        else if (hstyl(2) .eq. 13) then
          call cpsetc ('LOT','L:V-1Q H-60:$ZDV$')
        end if
      end if

C  Set up high/low label size correctly

      csiz = float(hsize)/1000.0
      call cpsetr ('HLS',csiz)

C  Put a label filter on if requested

      if (hfilt) then
        call cpseti ('HLO',7)
      else
        call cpseti ('HLO',0)
      end if

C  Set up the proper angle.  CONPACK expects a real.

      call cpsetr ('HLA',float(hangl))

C  Set up colors for the text and perimeter of the high/low labels

      call cpseti ('HIC',hcol(1))
      call cpseti ('LOC',lcol(1))

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine setlab (lputl,errsev)

C*****************************************************************************C
C  setlab   - This is a CONDRV routine                                        C
C  Section  - Design                                                          C
C  Purpose  - To set up line labels in a fashion specified by the user.  This C
C             routine also sets up line label specified color which may alter C
C             some line label coloring.                                       C
C                                                                             C
C  On entry - LPUTL indicates if CONREC style labels should be used.   ERRSEV C
C             indicates what severity of error will halt execution. Other in- C
C             is made through common blocks.                                  C
C                                                                             C
C  On exit  - Labels have been set up with CONPACK as per the users requests. C
C                                                                             C
C  Assume   - GKS is open.  CONPACK has been initialized.  Contour levels are C
C             set up.  Colors for contour levels are set up.                  C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             CPSETI              CONPACK utility*                            C
C             CPSETR              CONPACK utility*                            C
C             CPGETI              CONPACK utility*                            C
C             CPGETR              CONPACK utility*                            C
C             GETSPC              CONDRV utility                              C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill       Date - June 20, 1990      for the MM4 club  C
C*****************************************************************************C

C  Parameters

      parameter       (idcsp = -1)       ! color index for defaults

C  Integer variables

      integer          lputl,            ! line label flag                 (in)
     *                 errsev            ! error severity comparitor       (in)
      integer          lsize,            ! for common block LABDET
     *                 lortn,            ! for common block LABDET
     *                 langl,            ! for common block LABDET
     *                 lintv             ! for common block LABDET
      integer          zcol(3)           ! for common block ZLCOLS
      integer          lbco(3)           ! for common block LBCOLS
      integer          nclv,             ! number of contour levels     (local)
     *                 zl,               ! zero line position flag      (local)
     *                 clus,             ! contour line usage           (local)
     *                 linco             ! line color index             (local)

C  Logical variables

      logical          lputb,            ! for common block LABDET
     *                 lputp,            ! for common block LABDET
     *                 lfilb             ! for common block LABDET
      logical          hghlt,            ! for common block LBCOLS
     *                 same,             ! for common block LBCOLS
     *                 revrs             ! for common block LBCOLS
      logical          noplt             ! for common block NOPLOT

C  Real variables

      real             lprlw             ! for common block LABDET
      real             csiz,             ! size conversion variable     (local)
     *                 clev              ! contour level value          (local)

C  Common blocks

      common /labdet/  lputb,            ! put boxes on the line labels ?
     *                 lputp,            ! put perimeter on label boxes ?
     *                 lfilb,            ! fill label boxes ?
     *                 lprlw,            ! label box perimeter line width
     *                 lsize,            ! line label character size
     *                 lintv,            ! line label placement per line
     *                 langl,            ! line label angle
     *                 lortn             ! line label orientation
      common /lbcols/  hghlt,            ! highlighted labeled lines ?
     *                 same,             ! line same color as label ?
     *                 revrs,            ! text and fill reverse after zero ?
     *                 lbco              ! line label colors
      common /zrcols/  zcol              ! zero line colors
      common /noplot/  noplt             ! is no picture to be made ?

C****************************  subroutine begin  *****************************C

C  CONPACK internal parameters used in this routine are:
C  LIS - Label Interval Specifier
C  LLA - Line Label Angle
C  LLB - Line Label Box flag
C  LLL - Line Label Line width
C  LLO - Line Label Orientation
C  LLP - Line Label Positioning
C  LLS - Line Label Size
C  LLT - Line Label Text String
C  NCL - Number of Contour Levels
C  PAI - Parameter Array Index
C  CLU - Contour Level Usage
C  LLC - Line Label Color index
C  CLC - Contour Line Color index
C  CLV - Contour Level Value

C  CONREC style labels reduce flexibility somewhat, shield certain things
C  from being fadutzed with if they were requested

      if (lputl .gt. 0) then

C  If LPUTB is true then a box should be masked around all line labels

        if (lputb) then

C  If LPUTP is true then a line should be drawn delineating the above
C  mentioned box

          if (lputp) then

C  If LFILB is true, the above mentioned box should be filled in

            if (lfilb) then
              call cpseti ('LLB',3)
            else
              call cpseti ('LLB',1)
            end if

C  Since a line is wanted, we need to know what line width to use when
C  drawing the line

            call cpsetr ('LLL',lprlw)
          else
            if (lfilb) then
              call cpseti ('LLB',2)
            else
              call cpseti ('LLB',0)
            end if
            call cpsetr ('LLL',0.0)
          end if
        else
          call cpseti ('LLB',0)
        end if

C  Set up the proper size for the line labels

        csiz = float(lsize)/1000.0
        call cpsetr ('LLS',csiz)

C  Set up the proper orientation using LORTN and LANGL

        call cpseti ('LLO',lortn)
        if (lortn .eq. 0) then
          call cpsetr ('LLA',float(langl))
        end if
      else

C  Set up a label interval specifier for CONREC style labels

        lintv = 3
      end if

C  Set up the proper line label interval

      call cpseti ('LIS',lintv)
      call cpgeti ('NCL',nclv)
      zl = -1
      do 10 i = 1,nclv
        call cpseti ('PAI',i)
        call cpgetr ('CLV',clev)
        if (clev .eq. 0.0) then
          zl = i
          call cpgeti ('CLU',clus)
          if (clus .ne. 0) call cpseti ('CLU',1)
        else
          call cpseti ('CLU',1)
        end if
10    continue
      if (zl .lt. 0) then
        do 20 i = 1,nclv,lintv
          call cpseti ('PAI',i)
          call cpseti ('CLU',3)
20      continue
      else
        do 30 i = zl-lintv,1,-lintv
          call cpseti ('PAI',i)
          call cpseti ('CLU',3)
30      continue
        call cpseti ('PAI',zl)
        call cpgeti ('CLU',i)
        if (i .ne. 0) call cpseti ('CLU',3)
        do 40 i = zl+lintv,nclv,lintv
          call cpseti ('PAI',i)
          call cpseti ('CLU',3)
40      continue
      end if

C  Set up a good positioning scheme according to LPTUL and LPUTB
        
      if (lputl .eq. 0) then
        call cpseti ('LLP',1)
      else 
        call cpseti ('LLP',2)
      end if

C  Set up line, label and label box perimeter colors
C  First - If SAME is true then all labeled lines should be the same color
C          as the labels, that color is stored in LBCO(3)

      if (same) then
        do 50 i = 1,nclv
          call cpseti ('PAI',i)
          call cpgeti ('CLU',clus)
          if ((clus .eq. 3) .and. (lbco(3) .ne. idcsp)) then
            if (lbco(3) .eq. idcsp) then
              call cpgeti ('CLC',linco)
              call cpseti ('LLC',linco)
            else
              call cpseti ('CLC',lbco(3))
              call cpseti ('LLC',lbco(3))
            end if
          end if
50      continue

C  Second - If REVRS is true then lines on opposite sides of the zero line
C           should get flipped colors (that is the box fill color becomes
C           the text and perimeter color)

      else if ((revrs) .and. (zl .gt. 1)) then
        do 60 i = 1,zl-1
          call cpseti ('PAI',i)
          if (lbco(3) .eq. idcsp) then
            call cpgeti ('CLC',linco)
            call cpseti ('LLC',linco)
          else
            call cpseti ('LLC',lbco(3))
          end if
60      continue
        call cpseti ('PAI',zl)
        if (zcol(1) .eq. idcsp) then
          call cpgeti ('CLC',linco)
          call cpseti ('LLC',linco)
        else
          call cpseti ('LLC',zcol(1))
        end if
        do 70 i = zl+1,nclv
          call cpseti ('PAI',i)
          call cpseti ('LLC',lbco(2))
70      continue

C  Third - If HGHLT is true then the line and the label should be the same
C          color but the color depends on the default color of the line

      else if (hghlt) then
        do 80 i = 1,nclv
          call cpseti ('PAI',i)
          call cpgeti ('CLU',clus)
          call cpgetr ('CLV',clev)
          if (((clev .eq. 0.0) .and. ((zcol(1) .eq. idcsp) .or.
     *        (zcol(3) .eq. idcsp))) .or. (clev .ne. 0.0)) then
            if ((clus .eq. 3) .and. (.not. noplt)) then
              call cpgeti ('CLC',linco)
              call getspc (linco,errsev)
              if (((clev .eq. 0.0) .and. (zcol(3) .eq. idcsp)) .or.
     *            (clev .ne. 0.0))
     *          call cpseti ('CLC',linco)
              if (((clev .eq. 0.0) .and. (zcol(1) .eq. idcsp)) .or.
     *            (clev .ne. 0.0))
     *          call cpseti ('LLC',linco)
            end if
          end if
80      continue

C  Fourth - Line labels should just be set to there proper color

      else
        do 90 i = 1,nclv
          call cpseti ('PAI',i)
          if (lbco(1) .eq. idcsp) then
            call cpgeti ('CLC',linco)
            call cpseti ('LLC',linco)
          else
            call cpseti ('LLC',lbco(1))
          endif
          if (lbco(3) .ne. idcsp) then
            call cpgeti ('CLU',clus)
            if (clus .eq. 3) call cpseti ('CLC',lbco(3))
          end if
90      continue
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine setlin (zl)

C*****************************************************************************C
C  setcdt   - This is a CONDRV routine                                        C
C  Section  - Contour Lines                                                   C
C  Purpose  - To  set up the line width and dash pattern information given by C
C             the user.  To remove the zero line if the user requests.        C
C                                                                             C
C  On entry - ZL  is false if the zero line should be removed from the  plot. C
C             Line Width, Dash Pattern and partition information is passed in C
C             through common blocks.                                          C
C                                                                             C
C  On exit  - The line width and dash pattern has been set up.  The zero line C
C             has been removed if it was there and was not suppose to be.     C
C                                                                             C
C  Assume   - That GKS is open.                                               C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             CPGETI              CONPACK utility*                            C
C             CPSETI              CONPACK utility*                            C
C             CPGETR              CONPACK utility*                            C
C             CPSETR              CONPACK utility*                            C
C             ----------------------------------------------------------------C
C             * NCAR Graphics Routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill       Date - June 12, 1990      for the MM4 club  C
C*****************************************************************************C

C  Integer variables

      integer          ddpv(3)           ! for common block LWDPDT
      integer          pdpv(100)         ! for common block LWDPPR
      integer          nprt,             ! for common block PARINF
     *                 iprts(100,2)      ! for common block PARINF
      integer          nmlev,            ! number of contour levels     (local)
     *                 i,j               ! loop contours place keepers  (local)

C  Logical variables

      logical          zl                ! draw in the zero line ?         (in)
      logical          ints              ! for common block PARINF

C  Real variables

      real             dlwv(3)           ! for common block LWDPDT
      real             plwv(100)         ! for common block LWDPPR
      real             rprts(100,2)      ! for common block PARINF
      real             temp              ! contour level values         (local)

C  Common blocks

      common /lwdpdt/  dlwv,             ! details line width values
     *                 ddpv              ! details dash pattern values
      common /lwdppr/  plwv,             ! partition line width values
     *                 pdpv              ! partition dash pattern values
      common /parinf/  nprt,             ! number of partitions
     *                 iprts,            ! integer partitions
     *                 rprts,            ! real partitions
     *                 ints              ! are the partitions integers ?

C****************************  subroutine begin  *****************************C

C  The following CONPACK internal parameters are used
C  PAI - Parameter Array Index
C  NCL - Number of Contour Lines
C  CLV - Contour LeVels
C  CLD - Contour Line Dash pattern
C  CLL - Contour Line Line width
C  CLU - Contour Level Usage

C  Get the total number of levels

      call cpgeti ('NCL',nmlev)

C  Take out the zero line if requested by the user

      if (.not. zl) then
        do 10 i = 1,nmlev
          call cpseti ('PAI',i)
          call cpgetr ('CLV',temp)
          if (temp .eq. 0.0) call cpseti('CLU',0)
10      continue
      end if

C  Adjust the dash pattern
C  There are two ways the dash pattern may have been specified
C  1 - In the details table, the array DDPV contains the dash patterns
C      If that was not done, all values in DDPV will be zero
C  2 - In the partitions table, the array PDPV contains the dash patterns

      if (ddpv(1) .eq. 0) then

        do 20 i = 1,nprt

C  For each value from 1 to NPRT, loop through the levels and set the dash
C  pattern

          do 30 j = 1,nmlev

C  Set the current contour level

            call cpseti ('PAI',j)


C  If INTS is true then the partitions were specified in line numbers
C  not values

            if (ints) then
              if ((j .ge. iprts(i,1)) .and. (j .lt. iprts(i,2)))
     *          call cpseti ('CLD',pdpv(i))
            else
              call cpgetr ('CLV',temp)
              if ((temp .ge. rprts(i,1)) .and.
     *            (temp .lt. rprts(i,2)))
     *          call cpseti ('CLD',pdpv(i))
            end if
30        continue
20      continue
      else

C  There are three valuse in the array DDPV, the first is the dash
C  pattern for positive numbers, the second is for the zero line and
C  the third is for negative numbers

        do 40 i = 1,nmlev
          call cpseti ('PAI',i)
          call cpgetr ('CLV',temp)
          if (temp .lt. 0.0) then
            call cpseti ('CLD',ddpv(3))
          else if (temp .gt. 0.0) then
            call cpseti ('CLD',ddpv(1))
          else
            call cpseti ('CLD',ddpv(2))
          end if
40      continue
      end if

C  Adjust the linewith
C  There are two places the line width may be stored.
C  1 - In DLWV which is read in from the details table and will be negative
C      if it was not
C  2 - In PLWV which is read in from the partitions table

      if (dlwv(1) .lt. 0.0) then

        do 50 i = 1,nprt

C  For each value from 1 to NPRT, loop through the levels and set the line
C  width according to the level and the line width indicator

          do 60 j = 1,nmlev

C  Set the current contour level

            call cpseti ('PAI',j)

C  INTS will be true if the partitions were specified by level number and
C  not level value

            if (ints) then
              if ((j .ge. iprts(i,1)) .and. (j .lt. iprts(i,2)))
     *          call cpsetr ('CLL',plwv(i))
            else
              call cpgetr ('CLV',temp)
              if ((temp .ge. rprts(i,1)) .and.
     *            (temp .lt. rprts(i,2)))
     *          call cpsetr ('CLL',plwv(i))
            end if
60        continue
50      continue
      else

C  There are three values in DLWV, the first is the line width multiplier
C  for positive numbers, the second is for the zero line and the third is
C  for negative numbers

        do 70 i = 1,nmlev
          call cpseti ('PAI',i)
          call cpgetr ('CLV',temp)
          if (temp .lt. 0.0) then
            call cpsetr ('CLL',dlwv(3))
          else if (temp .gt. 0.0) then
            call cpsetr ('CLL',dlwv(1))
          else
            call cpsetr ('CLL',dlwv(2))
          end if
70      continue
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine setttl (tlen,title,pnum,scale)

C*****************************************************************************C
C  setttl   - This is a CONDRV routine                                        C
C  Section  - Desgin                                                          C
C  Purpose  - To set up a title or information label to be drawn.             C
C                                                                             C
C  On entry - TLEN  is negative if an information label should be set up.  If C
C             TLEN is zero then no title or information label should be drawn C
C             at all.  Otherwise TLEN is the number of characters in the user C
C             specified title string.   TITLE is that title string.   PNUM is C
C             the number of plots made since the last call to FRAME plus one. C
C             SCALE is the scaling factor used in labeling contour lines. The C
C             details and colors for  the title are passed in  through common C
C             blocks.                                                         C
C                                                                             C
C  On exit  - If NOPLT in common block NOPLOT  was true then an error message C
C             was used and the title was drawn.  If a title was  given by the C
C             user, then it was set up as an information label in CONPACK and C
C             all the details and colors given in the tables was used. If the C
C             user asked for an information label, this was set up with  CON- C
C             PACK and all the information in the table was used.             C
C                                                                             C
C  Assume   - GKS is open.                                                    C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             PCSETI              PLOTCHAR utility*                           C
C             PCGETR              PLOTCHAR utility*                           C
C             CPSETC              CONPACK utility*                            C
C             GETSET              SPPS*                                       C
C             SET                 SPPS*                                       C
C             PLCHHQ              PLOTCHAR utility*                           C
C             PRETTL              CONDRV utility                              C
C             ----------------------------------------------------------------C
C             * NCAR Graphics Routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill       Date - June 27, 1990      for the MM4 club  C
C*****************************************************************************C

C  Parameter

      parameter      (wspc = 0.00707500) ! white space between titles

C  Character variables

      character*120    title             ! title string                    (in)
      character*120    httl              ! title string                 (local)

C  Integer variables

      integer          tlen,             ! # of characters in TITLE        (in)
     *                 pnum              ! indicates if this is an overlay (in)
      integer          tsize             ! for common block TITDET
      integer          lttl              ! for common block TLOCAT
      integer          llsv              ! save variable                (local)

C  Logical variables

      logical          noplt             ! for common block NOPLOT
      logical          tputb,            ! for common block TITDET
     *                 tputp,            ! for common block TITDET
     *                 tfilb             ! for common block TITDET

C  Real variables

      real             scale             ! scale factor for the plot       (in)
      real             tprlw             ! for common block TITDET
      real             csiz,             ! for common block TLOCAT
     *                 boxx(4),          ! for common block TLOCAT
     *                 boxy(4),          ! for common block TLOCAT
     *                 xpos,             ! for common block TLOCAT
     *                 ypos              ! for common block TLOCAT
      real             flsv,             ! save variable                (local)
     *                 frsv,             ! save variable                (local)
     *                 fbsv,             ! save variable                (local)
     *                 ftsv,             ! save variable                (local)
     *                 ulsv,             ! save variable                (local)
     *                 ursv,             ! save variable                (local)
     *                 ubsv,             ! save variable                (local)
     *                 utsv,             ! save variable                (local)
     *                 place,            ! place to put title, vertical (local)
     *                 base,             ! first location of title      (local)
     *                 uwspc             ! actual used value of WSPC    (local)

C  Common blocks

      common /noplot/  noplt             ! draw a special title about errors
      common /titdet/  tputb,            ! put a box around the title ?
     *                 tputp,            ! draw the perimeter of the box ?
     *                 tfilb,            ! fill the box ?
     *                 tprlw,            ! title box perim. line width
     *                 tsize             ! title character size
      common /tlocat/  xpos,             ! horizontal center in frac. coords
     *                 ypos,             ! vertical center in frac. coords
     *                 boxx,             ! four x coords of text extent box
     *                 boxy,             ! four y coords of text extent box
     *                 csiz,             ! character size to use
     *                 lttl              ! final title string length
      common /tstrng/  httl              ! final title string

C****************************  subroutine begin  *****************************C

C  PLOTCHAR internal parameters used are:
C  TE  - Text Extent flag
C  DL  - Distance to the Left edge of the text extent box
C  DR  - Distance to the Right edge of the text extent box
C  DB  - Distance to the Bottom edge of the text extent box
C  DT  - Distance to the Top edge of the text extent box

C  CONPACK internal parameters used are:
C  ILT - Information Label Text

C  If a plot will be made, draw in the correct title

      if (.not. noplt) then

C  Set size of the title

        csiz = float(tsize)

C  Adjust the amount of white space between labels

        uwspc = wspc * (csiz/6.0)

C  Determine base (first title location)

        call getset (flsv,frsv,fbsv,ftsv,ulsv,ursv,ubsv,utsv,llsv)
        place = 1.0 - ftsv
        base  = float(int(place/(csiz/800.0 + uwspc)))
        place = base * (csiz/800.0 + uwspc)
        base  = ftsv + place - uwspc

C  Determine where to put title

        place = (pnum - 1) * (csiz/800.0 + uwspc)
        ypos  = base - place
        xpos  = 0.5

C  Set the text in the title

        if (tlen .lt. 0) then
          if (scale .eq. 1.0) then
            httl(1:24)  = 'Int = $ContourInterval$ '
            httl(25:48) = 'Min = $Contour Minimum$ '
            httl(49:71) = 'Max = $Contour Maximum$'
            lttl = 71
          else
            httl(1:24)  = 'Int = $ContourInterval$ '
            httl(25:48) = 'Min = $Contour Minimum$ '
            httl(48:72) = 'Max = $Contour Maximum$ '
            httl(73:92) = 'By $Scaling  Factor$'
            lttl = 92
          end if
        else
          lttl = tlen
        end if
        call prettl (tlen,title,scale,httl)

C  TPUTB is true if there should be a box around the title

        if (tputb) then
          call pcseti ('TE',1)
          call plchhq (xpos,ypos,httl(1:lttl),csiz,360.0,0.0)
          call pcgetr ('DL',flsv)
          call pcgetr ('DR',frsv)
          call pcgetr ('DT',ftsv)
          call pcgetr ('DB',fbsv)
          boxx(1) = cfux(xpos - 1.025 * flsv)
          boxx(2) = boxx(1)
          boxx(3) = cfux(xpos + 1.03 * frsv)
          boxx(4) = boxx(3)
          boxy(1) = cfuy(ypos - 2.5 * fbsv)
          boxy(4) = boxy(1)
          boxy(2) = cfuy(ypos + 1.8 * ftsv)
          boxy(3) = boxy(2)
          call pcseti ('TE',0)
        end if

C  Convert XPOS and YPOS to user coordinates

        xpos = cfux(xpos)
        ypos = cfuy(ypos)

C  Turn off the CONPACK information label

        call cpsetc ('ILT',' ')
      else

C  Draw in an error message for a title on the bottom of the screen
C  First get and save the any set calls

        call getset (flsv,frsv,fbsv,ftsv,ulsv,ursv,ubsv,utsv,llsv)

C  Normalize the screen

        call set (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)

C  Set up the error message in title

        httl(1:44) = 'No Plot Drawn Due To A Non-Correctable Error'
        lttl = 44

C  Draw the title

        call plchhq (0.5,0.35,httl(1:lttl),-1.0,0.0,0.0)

C  Restore the viewport

        call set (flsv,frsv,fbsv,ftsv,ulsv,ursv,ubsv,utsv,llsv)
      end if

C****************************  subroutine end  *******************************C

      return
      end
      subroutine shadem (xpoly,ypoly,nep,aid,gid,nid)

C*****************************************************************************C
C  shade    - This is a CONDRV routine                                        C
C  Section  - Fill                                                            C
C  Purpose  - To shade in the contour levels being plotted by CONPACK.        C
C                                                                             C
C  On entry - XPOLY, YPOLY, NEP define a polygon to be filled.  AID, GID, NID C
C             allow the routine to know when and how to shade.                C
C                                                                             C
C  On exit  - The incoming polygon has been shaded a shade of grey.           C
C                                                                             C
C  Assume   - GKS is open.                                                    C
C                                                                             C
C  Notes    - Routine             Location of Definition                      C
C             ----------------------------------------------------------------C
C             SFSGFA              SOFTFILL utility*                           C
C             CPGETI              CONPACK utility*                            C
C             SFSETR              SOFTFILL utility*                           C
C             ----------------------------------------------------------------C
C             * NCAR Graphics Routine                                         C
C                                                                             C
C             This routine is called by the AREAS routine ARSCAM.             C
C                                                                             C
C  Author   - Jeremy Asbill       Date - June 12, 1990      for the MM4 club  C
C*****************************************************************************C

C  Parameters

      parameter      (base = 0.0005)
      parameter      (smax = 0.01)

C  Character variables

      character*2     mask           ! for common block MAPFLI

C  Integer variables

      integer         aid(*),        ! area identifiers for the polygon    (in)
     *                gid(*),        ! group identifiers for the polygon   (in)
     *                nep,           ! number of points defining polygon   (in)
     *                nid            ! dimension of identifier arrays      (in)
      integer         nmlev,         ! number of contour levels         (local)
     *                ind(1200),     ! work array for SOFTFILL          (local)
     *                idsp,          ! area identifier for contouring   (local)
     *                idmp           ! area identifier for map          (local)

C  Logical variables

      logical         dosh           ! do shade the polygon             (local)
      logical         lhohl          ! for common block SHDDIR

C  Real variables

      real            xpoly(*),      ! x coords. of polygon points         (in)
     *                ypoly(*)       ! y coords. of polygon points         (in)
      real            incr           ! diff. of soft fill spacing between the
C                                      different contour levels         (local)
      real            dst(1100)      ! work array for SOFTFILL          (local)
      real            space          ! spacing in SOFTFILL              (local)

C  Common blocks

      common /shddir/ lhohl          ! shade for high to low or visa versa?
      common /mapfli/ mask           ! map masking indicator

C****************************  subroutine begin  *****************************C

C  CONPACK internal parameters used in this routine are :
C  NCL - Number of Contour Levels

C  SOFTFILL internal parameters used in this routine are :
C  SP  - Pattern Fill Line Spacing

C  First get the number of levels there are in all

      call cpgeti ('NCL',nmlev)

C  Calculate the INCR according to the number of levels

      incr = (smax - base)/(nmlev - 1)

C  Determine the area identifier

      dosh = .true.
      do 10 i = 1,nid
        if (aid(i) .lt. 0) dosh = .false.
10    continue

C  If the area needs to be shaded, calculate the spacing and set it

      if (dosh) then
        idsp = 0
        do 20 i = 1,nid
          if (gid(i) .eq. 3) idsp = aid(i)
          if (gid(i) .eq. 6) idmp = aid(i)
20      continue
        if (lhohl) then
          space = smax - (idsp - 1) * incr
        else
          space = (idsp - 1) * incr + base
        end if
        call sfsetr ('SP',space)

C  Determine if the map masks the area out

        if ((mask(1:2) .eq. 'LO') .or. (mask(1:2) .eq. 'lo') .or.
     *      (mask(1:2) .eq. 'Lo') .or. (mask(1:2) .eq. 'lO')) then
          if (mapaci(idmp) .eq. 1) dosh = .false.
        else if ((mask(1:2) .eq. 'LL') .or. (mask(1:2) .eq. 'll') .or.
     *           (mask(1:2) .eq. 'Ll') .or. (mask(1:2) .eq. 'lL')) then
          if (idmp .eq. 2) dosh = .false.
        else if ((mask(1:2) .eq. 'OO') .or. (mask(1:2) .eq. 'oo') .or.
     *           (mask(1:2) .eq. 'Oo') .or. (mask(1:2) .eq. 'oO')) then
          if (idmp .ne. 2) dosh = .false.
        else if ((mask(1:2) .eq. 'OL') .or. (mask(1:2) .eq. 'ol') .or.
     *           (mask(1:2) .eq. 'Ol') .or. (mask(1:2) .eq. 'oL')) then
          if (mapaci(idmp) .ne. 1) dosh = .false.
        end if

C  Also shade the area

        if (dosh) call sfsgfa (xpoly,ypoly,nep,dst,1100,ind,1200,1)
      end if

C*****************************  subroutine end  ******************************C

      return
      end
      subroutine subcon (indata,xdim,ydim,xstr,ystr,xend,yend)

C*****************************************************************************C
C  subcon   - This is a CONDRV routine                                        C
C  Section  - Design                                                          C
C  Purpose  - To determine  what portion of  the data is going to be  plotted C
C             an to initialize CONPACK with that subset of the data.          C
C                                                                             C
C  On entry - INDATA contains the data to be contoured.   XDIM  and  YDIM are C
C             the dimensions of INDATA.  XSTR, YSTR, XEND  and  YEND define a C
C             subset of INDATA that should actually be plotted.               C
C                                                                             C
C  On exit  - CONPACK has been initialized.  That is the internal  parameters C
C             describing  the data  array have been set up  for management in C
C             CONPACK routines.  The subset to  actually be plotted has  been C
C             transferred to common block DATAKP.                             C
C                                                                             C
C  Assume   - GKS is open.  Contouring information has been set up.           C
C                                                                             C
C  Notes    - Routine Name        Location of Definition                      C
C             ----------------------------------------------------------------C
C             CPRECT              CONPACK utility*                            C
C             CPPKCL              CONPACK utility*                            C
C             CPSETI              CONPACK utility*                            C
C             ----------------------------------------------------------------C
C             * NCAR Graphics routine                                         C
C                                                                             C
C  Author   - Jeremy Asbill      Date - August 10, 1990     for the MM4 club  C
C*****************************************************************************C

C  Integer variables

      integer          xdim,             ! x dimension of indata           (in)
     *                 ydim,             ! y dimension of indata           (in)
     *                 xstr,             ! x coord. of first grid to plot  (in)
     *                 ystr,             ! y coord. of first grid to plot  (in)
     *                 xend,             ! x coord. of last  grid to plot  (in)
     *                 yend              ! y coord. of last  grid to plot  (in)
      integer          iwork(1000)       ! for common block DATAKP
      integer          myx,              ! x dimension of MYWORK        (local)
     *                 myy,              ! y dimension of MYWORK        (local)
     *                 x,y,              ! loop counters                (local)
     *                 ix,iy             ! indexed loop counters        (local)

C  Real variablea

      real             indata(xdim,ydim) ! data to be contoured            (in)
      real             mywork(1000,1000),! for common block DATAKP
     *                 rwork(5000)       ! for common block DATAKP
      real             temp              ! test variable                (local)

C  Common blocks

      common /datakp/  mywork,           ! array of data to plot
     *                 iwork,            ! integer work space for CONPACK
     *                 rwork             ! real work space for CONPACK

C****************************  Subroutine Begin  *****************************C

C  CONPACK internal parameters used in this routine are :
C  CLS - Contour Level Selection flag

C  Determine dimensions of data that will actually be plotted

      myx = xend - xstr + 1
      myy = yend - ystr + 1

C  Transfer the data into the common block, but only the data that
C  will actually be plotted

      do 10   x = 1,myx
        do 20 y = 1,myy
          ix = xstr + x - 1
          iy = ystr + y - 1
          mywork(x,y) = indata(ix,iy)
20      continue
10    continue

C  Initialize CONPACK for rectangular array

      call cprect (mywork,1000,myx,myy,rwork,5000,iwork,1000)

C  Have CONPACK pick contour levels now and disable the ability for later

      call cppkcl (mywork,rwork,iwork)
      call cpseti ('CLS',0)

C*****************************  Subroutine End  ******************************C

      return
      end
      subroutine rdcolt (unum)

C*****************************************************************************C
C  rdcolt   - This is a stand-alone subroutine.                               C
C                                                                             C
C  purpose  - To set up a color table in the GKS graphics standard.           C
C                                                                             C
C  on entry - UNUM is the unit number from where to read the color table.     C
C             The following things must happen before calling this routine.   C
C             RDCOLT does not handle any of them.  GKS must be open and a     C
C             workstation must be open and active.                            C
C                                                                             C
C  on exit  - A color table has been defined.                                 C
C                                                                             C
C  notes    - The subroutine GSCR is defined in the GKS standard.  It stands  C
C             for GKS Set Color Representation and it assigns a color index   C
C             number to a color.                                              C
C                                                                             C
C             An example of a color table can be found in the directory       C
C             dp:[asbill.public.graphics.misc] and is called excol.tbl.  Your C
C             tables must follow this example considering a few flexibilities C
C             as follows:  You may include 1 to 100 colors with no index over C
C             99 specified.  The columns may be placed in any position so     C
C             long as the vertical bar character appears between each entry.  C
C                                                                             C
C                                                                             C
C  author   - Jeremy Asbill                                                   C
C  date     - January 18, 1991                                                C
C  for      - The MM4 Club                                                    C
C*****************************************************************************C

C  Character variables

      character*80  whline          ! a line from the color table       (local)

C  Integer variables

      integer       unum            ! unit number for table                (in)
      integer       i,j,            ! loop counters                     (local)
     *              conum           ! color number in the table         (local)

C  Logical variables

      logical       red,            ! has the red value been parsed?    (local)
     *              green,          ! has the green value been parsed?  (local)
     *              blue,           ! has the blue value been parsed?   (local)
     *              there           ! indicates if the table was found? (local)

C  Real variables

      real          rcomp,          ! amount of red in a color          (local)
     *              gcomp,          ! amount of green in a color        (local)
     *              bcomp           ! amount of blue in a color         (local)

C****************************  subroutine begin  *****************************C

C  Check to see if the table is at the unit number and if the table is
C  the write one

      if (unum .eq. 0) then
        there = .false.
      else
        call tbllok (unum,'COLOR TABL',0,there,whline,'RDCOLT')
      end if
      if (there) then
        backspace (unum)

C  Read in an entire line from the color table

10      continue
          read (unum,110) whline(1:80)
          if (whline(1:1) .eq. '-') goto 90

C    Initialize flags

          red   = .false.
          green = .false.
          blue  = .false.

C  Parse the line for its components

          i = 1
30        continue
          if ((whline(i:i) .ne. '|') .and. (whline(i:i) .ne. '!')) then 
            i = i + 1
            goto 30
          end if
          i = i + 1

C    Parse to a non-blank character

40        continue
          if (whline(i:i) .eq. ' ') then
            i = i + 1
            goto 40
          end if
          if (blue)  goto 70
          if (green) goto 60
          if (red)   goto 50

C    Red component

          read (whline(i:i+3),120) rcomp
          red = .true.
          i = i + 4
          goto 30

C    Green component

50        continue
          read (whline(i:i+3),120) gcomp
          green = .true.
          i = i + 4
          goto 30

C    Blue component

60        continue
          read (whline(i:i+3),120) bcomp
          blue = .true.
          i = i + 4
          goto 30

C    Color number

70        continue
          if (whline(i+1:i+1) .ne. ' ') then
            read (whline(i:i+1),130) conum
          else
            read (whline(i:i),140) conum
          end if

C   Assign the color table with GKS calls

          call gscr (1,conum,rcomp,gcomp,bcomp)
        goto 10
90      print *, 'RDCOLT - Color Table Set Up'
      else

C  Assign the default color table if so requested

        call gscr (1,0,0.00,0.00,0.00)    ! 0=black
        call gscr (1,1,1.00,1.00,1.00)    ! 1=white
        call gscr (1,2,0.66,0.66,0.66)    ! 2=light gray
        call gscr (1,3,0.40,0.40,0.40)    ! 3=dark gray
        call gscr (1,4,0.00,0.00,1.00)    ! 4=blue
        call gscr (1,5,0.00,1.00,0.00)    ! 5=green
        call gscr (1,6,1.00,0.00,0.00)    ! 6=red
        print *, 'RDCOLT - Default Color Table Set Up'
      end if

C*****************************  subroutine end  ******************************C

C  Format statements begin ...

100   format (A1)
110   format (A80)
120   format (F4.2)
130   format (I2)
140   format (I1)

C  Format statements end.

      return
      end
