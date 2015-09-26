      PROGRAM BUFR
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
C
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
      ict = 0    ! report counter
      open(3,file='littler',status='unknown',form='formatted')
      open(12,file='flist',status='old',form='formatted')
   87 continue
      read(12,'(a256)',end=988,err=987) cfin

c determine fm number of the data based on file name
c
      iln = index(cfin,'_') - 1
      if (cfin(1:iln) .eq. 'temp') then
        ifm = 35
      elseif (cfin(1:iln) .eq. 'pilot') then
        ifm = 32
      elseif (cfin(1:iln) .eq. 'airep') then
        ifm = 96
      elseif (cfin(1:iln) .eq. 'acars') then
        ifm = 96
      elseif (cfin(1:iln) .eq. 'ship') then
        ifm = 13
      elseif (cfin(1:iln) .eq. 'buoy') then
        ifm = 18
      elseif (cfin(1:iln) .eq. 'synop') then
        ifm = 12
      elseif (cfin(1:iln) .eq. 'aws') then
        ifm = 15
      elseif (cfin(1:iln) .eq. 'satob') then
        ifm = 88
      elseif (cfin(1:iln) .eq. 'satem') then
        ifm = 87
      else
        write(6,*) 'Observation type ',cfin(1:iln),' is not supported. 
     &Skipping.'
        go to 87
      endif

      ILN=INDEX(CFIN,' ')
      ILN=ILN-1

c     write(6,*) 'Processing FM-',ifm,' from file ',cfin(1:iln)
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
      write(6,*) 'opening ',cfin(1:iln)
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
c     WRITE(*,'(A,$)') ' DO YOU WANT TO PRINT( Y/N ) : '
         OPRT=.TRUE.
      ICODE=0
c     WRITE(*,'(A,$)') ' CODE TABLES TO BE PRINTED ( Y/N ) : '
         ICODE=1
c     WRITE(*,'(A,$)') ' RECORD NUMBER TO START FROM : '
      NR = 1
C
 201  CONTINUE
C
      OSEC3=.TRUE.
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

      iyr = ksec1(9)
      imo = ksec1(10)
      idy = ksec1(11)
      ihr = ksec1(12)
      imn = ksec1(13)
c     write(6,*) 'date = ',iyr,imo,idy,ihr,imn
C
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

c     write(6,*) 'ktdexl = ',ktdexl
c     do jj = 1,9
c       write(6,*) jj,' ksup = ',ksup(jj)
c     enddo
c     write(6,*) 'kel = ', KVALS/KSUP(6)
c     write(6,*) 'jelem = ',jelem
c     write(6,*) 'KVALS = ',kvals

      do ij = 1, ksup(6)    ! loop over reports
	do kj = 1,ktdexl
	  jj = kj + (ij-1)*kel
c         write(6,*) 'sta = ',ij,' val = ',values(jj),' cnames = ',
c    &        cnames(kj)
c         write(6,*) 'sta = ',ij,' ktdexp = ',ktdexp(kj),' cunits = ',
c    &        cunits(kj)
c         write(6,789) ij,ktdexp(kj),values(jj),cunits(kj),cnames(kj)
        enddo
      enddo
  789 format (i6,i8,f20.6,1x,a20,a80)
      call wlittler (values,kvals,ktdexp,jelem,ktdexl,kel,ksup(6),
     &   ict,3,ifm,cvals)
C
C*         4.6 PRINT SECTION 4 (DATA).
C              -----------------------
 460  CONTINUE
C
      ist = 1
      iend = ktdexl
C
C              PRINT DATA
C
      ICODE=0
 
c        CALL BUPRT(ICODE,IST,IEND,KEL,CNAMES,CUNITS,CVALS,
c    1              KVALS,VALUES,KSUP,KSEC1,IERR)
C
C     -----------------------------------------------------------------
C*          5. COLLECT DATA FOR REPACKING.
C              ---------------------------
 500  CONTINUE
 600  CONTINUE
C
      NPACK=0
C
      GO TO 300
C     -----------------------------------------------------------------
C
 101  CONTINUE
C
      go to 87
  987 write(6,*) 'error reading flist'
      stop
  988 write(6,*) 'end of file in flist'
      END
c-------------------------------------------------------
      subroutine wlittler (v,kvals,kt,jelem,ktdexl,kel,nsta,
     &    ict,iunit,ifm,cvals)
      parameter (xmis = -888888., mkx=300)
      real*8 v(kvals)
      real lat, lon, elev
      real p(mkx), z(mkx), t(mkx), td(mkx), spd(mkx), dir(mkx)
      integer kt(jelem)
      character ctime*12, ch2*2, ch3*3, ch4*4
      character*80 cvals(kel)
      character*40 id, name, platform, source
      logical is_sound
      id     = '                                        '
      name   = '                                        '
      source = '                                        '
      kx = 1
      if (ifm .eq. 12) then
        platform = 'FM-12 SYNOP                             '
	is_sound = .false.
      else if (ifm .eq. 13) then
        platform = 'FM-13 SHIP                              '
	is_sound = .false.
      else if (ifm .eq. 15) then
        platform = 'FM-15 METAR                             '
	is_sound = .false.
      else if (ifm .eq. 32) then
        platform = 'FM-32 PILOT                             '
	is_sound = .true.
      else if (ifm .eq. 35) then
        platform = 'FM-35 TEMP                              '
	is_sound = .true.
      else if (ifm .eq. 88) then
        platform = 'FM-88 SATOB                             '
	is_sound = .true.
      else if (ifm .eq. 96) then
        platform = 'FM-96 AIREP                             '
	is_sound = .true.
      endif
      do 100 i = 1, nsta
        ict = ict + 1
	iw = 0
        call initzero(p, z, t, td, spd, dir,
     &      slp, psfc, elev ,lat, lon, xmis, kx)
	kx = 0
	do k = 1, ktdexl
	  j = k + (i-1)*kel
c         write(6,*) i,kt(k),v(j)
	  if (kt(k) .eq. 1001) then
	    write(ch2,'(i2.2)') int(v(j))
	    id(1:2) = ch2
	  endif
	  if (kt(k) .eq. 1002) then
	    write(ch3,'(i3.3)') int(v(j))
	    id(3:5) = ch3
	  endif
	  if (kt(k) .eq. 1006) then    ! aircraft
	    m = int(v(j)) / 1000 
	    ml = int(v(j)) - m*1000
            id(1:ml) = cvals(m)(1:ml)
	    elev = xmis
	  endif
	  if (kt(k) .eq. 1006) then    ! satellite  not implemented by kma
	    write(ch2,'(i2.2)') int(v(j))
	    id = 'SAT'//ch2
	  endif
	  if (kt(k) .eq. 1011) then
	    m = int(v(j)) / 1000 
	    ml = int(v(j)) - m*1000
            id(1:ml) = cvals(m)(1:ml)
	    elev = 0.    ! watch out for Great Lakes
	  endif
	  if (kt(k) .eq. 4001) then
	    write(ch4,'(i4.4)') int(v(j))
	    ctime(1:4) = ch4
	  endif
	  if (kt(k) .eq. 4002) then
	    write(ch2,'(i2.2)') int(v(j))
	    ctime(5:6) = ch2
	  endif
	  if (kt(k) .eq. 4003) then
	    write(ch2,'(i2.2)') int(v(j))
	    ctime(7:8) = ch2
	  endif
	  if (kt(k) .eq. 4004) then
	    write(ch2,'(i2.2)') int(v(j))
	    ctime(9:10) = ch2
	  endif
	  if (kt(k) .eq. 4005) then
	    write(ch2,'(i2.2)') int(v(j))
	    ctime(11:12) = ch2
	  endif
	  if (kt(k) .eq. 5001) lat = verifi(v(j),-90.,90.,xmis)
	  if (kt(k) .eq. 6001) lon = verifi(v(j),-180.,180.,xmis)
	  if (kt(k) .eq. 5002) lat = verifi(v(j),-90.,90.,xmis)
	  if (kt(k) .eq. 6002) lon = verifi(v(j),-180.,180.,xmis)
	  if (kt(k) .eq. 7001) elev = verifi(v(j),-200.,9100.,xmis)
	  if (kt(k) .eq. 7030) elev = verifi(v(j),-200.,9100.,xmis)
	  if (kt(k) .eq. 7004) then
	    kx = kx + 1
	    p(kx) = verifi(v(j),100.,110000.,xmis)
	  endif
	  if (kt(k) .eq. 10004) then
	    psfc = verifi(v(j),100.,1100.,xmis)
	    if (psfc .gt. 0.) psfc = psfc * 100.
	    p(1) = psfc
            z(1) = elev   ! for sfc stations, 
	    kx = kx + 1
	  endif
	  if (kt(k) .eq. 10051) then
	    slp = verifi(v(j),100.,1100.,xmis)
	    if (slp .gt. 0.) slp = slp * 100.
	  endif
	  if (kt(k) .eq.  7002) then
	    kx = kx + 1
	    z(kx) = verifi(v(j),-200.,55000.,xmis)  ! m
	  endif
	  if (kt(k) .eq. 10009) z(kx) = verifi(v(j),-200.,55000.,xmis)  ! gpm
	  if (kt(k) .eq. 11011) dir(1) = verifi(v(j),0.,360.,xmis)  ! 10m
	  if (kt(k) .eq. 11012) spd(1) = verifi(v(j),0.,200.,xmis)  ! 10m
	  if (kt(k) .eq. 11001) dir(kx) = verifi(v(j),0.,360.,xmis)
	  if (kt(k) .eq. 11002) spd(kx) = verifi(v(j),0.,200.,xmis)
	  if (kt(k) .eq. 12004) t(1) = verifi(v(j),0.,360.,xmis)    ! 2m
	  if (kt(k) .eq. 12006) td(1) = verifi (v(j),0.,360.,xmis)  ! 2m
	  if (kt(k) .eq. 12001) t(kx) = verifi(v(j),0.,360.,xmis)   ! aircraft temp
	  if (kt(k) .eq. 12101) t(kx) = verifi(v(j),0.,360.,xmis)
	  if (kt(k) .eq. 12003) td(kx) = verifi (v(j),0.,360.,xmis) ! aircraft td
	  if (kt(k) .eq. 12103) td(kx) = verifi (v(j),0.,360.,xmis)
	  enddo
	  if (lat .eq. xmis .or. lon .eq. xmis) then
	    ict = ict - 1
            go to 100
	  endif
          if (is_sound .and. z(1) .eq. elev) then
	    psfc = p(1)
	  endif
	  if (is_sound .and. (ifm .ne. 96 .and. ifm .ne. 88 )) then
	    call cmprsnd (p, z, t, td, spd, dir, kx, xmis)
	  endif
            call write_obs (p, z,
     &      t, td, spd, dir,
     &      slp, psfc, elev ,lat, lon,
     &      ctime , kx ,
     &      id, name, platform, source,
     &      is_sound, bogus, ict, iunit)
  100 continue
      end
c-------------------------------------
      SUBROUTINE write_obs ( p, z, t, td, spd, dir, 
     &                      slp, psfc, ter, xlat, xlon, cdate, kx, 
     & string1, string2, string3, string4, is_sound, bogus, 
     & iseq_num, iunit)

      dimension p(kx), z(kx),t(kx),td(kx),spd(kx),dir(kx)

      character *20 date_char
      character *40 string1, string2 , string3 , string4
      CHARACTER *84  rpt_format 
      CHARACTER *22  meas_format 
      CHARACTER *14  end_format
      character cdate*12, cmin*4
      logical is_sound,bogus

      rpt_format =  ' ( 2f20.5 , 2a40 , ' 
     *             // ' 2a40 , 1f20.5 , 5i10 , 3L10 , ' 
     *             // ' 2i10 , a20 ,  13( f13.5 , i7 ) ) '
      meas_format =  ' ( 10( f13.5 , i7 ) ) '
      end_format = ' ( 3 ( i7 ) ) ' 

      date_char(17:20)='0000'
      date_char(1:6)='      '
      date_char(7:18) = cdate

      WRITE ( UNIT = iunit, ERR = 19, FMT = rpt_format ) 
     *        xlat,xlon, string1 , string2 , 
     *        string3, string4, ter, kx*6, 0, 0, iseq_num, 0, 
     *        is_sound,bogus,.false., 
     *         -888888, -888888, date_char, 
     *         slp,0,-888888.,0, -888888.,0, -888888.,0, psfc,0,
     *               -888888.,0, 
     *               -888888.,0, -888888.,0, -888888.,0, -888888.,0, 
     *               -888888.,0, 
     *               -888888.,0, -888888.,0
   
      do 100 k = 1 , kx
         WRITE ( UNIT = iunit , ERR = 19 , FMT = meas_format ) 
     *          p(k), 0, z(k),0, t(k),0, td(k),0, 
     *          spd(k),0, dir(k),0, 
     *          -888888.,0, -888888.,0,-888888.,0, -888888.,0
100   continue
      WRITE ( UNIT = iunit , ERR = 19 , FMT = meas_format ) 
     * -777777.,0, -777777.,0,float(kx),0,
     * -888888.,0, -888888.,0, -888888.,0, 
     * -888888.,0, -888888.,0, -888888.,0, 
     * -888888.,0
      WRITE ( UNIT = iunit , ERR = 19 , FMT = end_format )  kx, 0, 0

      return
19    continue
      print *,'troubles writing little_r observation'
      stop 19
      END
c------------------------------------------
      subroutine subdat (ccyymmddhh, dh, idate)
      INTEGER ccyymmddhh,ccyy,mmddhh,mm,dd,hh,dh
      character*10 cmin

      ccyy   = ccyymmddhh / 1000000
      mmddhh = MOD ( ccyymmddhh , 1000000 )
      mm     = mmddhh / 10000
      dd     = MOD ( mmddhh , 10000 ) / 100
      hh     = MOD ( mmddhh , 100 )

      hh = hh + dh
   10 IF ( hh .LT. 0 ) THEN
         hh = hh + 24
         CALL change_date ( ccyy, mm, dd, -1 )
      ELSEIF ( hh .GT. 23 ) THEN
         hh = hh - 24
         CALL change_date ( ccyy, mm, dd, 1 )
      ELSE
         WRITE (cmin,'(I4.4,3I2.2)') ccyy,mm,dd,hh
         read (cmin,'(i10)') idate
         return
      ENDIF
      GOTO 10
      END
C
      SUBROUTINE change_date ( ccyy, mm, dd, delta )
      INTEGER ccyy, mm, dd, delta
      INTEGER mmday(12)
      DATA    mmday/31,28,31,30,31,30,31,31,30,31,30,31/

      mmday(2) = 28
      IF ( MOD(ccyy,4) .EQ. 0 ) THEN
         IF     ( MOD(ccyy,400) .EQ. 0 ) THEN
            mmday(2) = 29
         ELSEIF ( MOD(ccyy,100) .NE. 0 ) THEN
            mmday(2) = 29
         ENDIF
      ENDIF

      dd = dd + delta
      IF ( dd .EQ. 0 ) THEN
         mm = mm - 1
         IF ( mm .EQ. 0 ) THEN
            mm = 12
            ccyy = ccyy - 1
         ENDIF
         dd = mmday(mm)
      ELSEIF ( dd .GT. mmday(mm) ) THEN
         dd = 1
         mm = mm + 1
         IF ( mm .GT. 12 ) THEN
            mm = 1
            ccyy = ccyy + 1
         ENDIF
      ENDIF
      RETURN
      END
c-----------------------------------------------------------------
      subroutine jdate (iyr, jday, ihr, jtime)
      character tmp*8
      integer mon(12)
      data mon/31,28,31,30,31,30,31,31,30,31,30,31/
      write(tmp,'(i4)') iyr
      read(tmp,'(2x,i2)') jyr
      if (mod(jyr,4) .eq. 0) mon(2) = 29
      m = 0
      do i = 1, 12
	m = m + mon(i)
	if (jday .le. m ) go to 10
      enddo
   10 continue
      if ( i .gt. 1 ) then
	idy = jday - ( m - mon(i))
      else
	idy = jday 
      endif
      write(tmp,'(4(i2.2))') jyr, i, idy, ihr
      read(tmp,'(i8)') jtime
      end
c-----------------------------------------------------------------
      real function verifi (x,xmin,xmax,xmis)
      real*8 x
      if (x .lt. xmin .or. x .gt. xmax) then
        verifi = xmis 
      else
        verifi = x
      endif
      end
c-----------------------------------------------------------------
      subroutine initzero(p, z, t, td, spd, dir,
     &      slp, psfc, elev ,lat, lon, xmis, kx)
      real p(kx), z(kx), t(kx), td(kx), spd(kx), dir(kx)
      real slp, psfc, elev, lat, lon, xmis
      do i = 1, kx
        p(i) = xmis
        z(i) = xmis
        t(i) = xmis
        td(i) = xmis
        spd(i) = xmis
        dir(i) = xmis
      enddo
      slp = xmis
      psfc = xmis
      elev = xmis
      lat = xmis
      lon = xmis
      return
      end
c-----------------------------------------------------------------
      subroutine cmprsnd (p, z, t, td, spd, dir, kx, xmis)
c kma files have levels with all missing data, so delete them
      real p(kx), z(kx), t(kx), td(kx), spd(kx), dir(kx)
      il = 1
      ih = kx
   10 continue
c     write(6,*) 'begin loop, il = ',il,' ih = ',ih
      do k = il, ih
        if (p(k) .eq. xmis .and. z(k) .eq. xmis .and. t(k) .eq. xmis 
     &      .and. td(k) .eq. xmis .and. spd(k) .eq. xmis .and. 
     &      dir(k) .eq. xmis) go to 20
      enddo
      kx = ih
      return
   20 continue
      do j = k, ih-1
        p(j) = p(j+1)
        z(j) = z(j+1)
        t(j) = t(j+1)
        td(j) = td(j+1)
        spd(j) = spd(j+1)
        dir(j) = dir(j+1)
      enddo
      il = min0(k,kx)
      ih = max0(ih - 1,1)
      if ( il .eq. 1 .and. ih .eq. 1 ) then
        kx = 1    ! if all levels are missing, return just one.
	return
      else
        goto 10
      endif
      end
