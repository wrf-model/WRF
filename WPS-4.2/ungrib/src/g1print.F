!
! Print information about a grib1 file.
!  Usage: "g1print [-v] [-V] filename"
!
!  Originally from gribscan.
!  January 2019: Length of field changed from 5 to 7.
! 
program g1print
  use module_grib
  interface
     subroutine parse_args(err, a1, h1, i1, l1, a2, h2, i2, l2,&
          a3, h3, i3, l3, hlast)
       integer :: err
       character(len=*) , optional :: a1, a2, a3
       character(len=*), optional :: h1, h2, h3
       integer , optional :: i1, i2, i3
       logical, optional :: l1, l2, l3
       character(len=*), optional :: hlast
     end subroutine parse_args
  end interface

  character(len=120) :: flnm
  character(len=30) :: hopt
  real, allocatable, dimension(:) :: datarray
  integer :: ierr, igherr
  integer :: cc
  character(len=100) :: fmt = '(I4,1X, &
       & I3,1x, A7,1x, &
       & I4, &
       & 2(1x,I4),2x,I4.4,2("-",I2.2),"_",I2.2,":",&
       & I2.2, 1x, "+", i3.2)'
  logical :: ivb = .FALSE.
  logical :: idb = .FALSE.
  integer :: year
  character(len=7) :: field
  integer :: ec_rec_len

  ec_rec_len = 0
! ec_rec_len = 26214508
  flnm = ' '
  call parse_args(ierr, a1='v', l1=ivb, a2='V', l2=idb, hlast=flnm)
  if (ierr.ne.0) then
     call getarg(0, hopt)
     write(*,'(//,"Usage: ", A, " [-v] [-V] file",/)') trim(hopt)
     write(*,'("     -v   : Print more information about the GRIB records")')
     write(*,'("     -V   : Print way too much information about the GRIB&
          & records")')
     write(*,'("     file : GRIB file to read"//)')
      stop
!    stop
  endif

  if (idb) ivb = .TRUE.

  call c_open(idum, munit, flnm, 1, ierr, 1)

  if (.not. ivb) then
     write(*,'(52("-"))')
     write(*,'(" rec GRIB GRIB  Lvl  Lvl  Lvl         Time      Fcst")')
     write(*,'(" Num Code name  Code one  two                   hour")')
     write(*,'(52("-"))')
  endif 

  irec = 0
  call gribget(munit, ierr, ec_rec_len)
  do while (ierr.eq.0) 
     irec = irec + 1
     call gribheader(0,igherr, ec_rec_len)
     if (igherr /= 0) then
        call deallogrib
        call gribget(munit, ierr, ec_rec_len)
        cycle
     endif

     call fieldname(sec1(2), sec1(3), sec1(4), sec1(7), sec1(24), field)

     if (ivb) then
        call gribprint(0)
        call gribprint(1)
        call gribprint(2)
        call gribprint(3)
        call gribprint(4)
           if (sec2(4).eq.50) then
              ndat = (infogrid(1)+1)*(infogrid(2)+1)
           else
              ndat = (infogrid(1)*infogrid(2))
           endif
           allocate(datarray(ndat))
           call gribdata(datarray, ndat)
	   fldmax = datarray(1)
	   fldmin = datarray(1)
	   do j = 1, ndat
	     if (datarray(j).gt.fldmax) fldmax=datarray(j)
	     if (datarray(j).lt.fldmin) fldmin=datarray(j)
	   enddo
	write(*,*) "  "
	write(*,*) "  ",field," : "
        write(*,'(5x,"Minimum Data Value ",t45,":",g14.5)') fldmin
        write(*,'(5x,"Maximum Data Value ",t45,":",g14.5)') fldmax
        write(*,'(//,70("*"))')
        if (idb) then
           print*, 'Datarray = ', Datarray
        endif
           deallocate(datarray)
     else
        CC = sec1(22)
        year = (cc-1)*100 + sec1(11)
	if ( sec1(16) .eq. 254) then
	  ifcst = sec1(17)/3600.
	else
	  ifcst = sec1(17)
	endif
        write(*,FMT) irec, sec1(7), field, sec1(8:10), year,sec1(12:15),ifcst
     endif

     call deallogrib

     call gribget(munit, ierr, ec_rec_len)
  enddo
  if (ierr.eq.1) write(*,'(/,"***** End-Of-File on C unit ", I3,/)') munit
  call c_close( munit, 0, ierr)

end program g1print

subroutine parse_args(err, a1, h1, i1, l1, a2, h2, i2, l2, a3, h3, i3, l3, &
     hlast)
  integer :: err
  character(len=*) , optional :: a1, a2, a3
  character(len=*), optional :: h1, h2, h3
  integer , optional :: i1, i2, i3
  logical, optional :: l1, l2, l3
  character(len=*), optional :: hlast

  character(len=100) :: hold
  integer :: ioff = 0

  if (present(hlast)) then
     ioff = -1
  endif

  err = 0

  narg = iargc()
  numarg = narg + ioff

  i = 1
  LOOP : do while ( i <= numarg)

     ierr = 1
     if (present(i1)) then
        call checkiarg(i, a1, i1, ierr)
     elseif (present(h1)) then
        call checkharg(i, a1, h1, ierr)
     elseif (present(l1)) then
        call checklarg(i, a1, l1, ierr)
     endif
     if (ierr.eq.0) cycle LOOP

     if (present(i2)) then
        call checkiarg(i, a2, i2, ierr)
     elseif (present(h2)) then
        call checkharg(i, a2, h2, ierr)
     elseif (present(l2)) then
        call checklarg(i, a2, l2, ierr)
     endif
     if (ierr.eq.0) cycle LOOP

     if (present(i3)) then
        call checkiarg(i, a3, i3, ierr)
     elseif (present(h3)) then
        call checkharg(i, a3, h3, ierr)
     elseif (present(l3)) then
        call checklarg(i, a3, l3, ierr)
     endif
     if (ierr.eq.0) cycle LOOP

     err = 1
     call getarg(1, hold)
     write(*, '("arg = ", A)') trim(hold)

     exit LOOP

  enddo LOOP

  if (present(hlast)) then
     if (narg.eq.0) then
        err = 1
     else
        call getarg(narg, hlast)
     endif
  endif

contains
  subroutine checkiarg(c, a, i, ierr)
    integer :: c
    character(len=*) :: a
    integer :: i

    character(len=100) :: hold
    ierr = 1

    call getarg(c, hold)

    if ('-'//a.eq.trim(hold)) then
       c = c + 1
       call getarg(c, hold)
       read(hold, *) i
       c = c + 1
       ierr = 0
    elseif ('-'//a .eq. hold(1:len_trim(a)+1)) then
       hold = hold(len_trim(a)+2: len(hold))
       read(hold, *) i
       c = c + 1
       ierr = 0
    endif
        
  end subroutine checkiarg
  subroutine checkharg(c, a, h, ierr)
    integer :: c
    character(len=*) :: a
    character(len=*) :: h

    character(len=100) :: hold
    ierr = 1

    call getarg(c, hold)

    if ('-'//a.eq.trim(hold)) then
       c = c + 1
       call getarg(c, hold)
       h = trim(hold)
       c = c + 1
       ierr = 0
    elseif ('-'//a .eq. hold(1:len_trim(a)+1)) then
       hold = hold(len_trim(a)+2: len(hold))
       h = trim(hold)
       c = c + 1
       ierr = 0
    endif
        
  end subroutine checkharg

  subroutine checklarg(c, a, l, ierr)
    integer :: c
    character(len=*) :: a
    logical :: l

    character(len=100) :: hold
    ierr = 1

    call getarg(c, hold)
    if ('-'//a.eq.trim(hold)) then
       l = .TRUE.
       c = c + 1
       ierr = 0
    endif
        
  end subroutine checklarg

end subroutine parse_args

  subroutine fieldname(ptv, centerid, procid, param, subc, field)
!
! This routine contains the tables for the various grib parameters
! Each individual parameter contains the info to identify itself.
! We don't have the complete tables here, just enough for normal wrf use.
! wgrib has more complete NCEP tables. (look in WRFV3/external/io_grib1/WGRIB)
!
!  ptv - parameter table version
!  centerid - id number ofthe originating center
!  procid - process id
!  param - parameter number
!  subc - sub center
!  field - 7-character field name (returned)

    integer, intent(in) :: ptv, centerid, procid, param, subc
    logical :: first=.true.
    character (len=7), intent(out) :: field
    character(len=7) :: table2(255), table129(255), afwa(255), ecmwf(255), &
      table130(255), table131(255), table132(255)
    common /paramids/ table2, table129, table130, table131, table132, afwa, ecmwf

     field = '     '
     if (first) call init_tables
     first = .false.
     if ( centerid .eq. 7 .and. ptv .eq. 2 ) then
       field = table2(param)
     else if ( centerid .eq. 7 .and. ptv .eq. 129 ) then
       field = table129(param)
     else if ( centerid .eq. 7 .and. ptv .eq. 130 ) then
       field = table130(param)
     else if ( centerid .eq. 7 .and. ptv .eq. 131 ) then
       field = table131(param)
     else if ( centerid .eq. 7 .and. ptv .eq. 132 ) then
       field = table132(param)
     else if ( centerid .eq. 57 ) then
       field = afwa(param)
     else if ( centerid .eq. 58 .and. ptv .eq. 2 ) then    ! navysst
       field = table2(param)
     else if ( centerid .eq. 59 .and. ptv .eq. 2 ) then    ! GSD
       field = table2(param)
     else if ( centerid .eq. 59 .and. ptv .eq. 129 ) then
       field = table129(param)
     else if ( centerid .eq. 98 .and. ptv .eq. 128 ) then  ! ECMWF
       field = ecmwf(param)
     else if ( ptv .eq. 2 ) then  ! previous default behavior - assume table 2 is ncep table 2
       field = table2(param)
     else
       field = '     '
     endif
     return
end subroutine fieldname 
!
     subroutine init_tables
    character(len=7) :: table2(255), table129(255), afwa(255), ecmwf(255), &
      table130(255), table131(255),  table132(255)
    common /paramids/ table2, table129, table130, table131, table132, afwa, ecmwf

       ! afwa
       do i = 1, 127
	 afwa(i) = table2(i)
       enddo
       do i = 128, 254
         afwa(i) = '     '
       enddo
       afwa(144) = 'DNWLR'
       afwa(145) = 'INSWR'
       afwa(155) = 'GDHFX'
       afwa(157) = 'XTRAJ'
       afwa(158) = 'YTRAJ'
       afwa(159) = 'PTRAJ'
       afwa(160) = 'TERID'
       afwa(161) = 'MDLTN'
       afwa(174) = 'SNOWD'
       afwa(175) = 'SNOAG'
       afwa(176) = 'SNOCL'
       afwa(177) = 'VSBLY'
       afwa(178) = 'CURWX'
       afwa(179) = 'CLAMT'
       afwa(180) = 'CLBAS'
       afwa(181) = 'CLTOP'
       afwa(182) = 'CLTYP'
       afwa(183) = 'UTIME'
       afwa(184) = 'SRCDT'
       afwa(196) = 'EPCDF'
       afwa(197) = 'EPALL'
       afwa(198) = 'EPGEO'
       afwa(199) = 'EPVAL'
       afwa(200) = 'SOILR'
       afwa(201) = 'SOILW'
       afwa(205) = 'TYPSL'
       afwa(206) = 'VLASH'
       afwa(207) = 'CANWT'
       afwa(208) = 'PEVAP'
       afwa(209) = 'WNDRN'
       afwa(210) = 'RHTMN'
       afwa(211) = 'SOILL'
       afwa(212) = 'VEGTP'
       afwa(213) = 'GREEN'
       afwa(234) = 'BGRUN'
       afwa(235) = 'SSRUN'

     ! ECMWF
! from http://www.ecmwf.int/services/archive/d/parameters/order=grib_parameter/table=128/
       do i = 1, 254
         ecmwf(i) = '     '
       enddo
         ecmwf(1) = 'STRF '
         ecmwf(2) = 'VPOT '
         ecmwf(3) = 'PT   '
         ecmwf(4) = 'EQPT '
         ecmwf(5) = 'SEPT '
         ecmwf(8) = 'SRO  '
         ecmwf(9) = 'SSRO '
         ecmwf(10) = 'WS   '
         ecmwf(26) = 'CL   '
         ecmwf(27) = 'CVL  '
         ecmwf(28) = 'CVH  '
         ecmwf(29) = 'TVL  '
         ecmwf(30) = 'TVH  '
         ecmwf(31) = 'CI   '
         ecmwf(32) = 'ASN  '
         ecmwf(33) = 'RSN  '
         ecmwf(34) = 'SSTK '
         ecmwf(35) = 'ISTL1'
         ecmwf(36) = 'ISTL2'
         ecmwf(37) = 'ISTL3'
         ecmwf(38) = 'ISTL4'
         ecmwf(39) = 'SWVL1'
         ecmwf(40) = 'SWVL2'
         ecmwf(41) = 'SWVL3'
         ecmwf(42) = 'SWVL4'
         ecmwf(43) = 'SLT  '
         ecmwf(44) = 'ES   '
         ecmwf(45) = 'SMLT '
         ecmwf(60) = 'PV   '
         ecmwf(74) = 'SDFOR'
         ecmwf(75) = 'CRWC '
         ecmwf(76) = 'CSWC '
         ecmwf(77) = 'ETADT'
         ecmwf(78) = 'TCLW '
         ecmwf(79) = 'TCIW '
         ecmwf(121) = 'MX2T6'
         ecmwf(122) = 'MN2T6'
         ecmwf(123) = '10FG6'
         ecmwf(124) = 'EMIS '
         ecmwf(127) = 'AT   '
         ecmwf(128) = 'BV   '
         ecmwf(129) = 'Z    '
         ecmwf(130) = 'T    '
         ecmwf(131) = 'U    '
         ecmwf(132) = 'V    '
         ecmwf(133) = 'Q    '
         ecmwf(134) = 'SP   '
         ecmwf(135) = 'W    '
         ecmwf(136) = 'TCW  '
         ecmwf(137) = 'TCWV '
         ecmwf(138) = 'VO   '
         ecmwf(139) = 'STL1 '
         ecmwf(140) = 'SWL1 '
         ecmwf(141) = 'SD   '
         ecmwf(142) = 'LSP  '
         ecmwf(143) = 'CP   '
         ecmwf(144) = 'SF   '
         ecmwf(145) = 'BLD  '
         ecmwf(146) = 'SSHF '
         ecmwf(147) = 'SLHF '
         ecmwf(148) = 'CHNK '
         ecmwf(149) = 'SNR  '
         ecmwf(150) = 'TNR  '
         ecmwf(151) = 'MSL  '
         ecmwf(152) = 'LNSP '
         ecmwf(153) = 'SWHR '
         ecmwf(154) = 'LWHR '
         ecmwf(155) = 'D    '
         ecmwf(156) = 'GH   '
         ecmwf(157) = 'R    '
         ecmwf(159) = 'BLH  '
         ecmwf(160) = 'SDOR '
         ecmwf(161) = 'ISOR '
         ecmwf(162) = 'ANOR '
         ecmwf(163) = 'SLOR '
         ecmwf(164) = 'TCC  '
         ecmwf(165) = '10U  '
         ecmwf(166) = '10V  '
         ecmwf(167) = '2T   '
         ecmwf(168) = '2D   '
         ecmwf(169) = 'SSRD '
         ecmwf(170) = 'STL2 '
         ecmwf(171) = 'SWL2 '
         ecmwf(172) = 'LSM  '
         ecmwf(173) = 'SR   '
         ecmwf(174) = 'AL   '
         ecmwf(175) = 'STRD '
         ecmwf(176) = 'SSR  '
         ecmwf(177) = 'STR  '
         ecmwf(178) = 'TSR  '
         ecmwf(179) = 'TTR  '
         ecmwf(180) = 'EWSS '
         ecmwf(181) = 'NSSS '
         ecmwf(182) = 'E    '
         ecmwf(183) = 'STL3 '
         ecmwf(184) = 'SWL3 '
         ecmwf(185) = 'CCC  '
         ecmwf(186) = 'LCC  '
         ecmwf(187) = 'MCC  '
         ecmwf(188) = 'HCC  '
         ecmwf(189) = 'SUND '
         ecmwf(194) = 'BTMP '
         ecmwf(195) = 'LGWS '
         ecmwf(196) = 'MGWS '
         ecmwf(197) = 'GWD  '
         ecmwf(198) = 'SRC  '
         ecmwf(199) = 'VEG  '
         ecmwf(200) = 'VSO  '
         ecmwf(201) = 'MX2T '
         ecmwf(202) = 'MN2T '
         ecmwf(203) = 'O3   '
         ecmwf(204) = 'PAW  '
         ecmwf(205) = 'RO   '
         ecmwf(206) = 'TCO3 '
         ecmwf(207) = '10SI '
         ecmwf(208) = 'TSRC '
         ecmwf(209) = 'TTRC '
         ecmwf(210) = 'SSRC '
         ecmwf(211) = 'STRC '
         ecmwf(212) = 'TISR '
         ecmwf(213) = 'VIMD '
         ecmwf(214) = 'DHR  '
         ecmwf(227) = 'CRNH '
         ecmwf(229) = 'IEWS '
         ecmwf(230) = 'INSS '
         ecmwf(231) = 'ISHF '
         ecmwf(232) = 'IE   '
         ecmwf(233) = 'ASQ  '
         ecmwf(234) = 'LSRH '
         ecmwf(235) = 'SKT  '
         ecmwf(236) = 'STL4 '
         ecmwf(237) = 'SWL4 '
         ecmwf(238) = 'TSN  '
         ecmwf(239) = 'CSF  '
         ecmwf(240) = 'LSF  '
         ecmwf(246) = 'CLWC '
         ecmwf(247) = 'CIWC '
         ecmwf(248) = 'CC   '
end subroutine init_tables

    block data  ptables
    character(len=7) :: table2(255), table129(255), afwa(255), ecmwf(255), &
      table130(255), table131(255), table132(255)
    common /paramids/ table2, table129, table130, table131, table132, afwa, ecmwf

  data table2 /'PRES','PRMSL','PTEND','PVORT','ICAHT','GP','HGT','DIST',&
       'HSTDV','TOZNE','TMP','VTMP','POT','EPOT','T MAX','T MIN','DPT',&
       'DEPR','LAPR','VIS','RDSP1','RDSP2','RDSP3','PLI','TMP A','PRESA',&
       'GP A','WVSP1','WVSP2','WVSP3','WDIR','WIND','U GRD','V GRD','STRM',&
       'V POT','MNTSF','SGCVV','V VEL','DZDT','ABS V','ABD D','REL V','REL D',&
       'VUCSH','VVCSG','DIR C','SP C','UOGRD','VOGRD','SPF H','R H','MIXR',&
       'P WAT','VAPP','SAT D','EVP','C ICE','PRATE','TSTM','A PCP','NCPCP',&
       'ACPCP','SRWEQ','WEASD','SNO D','MIXHT','TTHDP','MTHD','MTH A','T CDC',&
       'CDCON','L CDC','M CDC','H CDC','C WAT','BLI','SNO C','SNO L','WTMP',&
       'LAND','DSL M','SFC R','ALBDO','TSOIL','SOILM','VEG','SALTY','DEN',&
       'WATR','ICE C','ICETK','DICED','SICED','U ICE','V ICE','ICE G','ICE D',&
       'SNO M','HTSGW','WVDIR','WVHGT','WVPER','SWDIR','SWELL','SWPER','DIRPW',&
       'PERPW','DIRSW','PERSW','NSWRS','NLWRS','NSWRT','NLWRT','LWAVR','SWAVR',&
       'GRAD','BRTMP','LWRAD','SWRAD','LHTFL','SHTFL','BLYDP','U FLX','V FLX',&
       'WMIXE','IMG D',&
! 128-254 for use by originating center. NWS/NCEP Table 2 is coded here.
       'MSLSA','MSLMA','MSLET','LFT X','4LFTX','K X','S X','MCONV','VW SH',&
       'TSLSA','BVF 2','PV MW','CRAIN','CFRZR','CICEP','CSNOW','SOILW',&
       'PEVPR','CWORK','U-GWD','V-GWD','PV','COVMZ','COVTZ','COVTM','CLWMR',&
       'O3MR','GFLUX','CIN','CAPE','TKE','CONDP','CSUSF','CSDSF','CSULF',&
       'CSDLF','CFNSF','CFNLF','VBDSF','VDDSF','NBDSF','NDDSF','RWMR',&
       'SNMR','M FLX','LMH','LMV','MLYNO','NLAT','ELON','ICMR','GRMR','GUST',&
       'LPS X','LPS Y','HGT X','HGT Y','TPFI','TIPD','LTNG','RDRIP','VPTMP','HLCY',&
       'PROB','PROBN','POP','CPOFP','CPOZP','USTM','VSTM','NCIP','EVBS','EVCW',&
       'ICWAT','CWDI','VAFTD','DSWRF','DLWRF','UVI','MSTAV','SFEXC','MIXLY','TRANS',&
       'USWRF','ULWRF','CDLYR','CPRAT','TTDIA','TTRAD','TTPHY','PREIX','TSD1D',&
       'NLGSP','HPBL','5WAVH','CNWAT','SOTYP','VGTYP','BMIXL','AMIXL','PEVAP',&
       'SNOHF','5WAVA','MFLUX','DTRF','UTRF','BGRUN','SSRUN','SIPD','O3TOT',&
       'SNOWC','SNOT','COVTW','LRGHR','CNVHR','CNVMR','SHAHR','SHAMR','VDFHR',&
       'VDFUA','VDFVA','VDFMR','SWHR','LWHR','CD','FRICV','RI','  '/
  data table129 /'PRES','PRMSL','PTEND','PVORT','ICAHT','GP','HGT','DIST',&
       'HSTDV','TOZNE','TMP','VTMP','POT','EPOT','T MAX','T MIN','DPT',&
       'DEPR','LAPR','VIS','RDSP1','RDSP2','RDSP3','PLI','TMP A','PRESA',&
       'GP A','WVSP1','WVSP2','WVSP3','WDIR','WIND','U GRD','V GRD','STRM',&
       'V POT','MNTSF','SGCVV','V VEL','DZDT','ABS V','ABD D','REL V','REL D',&
       'VUCSH','VVCSG','DIR C','SP C','UOGRD','VOGRD','SPF H','R H','MIXR',&
       'P WAT','VAPP','SAT D','EVP','C ICE','PRATE','TSTM','A PCP','NCPCP',&
       'ACPCP','SRWEQ','WEASD','SNO D','MIXHT','TTHDP','MTHD','MTH A','T CDC',&
       'CDCON','L CDC','M CDC','H CDC','C WAT','BLI','SNO C','SNO L','WTMP',&
       'LAND','DSL M','SFC R','ALBDO','TSOIL','SOILM','VEG','SALTY','DEN',&
       'WATR','ICE C','ICETK','DICED','SICED','U ICE','V ICE','ICE G','ICE D',&
       'SNO M','HTSGW','WVDIR','WVHGT','WVPER','SWDIR','SWELL','SWPER','DIRPW',&
       'PERPW','DIRSW','PERSW','NSWRS','NLWRS','NSWRT','NLWRT','LWAVR','SWAVR',&
       'GRAD','BRTMP','LWRAD','SWRAD','LHTFL','SHTFL','BLYDP','U FLX','V FLX',&
       'WMIXE','IMG D',&
! All NCEP tables use the Table 2 values for the first 127 entries
! 128-254 for Table129
       'PAOT','PAOP','CWR','FRAIN','FICE','FRIME','CUEFI','TCOND','TCOLW',&
       'TCOLI','TCOLR','TCOLS','TCOLC','PLPL','HLPL','CEMS','COPD',&
       'PSIZ','TCWAT','TCICE','WDIF','WSTP','PTAN','PTNN','PTBN','PPAN',&
       'PPNN','PPBN','PMTC','PMTF','AETMP','AEDPT','AESPH','AEUWD','AEVWD',&
       'LPMTF','LIPMF','REFZR','REFZI','REFZC','TCLSW','TCOLM','ELRDI',&
       'TSEC','TSECA','NUM','AEPRS','ICSEV','ICPRB','LAVNI','HAVNI','FLGHT','OZCON',&
       'OZCAT','VEDH','SIGV','EWGT','CICEL','CIVIS','CIFLT','LAVV','LOVV','USCT',&
       'VSCT','LAUV','LOUV','TCHP','DBSS','ODHA','OHC','SSHG','SLTFL','DUVB',&
       'CDUVB','THFLX','UVAR','VVAR','UVVCC','MCLS','LAPP','LOPP','     ','REFO',&
       'REFD','REFC','SBT22','SBT23','SBT24','SBT25','MINRH','MAXRH','CEIL',&
       'PBLRE','SBC23','SBC24','RPRAT','SPRAT','FPRAT','IPRAT','UPHL','SURGE',&
       'ETSRG','RHPW','OZMAX1','OZMAX8','PDMAX1','PDMAX24','MAXREF','MXUPHL','MAXUVV',&
       'MAXDVV','MAXVIG','RETOP','VRATE','TCSRG20','TCSRG30','TCSRG40','TCSRG50','TCSRG60',&
       'TCSRG70','TCSRG80','TCSRG90','HINDX','DIFTEN','PSPCP','MAXUW','MAXVW','255'/
  data table130 /'PRES','PRMSL','PTEND','PVORT','ICAHT','GP','HGT','DIST',&
       'HSTDV','TOZNE','TMP','VTMP','POT','EPOT','T MAX','T MIN','DPT',&
       'DEPR','LAPR','VIS','RDSP1','RDSP2','RDSP3','PLI','TMP A','PRESA',&
       'GP A','WVSP1','WVSP2','WVSP3','WDIR','WIND','U GRD','V GRD','STRM',&
       'V POT','MNTSF','SGCVV','V VEL','DZDT','ABS V','ABD D','REL V','REL D',&
       'VUCSH','VVCSG','DIR C','SP C','UOGRD','VOGRD','SPF H','R H','MIXR',&
       'P WAT','VAPP','SAT D','EVP','C ICE','PRATE','TSTM','A PCP','NCPCP',&
       'ACPCP','SRWEQ','WEASD','SNO D','MIXHT','TTHDP','MTHD','MTH A','T CDC',&
       'CDCON','L CDC','M CDC','H CDC','C WAT','BLI','SNO C','SNO L','WTMP',&
       'LAND','DSL M','SFC R','ALBDO','TSOIL','SOILM','VEG','SALTY','DEN',&
       'WATR','ICE C','ICETK','DICED','SICED','U ICE','V ICE','ICE G','ICE D',&
       'SNO M','HTSGW','WVDIR','WVHGT','WVPER','SWDIR','SWELL','SWPER','DIRPW',&
       'PERPW','DIRSW','PERSW','NSWRS','NLWRS','NSWRT','NLWRT','LWAVR','SWAVR',&
       'GRAD','BRTMP','LWRAD','SWRAD','LHTFL','SHTFL','BLYDP','U FLX','V FLX',&
       'WMIXE','IMG D',&
! Table 130    LSM 
! 128-143 not yet assigned
       '     ','     ','     ','     ','     ','   ','   ','     ','     ',&
       '     ','     ','     ','     ','     ','     ','     ','SOILW',&
       'PEVPR','VEGT ','BARET','AVSFT','RADT','SSTOR','LSOIL','EWATR','     ',&
       'LSPA ','GFLUX','CIN','CAPE','TKE','MXSAL','SOILL','ASNOW','ARAIN',&
       'GWREC','QREC ','SNOWT','VBDSF','VDDSF','NBDSF','NDDSF','SNFALB',&
       'RLYRS','M FLX','   ','   ','     ','NLAT','ELON','FLDCAP','ACOND','SNOAG',&
       'CCOND','LAI','SFCRH','SALBD','    ','    ','NDVI','DRIP','VBSALB','VWSALB',&
       'NBSALB','NWSALB','FRZR','FROZR','TSNOW','MTERH','   ','SBSNO','EVBS','EVCW',&
       'VTCIN','VTCAPE','RSMIN','DSWRF','DLWRF','   ','MSTAV','SFEXC','     ','TRANS',&
       'USWRF','ULWRF','     ','     ','     ','     ','     ','     ','WILT',&
       'FLDCP','HPBL','SLTYP','CNWAT','SOTYP','VGTYP','BMIXL','AMIXL','PEVAP',&
       'SNOHF','SMREF','SMDRY','    ','    ','BGRUN','SSRUN','    ','     ',&
       'SNOWC','SNOT','POROS','SBT112','SBT113','SBT114','SBT115','     ','RCS',&
       'RCT','RCQ','RCSOL','    ','    ','CD','FRICV','RI','  '/
  data table131 /'PRES','PRMSL','PTEND','PVORT','ICAHT','GP','HGT','DIST',&
       'HSTDV','TOZNE','TMP','VTMP','POT','EPOT','T MAX','T MIN','DPT',&
       'DEPR','LAPR','VIS','RDSP1','RDSP2','RDSP3','PLI','TMP A','PRESA',&
       'GP A','WVSP1','WVSP2','WVSP3','WDIR','WIND','U GRD','V GRD','STRM',&
       'V POT','MNTSF','SGCVV','V VEL','DZDT','ABS V','ABD D','REL V','REL D',&
       'VUCSH','VVCSG','DIR C','SP C','UOGRD','VOGRD','SPF H','R H','MIXR',&
       'P WAT','VAPP','SAT D','EVP','C ICE','PRATE','TSTM','A PCP','NCPCP',&
       'ACPCP','SRWEQ','WEASD','SNO D','MIXHT','TTHDP','MTHD','MTH A','T CDC',&
       'CDCON','L CDC','M CDC','H CDC','C WAT','BLI','SNO C','SNO L','WTMP',&
       'LAND','DSL M','SFC R','ALBDO','TSOIL','SOILM','VEG','SALTY','DEN',&
       'WATR','ICE C','ICETK','DICED','SICED','U ICE','V ICE','ICE G','ICE D',&
       'SNO M','HTSGW','WVDIR','WVHGT','WVPER','SWDIR','SWELL','SWPER','DIRPW',&
       'PERPW','DIRSW','PERSW','NSWRS','NLWRS','NSWRT','NLWRT','LWAVR','SWAVR',&
       'GRAD','BRTMP','LWRAD','SWRAD','LHTFL','SHTFL','BLYDP','U FLX','V FLX',&
       'WMIXE','IMG D',&
! Table 131    NARR
       'MSLSA','     ','MSLET','LFT X','4LFTX','     ','PRESN','MCONV','VW SH',&
       '     ','     ','PVMWW','CRAIN','CFRZR','CICEP','CSNOW','SOILW',&
       'PEVPR','VEGT ','BARET','AVSFT','RADT','SSTOR','LSOIL','EWATR','CLWMR',&
       '    ','GFLUX','CIN','CAPE','TKE','MXSAL','SOILL','ASNOW','ARAIN',&
       'GWREC','QREC ','SNOWT','VBDSF','VDDSF','NBDSF','NDDSF','SNFAL',&
       'RLYRS','M FLX','LMH','LMV','MLYNO','NLAT','ELON','ICMR','ACOND','SNOAG',&
       'CCOND','LAI  ','SFCRH','SALBD','    ','    ','NDVI','DRIP','LANDN','HLCY',&
       'NLATN','ELONN','   ','CPOFP','     ','USTM','VSTM','SBSNO','EVBS','EVCW',&
       '     ','APCPN','RSMIN','DSWRF','DLWRF','ACPCP','MSTAV','SFEXC','     ','TRANS',&
       'USWRF','ULWRF','CDLYR','CPRAT','     ','TTRAD','     ','HGTN ','WILT ',&
       'FLDCP','HPBL','SLTYP','CNWAT','SOTYP','VGTYP','BMIXL','AMIXL','PEVAP',&
       'SNOHF','SMREF','SMDRY','WVINC','WCINC','BGRUN','SSRUN','    ','WVCON',&
       'SNOWC','SNOT','POROS','WCCON','WVUFL','WVVFL','WCUFL','WCVFL','RCS  ',&
       'RCT','RCQ','RCSOL','SWHR','LWHR','CD','FRICV','RI','  '/
  data table132 /'PRES','PRMSL','PTEND','PVORT','ICAHT','GP','HGT','DIST',&
       'HSTDV','TOZNE','TMP','VTMP','POT','EPOT','T MAX','T MIN','DPT',&
       'DEPR','LAPR','VIS','RDSP1','RDSP2','RDSP3','PLI','TMP A','PRESA',&
       'GP A','WVSP1','WVSP2','WVSP3','WDIR','WIND','U GRD','V GRD','STRM',&
       'V POT','MNTSF','SGCVV','V VEL','DZDT','ABS V','ABD D','REL V','REL D',&
       'VUCSH','VVCSG','DIR C','SP C','UOGRD','VOGRD','SPF H','R H','MIXR',&
       'P WAT','VAPP','SAT D','EVP','C ICE','PRATE','TSTM','A PCP','NCPCP',&
       'ACPCP','SRWEQ','WEASD','SNO D','MIXHT','TTHDP','MTHD','MTH A','T CDC',&
       'CDCON','L CDC','M CDC','H CDC','C WAT','BLI','SNO C','SNO L','WTMP',&
       'LAND','DSL M','SFC R','ALBDO','TSOIL','SOILM','VEG','SALTY','DEN',&
       'WATR','ICE C','ICETK','DICED','SICED','U ICE','V ICE','ICE G','ICE D',&
       'SNO M','HTSGW','WVDIR','WVHGT','WVPER','SWDIR','SWELL','SWPER','DIRPW',&
       'PERPW','DIRSW','PERSW','NSWRS','NLWRS','NSWRT','NLWRT','LWAVR','SWAVR',&
       'GRAD','BRTMP','LWRAD','SWRAD','LHTFL','SHTFL','BLYDP','U FLX','V FLX',&
       'WMIXE','IMG D',&
! Table 132    NCEP2  originally the same as table 2, but they diverged.
       'MSLSA','MSLMA','MSLET','LFT X','4LFTX','K X','S X','MCONV','VW SH',&
       'TSLSA','BVF 2','PV MW','CRAIN','CFRZR','CICEP','CSNOW','SOILW',&
       'PEVPR','CWORK','U-GWD','V-GWD','PV','     ','     ','     ','MFXDV',&
       '    ','GFLUX','CIN','CAPE','TKE','CONDP','CSUSF','CSDSF','CSULF',&
       'CSDLF','CFNSF','CFNLF','VBDSF','VDDSF','NBDSF','NDDSF','USTR',&
       'VSTR','M FLX','LMH','LMV','SGLYR','NLAT','ELON','UMAS','VMAS','XPRATE',&
       'LPS X','LPS Y','HGT X','HGT Y','STDZ','STDU','STDV','STDQ','STDTP','CBUW',&
       'CBVW','CBUQN','CBVQ','CBTW ','CBQW ','CBMZW','CBTZW','CBTMW','STDRH','SDTZ',&
       'ICWAT','SDTU','SDTV','DSWRF','DLWRF','SDTQ','MSTAV','SFEXC','MIXLY','SDTT',&
       'USWRF','ULWRF','CDLYR','CPRAT','TTDIA','TTRAD','TTPHY','PREIX','TSD1D',&
       'NLGSP','SDTRH','5WAVH','CNWAT','PLTRS','RHCLD','BMIXL','AMIXL','PEVAP',&
       'SNOHF','SNOEV','MFLUX','DTRF','UTRF','BGRUN','SSRUN','    ','O3TOT',&
       'SNOWC','SNOT','GLCR ','LRGHR','CNVHR','CNVMR','SHAHR','SHAMR','VDFHR',&
       'VDFUA','VDFVA','VDFMR','SWHR','LWHR','CD','FRICV','RI','  '/

    end
