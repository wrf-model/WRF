      module params_ecmwf
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! MODULE:    params_ecmwf
!   PRGMMR: Gordon         ORG: W/NP11    DATE: 2006-09-07
!
! ABSTRACT: This Fortran Module contains info on all the available 
!           ECMWF GRIB Parameters.
!
! PROGRAM HISTORY LOG:
! 2006-09-07  Gordon   -  Modified from Steve Gilbert's params.f for NCEP GRIB data
! 2007-04-20  Vuong    -  Add more parameters
! 2007-10-11  Vuong    -  Add more parameters
! 2011-11-16  Vuong    -  Add parameters MAX and MIN temperature
! 2013-07-24  Vuong    -  Removed space in abbreviation
!
! USAGE:    use params
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      integer,parameter :: MAXPARAM=179

      type gribparam
          integer :: g1tblver
          integer :: grib1val
          integer :: grib2dsc
          integer :: grib2cat
          integer :: grib2num
          character(len=8) :: abbrev
      end type gribparam

      type(gribparam),dimension(MAXPARAM) :: paramlist

      data paramlist(1) /gribparam(128,1,255,255,255,'STRF')/
      data paramlist(2) /gribparam(128,002,255,255,255,'VPOT')/
      data paramlist(3) /gribparam(128,003,255,255,255,'THTA')/
      data paramlist(4) /gribparam(128,004,255,255,255,'THTE')/
      data paramlist(5) /gribparam(128,005,255,255,255,'THTS')/
      data paramlist(6) /gribparam(128,011,255,255,255,'UDVW')/
      data paramlist(7) /gribparam(128,012,255,255,255,'VDVW')/
      data paramlist(8) /gribparam(128,013,255,255,255,'URTW')/
      data paramlist(9) /gribparam(128,014,255,255,255,'VRTW')/
      data paramlist(10) /gribparam(128,021,255,255,255,'UCTP')/
      data paramlist(11) /gribparam(128,022,255,255,255,'UCLN')/
      data paramlist(12) /gribparam(128,023,255,255,255,'UCDV')/
      data paramlist(13) /gribparam(128,026,255,255,255,'CLAK')/
      data paramlist(14) /gribparam(128,027,255,255,255,'CVEGL')/
      data paramlist(15) /gribparam(128,028,255,255,255,'CVEGH')/
      data paramlist(16) /gribparam(128,029,255,255,255,'TVEGL')/
      data paramlist(17) /gribparam(128,030,255,255,255,'TVEGH')/
      data paramlist(18) /gribparam(128,031,255,255,255,'CSICE')/
      data paramlist(19) /gribparam(128,032,255,255,255,'ASNOW')/
      data paramlist(20) /gribparam(128,033,255,255,255,'RSNOW')/
      data paramlist(21) /gribparam(128,034,255,255,255,'SSTK')/
      data paramlist(22) /gribparam(128,035,255,255,255,'ISTL1')/
      data paramlist(23) /gribparam(128,036,255,255,255,'ISTL2')/
      data paramlist(24) /gribparam(128,037,255,255,255,'ISTL3')/
      data paramlist(25) /gribparam(128,038,255,255,255,'ISTL4')/
      data paramlist(26) /gribparam(128,039,255,255,255,'SWVL1')/
      data paramlist(27) /gribparam(128,040,255,255,255,'SWVL2')/
      data paramlist(28) /gribparam(128,041,255,255,255,'SWVL3')/
      data paramlist(29) /gribparam(128,042,255,255,255,'SWVL4')/
      data paramlist(30) /gribparam(128,043,255,255,255,'SOILT')/
      data paramlist(31) /gribparam(128,044,255,255,255,'ESNOW')/
      data paramlist(32) /gribparam(128,045,255,255,255,'SMLT')/
      data paramlist(33) /gribparam(128,046,255,255,255,'SDUR')/
      data paramlist(34) /gribparam(128,047,255,255,255,'DSRP')/
      data paramlist(35) /gribparam(128,048,255,255,255,'MAGSS')/
      data paramlist(36) /gribparam(128,049,255,255,255,'GUST')/
      data paramlist(37) /gribparam(128,050,255,255,255,'LSPF')/
      data paramlist(38) /gribparam(128,051,255,255,255,'TMXK24')/
      data paramlist(39) /gribparam(128,052,255,255,255,'TMNK24')/
      data paramlist(40) /gribparam(128,053,255,255,255,'MONT')/
      data paramlist(41) /gribparam(128,054,255,255,255,'PRES')/
      data paramlist(42) /gribparam(128,060,255,255,255,'PVOR')/
      data paramlist(43) /gribparam(128,127,255,255,255,'ATIDE')/
      data paramlist(44) /gribparam(128,128,255,255,255,'BVAL')/
      data paramlist(45) /gribparam(128,129,255,255,255,'HGHT')/
      data paramlist(46) /gribparam(128,130,0,0,0,'TMPK')/
      data paramlist(47) /gribparam(128,131,0,2,2,'UWND')/
      data paramlist(48) /gribparam(128,132,0,2,3,'VWND')/
      data paramlist(49) /gribparam(128,133,255,255,255,'SPCH')/
      data paramlist(50) /gribparam(128,134,255,255,255,'PRES')/
      data paramlist(51) /gribparam(128,135,255,255,255,'OMEG')/
      data paramlist(52) /gribparam(128,136,255,255,255,'TCWTR')/
      data paramlist(53) /gribparam(128,137,255,255,255,'TCWV')/
      data paramlist(54) /gribparam(128,138,255,255,255,'VORT')/
      data paramlist(55) /gribparam(128,139,255,255,255,'STL1')/
      data paramlist(56) /gribparam(128,140,255,255,255,'SWL1')/
      data paramlist(57) /gribparam(128,141,255,255,255,'SNOWD')/
      data paramlist(58) /gribparam(128,142,255,255,255,'S--M')/
      data paramlist(59) /gribparam(128,143,255,255,255,'C--M')/
      data paramlist(60) /gribparam(128,144,255,255,255,'SNOW')/
      data paramlist(61) /gribparam(128,145,255,255,255,'BLDS')/
      data paramlist(62) /gribparam(128,146,255,255,255,'SSHF')/
      data paramlist(63) /gribparam(128,147,255,255,255,'SLHF')/
      data paramlist(64) /gribparam(128,148,255,255,255,'CHNK')/
      data paramlist(65) /gribparam(128,149,255,255,255,'SNRAD')/
      data paramlist(66) /gribparam(128,150,255,255,255,'TNRAD')/
      data paramlist(67) /gribparam(128,151,0,3,1,'PMSL')/
      data paramlist(68) /gribparam(128,152,255,255,255,'LNSP')/
      data paramlist(69) /gribparam(128,153,255,255,255,'SWHR')/
      data paramlist(70) /gribparam(128,154,255,255,255,'LWHR')/
      data paramlist(71) /gribparam(128,155,255,255,255,'DIVG')/
      data paramlist(72) /gribparam(128,156,0,3,5,'HGHT')/
      data paramlist(73) /gribparam(128,157,0,1,1,'RELH')/
      data paramlist(74) /gribparam(128,158,255,255,255,'TSPRES')/
      data paramlist(75) /gribparam(128,159,255,255,255,'BLHGHT')/
      data paramlist(76) /gribparam(128,160,255,255,255,'SDOR')/
      data paramlist(77) /gribparam(128,161,255,255,255,'ISOR')/
      data paramlist(78) /gribparam(128,162,255,255,255,'ANOR')/
      data paramlist(79) /gribparam(128,163,255,255,255,'SLOR')/
      data paramlist(80) /gribparam(128,164,0,6,1,'TCLD')/
      data paramlist(81) /gribparam(128,165,0,2,2,'UWND')/
      data paramlist(82) /gribparam(128,166,0,2,3,'VWND')/
      data paramlist(83) /gribparam(128,167,0,0,0,'TMPK')/
      data paramlist(84) /gribparam(128,168,0,0,6,'DWPK')/
      data paramlist(85) /gribparam(128,169,255,255,255,'SSRD')/
      data paramlist(86) /gribparam(128,170,255,255,255,'STL2')/
      data paramlist(87) /gribparam(128,171,255,255,255,'SWL2')/
      data paramlist(88) /gribparam(128,172,255,255,255,'LAND')/
      data paramlist(89) /gribparam(128,173,255,255,255,'SROUGH')/
      data paramlist(90) /gribparam(128,174,255,255,255,'ALBD')/
      data paramlist(91) /gribparam(128,175,255,255,255,'STRD')/
      data paramlist(92) /gribparam(128,176,255,255,255,'SSRAD')/
      data paramlist(93) /gribparam(128,177,255,255,255,'STRAD')/
      data paramlist(94) /gribparam(128,178,255,255,255,'TSRAD')/
      data paramlist(95) /gribparam(128,179,255,255,255,'TTRAD')/
      data paramlist(96) /gribparam(128,180,255,255,255,'EWSS')/
      data paramlist(97) /gribparam(128,181,255,255,255,'NSSS')/
      data paramlist(98) /gribparam(128,182,255,255,255,'EVAP')/
      data paramlist(99) /gribparam(128,183,255,255,255,'STL3')/
      data paramlist(100) /gribparam(128,184,255,255,255,'SWL3')/
      data paramlist(101) /gribparam(128,185,255,255,255,'CCLD')/
      data paramlist(102) /gribparam(128,186,255,255,255,'LCLD')/
      data paramlist(103) /gribparam(128,187,255,255,255,'MCLD')/
      data paramlist(104) /gribparam(128,188,255,255,255,'HCLD')/
      data paramlist(105) /gribparam(128,189,255,255,255,'SUND')/
      data paramlist(106) /gribparam(128,190,255,255,255,'EWOV')/
      data paramlist(107) /gribparam(128,191,255,255,255,'NSOV')/
      data paramlist(108) /gribparam(128,192,255,255,255,'NWOV')/
      data paramlist(109) /gribparam(128,193,255,255,255,'NEOV')/
      data paramlist(110) /gribparam(128,194,255,255,255,'BTMP')/
      data paramlist(111) /gribparam(128,195,255,255,255,'LGWS')/
      data paramlist(112) /gribparam(128,196,255,255,255,'MGWS')/
      data paramlist(113) /gribparam(128,197,255,255,255,'GWDS')/
      data paramlist(114) /gribparam(128,198,255,255,255,'SKRC')/
      data paramlist(115) /gribparam(128,199,255,255,255,'VEGE')/
      data paramlist(116) /gribparam(128,200,255,255,255,'VSGO')/
      data paramlist(117) /gribparam(128,201,0,0,4,'TMXK')/
      data paramlist(118) /gribparam(128,202,0,0,5,'TMNK')/
      data paramlist(119) /gribparam(128,203,255,255,255,'OZMR')/
      data paramlist(120) /gribparam(128,204,255,255,255,'PRAW')/
      data paramlist(121) /gribparam(128,205,255,255,255,'RUNOFF')/
      data paramlist(122) /gribparam(128,206,255,255,255,'TCOZ')/
      data paramlist(123) /gribparam(128,207,255,255,255,'SPED')/
      data paramlist(124) /gribparam(128,208,255,255,255,'TSRC')/
      data paramlist(125) /gribparam(128,209,255,255,255,'TTRC')/
      data paramlist(126) /gribparam(128,210,255,255,255,'SSRC')/
      data paramlist(127) /gribparam(128,211,255,255,255,'STRC')/
      data paramlist(128) /gribparam(128,212,255,255,255,'SINSOL')/
      data paramlist(129) /gribparam(128,214,255,255,255,'DHRAD')/
      data paramlist(130) /gribparam(128,215,255,255,255,'DHVD')/
      data paramlist(131) /gribparam(128,216,255,255,255,'DHCC')/
      data paramlist(132) /gribparam(128,217,255,255,255,'DHLC')/
      data paramlist(133) /gribparam(128,218,255,255,255,'VDZW')/
      data paramlist(134) /gribparam(128,219,255,255,255,'VDMW')/
      data paramlist(135) /gribparam(128,220,255,255,255,'EWGD')/
      data paramlist(136) /gribparam(128,221,255,255,255,'NSGD')/
      data paramlist(137) /gribparam(128,222,255,255,255,'CTZW')/
      data paramlist(138) /gribparam(128,223,255,255,255,'CTMW')/
      data paramlist(139) /gribparam(128,224,255,255,255,'VDHUM')/
      data paramlist(140) /gribparam(128,225,255,255,255,'HTCC')/
      data paramlist(141) /gribparam(128,226,255,255,255,'HTLC')/
      data paramlist(142) /gribparam(128,227,255,255,255,'CRNH')/
      data paramlist(143) /gribparam(128,228,0,1,8,'APCP')/
      data paramlist(144) /gribparam(128,229,255,255,255,'IEWS')/
      data paramlist(145) /gribparam(128,230,255,255,255,'INSS')/
      data paramlist(146) /gribparam(128,231,255,255,255,'ISHF')/
      data paramlist(147) /gribparam(128,232,255,255,255,'MFLUX')/
      data paramlist(148) /gribparam(128,233,255,255,255,'ASHUM')/
      data paramlist(149) /gribparam(128,234,255,255,255,'LSRH')/
      data paramlist(150) /gribparam(128,235,255,255,255,'SKTMP')/
      data paramlist(151) /gribparam(128,236,255,255,255,'STL4')/
      data paramlist(152) /gribparam(128,237,255,255,255,'SWL4')/
      data paramlist(153) /gribparam(128,238,255,255,255,'TSNOW')/
      data paramlist(154) /gribparam(128,239,255,255,255,'CSNOWF')/
      data paramlist(155) /gribparam(128,240,255,255,255,'LSNOWF')/
      data paramlist(156) /gribparam(128,241,255,255,255,'ACLD')/
      data paramlist(157) /gribparam(128,242,255,255,255,'ALWTND')/
      data paramlist(158) /gribparam(128,243,255,255,255,'FALBD')/
      data paramlist(159) /gribparam(128,244,255,255,255,'FSROUGH')/
      data paramlist(160) /gribparam(128,245,255,255,255,'FLSR')/
      data paramlist(161) /gribparam(128,246,255,255,255,'CLWC')/
      data paramlist(162) /gribparam(128,247,255,255,255,'CIWC')/
      data paramlist(163) /gribparam(128,248,255,255,255,'CLOUD')/
      data paramlist(164) /gribparam(128,249,255,255,255,'AIWTND')/
      data paramlist(165) /gribparam(128,250,255,255,255,'ICEAGE')/
      data paramlist(166) /gribparam(128,251,255,255,255,'ATTE')/
      data paramlist(167) /gribparam(128,252,255,255,255,'ATHE')/
      data paramlist(168) /gribparam(128,253,255,255,255,'ATZE')/
      data paramlist(169) /gribparam(128,254,255,255,255,'ATMW')/
      data paramlist(170) /gribparam(128,255,255,255,255,'MISS')/
! Added 4/20/2007 - For missing GRIB1 to GRIB2 conversions
      data paramlist(171) /gribparam(1,33,0,2,2,'UGRD')/
      data paramlist(172) /gribparam(1,34,0,2,3,'VGRD')/
      data paramlist(173) /gribparam(1,2,0,3,1,'PRMSL')/
      data paramlist(174) /gribparam(1,7,0,3,5,'HGT')/
! Added 10/11/2007- Add more parameters
      data paramlist(175) /gribparam(1,11,0,0,0,'TMP')/
      data paramlist(176) /gribparam(1,52,0,1,1,'RH')/
      data paramlist(177) /gribparam(1,41,0,2,10,'ABSV')/
! Added 11/16/2011- Add more parameters
      data paramlist(178) /gribparam(128,121,0,0,4,'TMXK')/
      data paramlist(179) /gribparam(128,122,0,0,5,'TMNK')/

      contains


         subroutine param_ecmwf_g1_to_g2(g1val,g1ver,g2disc,g2cat,g2num)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    param_ecmwf_g1_to_g2 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2001-06-05
!
! ABSTRACT: This subroutine returns the corresponding GRIB2 Discipline
!   Category and Number for a given GRIB1 parameter value and table version.
!
! PROGRAM HISTORY LOG:
! 2000-05-11  Gilbert
!
! USAGE:    CALL param_ecmwf_g1_to_g2(g1val,g1ver,g2disc,g2cat,g2num)
!   INPUT ARGUMENT LIST:
!     g1val    - GRIB1 parameter number for which discipline is requested
!     g1ver    - GRIB1 parameter table version number
!
!   OUTPUT ARGUMENT LIST:      
!     g2disc   - corresponding GRIB2 Discipline number
!     g2cat    - corresponding GRIB2 Category number
!     g2num    - corresponding GRIB2 Parameter number within Category g2cat
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$
           integer,intent(in) :: g1val,g1ver
           integer,intent(out) :: g2disc,g2cat,g2num

           g2disc=255
           g2cat=255
           g2num=255
! for testing
!           g2num=g1val
! for testing

           do n=1,MAXPARAM
              if ( paramlist(n)%grib1val.eq.g1val .AND.
     &            paramlist(n)%g1tblver.eq.g1ver ) then
                 g2disc=paramlist(n)%grib2dsc
                 g2cat=paramlist(n)%grib2cat
                 g2num=paramlist(n)%grib2num
c                print *,g2disc
c                print *,g2cat
c                print *,g2num
                 return
              endif
           enddo

           print *,'param_ecmwf_g1_to_g2:GRIB1 param ',g1val,
     &          ' not found.',
     &          ' for table version ',g1ver
           return
         end subroutine


         subroutine param_ecmwf_g2_to_g1(g2disc,g2cat,g2num,g1val,g1ver)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    param_ecmwf_g2_to_g1 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2002-01-04
!
! ABSTRACT: This function returns the GRIB 1 parameter number for 
!   a given GRIB2 Discipline, Category and Parameter number.
!
! PROGRAM HISTORY LOG:
! 2001-06-05  Gilbert
!
! USAGE:     call param_ecmwf_g2_to_g1(g2disc,g2cat,g2num,g1val,g1ver)
!   INPUT ARGUMENT LIST:
!     g2disc   - GRIB2 discipline number (See Code Table 0.0)
!     g2cat    - corresponding GRIB2 Category number
!     g2num    - corresponding GRIB2 Parameter number within Category g2cat
!
!   OUTPUT ARGUMENT LIST:      
!     g1val    - GRIB1 parameter number for which discipline is requested
!     g1ver    - GRIB1 parameter table version number
!
! REMARKS: None
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$
           integer,intent(in) :: g2disc,g2cat,g2num
           integer,intent(out) :: g1val,g1ver

           g1val=255
           g1ver=255

! for testing
!           if ( g2disc.eq.255.and.g2cat.eq.255 ) then
!             g1val=g2num
!             g1ver=2
!             return
!           endif
! for testing

           do n=1,MAXPARAM
              if (paramlist(n)%grib2dsc.eq.g2disc.AND.
     &             paramlist(n)%grib2cat.eq.g2cat.AND.
     &             paramlist(n)%grib2num.eq.g2num) then
                 g1val=paramlist(n)%grib1val
                 g1ver=paramlist(n)%g1tblver
                 return
              endif
           enddo

           print *,'param_ecmwf_g2_to_g1:GRIB2 param ',g2disc,g2cat,
     &              g2num,' not found.'
           return
         end subroutine


      end module

