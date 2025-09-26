      module params
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! MODULE:    params
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2001-06-05
!
! ABSTRACT: This Fortran Module contains info on all the available 
!           GRIB Parameters.
!
! PROGRAM HISTORY LOG:
! 2000-05-11  Gilbert
! 2003-08-07  Gilbert  -  Added more parameters
! 2003-09-26  Gilbert  -  Added more parameters
! 2005-11-17  Gordon   -  Added more parameters for the Wave & Smoke models 
! 2007-03-28  Vuong    -  Added more parameters
! 2007-10-10  Vuong    -  Added more parameters
! 2008-03-12  Vuong    -  Added more parameters
! 2008-06-30  Vuong    -  Reformat entry paramlist from 1 to 173
!                         Added more parameters and entire table 131 
! 2008-11-21  Vuong    -  Added more parameters
! 2009-06-02  Vuong    -  Added more parameters
! 2009-12-14  Vuong    -  Correction VEGT(4.2-0-210)
! 2010-07-27  Vuong    -  Added more parameters
! 2010-12-06  Vuong    -  Added more parameters
! 2011-05-24  Vuong    -  Added more parameters
! 2011-09-12  Vuong    -  Added more parameters
! 2012-09-12  Vuong    -  Added more parameters and change HINDEX to
!                         parameter from 3 to 2 and RHPW from Dis 0 cat 19
!                         to 1
! 2013-07-24  Vuong    -  Added more parameters and Removed
!                         spaces in abbreviation
!
! 2016-03-30  Vuong    -  Added parameter Heat Exchange Coefficient (CH)
!
! USAGE:    use params
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      integer,parameter :: MAXPARAM=801

      type gribparam
          integer :: g1tblver
          integer :: grib1val
          integer :: grib2dsc
          integer :: grib2cat
          integer :: grib2num
          character(len=8) :: abbrev
      end type gribparam

      type(gribparam),dimension(MAXPARAM) :: paramlist

      data paramlist(1) /gribparam(2,1,0,3,0,'PRES')/
      data paramlist(2) /gribparam(2,2,0,3,1,'PRMSL')/
      data paramlist(3) /gribparam(2,3,0,3,2,'PTEND')/
      data paramlist(4) /gribparam(2,4,0,2,14,'PVORT')/
      data paramlist(5) /gribparam(2,5,0,3,3,'ICAHT')/
      data paramlist(6) /gribparam(2,6,0,3,4,'GP')/
      data paramlist(7) /gribparam(2,7,0,3,5,'HGT')/
      data paramlist(8) /gribparam(2,8,0,3,6,'DIST')/
      data paramlist(9) /gribparam(2,9,0,3,7,'HSTDV')/
      data paramlist(10) /gribparam(2,10,0,14,0,'TOZNE')/
      data paramlist(11) /gribparam(2,11,0,0,0,'TMP')/
      data paramlist(12) /gribparam(2,12,0,0,1,'VTMP')/
      data paramlist(13) /gribparam(2,13,0,0,2,'POT')/
      data paramlist(14) /gribparam(2,14,0,0,3,'EPOT')/
      data paramlist(15) /gribparam(2,15,0,0,4,'TMAX')/
      data paramlist(16) /gribparam(2,16,0,0,5,'TMIN')/
      data paramlist(17) /gribparam(2,17,0,0,6,'DPT')/
      data paramlist(18) /gribparam(2,18,0,0,7,'DEPR')/
      data paramlist(19) /gribparam(2,19,0,0,8,'LAPR')/
      data paramlist(20) /gribparam(2,20,0,19,0,'VIS')/
      data paramlist(21) /gribparam(2,21,0,15,6,'RDSP1')/
      data paramlist(22) /gribparam(2,22,0,15,7,'RDSP2')/
      data paramlist(23) /gribparam(2,23,0,15,8,'RDSP3')/
      data paramlist(24) /gribparam(2,24,0,7,0,'PLI')/
      data paramlist(25) /gribparam(2,25,0,0,9,'TMPA')/
      data paramlist(26) /gribparam(2,26,0,3,8,'PRESA')/
      data paramlist(27) /gribparam(2,27,0,3,9,'GPA')/
      data paramlist(28) /gribparam(2,28,10,0,0,'WVSP1')/
      data paramlist(29) /gribparam(2,29,10,0,1,'WVSP2')/
      data paramlist(30) /gribparam(2,30,10,0,2,'WVSP3')/
      data paramlist(31) /gribparam(2,31,0,2,0,'WDIR')/
      data paramlist(32) /gribparam(2,32,0,2,1,'WIND')/
      data paramlist(33) /gribparam(2,33,0,2,2,'UGRD')/
      data paramlist(34) /gribparam(2,34,0,2,3,'VGRD')/
      data paramlist(35) /gribparam(2,35,0,2,4,'STRM')/
      data paramlist(36) /gribparam(2,36,0,2,5,'VPOT')/
      data paramlist(37) /gribparam(2,37,0,2,6,'MNTSF')/
      data paramlist(38) /gribparam(2,38,0,2,7,'SGCVV')/
      data paramlist(39) /gribparam(2,39,0,2,8,'VVEL')/
      data paramlist(40) /gribparam(2,40,0,2,9,'DZDT')/
      data paramlist(41) /gribparam(2,41,0,2,10,'ABSV')/
      data paramlist(42) /gribparam(2,42,0,2,11,'ABSD')/
      data paramlist(43) /gribparam(2,43,0,2,12,'RELV')/
      data paramlist(44) /gribparam(2,44,0,2,13,'RELD')/
      data paramlist(45) /gribparam(2,45,0,2,15,'VUCSH')/
      data paramlist(46) /gribparam(2,46,0,2,16,'VVCSH')/
      data paramlist(47) /gribparam(2,47,10,1,0,'DIRC')/
      data paramlist(48) /gribparam(2,48,10,1,1,'SPC')/
      data paramlist(49) /gribparam(2,49,10,1,2,'UOGRD')/
      data paramlist(50) /gribparam(2,50,10,1,3,'VOGRD')/
      data paramlist(51) /gribparam(2,51,0,1,0,'SPFH')/
      data paramlist(52) /gribparam(2,52,0,1,1,'RH')/
      data paramlist(53) /gribparam(2,53,0,1,2,'MIXR')/
      data paramlist(54) /gribparam(2,54,0,1,3,'PWAT')/
      data paramlist(55) /gribparam(2,55,0,1,4,'VAPP')/
      data paramlist(56) /gribparam(2,56,0,1,5,'SATD')/
      data paramlist(57) /gribparam(2,57,0,1,6,'EVP')/
      data paramlist(58) /gribparam(2,58,0,6,0,'CICE')/
      data paramlist(59) /gribparam(2,59,0,1,7,'PRATE')/
      data paramlist(60) /gribparam(2,60,0,19,2,'TSTM')/
      data paramlist(61) /gribparam(2,61,0,1,8,'APCP')/
      data paramlist(62) /gribparam(2,62,0,1,9,'NCPCP')/
      data paramlist(63) /gribparam(2,63,0,1,10,'ACPCP')/
      data paramlist(64) /gribparam(2,64,0,1,12,'SRWEQ')/
      data paramlist(65) /gribparam(2,65,0,1,13,'WEASD')/
      data paramlist(66) /gribparam(2,66,0,1,11,'SNOD')/
      data paramlist(67) /gribparam(2,67,0,19,3,'MIXHT')/
      data paramlist(68) /gribparam(2,68,10,4,2,'TTHDP')/
      data paramlist(69) /gribparam(2,69,10,4,0,'MTHD')/
      data paramlist(70) /gribparam(2,70,10,4,1,'MTHA')/
      data paramlist(71) /gribparam(2,71,0,6,1,'TCDC')/
      data paramlist(72) /gribparam(2,72,0,6,2,'CDCON')/
      data paramlist(73) /gribparam(2,73,0,6,3,'LCDC')/
      data paramlist(74) /gribparam(2,74,0,6,4,'MCDC')/
      data paramlist(75) /gribparam(2,75,0,6,5,'HCDC')/
      data paramlist(76) /gribparam(2,76,0,6,6,'CWAT')/
      data paramlist(77) /gribparam(2,77,0,7,1,'BLI')/
      data paramlist(78) /gribparam(2,78,0,1,14,'SNOC')/
      data paramlist(79) /gribparam(2,79,0,1,15,'SNOL')/
      data paramlist(80) /gribparam(2,80,10,3,0,'WTMP')/
      data paramlist(81) /gribparam(2,81,2,0,0,'LAND')/
      data paramlist(82) /gribparam(2,82,10,3,1,'DSLM')/
      data paramlist(83) /gribparam(2,83,2,0,1,'SFCR')/
      data paramlist(84) /gribparam(2,84,0,19,1,'ALBDO')/
      data paramlist(85) /gribparam(2,85,2,0,2,'TSOIL')/
      data paramlist(86) /gribparam(2,86,2,0,3,'SOILM')/
      data paramlist(87) /gribparam(2,87,2,0,4,'VEG')/
      data paramlist(88) /gribparam(2,88,10,4,3,'SALTY')/
      data paramlist(89) /gribparam(2,89,0,3,10,'DEN')/
      data paramlist(90) /gribparam(2,90,2,0,5,'WATR')/
      data paramlist(91) /gribparam(2,91,10,2,0,'ICEC')/
      data paramlist(92) /gribparam(2,92,10,2,1,'ICETK')/
      data paramlist(93) /gribparam(2,93,10,2,2,'DICED')/
      data paramlist(94) /gribparam(2,94,10,2,3,'SICED')/
      data paramlist(95) /gribparam(2,95,10,2,4,'UICE')/
      data paramlist(96) /gribparam(2,96,10,2,5,'VICE')/
      data paramlist(97) /gribparam(2,97,10,2,6,'ICEG')/
      data paramlist(98) /gribparam(2,98,10,2,7,'ICED')/
      data paramlist(99) /gribparam(2,99,0,1,16,'SNOM')/
      data paramlist(100) /gribparam(2,100,10,0,3,'HTSGW')/
      data paramlist(101) /gribparam(2,101,10,0,4,'WVDIR')/
      data paramlist(102) /gribparam(2,102,10,0,5,'WVHGT')/
      data paramlist(103) /gribparam(2,103,10,0,6,'WVPER')/
      data paramlist(104) /gribparam(2,104,10,0,7,'SWDIR')/
      data paramlist(105) /gribparam(2,105,10,0,8,'SWELL')/
      data paramlist(106) /gribparam(2,106,10,0,9,'SWPER')/
      data paramlist(107) /gribparam(2,107,10,0,10,'DIRPW')/
      data paramlist(108) /gribparam(2,108,10,0,11,'PERPW')/
      data paramlist(109) /gribparam(2,109,10,0,12,'DIRSW')/
      data paramlist(110) /gribparam(2,110,10,0,13,'PERSW')/
      data paramlist(111) /gribparam(2,111,0,4,0,'NSWRS')/
      data paramlist(112) /gribparam(2,112,0,5,0,'NLWRS')/
      data paramlist(113) /gribparam(2,113,0,4,1,'NSWRT')/
      data paramlist(114) /gribparam(2,114,0,5,1,'NLWRT')/
      data paramlist(115) /gribparam(2,115,0,5,2,'LWAVR')/
      data paramlist(116) /gribparam(2,116,0,4,2,'SWAVR')/
      data paramlist(117) /gribparam(2,117,0,4,3,'GRAD')/
      data paramlist(118) /gribparam(2,118,0,4,4,'BRTMP')/
      data paramlist(119) /gribparam(2,119,0,4,5,'LWRAD')/
      data paramlist(120) /gribparam(2,120,0,4,6,'SWRAD')/
      data paramlist(121) /gribparam(2,121,0,0,10,'LHTFL')/
      data paramlist(122) /gribparam(2,122,0,0,11,'SHTFL')/
      data paramlist(123) /gribparam(2,123,0,2,20,'BLYDP')/
      data paramlist(124) /gribparam(2,124,0,2,17,'UFLX')/
      data paramlist(125) /gribparam(2,125,0,2,18,'VFLX')/
      data paramlist(126) /gribparam(2,126,0,2,19,'WMIXE')/
      data paramlist(127) /gribparam(2,127,255,255,255,'IMGD')/
!
!  GRIB1 parameters in NCEP Local Table version 2
!  Added 8/07/2003
!
      data paramlist(128) /gribparam(2,229,0,0,192,'SNOHF')/
      data paramlist(129) /gribparam(2,153,0,1,22,'CLMR')/
      data paramlist(130) /gribparam(2,140,0,1,192,'CRAIN')/
      data paramlist(131) /gribparam(2,141,0,1,193,'CFRZR')/
      data paramlist(132) /gribparam(2,142,0,1,194,'CICEP')/
      data paramlist(133) /gribparam(2,143,0,1,195,'CSNOW')/
      data paramlist(134) /gribparam(2,214,0,1,196,'CPRAT')/
      data paramlist(135) /gribparam(2,135,0,1,197,'MCONV')/
      data paramlist(136) /gribparam(2,194,1,1,193,'CPOFP')/
      data paramlist(137) /gribparam(2,228,0,1,199,'PEVAP')/
      data paramlist(138) /gribparam(2,136,0,2,192,'VWSH')/
      data paramlist(139) /gribparam(2,172,0,2,193,'MFLX')/
      data paramlist(140) /gribparam(2,196,0,2,194,'USTM')/
      data paramlist(141) /gribparam(2,197,0,2,195,'VSTM')/
      data paramlist(142) /gribparam(2,252,0,2,196,'CD')/
      data paramlist(143) /gribparam(2,253,0,2,197,'FRICV')/
      data paramlist(144) /gribparam(2,130,0,3,192,'MSLET')/
      data paramlist(145) /gribparam(2,204,0,4,192,'DSWRF')/
      data paramlist(146) /gribparam(2,211,0,4,193,'USWRF')/
      data paramlist(147) /gribparam(2,205,0,5,192,'DLWRF')/
      data paramlist(148) /gribparam(2,212,0,5,193,'ULWRF')/
      data paramlist(149) /gribparam(2,213,0,6,192,'CDLYR')/
      data paramlist(150) /gribparam(2,132,0,7,193,'4LFTX')/
      data paramlist(151) /gribparam(2,157,0,7,6,'CAPE')/
      data paramlist(152) /gribparam(2,156,0,7,7,'CIN')/
      data paramlist(153) /gribparam(2,190,0,7,8,'HLCY')/
      data paramlist(154) /gribparam(2,131,0,7,192,'LFTX')/
      data paramlist(155) /gribparam(2,158,0,19,11,'TKE')/
      data paramlist(156) /gribparam(2,176,0,191,192,'NLAT')/
      data paramlist(157) /gribparam(2,177,0,191,193,'ELON')/
      data paramlist(158) /gribparam(2,234,1,0,192,'BGRUN')/
      data paramlist(159) /gribparam(2,235,1,0,193,'SSRUN')/
      data paramlist(160) /gribparam(2,144,2,0,192,'SOILW')/
      data paramlist(161) /gribparam(2,155,2,0,193,'GFLUX')/
      data paramlist(162) /gribparam(2,207,2,0,194,'MSTAV')/
      data paramlist(163) /gribparam(2,208,2,0,195,'SFEXC')/
      data paramlist(164) /gribparam(2,223,2,0,196,'CNWAT')/
      data paramlist(165) /gribparam(2,226,2,0,197,'BMIXL')/
      data paramlist(166) /gribparam(2,154,0,14,192,'O3MR')/
      data paramlist(167) /gribparam(2,222,0,3,193,'5WAVH')/
      data paramlist(168) /gribparam(2,145,0,1,200,'PEVPR')/
      data paramlist(169) /gribparam(2,146,0,6,193,'CWORK')/
      data paramlist(170) /gribparam(2,147,0,3,194,'U-GWD')/
      data paramlist(171) /gribparam(2,148,0,3,195,'V-GWD')/
      data paramlist(172) /gribparam(2,221,0,3,196,'HPBL')/
      data paramlist(173) /gribparam(2,230,0,3,197,'5WAVA')/
! Added 9/26/2003
      data paramlist(174) /gribparam(130,160,2,3,192,'SOILL')/
      data paramlist(175) /gribparam(130,171,2,3,193,'RLYRS')/
      data paramlist(176) /gribparam(130,219,2,0,201,'WILT')/
      data paramlist(177) /gribparam(130,222,2,3,194,'SLTYP')/
      data paramlist(178) /gribparam(2,224,2,3,0,'SOTYP')/
      data paramlist(179) /gribparam(2,225,2,0,198,'VGTYP')/
      data paramlist(180) /gribparam(130,230,2,3,195,'SMREF')/
      data paramlist(181) /gribparam(130,231,2,3,196,'SMDRY')/
      data paramlist(182) /gribparam(2,238,0,1,201,'SNOWC')/
      data paramlist(183) /gribparam(130,240,2,3,197,'POROS')/
      data paramlist(184) /gribparam(129,131,0,1,202,'FRAIN')/
      data paramlist(185) /gribparam(129,132,0,6,199,'FICE')/
      data paramlist(186) /gribparam(129,133,0,1,203,'RIME')/
      data paramlist(187) /gribparam(129,134,0,6,194,'CUEFI')/
      data paramlist(188) /gribparam(129,135,0,6,195,'TCOND')/
      data paramlist(189) /gribparam(129,136,0,6,196,'TCOLW')/
      data paramlist(190) /gribparam(129,137,0,6,197,'TCOLI')/
      data paramlist(191) /gribparam(129,138,0,1,204,'TCOLR')/
      data paramlist(192) /gribparam(129,139,0,1,205,'TCOLS')/
      data paramlist(193) /gribparam(129,140,0,6,198,'TCOLC')/
      data paramlist(194) /gribparam(130,159,0,19,192,'MXSALB')/
      data paramlist(195) /gribparam(130,170,0,19,193,'SNFALB')/
      data paramlist(196) /gribparam(2,170,0,1,24,'RWMR')/
      data paramlist(197) /gribparam(2,171,0,1,25,'SNMR')/
      data paramlist(198) /gribparam(130,181,2,0,199,'CCOND')/
      data paramlist(199) /gribparam(130,203,2,0,200,'RSMIN')/
      data paramlist(200) /gribparam(130,246,2,0,202,'RCS')/
      data paramlist(201) /gribparam(130,247,2,0,203,'RCT')/
      data paramlist(202) /gribparam(130,248,2,0,204,'RCQ')/
      data paramlist(203) /gribparam(130,249,2,0,205,'RCSOL')/
      data paramlist(204) /gribparam(2,254,0,7,194,'RI')/
      data paramlist(205) /gribparam(129,190,3,1,192,'USCT')/
      data paramlist(206) /gribparam(129,191,3,1,193,'VSCT')/
      data paramlist(207) /gribparam(129,171,0,191,194,'TSEC')/
      data paramlist(208) /gribparam(129,180,0,14,193,'OZCON')/
      data paramlist(209) /gribparam(129,181,0,14,194,'OZCAT')/
      data paramlist(210) /gribparam(2,193,1,1,2,'POP')/
      data paramlist(211) /gribparam(2,195,1,1,192,'CPOZP')/
      data paramlist(212) /gribparam(2,180,0,2,22,'GUST')/
! Added 11/17/2005 - for wave models
      data paramlist(213) /gribparam(0,31,0,2,0,'WDIR')/
      data paramlist(214) /gribparam(0,32,0,2,1,'WIND')/
      data paramlist(215) /gribparam(0,33,0,2,2,'UGRD')/
      data paramlist(216) /gribparam(0,34,0,2,3,'VGRD')/
      data paramlist(217) /gribparam(0,100,10,0,3,'HTSGW')/
      data paramlist(218) /gribparam(0,101,10,0,4,'WVDIR')/
      data paramlist(219) /gribparam(0,103,10,0,6,'WVPER')/
      data paramlist(220) /gribparam(0,107,10,0,10,'DIRPW')/
      data paramlist(221) /gribparam(0,108,10,0,11,'PERPW')/
      data paramlist(222) /gribparam(0,109,10,0,12,'DIRSW')/
      data paramlist(223) /gribparam(0,110,10,0,13,'PERSW')/
! Added 1/26/2006 - 
      data paramlist(224) /gribparam(129,156,0,13,192,'PMTC')/
      data paramlist(225) /gribparam(129,157,0,13,193,'PMTF')/
      data paramlist(226) /gribparam(3,11,0,0,0,'TMP')/
      data paramlist(227) /gribparam(2,129,0,3,198,'MSLMA')/
      data paramlist(228) /gribparam(129,163,0,13,194,'LPMTF')/
      data paramlist(229) /gribparam(129,164,0,13,195,'LIPMF')/
! Added 3/6/2006 - For missing GRIB1 to GRIB2 conversions
      data paramlist(230) /gribparam(2,178,0,1,23,'ICMR')/
      data paramlist(231) /gribparam(2,179,0,1,32,'GRMR')/
      data paramlist(232) /gribparam(2,186,0,1,206,'TIPD')/
      data paramlist(233) /gribparam(2,187,0,17,192,'LTNG')/
      data paramlist(234) /gribparam(2,188,2,0,206,'RDRIP')/
      data paramlist(235) /gribparam(2,189,0,0,15,'VPTMP')/
      data paramlist(236) /gribparam(2,198,0,1,207,'NCIP')/
      data paramlist(237) /gribparam(2,239,0,1,208,'SNOT')/
      data paramlist(238) /gribparam(2,128,0,3,1,'MSLSA')/
      data paramlist(239) /gribparam(2,137,0,3,199,'TSLSA')/
      data paramlist(240) /gribparam(129,141,0,3,200,'PLPL')/
      data paramlist(241) /gribparam(129,200,0,4,194,'DUVB')/
      data paramlist(242) /gribparam(129,201,0,4,195,'CDUVB')/
      data paramlist(243) /gribparam(2,201,2,0,207,'ICWAT')/
      data paramlist(244) /gribparam(2,209,0,19,204,'MIXLY')/
      data paramlist(245) /gribparam(2,216,0,0,193,'TTRAD')/
      data paramlist(246) /gribparam(129,211,0,16,195,'REFD')/
      data paramlist(247) /gribparam(129,212,0,16,196,'REFC')/
      data paramlist(248) /gribparam(2,161,0,4,196,'CSDSF')/
      data paramlist(249) /gribparam(129,168,0,1,209,'TCLSW')/
      data paramlist(250) /gribparam(129,169,0,1,210,'TCOLM')/
      data paramlist(251) /gribparam(2,181,0,3,201,'LPSX')/
      data paramlist(252) /gribparam(2,182,0,3,202,'LPSY')/
      data paramlist(253) /gribparam(2,183,0,3,203,'HGTX')/
      data paramlist(254) /gribparam(2,184,0,3,204,'HGTY')/
      data paramlist(255) /gribparam(128,254,0,0,194,'REV')/
! Added 4/20/2007 - For missing GRIB1 to GRIB2 conversions
      data paramlist(256) /gribparam(1,91,10,2,0,'ICEC')/
      data paramlist(257) /gribparam(0,49,10,1,2,'UOGRD')/
      data paramlist(258) /gribparam(0,50,10,1,3,'VOGRD')/
      data paramlist(259) /gribparam(0,80,10,3,0,'WTMP')/
      data paramlist(260) /gribparam(0,82,10,3,1,'DSLM')/
      data paramlist(261) /gribparam(0,88,10,4,3,'SALTY')/
      data paramlist(262) /gribparam(1,49,10,1,2,'UOGRD')/
      data paramlist(263) /gribparam(1,50,10,1,3,'VOGRD')/
      data paramlist(264) /gribparam(1,80,10,3,0,'WTMP')/
      data paramlist(265) /gribparam(1,88,10,4,3,'SALTY')/
      data paramlist(266) /gribparam(1,40,0,2,9,'DZDT')/
      data paramlist(267) /gribparam(1,67,0,19,3,'MIXHT')/
      data paramlist(268) /gribparam(3,2,0,3,1,'PRMSL')/
      data paramlist(269) /gribparam(3,7,0,3,5,'HGT')/
      data paramlist(270) /gribparam(128,130,10,3,194,'ELEV')/
      data paramlist(271) /gribparam(129,217,0,1,198,'MINRH')/
      data paramlist(272) /gribparam(129,218,0,1,27,'MAXRH')/
      data paramlist(273) /gribparam(130,161,0,1,29,'ASNOW')/
      data paramlist(274) /gribparam(129,165,0,16,192,'REFZR')/
      data paramlist(275) /gribparam(129,166,0,16,193,'REFZI')/
      data paramlist(276) /gribparam(129,167,0,16,194,'REFZC')/
      data paramlist(277) /gribparam(129,192,0,2,198,'LAUV')/
      data paramlist(278) /gribparam(129,193,0,2,199,'LOUV')/
      data paramlist(279) /gribparam(129,188,0,2,200,'LAVV')/
      data paramlist(280) /gribparam(129,189,0,2,201,'LOVV')/
      data paramlist(281) /gribparam(129,207,0,2,202,'LAPP')/
      data paramlist(282) /gribparam(129,208,0,2,203,'LOPP')/
      data paramlist(283) /gribparam(129,198,10,3,195,'SSHG')/
      data paramlist(284) /gribparam(1,33,0,2,2,'UGRD')/
      data paramlist(285) /gribparam(1,34,0,2,3,'VGRD')/
      data paramlist(286) /gribparam(1,2,0,3,1,'PRMSL')/
      data paramlist(287) /gribparam(1,7,0,3,5,'HGT')/
      data paramlist(288) /gribparam(128,186,10,4,192,'WTMPC')/
      data paramlist(289) /gribparam(128,187,10,4,193,'SALIN')/
      data paramlist(290) /gribparam(128,177,10,3,196,'P2OMLT')/
      data paramlist(291) /gribparam(128,178,10,1,192,'OMLU')/
      data paramlist(292) /gribparam(128,179,10,1,193,'OMLV')/
      data paramlist(293) /gribparam(128,183,10,1,194,'UBARO')/
      data paramlist(294) /gribparam(128,184,10,1,195,'VBARO')/
      data paramlist(295) /gribparam(129,179,0,19,205,'FLGHT')/
      data paramlist(296) /gribparam(129,185,0,19,206,'CICEL')/
      data paramlist(297) /gribparam(129,186,0,19,207,'CIVIS')/
      data paramlist(298) /gribparam(129,187,0,19,208,'CIFLT')/
      data paramlist(299) /gribparam(129,177,0,19,209,'LAVNI')/
      data paramlist(300) /gribparam(129,178,0,19,210,'HAVNI')/
      data paramlist(301) /gribparam(130,189,0,19,211,'SBSALB')/
      data paramlist(302) /gribparam(130,190,0,19,212,'SWSALB')/
      data paramlist(303) /gribparam(130,191,0,19,213,'NBSALB')/
      data paramlist(304) /gribparam(130,192,0,19,214,'NWSALB')/
      data paramlist(305) /gribparam(129,149,10,0,192,'WSTP')/
      data paramlist(306) /gribparam(128,188,0,1,211,'EMNP')/
      data paramlist(307) /gribparam(128,192,0,3,205,'LAYTH')/
      data paramlist(308) /gribparam(129,219,0,6,13,'CEIL')/
      data paramlist(309) /gribparam(129,220,0,19,12,'PBLREG')/
      data paramlist(310) /gribparam(130,179,2,0,228,'ACOND')/
      data paramlist(311) /gribparam(130,198,0,1,212,'SBSNO')/
      data paramlist(312) /gribparam(2,199,2,3,198,'EVBS')/
      data paramlist(313) /gribparam(2,200,2,0,229,'EVCW')/
      data paramlist(314) /gribparam(2,210,2,0,230,'TRANS')/
      data paramlist(315) /gribparam(129,182,0,2,204,'VEDH')/
      data paramlist(320) /gribparam(2,241,0,0,195,'LRGHR')/
      data paramlist(321) /gribparam(2,242,0,0,196,'CNVHR')/
      data paramlist(322) /gribparam(140,168,0,19,20,'ICIP')/
      data paramlist(323) /gribparam(140,169,0,19,20,'ICIP')/
      data paramlist(324) /gribparam(140,170,0,19,21,'CTP')/
      data paramlist(325) /gribparam(140,171,0,19,21,'CTP')/
      data paramlist(326) /gribparam(140,172,0,19,22,'CAT')/
      data paramlist(327) /gribparam(140,173,0,19,22,'CAT')/
      data paramlist(328) /gribparam(140,174,0,6,25,'CBHE')/
      data paramlist(329) /gribparam(140,175,255,255,255,'IMGD')/
      data paramlist(330) /gribparam(140,176,255,255,255,'IMGD')/
      data paramlist(331) /gribparam(140,177,255,255,255,'IMGD')/
      data paramlist(332) /gribparam(140,178,255,255,255,'IMGD')/
      data paramlist(333) /gribparam(140,179,0,3,3,'ICAHT')/
      data paramlist(334) /gribparam(140,180,0,3,3,'ICAHT')/
      data paramlist(335) /gribparam(140,181,255,255,255,'IMGD')/
      data paramlist(336) /gribparam(140,182,255,255,255,'IMGD')/
      data paramlist(337) /gribparam(129,76,0,6,6,'CWAT')/
! Added 8/24/2007
      data paramlist(338) /gribparam(0,104,10,0,7,'SWDIR')/
      data paramlist(339) /gribparam(0,105,10,0,8,'SWELL')/
      data paramlist(340) /gribparam(0,106,10,0,9,'SWPER')/
      data paramlist(341) /gribparam(0,102,10,0,5,'WVHGT')/
      data paramlist(342) /gribparam(129,213,3,192,0,'SBT122')/
      data paramlist(343) /gribparam(129,214,3,192,1,'SBT123')/
      data paramlist(344) /gribparam(129,215,3,192,2,'SBT124')/
      data paramlist(345) /gribparam(129,216,3,192,3,'SBT126')/
      data paramlist(346) /gribparam(129,221,3,192,4,'SBC123')/
      data paramlist(347) /gribparam(129,222,3,192,5,'SBC124')/
      data paramlist(348) /gribparam(129,228,10,3,192,'SURGE')/
      data paramlist(349) /gribparam(129,229,10,3,193,'ETSRG')/
      data paramlist(350) /gribparam(2,149,0,2,14,'PVORT')/
      data paramlist(351) /gribparam(2,150,0,192,1,'COVMZ')/
      data paramlist(352) /gribparam(2,151,0,192,2,'COVTZ')/
      data paramlist(353) /gribparam(2,152,0,192,3,'COVTM')/
      data paramlist(354) /gribparam(129,202,0,0,197,'THFLX')/
      data paramlist(355) /gribparam(3,33,0,2,2,'UGRD')/
      data paramlist(356) /gribparam(3,34,0,2,3,'VGRD')/
      data paramlist(357) /gribparam(3,40,0,2,9,'DZDT')/
      data paramlist(358) /gribparam(3,124,0,2,17,'UFLX')/
      data paramlist(359) /gribparam(3,125,0,2,18,'VFLX')/
      data paramlist(360) /gribparam(3,8,0,3,6,'DIST')/
      data paramlist(361) /gribparam(3,13,0,0,2,'POT')/
      data paramlist(362) /gribparam(3,88,10,4,3,'SALTY')/
      data paramlist(363) /gribparam(3,49,10,1,2,'UOGRD')/
      data paramlist(364) /gribparam(3,50,10,1,3,'VOGRD')/
      data paramlist(365) /gribparam(2,215,0,0,198,'TTDIA')/
      data paramlist(366) /gribparam(2,217,0,0,199,'TTPHY')/
      data paramlist(367) /gribparam(130,154,2,3,199,'LSPA')/
      data paramlist(368) /gribparam(2,250,0,4,197,'SWHR')/
      data paramlist(369) /gribparam(2,251,0,5,194,'LWHR')/
      data paramlist(370) /gribparam(2,160,0,4,198,'CSUSF')/
      data paramlist(371) /gribparam(2,162,0,5,195,'CSULF')/
      data paramlist(372) /gribparam(2,163,0,5,196,'CSDLF')/
      data paramlist(373) /gribparam(2,164,0,4,199,'CFNSF')/
      data paramlist(374) /gribparam(2,165,0,5,197,'CFNLF')/
      data paramlist(375) /gribparam(2,166,0,4,200,'VBDSF')/
      data paramlist(376) /gribparam(2,167,0,4,201,'VDDSF')/
      data paramlist(377) /gribparam(2,168,0,4,202,'NBDSF')/
      data paramlist(378) /gribparam(2,169,0,4,203,'NDDSF')/
      data paramlist(379) /gribparam(2,206,0,7,196,'UVI')/
      data paramlist(380) /gribparam(2,219,0,0,200,'TSD1D')/
      data paramlist(381) /gribparam(2,220,0,3,206,'NLGSP')/
      data paramlist(382) /gribparam(2,244,0,0,201,'SHAHR')/
      data paramlist(383) /gribparam(2,246,0,0,202,'VDFHR')/
      data paramlist(384) /gribparam(2,243,0,1,213,'CNVMR')/
      data paramlist(385) /gribparam(2,245,0,1,214,'SHAMR')/
      data paramlist(386) /gribparam(2,249,0,1,215,'VDFMR')/
      data paramlist(387) /gribparam(2,247,0,2,208,'VDFUA')/
      data paramlist(388) /gribparam(2,248,0,2,209,'VDFVA')/
      data paramlist(389) /gribparam(3,202,0,7,195,'CWDI')/
      data paramlist(390) /gribparam(2,232,0,4,204,'DTRF')/
      data paramlist(391) /gribparam(2,233,0,4,205,'UTRF')/
      data paramlist(392) /gribparam(2,231,0,6,200,'MFLUX')/
      data paramlist(393) /gribparam(2,202,0,7,195,'CWDI')/
      data paramlist(394) /gribparam(2,203,0,19,232,'VAFTD')/
      data paramlist(395) /gribparam(3,238,0,1,201,'SNOWC')/
      data paramlist(396) /gribparam(3,66,0,1,11,'SNOD')/
      data paramlist(397) /gribparam(2,133,0,7,2,'KX')/
      data paramlist(398) /gribparam(2,134,0,7,5,'SX')/
      data paramlist(399) /gribparam(128,191,10,4,194,'BKENG')/
      data paramlist(400) /gribparam(129,195,10,4,195,'DBSS')/
      data paramlist(401) /gribparam(128,171,10,3,197,'AOHFLX')/
      data paramlist(402) /gribparam(128,180,10,3,198,'ASHFL')/
      data paramlist(403) /gribparam(128,193,10,3,199,'SSTT')/
      data paramlist(404) /gribparam(128,194,10,3,200,'SSST')/
      data paramlist(405) /gribparam(128,190,10,3,201,'KENG')/
      data paramlist(406) /gribparam(128,185,10,4,196,'INTFD')/
      data paramlist(407) /gribparam(129,199,10,3,202,'SLTFL')/
      data paramlist(408) /gribparam(129,197,10,4,197,'OHC')/
      data paramlist(409) /gribparam(2,159,0,1,216,'CONP')/
      data paramlist(410) /gribparam(2,175,0,191,195,'MLYNO')/
      data paramlist(411) /gribparam(129,223,0,1,65,'RPRATE')/
      data paramlist(412) /gribparam(129,224,0,1,66,'SPRATE')/
      data paramlist(413) /gribparam(129,225,0,1,67,'FPRATE')/
      data paramlist(414) /gribparam(129,226,0,1,68,'IPRATE')/
      data paramlist(415) /gribparam(129,227,0,7,197,'UPHL')/
      data paramlist(416) /gribparam(3,87,2,0,4,'VEG')/
      data paramlist(417) /gribparam(129,130,1,1,195,'CWR')/
      data paramlist(418) /gribparam(2,240,0,192,4,'COVTW')/
      data paramlist(419) /gribparam(133,164,0,192,5,'COVZZ')/
      data paramlist(420) /gribparam(133,165,0,192,6,'COVMM')/
      data paramlist(421) /gribparam(133,166,0,192,7,'COVQZ')/
      data paramlist(422) /gribparam(133,167,0,192,8,'COVQM')/
      data paramlist(423) /gribparam(133,168,0,192,9,'COVTVV')/
      data paramlist(424) /gribparam(133,169,0,192,10,'COVQVV')/
      data paramlist(425) /gribparam(133,203,0,192,11,'COVPSPS')/
      data paramlist(426) /gribparam(133,206,0,192,12,'COVQQ')/
      data paramlist(427) /gribparam(133,220,0,192,13,'COVVVVV')/
      data paramlist(428) /gribparam(133,234,0,192,14,'COVTT')/
      data paramlist(429) /gribparam(133,201,0,0,203,'THZ0')/
      data paramlist(430) /gribparam(133,195,0,1,218,'QZ0')/
      data paramlist(431) /gribparam(133,204,0,1,219,'QMAX')/
      data paramlist(432) /gribparam(133,205,0,1,220,'QMIN')/
      data paramlist(433) /gribparam(133,181,0,2,210,'GWDU')/
      data paramlist(434) /gribparam(133,182,0,2,211,'GWDV')/
      data paramlist(435) /gribparam(133,183,0,2,212,'CNVU')/
      data paramlist(436) /gribparam(133,184,0,2,213,'CNVV')/
      data paramlist(437) /gribparam(133,236,0,2,214,'WTEND')/
      data paramlist(438) /gribparam(133,154,0,2,215,'OMGALF')/
      data paramlist(439) /gribparam(133,196,0,2,216,'CNGWDU')/
      data paramlist(440) /gribparam(133,197,0,2,217,'CNGWDV')/
      data paramlist(441) /gribparam(133,202,0,3,207,'CNVUMF')/
      data paramlist(442) /gribparam(133,209,0,3,208,'CNVDMF')/
      data paramlist(443) /gribparam(133,219,0,3,209,'CNVDEMF')/
      data paramlist(444) /gribparam(133,173,0,1,217,'LRGMR')/
      data paramlist(445) /gribparam(133,174,0,14,195,'VDFOZ')/
      data paramlist(446) /gribparam(133,175,0,14,196,'POZ')/
      data paramlist(447) /gribparam(133,188,0,14,197,'TOZ')/
      data paramlist(448) /gribparam(133,139,0,14,198,'POZT')/
      data paramlist(449) /gribparam(133,239,0,14,199,'POZO')/
      data paramlist(450) /gribparam(133,185,2,0,208,'AKHS')/
      data paramlist(451) /gribparam(133,186,2,0,209,'AKMS')/
      data paramlist(452) /gribparam(133,193,0,19,218,'EPSR')/
      data paramlist(453) /gribparam(130,229,0,0,192,'SNOHF')/
      data paramlist(454) /gribparam(129,194,0,0,204,'TCHP')/
! Added 5/29/2008
      data paramlist(455) /gribparam(2,185,0,19,219,'TPFI')/
      data paramlist(456) /gribparam(130,182,0,7,198,'LAI')/
      data paramlist(457) /gribparam(2,173,0,3,210,'LMH')/
      data paramlist(458) /gribparam(2,174,0,2,218,'LMV')/
! Added 6/30/2008   Add GRIB1 parameters in Table version 131
       data paramlist(459) /gribparam(131,1,0,3,0,'PRES')/
       data paramlist(460) /gribparam(131,2,0,3,1,'PRMSL')/
       data paramlist(461) /gribparam(131,3,0,3,2,'PTEND')/
       data paramlist(462) /gribparam(131,4,0,2,14,'PVORT')/
       data paramlist(463) /gribparam(131,5,0,3,3,'ICAHT')/
       data paramlist(464) /gribparam(131,6,0,3,4,'GP')/
       data paramlist(465) /gribparam(131,7,0,3,5,'HGT')/
       data paramlist(466) /gribparam(131,8,0,3,6,'DIST')/
       data paramlist(467) /gribparam(131,9,0,3,7,'HSTDV')/
       data paramlist(468) /gribparam(131,10,0,14,0,'TOZNE')/
       data paramlist(469) /gribparam(131,11,0,0,0,'TMP')/
       data paramlist(470) /gribparam(131,12,0,0,1,'VTMP')/
       data paramlist(471) /gribparam(131,13,0,0,2,'POT')/
       data paramlist(472) /gribparam(131,14,0,0,3,'EPOT')/
       data paramlist(473) /gribparam(131,15,0,0,4,'TMAX')/
       data paramlist(474) /gribparam(131,16,0,0,5,'TMIN')/
       data paramlist(475) /gribparam(131,17,0,0,6,'DPT')/
       data paramlist(476) /gribparam(131,18,0,0,7,'DEPR')/
       data paramlist(477) /gribparam(131,19,0,0,8,'LAPR')/
       data paramlist(478) /gribparam(131,20,0,19,0,'VIS')/
       data paramlist(479) /gribparam(131,21,0,15,6,'RDSP1')/
       data paramlist(480) /gribparam(131,22,0,15,7,'RDSP2')/
       data paramlist(481) /gribparam(131,23,0,15,8,'RDSP3')/
       data paramlist(482) /gribparam(131,24,0,7,0,'PLI')/
       data paramlist(483) /gribparam(131,25,0,0,9,'TMPA')/
       data paramlist(484) /gribparam(131,26,0,3,8,'PRESA')/
       data paramlist(485) /gribparam(131,27,0,3,9,'GPA')/
       data paramlist(486) /gribparam(131,28,10,0,0,'WVSP1')/
       data paramlist(487) /gribparam(131,29,10,0,1,'WVSP2')/
       data paramlist(488) /gribparam(131,30,10,0,2,'WVSP3')/
       data paramlist(489) /gribparam(131,31,0,2,0,'WDIR')/
       data paramlist(490) /gribparam(131,32,0,2,1,'WIND')/
       data paramlist(491) /gribparam(131,33,0,2,2,'UGRD')/
       data paramlist(492) /gribparam(131,34,0,2,3,'VGRD')/
       data paramlist(493) /gribparam(131,35,0,2,4,'STRM')/
       data paramlist(494) /gribparam(131,36,0,2,5,'VPOT')/
       data paramlist(495) /gribparam(131,37,0,2,6,'MNTSF')/
       data paramlist(496) /gribparam(131,38,0,2,7,'SGCVV')/
       data paramlist(497) /gribparam(131,39,0,2,8,'VVEL')/
       data paramlist(498) /gribparam(131,40,0,2,9,'DZDT')/
       data paramlist(499) /gribparam(131,41,0,2,10,'ABSV')/
       data paramlist(500) /gribparam(131,42,0,2,11,'ABSD')/
       data paramlist(501) /gribparam(131,43,0,2,12,'RELV')/
       data paramlist(502) /gribparam(131,44,0,2,13,'RELD')/
       data paramlist(503) /gribparam(131,45,0,2,15,'VUCSH')/
       data paramlist(504) /gribparam(131,46,0,2,16,'VVCSH')/
       data paramlist(505) /gribparam(131,47,10,1,0,'DIRC')/
       data paramlist(506) /gribparam(131,48,10,1,1,'SPC')/
       data paramlist(507) /gribparam(131,49,10,1,2,'UOGRD')/
       data paramlist(508) /gribparam(131,50,10,1,3,'VOGRD')/
       data paramlist(509) /gribparam(131,51,0,1,0,'SPFH')/
       data paramlist(510) /gribparam(131,52,0,1,1,'RH')/
       data paramlist(511) /gribparam(131,53,0,1,2,'MIXR')/
       data paramlist(512) /gribparam(131,54,0,1,3,'PWAT')/
       data paramlist(513) /gribparam(131,55,0,1,4,'VAPP')/
       data paramlist(514) /gribparam(131,56,0,1,5,'SATD')/
       data paramlist(515) /gribparam(131,57,0,1,6,'EVP')/
       data paramlist(516) /gribparam(131,58,0,6,0,'CICE')/
       data paramlist(517) /gribparam(131,59,0,1,7,'PRATE')/
       data paramlist(518) /gribparam(131,60,0,19,2,'TSTM')/
       data paramlist(519) /gribparam(131,61,0,1,8,'APCP')/
       data paramlist(520) /gribparam(131,62,0,1,9,'NCPCP')/
       data paramlist(521) /gribparam(131,63,0,1,10,'ACPCP')/
       data paramlist(522) /gribparam(131,64,0,1,12,'SRWEQ')/
       data paramlist(523) /gribparam(131,65,0,1,13,'WEASD')/
       data paramlist(524) /gribparam(131,66,0,1,11,'SNOD')/
       data paramlist(525) /gribparam(131,67,0,19,3,'MIXHT')/
       data paramlist(526) /gribparam(131,68,10,4,2,'TTHDP')/
       data paramlist(527) /gribparam(131,69,10,4,0,'MTHD')/
       data paramlist(528) /gribparam(131,70,10,4,1,'MTHA')/
       data paramlist(529) /gribparam(131,71,0,6,1,'TCDC')/
       data paramlist(530) /gribparam(131,72,0,6,2,'CDCON')/
       data paramlist(531) /gribparam(131,73,0,6,3,'LCDC')/
       data paramlist(532) /gribparam(131,74,0,6,4,'MCDC')/
       data paramlist(533) /gribparam(131,75,0,6,5,'HCDC')/
       data paramlist(534) /gribparam(131,76,0,6,6,'CWAT')/
       data paramlist(535) /gribparam(131,77,0,7,1,'BLI')/
       data paramlist(536) /gribparam(131,78,0,1,14,'SNOC')/
       data paramlist(537) /gribparam(131,79,0,1,15,'SNOL')/
       data paramlist(538) /gribparam(131,80,10,3,0,'WTMP')/
       data paramlist(539) /gribparam(131,81,2,0,0,'LAND')/
       data paramlist(540) /gribparam(131,82,10,3,1,'DSLM')/
       data paramlist(541) /gribparam(131,83,2,0,1,'SFCR')/
       data paramlist(542) /gribparam(131,84,0,19,1,'ALBDO')/
       data paramlist(543) /gribparam(131,85,2,0,2,'TSOIL')/
       data paramlist(544) /gribparam(131,86,2,0,3,'SOILM')/
       data paramlist(545) /gribparam(131,87,2,0,4,'VEG')/
       data paramlist(546) /gribparam(131,88,10,4,3,'SALTY')/
       data paramlist(547) /gribparam(131,89,0,3,10,'DEN')/
       data paramlist(548) /gribparam(131,90,2,0,5,'WATR')/
       data paramlist(549) /gribparam(131,91,10,2,0,'ICEC')/
       data paramlist(550) /gribparam(131,92,10,2,1,'ICETK')/
       data paramlist(551) /gribparam(131,93,10,2,2,'DICED')/
       data paramlist(552) /gribparam(131,94,10,2,3,'SICED')/
       data paramlist(553) /gribparam(131,95,10,2,4,'UICE')/
       data paramlist(554) /gribparam(131,96,10,2,5,'VICE')/
       data paramlist(555) /gribparam(131,97,10,2,6,'ICEG')/
       data paramlist(556) /gribparam(131,98,10,2,7,'ICED')/
       data paramlist(557) /gribparam(131,99,0,1,16,'SNOM')/
       data paramlist(558) /gribparam(131,100,10,0,3,'HTSGW')/
       data paramlist(559) /gribparam(131,101,10,0,4,'WVDIR')/
       data paramlist(560) /gribparam(131,102,10,0,5,'WVHGT')/
       data paramlist(561) /gribparam(131,103,10,0,6,'WVPER')/
       data paramlist(562) /gribparam(131,104,10,0,7,'SWDIR')/
       data paramlist(563) /gribparam(131,105,10,0,8,'SWELL')/
       data paramlist(564) /gribparam(131,106,10,0,9,'SWPER')/
       data paramlist(565) /gribparam(131,107,10,0,10,'DIRPW')/
       data paramlist(566) /gribparam(131,108,10,0,11,'PERPW')/
       data paramlist(567) /gribparam(131,109,10,0,12,'DIRSW')/
       data paramlist(568) /gribparam(131,110,10,0,13,'PERSW')/
       data paramlist(569) /gribparam(131,111,0,4,0,'NSWRS')/
       data paramlist(570) /gribparam(131,112,0,5,0,'NLWRS')/
       data paramlist(571) /gribparam(131,113,0,4,1,'NSWRT')/
       data paramlist(572) /gribparam(131,114,0,5,1,'NLWRT')/
       data paramlist(573) /gribparam(131,115,0,5,2,'LWAVR')/
       data paramlist(574) /gribparam(131,116,0,4,2,'SWAVR')/
       data paramlist(575) /gribparam(131,117,0,4,3,'GRAD')/
       data paramlist(576) /gribparam(131,118,0,4,4,'BRTMP')/
       data paramlist(577) /gribparam(131,119,0,4,5,'LWRAD')/
       data paramlist(578) /gribparam(131,120,0,4,6,'SWRAD')/
       data paramlist(579) /gribparam(131,121,0,0,10,'LHTFL')/
       data paramlist(580) /gribparam(131,122,0,0,11,'SHTFL')/
       data paramlist(581) /gribparam(131,123,0,2,20,'BLYDP')/
       data paramlist(582) /gribparam(131,124,0,2,17,'UFLX')/
       data paramlist(583) /gribparam(131,125,0,2,18,'VFLX')/
       data paramlist(584) /gribparam(131,126,0,2,19,'WMIXE')/
       data paramlist(585) /gribparam(131,127,255,255,255,'IMGD')/
       data paramlist(586) /gribparam(131,128,0,3,1,'MSLSA')/
       data paramlist(587) /gribparam(131,130,0,3,192,'MSLET')/
       data paramlist(588) /gribparam(131,131,0,7,192,'LFTX')/
       data paramlist(589) /gribparam(131,132,0,7,193,'4LFTX')/
       data paramlist(590) /gribparam(131,134,0,3,212,'PRESN')/
       data paramlist(591) /gribparam(131,135,0,1,197,'MCONV')/
       data paramlist(592) /gribparam(131,136,0,2,192,'VWSH')/
       data paramlist(593) /gribparam(131,137,0,2,219,'PVMWW')/
       data paramlist(594) /gribparam(131,140,0,1,192,'CRAIN')/
       data paramlist(595) /gribparam(131,141,0,1,193,'CFRZR')/
       data paramlist(596) /gribparam(131,142,0,1,194,'CICEP')/
       data paramlist(597) /gribparam(131,143,0,1,195,'CSNOW')/
       data paramlist(598) /gribparam(131,144,2,0,192,'SOILW')/
       data paramlist(599) /gribparam(131,145,0,1,200,'PEVPR')/
       data paramlist(600) /gribparam(131,146,2,0,210,'VEGT')/
       data paramlist(601) /gribparam(131,147,2,3,200,'BARET')/
       data paramlist(602) /gribparam(131,148,2,3,201,'AVSFT')/
       data paramlist(603) /gribparam(131,149,2,3,202,'RADT')/
       data paramlist(604) /gribparam(131,150,2,0,211,'SSTOR')/
       data paramlist(605) /gribparam(131,151,2,0,212,'LSOIL')/
       data paramlist(606) /gribparam(131,152,2,0,213,'EWATR')/
       data paramlist(607) /gribparam(131,153,0,1,22,'CLMR')/
       data paramlist(608) /gribparam(131,155,2,0,193,'GFLUX')/
       data paramlist(609) /gribparam(131,156,0,7,7,'CIN')/
       data paramlist(610) /gribparam(131,157,0,7,6,'CAPE')/
       data paramlist(611) /gribparam(131,158,0,19,11,'TKE')/
       data paramlist(612) /gribparam(131,159,0,19,192,'MXSALB')/
       data paramlist(613) /gribparam(131,160,2,3,192,'SOILL')/
       data paramlist(614) /gribparam(131,161,0,1,29,'ASNOW')/
       data paramlist(615) /gribparam(131,162,0,1,221,'ARAIN')/
       data paramlist(616) /gribparam(131,163,2,0,214,'GWREC')/
       data paramlist(617) /gribparam(131,164,2,0,215,'QREC')/
       data paramlist(618) /gribparam(131,165,0,1,222,'SNOWT')/
       data paramlist(619) /gribparam(131,166,0,4,200,'VBDSF')/
       data paramlist(620) /gribparam(131,167,0,4,201,'VDDSF')/
       data paramlist(621) /gribparam(131,168,0,4,202,'NBDSF')/
       data paramlist(622) /gribparam(131,169,0,4,203,'NDDSF')/
       data paramlist(623) /gribparam(131,170,0,19,193,'SNFALB')/
       data paramlist(624) /gribparam(131,171,2,3,193,'RLYRS')/
       data paramlist(625) /gribparam(131,172,0,2,193,'MFLX')/
       data paramlist(626) /gribparam(131,173,0,3,210,'LMH')/
       data paramlist(627) /gribparam(131,174,0,2,218,'LMV')/
       data paramlist(628) /gribparam(131,175,0,191,195,'MLYNO')/
       data paramlist(629) /gribparam(131,176,0,191,192,'NLAT')/
       data paramlist(630) /gribparam(131,177,0,191,193,'ELON')/
       data paramlist(631) /gribparam(131,178,0,1,23,'ICMR')/
       data paramlist(632) /gribparam(131,179,2,0,228,'ACOND')/
       data paramlist(633) /gribparam(131,180,0,1,17,'SNOAG')/
       data paramlist(634) /gribparam(131,181,2,0,199,'CCOND')/
       data paramlist(635) /gribparam(131,182,0,7,198,'LAI')/
       data paramlist(636) /gribparam(131,183,2,0,216,'SFCRH')/
       data paramlist(637) /gribparam(131,184,0,19,19,'SALBD')/
       data paramlist(638) /gribparam(131,187,2,0,217,'NDVI')/
       data paramlist(639) /gribparam(131,188,2,0,206,'RDRIP')/
       data paramlist(640) /gribparam(131,189,2,0,218,'LANDN')/
       data paramlist(641) /gribparam(131,190,0,7,8,'HLCY')/
       data paramlist(642) /gribparam(131,191,0,191,196,'NLATN')/
       data paramlist(643) /gribparam(131,192,0,191,197,'ELONN')/
       data paramlist(644) /gribparam(131,194,1,1,193,'CPOFP')/
       data paramlist(645) /gribparam(131,196,0,2,194,'USTM')/
       data paramlist(646) /gribparam(131,197,0,2,195,'VSTM')/
       data paramlist(647) /gribparam(131,198,0,1,212,'SBSNO')/
       data paramlist(648) /gribparam(131,199,2,3,198,'EVBS')/
       data paramlist(649) /gribparam(131,200,2,0,229,'EVCW')/
       data paramlist(650) /gribparam(131,202,0,1,223,'APCPN')/
       data paramlist(651) /gribparam(131,203,2,0,200,'RSMIN')/
       data paramlist(652) /gribparam(131,204,0,4,192,'DSWRF')/
       data paramlist(653) /gribparam(131,205,0,5,192,'DLWRF')/
       data paramlist(654) /gribparam(131,206,0,1,224,'ACPCPN')/
       data paramlist(655) /gribparam(131,207,2,0,194,'MSTAV')/
       data paramlist(656) /gribparam(131,208,2,0,195,'SFEXC')/
       data paramlist(657) /gribparam(131,210,2,0,230,'TRANS')/
       data paramlist(658) /gribparam(131,211,0,4,193,'USWRF')/
       data paramlist(659) /gribparam(131,212,0,5,193,'ULWRF')/
       data paramlist(660) /gribparam(131,213,0,6,192,'CDLYR')/
       data paramlist(661) /gribparam(131,214,0,1,196,'CPRAT')/
       data paramlist(662) /gribparam(131,216,0,0,193,'TTRAD')/
       data paramlist(663) /gribparam(131,218,0,3,211,'HGTN')/
       data paramlist(664) /gribparam(131,219,2,0,201,'WILT')/
       data paramlist(665) /gribparam(130,220,2,3,203,'FLDCP')/
       data paramlist(666) /gribparam(131,221,0,3,196,'HPBL')/
       data paramlist(667) /gribparam(131,222,2,3,194,'SLTYP')/
       data paramlist(668) /gribparam(131,223,2,0,196,'CNWAT')/
       data paramlist(669) /gribparam(131,224,2,3,0,'SOTYP')/
       data paramlist(670) /gribparam(131,225,2,0,198,'VGTYP')/
       data paramlist(671) /gribparam(131,226,2,0,197,'BMIXL')/
       data paramlist(672) /gribparam(131,227,2,0,219,'AMIXL')/
       data paramlist(673) /gribparam(131,228,0,1,199,'PEVAP')/
       data paramlist(674) /gribparam(131,229,0,0,192,'SNOHF')/
       data paramlist(675) /gribparam(131,230,2,3,195,'SMREF')/
       data paramlist(676) /gribparam(131,231,2,3,196,'SMDRY')/
       data paramlist(677) /gribparam(131,232,2,0,220,'WVINC')/
       data paramlist(678) /gribparam(131,233,2,0,221,'WCINC')/
       data paramlist(679) /gribparam(131,234,1,0,192,'BGRUN')/
       data paramlist(680) /gribparam(131,235,1,0,193,'SSRUN')/
       data paramlist(681) /gribparam(131,237,2,0,222,'WVCONV')/
       data paramlist(682) /gribparam(131,238,0,1,201,'SNOWC')/
       data paramlist(683) /gribparam(131,239,0,1,208,'SNOT')/
       data paramlist(684) /gribparam(131,240,2,3,197,'POROS')/
       data paramlist(685) /gribparam(131,241,2,0,223,'WCCONV')/
       data paramlist(686) /gribparam(131,242,2,0,224,'WVUFLX')/
       data paramlist(687) /gribparam(131,243,2,0,225,'WVVFLX')/
       data paramlist(688) /gribparam(131,244,2,0,226,'WCUFLX')/
       data paramlist(689) /gribparam(131,245,2,0,227,'WCVFLX')/
       data paramlist(690) /gribparam(131,246,2,0,202,'RCS')/
       data paramlist(691) /gribparam(131,247,2,0,203,'RCT')/
       data paramlist(692) /gribparam(131,248,2,0,204,'RCQ')/
       data paramlist(693) /gribparam(131,249,2,0,205,'RCSOL')/
       data paramlist(694) /gribparam(131,250,0,4,197,'SWHR')/
       data paramlist(695) /gribparam(131,251,0,5,194,'LWHR')/
       data paramlist(696) /gribparam(131,252,0,2,196,'CD')/
       data paramlist(697) /gribparam(131,253,0,2,197,'FRICV')/
       data paramlist(698) /gribparam(131,254,0,7,194,'RI')/
       data paramlist(699) /gribparam(129,62,0,1,9,'NCPCP')/
       data paramlist(700) /gribparam(129,63,0,1,10,'ACPCP')/
       data paramlist(701) /gribparam(131,220,2,3,203,'FLDCP')/
       data paramlist(702) /gribparam(129,231,0,14,200,'OZMAX1')/
       data paramlist(703) /gribparam(129,232,0,14,201,'OZMAX8')/
       data paramlist(704) /gribparam(129,240,0,16,197,'RETOP')/
       data paramlist(705) /gribparam(133,191,0,6,201,'SUNSD')/
       data paramlist(706) /gribparam(129,233,0,14,202,'PDMAX1')/
       data paramlist(707) /gribparam(129,234,0,14,203,'PDMAX24')/
       data paramlist(708) /gribparam(129,242,10,3,242,'TCSRG20')/
       data paramlist(709) /gribparam(129,243,10,3,243,'TCSRG30')/
       data paramlist(710) /gribparam(129,244,10,3,244,'TCSRG40')/
       data paramlist(711) /gribparam(129,245,10,3,245,'TCSRG50')/
       data paramlist(712) /gribparam(129,246,10,3,246,'TCSRG60')/
       data paramlist(713) /gribparam(129,247,10,3,247,'TCSRG70')/
       data paramlist(714) /gribparam(129,248,10,3,248,'TCSRG80')/
       data paramlist(715) /gribparam(129,249,10,3,249,'TCSRG90')/
       data paramlist(716) /gribparam(3,1,0,3,0,'PRES')/
       data paramlist(717) /gribparam(3,52,0,1,1,'RH')/
       data paramlist(718) /gribparam(3,63,0,1,10,'ACPCP')/
       data paramlist(719) /gribparam(3,61,0,1,8,'APCP')/
       data paramlist(720) /gribparam(3,41,0,2,10,'ABSV')/
       data paramlist(721) /gribparam(3,100,10,0,3,'HTSGW')/
       data paramlist(722) /gribparam(3,101,10,0,4,'WVDIR')/
       data paramlist(723) /gribparam(3,103,10,0,6,'WVPER')/
       data paramlist(724) /gribparam(3,104,10,0,7,'SWDIR')/
       data paramlist(725) /gribparam(3,105,10,0,8,'SWELL')/
       data paramlist(726) /gribparam(3,107,10,0,10,'DIRPW')/
       data paramlist(727) /gribparam(3,108,10,0,11,'PERPW')/
       data paramlist(728) /gribparam(3,109,10,0,12,'DIRSW')/
       data paramlist(729) /gribparam(3,110,10,0,13,'PERSW')/
       data paramlist(730) /gribparam(133,192,10,191,1,'MOSF')/
       data paramlist(731) /gribparam(130,193,0,1,225,'FRZR')/
       data paramlist(732) /gribparam(130,194,0,1,227,'FROZR')/
       data paramlist(733) /gribparam(130,195,0,1,241,'TSNOW')/
       data paramlist(734) /gribparam(130,196,2,0,7,'MTERH')/
! Added 12/06/2010
       data paramlist(735) /gribparam(128,195,10,4,4,'OVHD')/
       data paramlist(736) /gribparam(128,196,10,4,5,'OVSD')/
       data paramlist(737) /gribparam(128,197,10,4,6,'OVMD')/
       data paramlist(738) /gribparam(130,64,0,1,12,'SRWEQ')/
       data paramlist(739) /gribparam(130,241,3,192,6,'SBT112')/
       data paramlist(740) /gribparam(130,242,3,192,7,'SBT113')/
       data paramlist(741) /gribparam(130,243,3,192,8,'SBT114')/
       data paramlist(742) /gribparam(130,244,3,192,9,'SBT115')/
       data paramlist(743) /gribparam(129,235,0,16,198,'MAXREF')/
       data paramlist(744) /gribparam(129,236,0,7,199,'MXUPHL')/
       data paramlist(745) /gribparam(129,237,0,2,220,'MAXUVV')/
       data paramlist(746) /gribparam(129,238,0,2,221,'MAXDVV')/
       data paramlist(747) /gribparam(129,253,0,2,222,'MAXUW')/
       data paramlist(748) /gribparam(129,254,0,2,223,'MAXVW')/
       data paramlist(749) /gribparam(129,241,0,2,224,'VRATE')/
       data paramlist(750) /gribparam(129,250,2,4,2,'HINDEX')/
       data paramlist(751) /gribparam(129,175,0,19,234,'ICSEV')/
       data paramlist(752) /gribparam(129,176,0,19,233,'ICPRB')/
       data paramlist(753) /gribparam(2,236,0,19,217,'SIPD')/
       data paramlist(754) /gribparam(129,230,0,1,242,'RHPW')/
       data paramlist(755) /gribparam(130,206,0,15,3,'VIL')/
       data paramlist(756) /gribparam(255,255,0,20,101,'ATMTK')/
       data paramlist(757) /gribparam(255,255,0,20,102,'AOTK')/
       data paramlist(758) /gribparam(255,255,0,20,103,'SSALBK')/
       data paramlist(759) /gribparam(255,255,0,20,104,'ASYSFK')/
       data paramlist(760) /gribparam(255,255,0,20,105,'AECOEF')/
       data paramlist(761) /gribparam(255,255,0,20,106,'AACOEF')/
       data paramlist(762) /gribparam(255,255,0,20,107,'ALBSAT')/
       data paramlist(763) /gribparam(255,255,0,20,108,'ALBGRD')/
       data paramlist(764) /gribparam(255,255,0,20,109,'ALESAT')/
       data paramlist(765) /gribparam(255,255,0,20,110,'ALEGRD')/
       data paramlist(766) /gribparam(255,255,0,20,9,'WLSMFLX')/
       data paramlist(767) /gribparam(255,255,0,20,10,'WDCPMFLX')/
       data paramlist(768) /gribparam(255,255,0,20,11,'SEDMFLX')/
       data paramlist(769) /gribparam(255,255,0,20,12,'DDMFLX')/
       data paramlist(770) /gribparam(255,255,0,20,13,'TRANHH')/
       data paramlist(771) /gribparam(255,255,0,20,14,'TRSDS')/
       data paramlist(772) /gribparam(255,255,0,20,59,'ANCON')/
! Added 08/08/2013
       data paramlist(773) /gribparam(131,193,0,0,21,'APTMP')/
       data paramlist(774) /gribparam(131,137,0,17,0,'LTNGSD')/
       data paramlist(775) /gribparam(131,194,0,1,39,'CPOFP')/
! Added 03/30/2016
       data paramlist(776) /gribparam(128,144,10,3,203,'CH')/
! Added 04/28/16 for HRRR fields. NCEP hasn't updated this routine in 3 years
       data paramlist(777) /gribparam(131,207,2,0,11,'MSTAV')/
       data paramlist(778) /gribparam(129,240,0,16,3,'RETOP')/
       data paramlist(779) /gribparam(131,221,0,3,18,'HPBL')/
       data paramlist(780) /gribparam(2,131,0,7,10,'LFTX')/
       data paramlist(781) /gribparam(2,132,0,7,11,'4LFTX')/
       data paramlist(782) /gribparam(2,212,0,5,4,'ULWRF')/
       data paramlist(783) /gribparam(2,196,0,2,27,'USTM')/
       data paramlist(784) /gribparam(2,197,0,2,28,'VSTM')/
       data paramlist(785) /gribparam(129,255,0,1,74,'TCOLG')/
       data paramlist(786) /gribparam(2,140,0,1,33,'CRAIN')/
       data paramlist(787) /gribparam(2,141,0,1,34,'CFRZR')/
       data paramlist(788) /gribparam(2,142,0,1,35,'CICEP')/
       data paramlist(789) /gribparam(2,143,0,1,36,'CSNOW')/
       data paramlist(790) /gribparam(2,238,0,1,42,'SNOWC')/
       data paramlist(791) /gribparam(2,204,0,4,7,'DSWRF')/
! Added 04/6/19 for HRRR fields.
! table version, grib1 value, grib2 desc, grib2 category, grib2num, abbreviation
       data paramlist(792) /gribparam(2,255,0,1,31,'HAIL')/
       data paramlist(793) /gribparam(2,255,0,1,82,'CIMIXR')/
       data paramlist(794) /gribparam(2,253,0,2,30,'FRICV')/
       data paramlist(795) /gribparam(2,211,0,4,8,'USWRF')/
       data paramlist(796) /gribparam(2,205,0,5,3,'DLWRF')/
       data paramlist(797) /gribparam(2,255,0,7,200,'MNUPHL')/
       data paramlist(798) /gribparam(2,223,2,0,13,'CNWAT')/
       data paramlist(799) /gribparam(2,155,2,0,10,'GFLUX')/
       data paramlist(800) /gribparam(2,235,1,0,6,'SSRUN')/
       data paramlist(801) /gribparam(2,234,1,0,5,'BGRUN')/

      contains


         subroutine param_g1_to_g2(g1val,g1ver,g2disc,g2cat,g2num)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    param_g1_to_g2 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2001-06-05
!
! ABSTRACT: This subroutine returns the corresponding GRIB2 Discipline
!   Category and Number for a given GRIB1 parameter value and table version.
!
! PROGRAM HISTORY LOG:
! 2000-05-11  Gilbert
!
! USAGE:    CALL param_g1_to_g2(g1val,g1ver,g2disc,g2cat,g2num)
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
              if (paramlist(n)%grib1val.eq.g1val .AND.
     &            paramlist(n)%g1tblver.eq.g1ver ) then
                 g2disc=paramlist(n)%grib2dsc
                 g2cat=paramlist(n)%grib2cat
                 g2num=paramlist(n)%grib2num
                 return
              endif
           enddo

           print *,'param_g1_to_g2:GRIB1 param ',g1val,' not found.',
     &             ' for table version ',g1ver
           return
         end subroutine

         character(len=8) function param_get_abbrev(g2disc,g2cat,g2num)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    param_get_abbrev 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2002-01-04
!
! ABSTRACT: This function returns the parameter abbreviation for
!   a given GRIB2 Discipline, Category and Parameter number.
!
! PROGRAM HISTORY LOG:
! 2001-06-05  Gilbert
!
! USAGE:     abrev=param_get_abbrev(g2disc,g2cat,g2num)
!   INPUT ARGUMENT LIST:
!     g2disc   - GRIB2 discipline number (See Code Table 0.0)
!     g2cat    - corresponding GRIB2 Category number
!     g2num    - corresponding GRIB2 Parameter number within Category g2cat
!
! RETURNS:  ASCII Paramter Abbreviation
!
! REMARKS: None
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$
           integer,intent(in) :: g2disc,g2cat,g2num

           param_get_abbrev='UNKNOWN '

           do n=1,MAXPARAM
              if (paramlist(n)%grib2dsc.eq.g2disc.AND.
     &             paramlist(n)%grib2cat.eq.g2cat.AND.
     &             paramlist(n)%grib2num.eq.g2num) then
                 param_get_abbrev=paramlist(n)%abbrev
                 return
              endif
           enddo

!           print *,'param_get_abbrev:GRIB2 param ',g2disc,g2cat,
!     &              g2num,' not found.'
           return
         end function


         subroutine param_g2_to_g1(g2disc,g2cat,g2num,g1val,g1ver)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    param_g2_to_g1 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2002-01-04
!
! ABSTRACT: This function returns the GRIB 1 parameter number for 
!   a given GRIB2 Discipline, Category and Parameter number.
!
! PROGRAM HISTORY LOG:
! 2001-06-05  Gilbert
!
! USAGE:     call param_g2_to_g1(g2disc,g2cat,g2num,g1val,g1ver)
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

           print *,'param_g2_to_g1:GRIB2 param ',g2disc,g2cat,
     &              g2num,' not found.'
           return
         end subroutine

      end module
