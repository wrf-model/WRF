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
!
! USAGE:    use params
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      integer,parameter :: MAXPARAM=212

      type gribparam
          integer :: g1tblver
          integer :: grib1val
          integer :: grib2dsc
          integer :: grib2cat
          integer :: grib2num
          character(len=8) :: abbrev
      end type gribparam

      type(gribparam),dimension(MAXPARAM) :: paramlist

      data paramlist(1)%g1tblver /2/ 
      data paramlist(1)%grib1val /1/ 
      data paramlist(1)%grib2dsc /0/ 
      data paramlist(1)%grib2cat /3/ 
      data paramlist(1)%grib2num /0/ 
      data paramlist(1)%abbrev   /'PRES    '/ 
      data paramlist(2)%g1tblver /2/ 
      data paramlist(2)%grib1val /2/ 
      data paramlist(2)%grib2dsc /0/ 
      data paramlist(2)%grib2cat /3/ 
      data paramlist(2)%grib2num /1/ 
      data paramlist(2)%abbrev   /'PRMSL   '/ 
      data paramlist(3)%g1tblver /2/ 
      data paramlist(3)%grib1val /3/ 
      data paramlist(3)%grib2dsc /0/ 
      data paramlist(3)%grib2cat /3/ 
      data paramlist(3)%grib2num /2/ 
      data paramlist(3)%abbrev   /'PTEND   '/ 
      data paramlist(4)%g1tblver /2/ 
      data paramlist(4)%grib1val /4/ 
      data paramlist(4)%grib2dsc /0/ 
      data paramlist(4)%grib2cat /2/ 
      data paramlist(4)%grib2num /14/ 
      data paramlist(4)%abbrev   /'PVORT   '/ 
      data paramlist(5)%g1tblver /2/ 
      data paramlist(5)%grib1val /5/ 
      data paramlist(5)%grib2dsc /0/ 
      data paramlist(5)%grib2cat /3/ 
      data paramlist(5)%grib2num /3/ 
      data paramlist(5)%abbrev   /'ICAHT   '/ 
      data paramlist(6)%g1tblver /2/ 
      data paramlist(6)%grib1val /6/ 
      data paramlist(6)%grib2dsc /0/ 
      data paramlist(6)%grib2cat /3/ 
      data paramlist(6)%grib2num /4/ 
      data paramlist(6)%abbrev   /'GP      '/ 
      data paramlist(7)%g1tblver /2/ 
      data paramlist(7)%grib1val /7/ 
      data paramlist(7)%grib2dsc /0/ 
      data paramlist(7)%grib2cat /3/ 
      data paramlist(7)%grib2num /5/ 
      data paramlist(7)%abbrev   /'HGT     '/ 
      data paramlist(8)%g1tblver /2/ 
      data paramlist(8)%grib1val /8/ 
      data paramlist(8)%grib2dsc /0/ 
      data paramlist(8)%grib2cat /3/ 
      data paramlist(8)%grib2num /6/ 
      data paramlist(8)%abbrev   /'DIST    '/ 
      data paramlist(9)%g1tblver /2/ 
      data paramlist(9)%grib1val /9/ 
      data paramlist(9)%grib2dsc /0/ 
      data paramlist(9)%grib2cat /3/ 
      data paramlist(9)%grib2num /7/ 
      data paramlist(9)%abbrev   /'HSTDV   '/ 
      data paramlist(10)%g1tblver /2/ 
      data paramlist(10)%grib1val /10/ 
      data paramlist(10)%grib2dsc /0/ 
      data paramlist(10)%grib2cat /14/ 
      data paramlist(10)%grib2num /0/ 
      data paramlist(10)%abbrev   /'TOZNE   '/ 
      data paramlist(11)%g1tblver /2/ 
      data paramlist(11)%grib1val /11/ 
      data paramlist(11)%grib2dsc /0/ 
      data paramlist(11)%grib2cat /0/ 
      data paramlist(11)%grib2num /0/ 
      data paramlist(11)%abbrev   /'TMP     '/ 
      data paramlist(12)%g1tblver /2/ 
      data paramlist(12)%grib1val /12/ 
      data paramlist(12)%grib2dsc /0/ 
      data paramlist(12)%grib2cat /0/ 
      data paramlist(12)%grib2num /1/ 
      data paramlist(12)%abbrev   /'VTMP    '/ 
      data paramlist(13)%g1tblver /2/ 
      data paramlist(13)%grib1val /13/ 
      data paramlist(13)%grib2dsc /0/ 
      data paramlist(13)%grib2cat /0/ 
      data paramlist(13)%grib2num /2/ 
      data paramlist(13)%abbrev   /'POT     '/ 
      data paramlist(14)%g1tblver /2/ 
      data paramlist(14)%grib1val /14/ 
      data paramlist(14)%grib2dsc /0/ 
      data paramlist(14)%grib2cat /0/ 
      data paramlist(14)%grib2num /3/ 
      data paramlist(14)%abbrev   /'EPOT    '/ 
      data paramlist(15)%g1tblver /2/ 
      data paramlist(15)%grib1val /15/ 
      data paramlist(15)%grib2dsc /0/ 
      data paramlist(15)%grib2cat /0/ 
      data paramlist(15)%grib2num /4/ 
      data paramlist(15)%abbrev   /'T MAX   '/ 
      data paramlist(16)%g1tblver /2/ 
      data paramlist(16)%grib1val /16/ 
      data paramlist(16)%grib2dsc /0/ 
      data paramlist(16)%grib2cat /0/ 
      data paramlist(16)%grib2num /5/ 
      data paramlist(16)%abbrev   /'T MIN   '/ 
      data paramlist(17)%g1tblver /2/ 
      data paramlist(17)%grib1val /17/ 
      data paramlist(17)%grib2dsc /0/ 
      data paramlist(17)%grib2cat /0/ 
      data paramlist(17)%grib2num /6/ 
      data paramlist(17)%abbrev   /'DPT     '/ 
      data paramlist(18)%g1tblver /2/ 
      data paramlist(18)%grib1val /18/ 
      data paramlist(18)%grib2dsc /0/ 
      data paramlist(18)%grib2cat /0/ 
      data paramlist(18)%grib2num /7/ 
      data paramlist(18)%abbrev   /'DEPR    '/ 
      data paramlist(19)%g1tblver /2/ 
      data paramlist(19)%grib1val /19/ 
      data paramlist(19)%grib2dsc /0/ 
      data paramlist(19)%grib2cat /0/ 
      data paramlist(19)%grib2num /8/ 
      data paramlist(19)%abbrev   /'LAPR    '/ 
      data paramlist(20)%g1tblver /2/ 
      data paramlist(20)%grib1val /20/ 
      data paramlist(20)%grib2dsc /0/ 
      data paramlist(20)%grib2cat /19/ 
      data paramlist(20)%grib2num /0/ 
      data paramlist(20)%abbrev   /'VIS     '/ 
      data paramlist(21)%g1tblver /2/ 
      data paramlist(21)%grib1val /21/ 
      data paramlist(21)%grib2dsc /0/ 
      data paramlist(21)%grib2cat /15/ 
      data paramlist(21)%grib2num /6/ 
      data paramlist(21)%abbrev   /'RDSP1   '/ 
      data paramlist(22)%g1tblver /2/ 
      data paramlist(22)%grib1val /22/ 
      data paramlist(22)%grib2dsc /0/ 
      data paramlist(22)%grib2cat /15/ 
      data paramlist(22)%grib2num /7/ 
      data paramlist(22)%abbrev   /'RDSP2   '/ 
      data paramlist(23)%g1tblver /2/ 
      data paramlist(23)%grib1val /23/ 
      data paramlist(23)%grib2dsc /0/ 
      data paramlist(23)%grib2cat /15/ 
      data paramlist(23)%grib2num /8/ 
      data paramlist(23)%abbrev   /'RDSP3   '/ 
      data paramlist(24)%g1tblver /2/ 
      data paramlist(24)%grib1val /24/ 
      data paramlist(24)%grib2dsc /0/ 
      data paramlist(24)%grib2cat /7/ 
      data paramlist(24)%grib2num /0/ 
      data paramlist(24)%abbrev   /'PLI     '/ 
      data paramlist(25)%g1tblver /2/ 
      data paramlist(25)%grib1val /25/ 
      data paramlist(25)%grib2dsc /0/ 
      data paramlist(25)%grib2cat /0/ 
      data paramlist(25)%grib2num /9/ 
      data paramlist(25)%abbrev   /'TMP A   '/ 
      data paramlist(26)%g1tblver /2/ 
      data paramlist(26)%grib1val /26/ 
      data paramlist(26)%grib2dsc /0/ 
      data paramlist(26)%grib2cat /3/ 
      data paramlist(26)%grib2num /8/ 
      data paramlist(26)%abbrev   /'PRESA   '/ 
      data paramlist(27)%g1tblver /2/ 
      data paramlist(27)%grib1val /27/ 
      data paramlist(27)%grib2dsc /0/ 
      data paramlist(27)%grib2cat /3/ 
      data paramlist(27)%grib2num /9/ 
      data paramlist(27)%abbrev   /'GP A    '/ 
      data paramlist(28)%g1tblver /2/ 
      data paramlist(28)%grib1val /28/ 
      data paramlist(28)%grib2dsc /10/ 
      data paramlist(28)%grib2cat /0/ 
      data paramlist(28)%grib2num /0/ 
      data paramlist(28)%abbrev   /'WVSP1   '/ 
      data paramlist(29)%g1tblver /2/ 
      data paramlist(29)%grib1val /29/ 
      data paramlist(29)%grib2dsc /10/ 
      data paramlist(29)%grib2cat /0/ 
      data paramlist(29)%grib2num /1/ 
      data paramlist(29)%abbrev   /'WVSP2   '/ 
      data paramlist(30)%g1tblver /2/ 
      data paramlist(30)%grib1val /30/ 
      data paramlist(30)%grib2dsc /10/ 
      data paramlist(30)%grib2cat /0/ 
      data paramlist(30)%grib2num /2/ 
      data paramlist(30)%abbrev   /'WVSP3   '/ 
      data paramlist(31)%g1tblver /2/ 
      data paramlist(31)%grib1val /31/ 
      data paramlist(31)%grib2dsc /0/ 
      data paramlist(31)%grib2cat /2/ 
      data paramlist(31)%grib2num /0/ 
      data paramlist(31)%abbrev   /'WDIR    '/ 
      data paramlist(32)%g1tblver /2/ 
      data paramlist(32)%grib1val /32/ 
      data paramlist(32)%grib2dsc /0/ 
      data paramlist(32)%grib2cat /2/ 
      data paramlist(32)%grib2num /1/ 
      data paramlist(32)%abbrev   /'WIND    '/ 
      data paramlist(33)%g1tblver /2/ 
      data paramlist(33)%grib1val /33/ 
      data paramlist(33)%grib2dsc /0/ 
      data paramlist(33)%grib2cat /2/ 
      data paramlist(33)%grib2num /2/ 
      data paramlist(33)%abbrev   /'U GRD   '/ 
      data paramlist(34)%g1tblver /2/ 
      data paramlist(34)%grib1val /34/ 
      data paramlist(34)%grib2dsc /0/ 
      data paramlist(34)%grib2cat /2/ 
      data paramlist(34)%grib2num /3/ 
      data paramlist(34)%abbrev   /'V GRD   '/ 
      data paramlist(35)%g1tblver /2/ 
      data paramlist(35)%grib1val /35/ 
      data paramlist(35)%grib2dsc /0/ 
      data paramlist(35)%grib2cat /2/ 
      data paramlist(35)%grib2num /4/ 
      data paramlist(35)%abbrev   /'STRM    '/ 
      data paramlist(36)%g1tblver /2/ 
      data paramlist(36)%grib1val /36/ 
      data paramlist(36)%grib2dsc /0/ 
      data paramlist(36)%grib2cat /2/ 
      data paramlist(36)%grib2num /5/ 
      data paramlist(36)%abbrev   /'V POT   '/ 
      data paramlist(37)%g1tblver /2/ 
      data paramlist(37)%grib1val /37/ 
      data paramlist(37)%grib2dsc /0/ 
      data paramlist(37)%grib2cat /2/ 
      data paramlist(37)%grib2num /6/ 
      data paramlist(37)%abbrev   /'MNTSF   '/ 
      data paramlist(38)%g1tblver /2/ 
      data paramlist(38)%grib1val /38/ 
      data paramlist(38)%grib2dsc /0/ 
      data paramlist(38)%grib2cat /2/ 
      data paramlist(38)%grib2num /7/ 
      data paramlist(38)%abbrev   /'SGCVV   '/ 
      data paramlist(39)%g1tblver /2/ 
      data paramlist(39)%grib1val /39/ 
      data paramlist(39)%grib2dsc /0/ 
      data paramlist(39)%grib2cat /2/ 
      data paramlist(39)%grib2num /8/ 
      data paramlist(39)%abbrev   /'V VEL   '/ 
      data paramlist(40)%g1tblver /2/ 
      data paramlist(40)%grib1val /40/ 
      data paramlist(40)%grib2dsc /0/ 
      data paramlist(40)%grib2cat /2/ 
      data paramlist(40)%grib2num /9/ 
      data paramlist(40)%abbrev   /'DZDT    '/ 
      data paramlist(41)%g1tblver /2/ 
      data paramlist(41)%grib1val /41/ 
      data paramlist(41)%grib2dsc /0/ 
      data paramlist(41)%grib2cat /2/ 
      data paramlist(41)%grib2num /10/ 
      data paramlist(41)%abbrev   /'ABS V   '/ 
      data paramlist(42)%g1tblver /2/ 
      data paramlist(42)%grib1val /42/ 
      data paramlist(42)%grib2dsc /0/ 
      data paramlist(42)%grib2cat /2/ 
      data paramlist(42)%grib2num /11/ 
      data paramlist(42)%abbrev   /'ABS D   '/ 
      data paramlist(43)%g1tblver /2/ 
      data paramlist(43)%grib1val /43/ 
      data paramlist(43)%grib2dsc /0/ 
      data paramlist(43)%grib2cat /2/ 
      data paramlist(43)%grib2num /12/ 
      data paramlist(43)%abbrev   /'REL V   '/ 
      data paramlist(44)%g1tblver /2/ 
      data paramlist(44)%grib1val /44/ 
      data paramlist(44)%grib2dsc /0/ 
      data paramlist(44)%grib2cat /2/ 
      data paramlist(44)%grib2num /13/ 
      data paramlist(44)%abbrev   /'REL D   '/ 
      data paramlist(45)%g1tblver /2/ 
      data paramlist(45)%grib1val /45/ 
      data paramlist(45)%grib2dsc /0/ 
      data paramlist(45)%grib2cat /2/ 
      data paramlist(45)%grib2num /15/ 
      data paramlist(45)%abbrev   /'VUCSH   '/ 
      data paramlist(46)%g1tblver /2/ 
      data paramlist(46)%grib1val /46/ 
      data paramlist(46)%grib2dsc /0/ 
      data paramlist(46)%grib2cat /2/ 
      data paramlist(46)%grib2num /16/ 
      data paramlist(46)%abbrev   /'VVCSH   '/ 
      data paramlist(47)%g1tblver /2/ 
      data paramlist(47)%grib1val /47/ 
      data paramlist(47)%grib2dsc /10/ 
      data paramlist(47)%grib2cat /1/ 
      data paramlist(47)%grib2num /0/ 
      data paramlist(47)%abbrev   /'DIR C   '/ 
      data paramlist(48)%g1tblver /2/ 
      data paramlist(48)%grib1val /48/ 
      data paramlist(48)%grib2dsc /10/ 
      data paramlist(48)%grib2cat /1/ 
      data paramlist(48)%grib2num /1/ 
      data paramlist(48)%abbrev   /'SP C    '/ 
      data paramlist(49)%g1tblver /2/ 
      data paramlist(49)%grib1val /49/ 
      data paramlist(49)%grib2dsc /10/ 
      data paramlist(49)%grib2cat /1/ 
      data paramlist(49)%grib2num /2/ 
      data paramlist(49)%abbrev   /'UOGRD   '/ 
      data paramlist(50)%g1tblver /2/ 
      data paramlist(50)%grib1val /50/ 
      data paramlist(50)%grib2dsc /10/ 
      data paramlist(50)%grib2cat /1/ 
      data paramlist(50)%grib2num /3/ 
      data paramlist(50)%abbrev   /'VOGRD   '/ 
      data paramlist(51)%g1tblver /2/ 
      data paramlist(51)%grib1val /51/ 
      data paramlist(51)%grib2dsc /0/ 
      data paramlist(51)%grib2cat /1/ 
      data paramlist(51)%grib2num /0/ 
      data paramlist(51)%abbrev   /'SPF H   '/ 
      data paramlist(52)%g1tblver /2/ 
      data paramlist(52)%grib1val /52/ 
      data paramlist(52)%grib2dsc /0/ 
      data paramlist(52)%grib2cat /1/ 
      data paramlist(52)%grib2num /1/ 
      data paramlist(52)%abbrev   /'R H     '/ 
      data paramlist(53)%g1tblver /2/ 
      data paramlist(53)%grib1val /53/ 
      data paramlist(53)%grib2dsc /0/ 
      data paramlist(53)%grib2cat /1/ 
      data paramlist(53)%grib2num /2/ 
      data paramlist(53)%abbrev   /'MIXR    '/ 
      data paramlist(54)%g1tblver /2/ 
      data paramlist(54)%grib1val /54/ 
      data paramlist(54)%grib2dsc /0/ 
      data paramlist(54)%grib2cat /1/ 
      data paramlist(54)%grib2num /3/ 
      data paramlist(54)%abbrev   /'P WAT   '/ 
      data paramlist(55)%g1tblver /2/ 
      data paramlist(55)%grib1val /55/ 
      data paramlist(55)%grib2dsc /0/ 
      data paramlist(55)%grib2cat /1/ 
      data paramlist(55)%grib2num /4/ 
      data paramlist(55)%abbrev   /'VAPP    '/ 
      data paramlist(56)%g1tblver /2/ 
      data paramlist(56)%grib1val /56/ 
      data paramlist(56)%grib2dsc /0/ 
      data paramlist(56)%grib2cat /1/ 
      data paramlist(56)%grib2num /5/ 
      data paramlist(56)%abbrev   /'SAT D   '/ 
      data paramlist(57)%g1tblver /2/ 
      data paramlist(57)%grib1val /57/ 
      data paramlist(57)%grib2dsc /0/ 
      data paramlist(57)%grib2cat /1/ 
      data paramlist(57)%grib2num /6/ 
      data paramlist(57)%abbrev   /'EVP     '/ 
      data paramlist(58)%g1tblver /2/ 
      data paramlist(58)%grib1val /58/ 
      data paramlist(58)%grib2dsc /0/ 
      data paramlist(58)%grib2cat /6/ 
      data paramlist(58)%grib2num /0/ 
      data paramlist(58)%abbrev   /'C ICE   '/ 
      data paramlist(59)%g1tblver /2/ 
      data paramlist(59)%grib1val /59/ 
      data paramlist(59)%grib2dsc /0/ 
      data paramlist(59)%grib2cat /1/ 
      data paramlist(59)%grib2num /7/ 
      data paramlist(59)%abbrev   /'PRATE   '/ 
      data paramlist(60)%g1tblver /2/ 
      data paramlist(60)%grib1val /60/ 
      data paramlist(60)%grib2dsc /0/ 
      data paramlist(60)%grib2cat /19/ 
      data paramlist(60)%grib2num /2/ 
      data paramlist(60)%abbrev   /'TSTM    '/ 
      data paramlist(61)%g1tblver /2/ 
      data paramlist(61)%grib1val /61/ 
      data paramlist(61)%grib2dsc /0/ 
      data paramlist(61)%grib2cat /1/ 
      data paramlist(61)%grib2num /8/ 
      data paramlist(61)%abbrev   /'A PCP   '/ 
      data paramlist(62)%g1tblver /2/ 
      data paramlist(62)%grib1val /62/ 
      data paramlist(62)%grib2dsc /0/ 
      data paramlist(62)%grib2cat /1/ 
      data paramlist(62)%grib2num /9/ 
      data paramlist(62)%abbrev   /'NCPCP   '/ 
      data paramlist(63)%g1tblver /2/ 
      data paramlist(63)%grib1val /63/ 
      data paramlist(63)%grib2dsc /0/ 
      data paramlist(63)%grib2cat /1/ 
      data paramlist(63)%grib2num /10/ 
      data paramlist(63)%abbrev   /'ACPCP   '/ 
      data paramlist(64)%g1tblver /2/ 
      data paramlist(64)%grib1val /64/ 
      data paramlist(64)%grib2dsc /0/ 
      data paramlist(64)%grib2cat /1/ 
      data paramlist(64)%grib2num /12/ 
      data paramlist(64)%abbrev   /'SRWEQ   '/ 
      data paramlist(65)%g1tblver /2/ 
      data paramlist(65)%grib1val /65/ 
      data paramlist(65)%grib2dsc /0/ 
      data paramlist(65)%grib2cat /1/ 
      data paramlist(65)%grib2num /13/ 
      data paramlist(65)%abbrev   /'WEASD   '/ 
      data paramlist(66)%g1tblver /2/ 
      data paramlist(66)%grib1val /66/ 
      data paramlist(66)%grib2dsc /0/ 
      data paramlist(66)%grib2cat /1/ 
      data paramlist(66)%grib2num /11/ 
      data paramlist(66)%abbrev   /'SNO D   '/ 
      data paramlist(67)%g1tblver /2/ 
      data paramlist(67)%grib1val /67/ 
      data paramlist(67)%grib2dsc /0/ 
      data paramlist(67)%grib2cat /19/ 
      data paramlist(67)%grib2num /3/ 
      data paramlist(67)%abbrev   /'MIXHT   '/ 
      data paramlist(68)%g1tblver /2/ 
      data paramlist(68)%grib1val /68/ 
      data paramlist(68)%grib2dsc /10/ 
      data paramlist(68)%grib2cat /5/ 
      data paramlist(68)%grib2num /2/ 
      data paramlist(68)%abbrev   /'TTHDP   '/ 
      data paramlist(69)%g1tblver /2/ 
      data paramlist(69)%grib1val /69/ 
      data paramlist(69)%grib2dsc /10/ 
      data paramlist(69)%grib2cat /5/ 
      data paramlist(69)%grib2num /0/ 
      data paramlist(69)%abbrev   /'MTHD    '/ 
      data paramlist(70)%g1tblver /2/ 
      data paramlist(70)%grib1val /70/ 
      data paramlist(70)%grib2dsc /10/ 
      data paramlist(70)%grib2cat /5/ 
      data paramlist(70)%grib2num /1/ 
      data paramlist(70)%abbrev   /'MTH A   '/ 
      data paramlist(71)%g1tblver /2/ 
      data paramlist(71)%grib1val /71/ 
      data paramlist(71)%grib2dsc /0/ 
      data paramlist(71)%grib2cat /6/ 
      data paramlist(71)%grib2num /1/ 
      data paramlist(71)%abbrev   /'T CDC   '/ 
      data paramlist(72)%g1tblver /2/ 
      data paramlist(72)%grib1val /72/ 
      data paramlist(72)%grib2dsc /0/ 
      data paramlist(72)%grib2cat /6/ 
      data paramlist(72)%grib2num /2/ 
      data paramlist(72)%abbrev   /'CDCON   '/ 
      data paramlist(73)%g1tblver /2/ 
      data paramlist(73)%grib1val /73/ 
      data paramlist(73)%grib2dsc /0/ 
      data paramlist(73)%grib2cat /6/ 
      data paramlist(73)%grib2num /3/ 
      data paramlist(73)%abbrev   /'L CDC   '/ 
      data paramlist(74)%g1tblver /2/ 
      data paramlist(74)%grib1val /74/ 
      data paramlist(74)%grib2dsc /0/ 
      data paramlist(74)%grib2cat /6/ 
      data paramlist(74)%grib2num /4/ 
      data paramlist(74)%abbrev   /'M CDC   '/ 
      data paramlist(75)%g1tblver /2/ 
      data paramlist(75)%grib1val /75/ 
      data paramlist(75)%grib2dsc /0/ 
      data paramlist(75)%grib2cat /6/ 
      data paramlist(75)%grib2num /5/ 
      data paramlist(75)%abbrev   /'H CDC   '/ 
      data paramlist(76)%g1tblver /2/ 
      data paramlist(76)%grib1val /76/ 
      data paramlist(76)%grib2dsc /0/ 
      data paramlist(76)%grib2cat /6/ 
      data paramlist(76)%grib2num /6/ 
      data paramlist(76)%abbrev   /'C WAT   '/ 
      data paramlist(77)%g1tblver /2/ 
      data paramlist(77)%grib1val /77/ 
      data paramlist(77)%grib2dsc /0/ 
      data paramlist(77)%grib2cat /7/ 
      data paramlist(77)%grib2num /1/ 
      data paramlist(77)%abbrev   /'BLI     '/ 
      data paramlist(78)%g1tblver /2/ 
      data paramlist(78)%grib1val /78/ 
      data paramlist(78)%grib2dsc /0/ 
      data paramlist(78)%grib2cat /1/ 
      data paramlist(78)%grib2num /14/ 
      data paramlist(78)%abbrev   /'SNO C   '/ 
      data paramlist(79)%g1tblver /2/ 
      data paramlist(79)%grib1val /79/ 
      data paramlist(79)%grib2dsc /0/ 
      data paramlist(79)%grib2cat /1/ 
      data paramlist(79)%grib2num /15/ 
      data paramlist(79)%abbrev   /'SNO L   '/ 
      data paramlist(80)%g1tblver /2/ 
      data paramlist(80)%grib1val /80/ 
      data paramlist(80)%grib2dsc /10/ 
      data paramlist(80)%grib2cat /4/ 
      data paramlist(80)%grib2num /0/ 
      data paramlist(80)%abbrev   /'WTMP    '/ 
      data paramlist(81)%g1tblver /2/ 
      data paramlist(81)%grib1val /81/ 
      data paramlist(81)%grib2dsc /2/ 
      data paramlist(81)%grib2cat /0/ 
      data paramlist(81)%grib2num /0/ 
      data paramlist(81)%abbrev   /'LAND    '/ 
      data paramlist(82)%g1tblver /2/ 
      data paramlist(82)%grib1val /82/ 
      data paramlist(82)%grib2dsc /10/ 
      data paramlist(82)%grib2cat /4/ 
      data paramlist(82)%grib2num /1/ 
      data paramlist(82)%abbrev   /'DSL M   '/ 
      data paramlist(83)%g1tblver /2/ 
      data paramlist(83)%grib1val /83/ 
      data paramlist(83)%grib2dsc /2/ 
      data paramlist(83)%grib2cat /0/ 
      data paramlist(83)%grib2num /1/ 
      data paramlist(83)%abbrev   /'SFC R   '/ 
      data paramlist(84)%g1tblver /2/ 
      data paramlist(84)%grib1val /84/ 
      data paramlist(84)%grib2dsc /0/ 
      data paramlist(84)%grib2cat /19/ 
      data paramlist(84)%grib2num /1/ 
      data paramlist(84)%abbrev   /'ALBDO   '/ 
      data paramlist(85)%g1tblver /2/ 
      data paramlist(85)%grib1val /85/ 
      data paramlist(85)%grib2dsc /2/ 
      data paramlist(85)%grib2cat /0/ 
      data paramlist(85)%grib2num /2/ 
      data paramlist(85)%abbrev   /'TSOIL   '/ 
      data paramlist(86)%g1tblver /2/ 
      data paramlist(86)%grib1val /86/ 
      data paramlist(86)%grib2dsc /2/ 
      data paramlist(86)%grib2cat /0/ 
      data paramlist(86)%grib2num /3/ 
      data paramlist(86)%abbrev   /'SOIL M '/ 
      data paramlist(87)%g1tblver /2/ 
      data paramlist(87)%grib1val /87/ 
      data paramlist(87)%grib2dsc /2/ 
      data paramlist(87)%grib2cat /0/ 
      data paramlist(87)%grib2num /4/ 
      data paramlist(87)%abbrev   /'VEG    '/ 
      data paramlist(88)%g1tblver /2/ 
      data paramlist(88)%grib1val /88/ 
      data paramlist(88)%grib2dsc /10/ 
      data paramlist(88)%grib2cat /5/ 
      data paramlist(88)%grib2num /3/ 
      data paramlist(88)%abbrev   /'SALTY  '/ 
      data paramlist(89)%g1tblver /2/ 
      data paramlist(89)%grib1val /89/ 
      data paramlist(89)%grib2dsc /0/ 
      data paramlist(89)%grib2cat /3/ 
      data paramlist(89)%grib2num /10/ 
      data paramlist(89)%abbrev   /'DEN    '/ 
      data paramlist(90)%g1tblver /2/ 
      data paramlist(90)%grib1val /90/ 
      data paramlist(90)%grib2dsc /2/ 
      data paramlist(90)%grib2cat /0/ 
      data paramlist(90)%grib2num /5/ 
      data paramlist(90)%abbrev   /'WATR   '/ 
      data paramlist(91)%g1tblver /2/ 
      data paramlist(91)%grib1val /91/ 
      data paramlist(91)%grib2dsc /10/ 
      data paramlist(91)%grib2cat /2/ 
      data paramlist(91)%grib2num /0/ 
      data paramlist(91)%abbrev   /'ICE C  '/ 
      data paramlist(92)%g1tblver /2/ 
      data paramlist(92)%grib1val /92/ 
      data paramlist(92)%grib2dsc /10/ 
      data paramlist(92)%grib2cat /2/ 
      data paramlist(92)%grib2num /1/ 
      data paramlist(92)%abbrev   /'ICETK  '/ 
      data paramlist(93)%g1tblver /2/ 
      data paramlist(93)%grib1val /93/ 
      data paramlist(93)%grib2dsc /10/ 
      data paramlist(93)%grib2cat /2/ 
      data paramlist(93)%grib2num /2/ 
      data paramlist(93)%abbrev   /'DICED  '/ 
      data paramlist(94)%g1tblver /2/ 
      data paramlist(94)%grib1val /94/ 
      data paramlist(94)%grib2dsc /10/ 
      data paramlist(94)%grib2cat /2/ 
      data paramlist(94)%grib2num /3/ 
      data paramlist(94)%abbrev   /'SICED  '/ 
      data paramlist(95)%g1tblver /2/ 
      data paramlist(95)%grib1val /95/ 
      data paramlist(95)%grib2dsc /10/ 
      data paramlist(95)%grib2cat /2/ 
      data paramlist(95)%grib2num /4/ 
      data paramlist(95)%abbrev   /'U ICE  '/ 
      data paramlist(96)%g1tblver /2/ 
      data paramlist(96)%grib1val /96/ 
      data paramlist(96)%grib2dsc /10/ 
      data paramlist(96)%grib2cat /2/ 
      data paramlist(96)%grib2num /5/ 
      data paramlist(96)%abbrev   /'V ICE  '/ 
      data paramlist(97)%g1tblver /2/ 
      data paramlist(97)%grib1val /97/ 
      data paramlist(97)%grib2dsc /10/ 
      data paramlist(97)%grib2cat /2/ 
      data paramlist(97)%grib2num /6/ 
      data paramlist(97)%abbrev   /'ICE G  '/ 
      data paramlist(98)%g1tblver /2/ 
      data paramlist(98)%grib1val /98/ 
      data paramlist(98)%grib2dsc /10/ 
      data paramlist(98)%grib2cat /2/ 
      data paramlist(98)%grib2num /7/ 
      data paramlist(98)%abbrev   /'ICE D  '/ 
      data paramlist(99)%g1tblver /2/ 
      data paramlist(99)%grib1val /99/ 
      data paramlist(99)%grib2dsc /0/ 
      data paramlist(99)%grib2cat /1/ 
      data paramlist(99)%grib2num /16/ 
      data paramlist(99)%abbrev   /'SNO M  '/ 
      data paramlist(100)%g1tblver /2/ 
      data paramlist(100)%grib1val /100/ 
      data paramlist(100)%grib2dsc /10/ 
      data paramlist(100)%grib2cat /0/ 
      data paramlist(100)%grib2num /3/ 
      data paramlist(100)%abbrev   /'HTSGW  '/ 
      data paramlist(101)%g1tblver /2/ 
      data paramlist(101)%grib1val /101/ 
      data paramlist(101)%grib2dsc /10/ 
      data paramlist(101)%grib2cat /0/ 
      data paramlist(101)%grib2num /4/ 
      data paramlist(101)%abbrev   /'WVDIR  '/ 
      data paramlist(102)%g1tblver /2/ 
      data paramlist(102)%grib1val /102/ 
      data paramlist(102)%grib2dsc /10/ 
      data paramlist(102)%grib2cat /0/ 
      data paramlist(102)%grib2num /5/ 
      data paramlist(102)%abbrev   /'WVHGT  '/ 
      data paramlist(103)%g1tblver /2/ 
      data paramlist(103)%grib1val /103/ 
      data paramlist(103)%grib2dsc /10/ 
      data paramlist(103)%grib2cat /0/ 
      data paramlist(103)%grib2num /6/ 
      data paramlist(103)%abbrev   /'WVPER  '/ 
      data paramlist(104)%g1tblver /2/ 
      data paramlist(104)%grib1val /104/ 
      data paramlist(104)%grib2dsc /10/ 
      data paramlist(104)%grib2cat /0/ 
      data paramlist(104)%grib2num /7/ 
      data paramlist(104)%abbrev   /'SWDIR  '/ 
      data paramlist(105)%g1tblver /2/ 
      data paramlist(105)%grib1val /105/ 
      data paramlist(105)%grib2dsc /10/ 
      data paramlist(105)%grib2cat /0/ 
      data paramlist(105)%grib2num /8/ 
      data paramlist(105)%abbrev   /'SWELL  '/ 
      data paramlist(106)%g1tblver /2/ 
      data paramlist(106)%grib1val /106/ 
      data paramlist(106)%grib2dsc /10/ 
      data paramlist(106)%grib2cat /0/ 
      data paramlist(106)%grib2num /9/ 
      data paramlist(106)%abbrev   /'SWPER  '/ 
      data paramlist(107)%g1tblver /2/ 
      data paramlist(107)%grib1val /107/ 
      data paramlist(107)%grib2dsc /10/ 
      data paramlist(107)%grib2cat /0/ 
      data paramlist(107)%grib2num /10/ 
      data paramlist(107)%abbrev   /'DIRPW  '/ 
      data paramlist(108)%g1tblver /2/ 
      data paramlist(108)%grib1val /108/ 
      data paramlist(108)%grib2dsc /10/ 
      data paramlist(108)%grib2cat /0/ 
      data paramlist(108)%grib2num /11/ 
      data paramlist(108)%abbrev   /'PERPW  '/ 
      data paramlist(109)%g1tblver /2/ 
      data paramlist(109)%grib1val /109/ 
      data paramlist(109)%grib2dsc /10/ 
      data paramlist(109)%grib2cat /0/ 
      data paramlist(109)%grib2num /12/ 
      data paramlist(109)%abbrev   /'DIRSW  '/ 
      data paramlist(110)%g1tblver /2/ 
      data paramlist(110)%grib1val /110/ 
      data paramlist(110)%grib2dsc /10/ 
      data paramlist(110)%grib2cat /0/ 
      data paramlist(110)%grib2num /13/ 
      data paramlist(110)%abbrev   /'PERSW  '/ 
      data paramlist(111)%g1tblver /2/ 
      data paramlist(111)%grib1val /111/ 
      data paramlist(111)%grib2dsc /0/ 
      data paramlist(111)%grib2cat /4/ 
      data paramlist(111)%grib2num /0/ 
      data paramlist(111)%abbrev   /'NSWRS  '/ 
      data paramlist(112)%g1tblver /2/ 
      data paramlist(112)%grib1val /112/ 
      data paramlist(112)%grib2dsc /0/ 
      data paramlist(112)%grib2cat /5/ 
      data paramlist(112)%grib2num /0/ 
      data paramlist(112)%abbrev   /'NLWRS  '/ 
      data paramlist(113)%g1tblver /2/ 
      data paramlist(113)%grib1val /113/ 
      data paramlist(113)%grib2dsc /0/ 
      data paramlist(113)%grib2cat /4/ 
      data paramlist(113)%grib2num /1/ 
      data paramlist(113)%abbrev   /'NSWRT  '/ 
      data paramlist(114)%g1tblver /2/ 
      data paramlist(114)%grib1val /114/ 
      data paramlist(114)%grib2dsc /0/ 
      data paramlist(114)%grib2cat /5/ 
      data paramlist(114)%grib2num /1/ 
      data paramlist(114)%abbrev   /'NLWRT  '/ 
      data paramlist(115)%g1tblver /2/ 
      data paramlist(115)%grib1val /115/ 
      data paramlist(115)%grib2dsc /0/ 
      data paramlist(115)%grib2cat /5/ 
      data paramlist(115)%grib2num /2/ 
      data paramlist(115)%abbrev   /'LWAVR  '/ 
      data paramlist(116)%g1tblver /2/ 
      data paramlist(116)%grib1val /116/ 
      data paramlist(116)%grib2dsc /0/ 
      data paramlist(116)%grib2cat /4/ 
      data paramlist(116)%grib2num /2/ 
      data paramlist(116)%abbrev   /'SWAVR  '/ 
      data paramlist(117)%g1tblver /2/ 
      data paramlist(117)%grib1val /117/ 
      data paramlist(117)%grib2dsc /0/ 
      data paramlist(117)%grib2cat /4/ 
      data paramlist(117)%grib2num /3/ 
      data paramlist(117)%abbrev   /'G RAD  '/ 
      data paramlist(118)%g1tblver /2/ 
      data paramlist(118)%grib1val /118/ 
      data paramlist(118)%grib2dsc /0/ 
      data paramlist(118)%grib2cat /4/ 
      data paramlist(118)%grib2num /4/ 
      data paramlist(118)%abbrev   /'BRTMP  '/ 
      data paramlist(119)%g1tblver /2/ 
      data paramlist(119)%grib1val /119/ 
      data paramlist(119)%grib2dsc /0/ 
      data paramlist(119)%grib2cat /4/ 
      data paramlist(119)%grib2num /5/ 
      data paramlist(119)%abbrev   /'LWRAD  '/ 
      data paramlist(120)%g1tblver /2/ 
      data paramlist(120)%grib1val /120/ 
      data paramlist(120)%grib2dsc /0/ 
      data paramlist(120)%grib2cat /4/ 
      data paramlist(120)%grib2num /6/ 
      data paramlist(120)%abbrev   /'SWRAD  '/ 
      data paramlist(121)%g1tblver /2/ 
      data paramlist(121)%grib1val /121/ 
      data paramlist(121)%grib2dsc /0/ 
      data paramlist(121)%grib2cat /0/ 
      data paramlist(121)%grib2num /10/ 
      data paramlist(121)%abbrev   /'LHTFL  '/ 
      data paramlist(122)%g1tblver /2/ 
      data paramlist(122)%grib1val /122/ 
      data paramlist(122)%grib2dsc /0/ 
      data paramlist(122)%grib2cat /0/ 
      data paramlist(122)%grib2num /11/ 
      data paramlist(122)%abbrev   /'SHTFL  '/ 
      data paramlist(123)%g1tblver /2/ 
      data paramlist(123)%grib1val /123/ 
      data paramlist(123)%grib2dsc /0/ 
      data paramlist(123)%grib2cat /2/ 
      data paramlist(123)%grib2num /20/ 
      data paramlist(123)%abbrev   /'BLYDP  '/ 
      data paramlist(124)%g1tblver /2/ 
      data paramlist(124)%grib1val /124/ 
      data paramlist(124)%grib2dsc /0/ 
      data paramlist(124)%grib2cat /2/ 
      data paramlist(124)%grib2num /17/ 
      data paramlist(124)%abbrev   /'U FLX  '/ 
      data paramlist(125)%g1tblver /2/ 
      data paramlist(125)%grib1val /125/ 
      data paramlist(125)%grib2dsc /0/ 
      data paramlist(125)%grib2cat /2/ 
      data paramlist(125)%grib2num /18/ 
      data paramlist(125)%abbrev   /'V FLX  '/ 
      data paramlist(126)%g1tblver /2/ 
      data paramlist(126)%grib1val /126/ 
      data paramlist(126)%grib2dsc /0/ 
      data paramlist(126)%grib2cat /2/ 
      data paramlist(126)%grib2num /19/ 
      data paramlist(126)%abbrev   /'WMIXE  '/ 
      data paramlist(127)%g1tblver /2/ 
      data paramlist(127)%grib1val /127/ 
      data paramlist(127)%grib2dsc /255/ 
      data paramlist(127)%grib2cat /255/ 
      data paramlist(127)%grib2num /255/ 
      data paramlist(127)%abbrev   /'IMG D  '/ 
!
!  GRIB1 parameters in NCEP Local Table version 2
!  Added 8/07/2003
!
      data paramlist(128)%g1tblver /2/ 
      data paramlist(128)%grib1val /229/ 
      data paramlist(128)%grib2dsc /0/ 
      data paramlist(128)%grib2cat /0/ 
      data paramlist(128)%grib2num /192/ 
      data paramlist(128)%abbrev   /'SNOHF  '/ 
      data paramlist(129)%g1tblver /2/ 
      data paramlist(129)%grib1val /153/ 
      data paramlist(129)%grib2dsc /0/ 
      data paramlist(129)%grib2cat /1/ 
      data paramlist(129)%grib2num /22/ 
      data paramlist(129)%abbrev   /'CLWMR  '/ 
      data paramlist(130)%g1tblver /2/ 
      data paramlist(130)%grib1val /140/ 
      data paramlist(130)%grib2dsc /0/ 
      data paramlist(130)%grib2cat /1/ 
      data paramlist(130)%grib2num /192/ 
      data paramlist(130)%abbrev   /'CRAIN  '/ 
      data paramlist(131)%g1tblver /2/ 
      data paramlist(131)%grib1val /141/ 
      data paramlist(131)%grib2dsc /0/ 
      data paramlist(131)%grib2cat /1/ 
      data paramlist(131)%grib2num /193/ 
      data paramlist(131)%abbrev   /'CFRZR  '/ 
      data paramlist(132)%g1tblver /2/ 
      data paramlist(132)%grib1val /142/ 
      data paramlist(132)%grib2dsc /0/ 
      data paramlist(132)%grib2cat /1/ 
      data paramlist(132)%grib2num /194/ 
      data paramlist(132)%abbrev   /'CICEP  '/ 
      data paramlist(133)%g1tblver /2/ 
      data paramlist(133)%grib1val /143/ 
      data paramlist(133)%grib2dsc /0/ 
      data paramlist(133)%grib2cat /1/ 
      data paramlist(133)%grib2num /195/ 
      data paramlist(133)%abbrev   /'CSNOW  '/ 
      data paramlist(134)%g1tblver /2/ 
      data paramlist(134)%grib1val /214/ 
      data paramlist(134)%grib2dsc /0/ 
      data paramlist(134)%grib2cat /1/ 
      data paramlist(134)%grib2num /196/ 
      data paramlist(134)%abbrev   /'CPRAT  '/ 
      data paramlist(135)%g1tblver /2/ 
      data paramlist(135)%grib1val /135/ 
      data paramlist(135)%grib2dsc /0/ 
      data paramlist(135)%grib2cat /1/ 
      data paramlist(135)%grib2num /197/ 
      data paramlist(135)%abbrev   /'MCONV  '/ 
      data paramlist(136)%g1tblver /2/ 
      data paramlist(136)%grib1val /194/ 
      data paramlist(136)%grib2dsc /1/ 
      data paramlist(136)%grib2cat /1/ 
      data paramlist(136)%grib2num /193/ 
      data paramlist(136)%abbrev   /'CPOFP  '/ 
      data paramlist(137)%g1tblver /2/ 
      data paramlist(137)%grib1val /228/ 
      data paramlist(137)%grib2dsc /0/ 
      data paramlist(137)%grib2cat /1/ 
      data paramlist(137)%grib2num /199/ 
      data paramlist(137)%abbrev   /'PEVAP  '/ 
      data paramlist(138)%g1tblver /2/ 
      data paramlist(138)%grib1val /136/ 
      data paramlist(138)%grib2dsc /0/ 
      data paramlist(138)%grib2cat /2/ 
      data paramlist(138)%grib2num /192/ 
      data paramlist(138)%abbrev   /'VW SH  '/ 
      data paramlist(139)%g1tblver /2/ 
      data paramlist(139)%grib1val /172/ 
      data paramlist(139)%grib2dsc /0/ 
      data paramlist(139)%grib2cat /2/ 
      data paramlist(139)%grib2num /193/ 
      data paramlist(139)%abbrev   /'M FLX  '/ 
      data paramlist(140)%g1tblver /2/ 
      data paramlist(140)%grib1val /196/ 
      data paramlist(140)%grib2dsc /0/ 
      data paramlist(140)%grib2cat /2/ 
      data paramlist(140)%grib2num /194/ 
      data paramlist(140)%abbrev   /'USTM   '/ 
      data paramlist(141)%g1tblver /2/ 
      data paramlist(141)%grib1val /197/ 
      data paramlist(141)%grib2dsc /0/ 
      data paramlist(141)%grib2cat /2/ 
      data paramlist(141)%grib2num /195/ 
      data paramlist(141)%abbrev   /'VSTM   '/ 
      data paramlist(142)%g1tblver /2/ 
      data paramlist(142)%grib1val /252/ 
      data paramlist(142)%grib2dsc /0/ 
      data paramlist(142)%grib2cat /2/ 
      data paramlist(142)%grib2num /196/ 
      data paramlist(142)%abbrev   /'CD     '/ 
      data paramlist(143)%g1tblver /2/ 
      data paramlist(143)%grib1val /253/ 
      data paramlist(143)%grib2dsc /0/ 
      data paramlist(143)%grib2cat /2/ 
      data paramlist(143)%grib2num /197/ 
      data paramlist(143)%abbrev   /'FRICV  '/ 
      data paramlist(144)%g1tblver /2/ 
      data paramlist(144)%grib1val /130/ 
      data paramlist(144)%grib2dsc /0/ 
      data paramlist(144)%grib2cat /3/ 
      data paramlist(144)%grib2num /192/ 
      data paramlist(144)%abbrev   /'MSLET  '/ 
      data paramlist(145)%g1tblver /2/ 
      data paramlist(145)%grib1val /204/ 
      data paramlist(145)%grib2dsc /0/ 
      data paramlist(145)%grib2cat /4/ 
      data paramlist(145)%grib2num /192/ 
      data paramlist(145)%abbrev   /'DSWRF  '/ 
      data paramlist(146)%g1tblver /2/ 
      data paramlist(146)%grib1val /211/ 
      data paramlist(146)%grib2dsc /0/ 
      data paramlist(146)%grib2cat /4/ 
      data paramlist(146)%grib2num /193/ 
      data paramlist(146)%abbrev   /'USWRF  '/ 
      data paramlist(147)%g1tblver /2/ 
      data paramlist(147)%grib1val /205/ 
      data paramlist(147)%grib2dsc /0/ 
      data paramlist(147)%grib2cat /5/ 
      data paramlist(147)%grib2num /192/ 
      data paramlist(147)%abbrev   /'DLWRF  '/ 
      data paramlist(148)%g1tblver /2/ 
      data paramlist(148)%grib1val /212/ 
      data paramlist(148)%grib2dsc /0/ 
      data paramlist(148)%grib2cat /5/ 
      data paramlist(148)%grib2num /193/ 
      data paramlist(148)%abbrev   /'ULWRF  '/ 
      data paramlist(149)%g1tblver /2/ 
      data paramlist(149)%grib1val /213/ 
      data paramlist(149)%grib2dsc /0/ 
      data paramlist(149)%grib2cat /6/ 
      data paramlist(149)%grib2num /192/ 
      data paramlist(149)%abbrev   /'CDLYR  '/ 
      data paramlist(150)%g1tblver /2/ 
      data paramlist(150)%grib1val /132/ 
      data paramlist(150)%grib2dsc /0/ 
      data paramlist(150)%grib2cat /7/ 
      data paramlist(150)%grib2num /193/ 
      data paramlist(150)%abbrev   /'4LFTX  '/ 
      data paramlist(151)%g1tblver /2/ 
      data paramlist(151)%grib1val /157/ 
      data paramlist(151)%grib2dsc /0/ 
      data paramlist(151)%grib2cat /7/ 
      data paramlist(151)%grib2num /6/ 
      data paramlist(151)%abbrev   /'CAPE   '/ 
      data paramlist(152)%g1tblver /2/ 
      data paramlist(152)%grib1val /156/ 
      data paramlist(152)%grib2dsc /0/ 
      data paramlist(152)%grib2cat /7/ 
      data paramlist(152)%grib2num /7/ 
      data paramlist(152)%abbrev   /'CIN    '/ 
      data paramlist(153)%g1tblver /2/ 
      data paramlist(153)%grib1val /190/ 
      data paramlist(153)%grib2dsc /0/ 
      data paramlist(153)%grib2cat /7/ 
      data paramlist(153)%grib2num /8/ 
      data paramlist(153)%abbrev   /'HLCY   '/ 
      data paramlist(154)%g1tblver /2/ 
      data paramlist(154)%grib1val /131/ 
      data paramlist(154)%grib2dsc /0/ 
      data paramlist(154)%grib2cat /7/ 
      data paramlist(154)%grib2num /192/ 
      data paramlist(154)%abbrev   /'LFT X  '/ 
      data paramlist(155)%g1tblver /2/ 
      data paramlist(155)%grib1val /158/ 
      data paramlist(155)%grib2dsc /0/ 
      data paramlist(155)%grib2cat /19/ 
      data paramlist(155)%grib2num /11/ 
      data paramlist(155)%abbrev   /'TKE    '/ 
      data paramlist(156)%g1tblver /2/ 
      data paramlist(156)%grib1val /176/ 
      data paramlist(156)%grib2dsc /0/ 
      data paramlist(156)%grib2cat /191/ 
      data paramlist(156)%grib2num /192/ 
      data paramlist(156)%abbrev   /'NLAT   '/ 
      data paramlist(157)%g1tblver /2/ 
      data paramlist(157)%grib1val /177/ 
      data paramlist(157)%grib2dsc /0/ 
      data paramlist(157)%grib2cat /191/ 
      data paramlist(157)%grib2num /193/ 
      data paramlist(157)%abbrev   /'ELON   '/ 
      data paramlist(158)%g1tblver /2/ 
      data paramlist(158)%grib1val /234/ 
      data paramlist(158)%grib2dsc /1/ 
      data paramlist(158)%grib2cat /0/ 
      data paramlist(158)%grib2num /192/ 
      data paramlist(158)%abbrev   /'BGRUN  '/ 
      data paramlist(159)%g1tblver /2/ 
      data paramlist(159)%grib1val /235/ 
      data paramlist(159)%grib2dsc /1/ 
      data paramlist(159)%grib2cat /0/ 
      data paramlist(159)%grib2num /193/ 
      data paramlist(159)%abbrev   /'SSRUN  '/ 
      data paramlist(160)%g1tblver /2/ 
      data paramlist(160)%grib1val /144/ 
      data paramlist(160)%grib2dsc /2/ 
      data paramlist(160)%grib2cat /0/ 
      data paramlist(160)%grib2num /192/ 
      data paramlist(160)%abbrev   /'SOILW  '/ 
      data paramlist(161)%g1tblver /2/ 
      data paramlist(161)%grib1val /155/ 
      data paramlist(161)%grib2dsc /2/ 
      data paramlist(161)%grib2cat /0/ 
      data paramlist(161)%grib2num /193/ 
      data paramlist(161)%abbrev   /'GFLUX  '/ 
      data paramlist(162)%g1tblver /2/ 
      data paramlist(162)%grib1val /207/ 
      data paramlist(162)%grib2dsc /2/ 
      data paramlist(162)%grib2cat /0/ 
      data paramlist(162)%grib2num /194/ 
      data paramlist(162)%abbrev   /'MSTAV  '/ 
      data paramlist(163)%g1tblver /2/ 
      data paramlist(163)%grib1val /208/ 
      data paramlist(163)%grib2dsc /2/ 
      data paramlist(163)%grib2cat /0/ 
      data paramlist(163)%grib2num /195/ 
      data paramlist(163)%abbrev   /'SFEXC  '/ 
      data paramlist(164)%g1tblver /2/ 
      data paramlist(164)%grib1val /223/ 
      data paramlist(164)%grib2dsc /2/ 
      data paramlist(164)%grib2cat /0/ 
      data paramlist(164)%grib2num /196/ 
      data paramlist(164)%abbrev   /'CNWAT  '/ 
      data paramlist(165)%g1tblver /2/ 
      data paramlist(165)%grib1val /226/ 
      data paramlist(165)%grib2dsc /2/ 
      data paramlist(165)%grib2cat /0/ 
      data paramlist(165)%grib2num /197/ 
      data paramlist(165)%abbrev   /'BMIXL  '/ 
      data paramlist(166)%g1tblver /2/ 
      data paramlist(166)%grib1val /154/ 
      data paramlist(166)%grib2dsc /0/ 
      data paramlist(166)%grib2cat /14/ 
      data paramlist(166)%grib2num /192/ 
      data paramlist(166)%abbrev   /'O3MR   '/ 
      data paramlist(167)%g1tblver /2/ 
      data paramlist(167)%grib1val /222/ 
      data paramlist(167)%grib2dsc /0/ 
      data paramlist(167)%grib2cat /3/ 
      data paramlist(167)%grib2num /193/ 
      data paramlist(167)%abbrev   /'5WAVH  '/ 
      data paramlist(168)%g1tblver /2/ 
      data paramlist(168)%grib1val /145/ 
      data paramlist(168)%grib2dsc /0/ 
      data paramlist(168)%grib2cat /1/ 
      data paramlist(168)%grib2num /200/ 
      data paramlist(168)%abbrev   /'PEVPR  '/ 
      data paramlist(169)%g1tblver /2/ 
      data paramlist(169)%grib1val /146/ 
      data paramlist(169)%grib2dsc /0/ 
      data paramlist(169)%grib2cat /6/ 
      data paramlist(169)%grib2num /193/ 
      data paramlist(169)%abbrev   /'CWORK  '/ 
      data paramlist(170)%g1tblver /2/ 
      data paramlist(170)%grib1val /147/ 
      data paramlist(170)%grib2dsc /0/ 
      data paramlist(170)%grib2cat /3/ 
      data paramlist(170)%grib2num /194/ 
      data paramlist(170)%abbrev   /'U-GWD  '/ 
      data paramlist(171)%g1tblver /2/ 
      data paramlist(171)%grib1val /148/ 
      data paramlist(171)%grib2dsc /0/ 
      data paramlist(171)%grib2cat /3/ 
      data paramlist(171)%grib2num /195/ 
      data paramlist(171)%abbrev   /'V-GWD  '/ 
      data paramlist(172)%g1tblver /2/ 
      data paramlist(172)%grib1val /221/ 
      data paramlist(172)%grib2dsc /0/ 
      data paramlist(172)%grib2cat /3/ 
      data paramlist(172)%grib2num /196/ 
      data paramlist(172)%abbrev   /'HPBL   '/ 
      data paramlist(173)%g1tblver /2/ 
      data paramlist(173)%grib1val /230/ 
      data paramlist(173)%grib2dsc /0/ 
      data paramlist(173)%grib2cat /3/ 
      data paramlist(173)%grib2num /197/ 
      data paramlist(173)%abbrev   /'5WAVA  '/ 
! Added 9/26/2003
      data paramlist(174) /gribparam(130,160,2,3,192,'SOILL   ')/
      data paramlist(175) /gribparam(130,171,2,3,193,'UNKNOWN ')/
      data paramlist(176) /gribparam(130,219,2,0,201,'WILT    ')/
      data paramlist(177) /gribparam(130,222,2,3,194,'SLTYP   ')/
      data paramlist(178) /gribparam(2,224,2,3,0,'SOTYP   ')/
      data paramlist(179) /gribparam(2,225,2,0,198,'VGTYP   ')/
      data paramlist(180) /gribparam(130,230,2,3,195,'SMREF   ')/
      data paramlist(181) /gribparam(130,231,2,3,196,'SMDRY   ')/
      data paramlist(182) /gribparam(2,238,0,1,201,'SNOWC   ')/
      data paramlist(183) /gribparam(130,240,2,3,197,'POROS   ')/
      data paramlist(184) /gribparam(129,131,0,1,202,'FRAIN   ')/
      data paramlist(185) /gribparam(129,132,0,6,199,'FICE    ')/
      data paramlist(186) /gribparam(129,133,0,1,203,'FRIME   ')/
      data paramlist(187) /gribparam(129,134,0,6,194,'CUEFI   ')/
      data paramlist(188) /gribparam(129,135,0,6,195,'TCOND   ')/
      data paramlist(189) /gribparam(129,136,0,6,196,'TCOLW   ')/
      data paramlist(190) /gribparam(129,137,0,6,197,'TCOLI   ')/
      data paramlist(191) /gribparam(129,138,0,1,204,'TCOLR   ')/
      data paramlist(192) /gribparam(129,139,0,1,205,'TCOLS   ')/
      data paramlist(193) /gribparam(129,140,0,6,198,'TCOLC   ')/
      data paramlist(194) /gribparam(130,159,0,19,192,'MXSALB  ')/
      data paramlist(195) /gribparam(130,170,0,19,193,'SNFALB  ')/
      data paramlist(196) /gribparam(2,170,0,1,24,'RWMR    ')/
      data paramlist(197) /gribparam(2,171,0,1,25,'SNMR    ')/
      data paramlist(198) /gribparam(130,181,2,0,199,'CCOND   ')/
      data paramlist(199) /gribparam(130,203,2,0,200,'RSMIN   ')/
      data paramlist(200) /gribparam(130,246,2,0,202,'RCS     ')/
      data paramlist(201) /gribparam(130,247,2,0,203,'RCT     ')/
      data paramlist(202) /gribparam(130,248,2,0,204,'RCQ     ')/
      data paramlist(203) /gribparam(130,249,2,0,205,'RCSOL   ')/
      data paramlist(204) /gribparam(2,254,0,7,194,'RI      ')/
      data paramlist(205) /gribparam(129,190,3,1,192,'USCT    ')/
      data paramlist(206) /gribparam(129,191,3,1,193,'VSCT    ')/
      data paramlist(207) /gribparam(129,171,0,191,194,'TSEC    ')/
      data paramlist(208) /gribparam(129,180,0,14,193,'OZCON   ')/
      data paramlist(209) /gribparam(129,181,0,14,194,'OZCAT   ')/
      data paramlist(210) /gribparam(2,193,1,1,2,'POP     ')/
      data paramlist(211) /gribparam(2,195,1,1,192,'CPOZP   ')/
      data paramlist(212) /gribparam(2,180,0,2,22,'GUST    ')/


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

