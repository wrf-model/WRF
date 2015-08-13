!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! CBMZ module: see module_cbmz.F for information and terms of use
!**********************************************************************************  
      module module_cbmz_rodas_prep


      contains


!-----------------------------------------------------------------------
!   cbmz_v02r01_torodas.f - created on 18-nov-2003 from previous
!        cbmz_v02r01_torodas.f        cbmz_v02r01_mapconcs.f
!        cbmz_v02r01_maprates.f       cbmz_v02r01_dydt.f
!        cbmz_v02r01_jacob.f          cbmz_v02r01_decomp.f
!        cbmz_v02r01_solve.f
!   so now everything is in a single file
!-----------------------------------------------------------------------

      subroutine cbmz_v02r01_torodas(   &
          ngas, taa, tzz,   &
          stot, atol, rtol, yposlimit, yneglimit,   &
          sfixedkpp, rconstkpp,   &
          hmin, hstart,   &
          info_rodas, iok, lunerr, idydt_sngldble )
!
!   interfaces to rodas3 solver formechanism-version-regime =cbmz_v02r01
!
!   *** do not include any pegasus common blocks ***
!
      use module_data_cbmz
      use module_cbmz_rodas3_solver, only:  rodas3_ff_x2
      implicit none

!   subr parameters
      integer ngas, iok, lunerr, idydt_sngldble
      integer info_rodas(6)
      real taa, tzz, hmin, hstart
      real stot(ngas), atol(ngas), rtol(ngas)
      real yposlimit(ngas), yneglimit(ngas)
      real sfixedkpp(nfixed_kppmax), rconstkpp(nreact_kppmax)

!   local variables

!     external cbmz_v02r01_dydt
!     external cbmz_v02r01_jacob
!     external cbmz_v02r01_decomp
!     external cbmz_v02r01_solve

      integer i

      real hmax

      integer lu_crow_v(nvar_r01_kpp + 1)
      save    lu_crow_v
      integer lu_diag_v(nvar_r01_kpp + 1)
      save    lu_diag_v
      integer lu_icol_v(lu_nonzero_v_r01_kpp)
      save    lu_icol_v

      data lu_icol_v /   &
        1,  4, 25,  2, 22, 24,  3, 26,  4, 25,  5, 25,   &
        6, 24, 25,  7, 19, 25,  8, 22, 28,  9, 23, 28,   &
       10, 21, 25, 11, 18, 20, 23, 25, 12, 24, 25, 28,   &
       13, 24, 25, 27, 28, 14, 21, 24, 25, 15, 19, 24,   &
       25,  3, 16, 23, 26, 27, 28,  9, 17, 18, 20, 23,   &
       24, 25, 28, 10, 14, 18, 21, 23, 24, 25, 27,  7,   &
       15, 19, 23, 24, 25, 27,  5, 15, 19, 20, 23, 24,   &
       25, 27, 14, 20, 21, 22, 23, 24, 25, 27,  8, 20,   &
       22, 23, 24, 25, 27, 28,  9, 16, 17, 18, 19, 20,   &
       21, 22, 23, 24, 25, 26, 27, 28,  4,  5,  6, 10,   &
       11, 12, 14, 15, 18, 19, 20, 21, 22, 23, 24, 25,   &
       26, 27, 28,  3,  4,  5,  6,  7, 10, 11, 12, 13,   &
       14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,   &
       27, 28, 16, 22, 23, 24, 25, 26, 27, 28, 13, 16,   &
       19, 21, 22, 23, 24, 25, 26, 27, 28,  8,  9, 12,   &
       13, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,   &
       27, 28 /

      data lu_crow_v /   &
        1,  4,  7,  9, 11, 13, 16, 19, 22, 25, 28, 33,   &
       37, 42, 46, 50, 56, 64, 72, 79, 87, 95,103,117,   &
      136,159,167,178,195 /

      data lu_diag_v /   &
        1,  4,  7,  9, 11, 13, 16, 19, 22, 25, 28, 33,   &
       37, 42, 46, 51, 57, 66, 74, 82, 89, 97,111,131,   &
      155,164,176,194,195 /



      info_rodas(1) = 1
      do i = 2, 6
          info_rodas(i) = 0
      end do
      hmax = tzz - taa

!   do not integrate if hmax is less/equal to hmin
!   because hmin is generally 0.1 s or less
      if (hmax .le. 1.001*hmin) then
          iok = 11
          return
      end if

      call rodas3_ff_x2(   &
           nvar_r01_kpp, taa, tzz, hmin, hmax, hstart,   &
           stot, atol, rtol, yposlimit, yneglimit,   &
           sfixedkpp, rconstkpp,   &
           lu_nonzero_v_r01_kpp, lu_crow_v, lu_diag_v, lu_icol_v,   &
           info_rodas, iok, lunerr,   &
           cbmz_v02r01_dydt,   &
           cbmz_v02r01_jacob,   &
           cbmz_v02r01_decomp,   &
           cbmz_v02r01_solve )

      return
      end subroutine cbmz_v02r01_torodas 


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r01_mapconcs( imap, nyy, yy, yyfixed, cbox )
!
!   maps species concentrations (gaschemistry cbox array <--> kpp yy array)
!   for mechanism-version-regime = cbmz_v02r01
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     imap = mapping direction flag [input]
!         0 = map cbox --> yy and yyfixed
!         1 = map yy --> cbox
      integer imap
!     nyy = number of kpp "variable" species [output]
      integer nyy
!     yy = kpp species concentrations array [input/output]
      real yy(nvar_r01_kpp)
!     yyfixed = kpp species concentrations array [input/output]
      real yyfixed(nfixed_kppmax)
!     cbox = main gaschemistry species conc array [input/output]
      real cbox(ngas_z)

!   local variables
      integer ih2so4_kpp
      parameter ( ih2so4_kpp = 1 )
      integer ircooh_kpp
      parameter ( ircooh_kpp = 2 )
      integer io1d_kpp
      parameter ( io1d_kpp = 3 )
      integer iso2_kpp
      parameter ( iso2_kpp = 4 )
      integer ic2h5oh_kpp
      parameter ( ic2h5oh_kpp = 5 )
      integer ih2o2_kpp
      parameter ( ih2o2_kpp = 6 )
      integer ic2h6_kpp
      parameter ( ic2h6_kpp = 7 )
      integer ipan_kpp
      parameter ( ipan_kpp = 8 )
      integer in2o5_kpp
      parameter ( in2o5_kpp = 9 )
      integer ich3oh_kpp
      parameter ( ich3oh_kpp = 10 )
      integer ico_kpp
      parameter ( ico_kpp = 11 )
      integer ihno4_kpp
      parameter ( ihno4_kpp = 12 )
      integer ihono_kpp
      parameter ( ihono_kpp = 13 )
      integer ich3ooh_kpp
      parameter ( ich3ooh_kpp = 14 )
      integer iethooh_kpp
      parameter ( iethooh_kpp = 15 )
      integer io3p_kpp
      parameter ( io3p_kpp = 16 )
      integer ihno3_kpp
      parameter ( ihno3_kpp = 17 )
      integer ihcho_kpp
      parameter ( ihcho_kpp = 18 )
      integer iethp_kpp
      parameter ( iethp_kpp = 19 )
      integer iald2_kpp
      parameter ( iald2_kpp = 20 )
      integer ich3o2_kpp
      parameter ( ich3o2_kpp = 21 )
      integer ic2o3_kpp
      parameter ( ic2o3_kpp = 22 )
      integer ino3_kpp
      parameter ( ino3_kpp = 23 )
      integer iho2_kpp
      parameter ( iho2_kpp = 24 )
      integer ioh_kpp
      parameter ( ioh_kpp = 25 )
      integer io3_kpp
      parameter ( io3_kpp = 26 )
      integer ino_kpp
      parameter ( ino_kpp = 27 )
      integer ino2_kpp
      parameter ( ino2_kpp = 28 )

! indexes declaration for fixed species
      integer ich4_kpp
      parameter ( ich4_kpp = 1 )
      integer ih2o_kpp
      parameter ( ih2o_kpp = 2 )
      integer ih2_kpp
      parameter ( ih2_kpp = 3 )
      integer io2_kpp
      parameter ( io2_kpp = 4 )
      integer in2_kpp
      parameter ( in2_kpp = 5 )



      nyy = nvar_r01_kpp

      if (imap .le. 0) goto 1000
      if (imap .ge. 1) goto 2000


!
!   map cbox values into yyvarkpp and yyfixkpp
!
1000  continue
      yy(ih2so4_kpp)	= cbox(ih2so4_z)
      yy(ircooh_kpp)	= cbox(ircooh_z)
      yy(io1d_kpp)	= cbox(io1d_z)
      yy(iso2_kpp)	= cbox(iso2_z)
      yy(ic2h5oh_kpp)	= cbox(ic2h5oh_z)
      yy(ih2o2_kpp)	= cbox(ih2o2_z)
      yy(ic2h6_kpp)	= cbox(ic2h6_z)
      yy(ipan_kpp)	= cbox(ipan_z)
      yy(in2o5_kpp)	= cbox(in2o5_z)
      yy(ich3oh_kpp)	= cbox(ich3oh_z)
      yy(ico_kpp)	= cbox(ico_z)
      yy(ihno4_kpp)	= cbox(ihno4_z)
      yy(ihono_kpp)	= cbox(ihono_z)
      yy(ich3ooh_kpp)	= cbox(ich3ooh_z)
      yy(iethooh_kpp)	= cbox(iethooh_z)
      yy(io3p_kpp)	= cbox(io3p_z)
      yy(ihno3_kpp)	= cbox(ihno3_z)
      yy(ihcho_kpp)	= cbox(ihcho_z)
      yy(iethp_kpp)	= cbox(iethp_z)
      yy(iald2_kpp)	= cbox(iald2_z)
      yy(ich3o2_kpp)	= cbox(ich3o2_z)
      yy(ic2o3_kpp)	= cbox(ic2o3_z)
      yy(ino3_kpp)	= cbox(ino3_z)
      yy(iho2_kpp)	= cbox(iho2_z)
      yy(ioh_kpp)	= cbox(ioh_z)
      yy(io3_kpp)	= cbox(io3_z)
      yy(ino_kpp)	= cbox(ino_z)
      yy(ino2_kpp)	= cbox(ino2_z)

      yyfixed(ich4_kpp) = cbox(ich4_z)
      yyfixed(ih2o_kpp) = cbox(ih2o_z)
      yyfixed(ih2_kpp) = cbox(ih2_z)
      yyfixed(io2_kpp) = cbox(io2_z)
      yyfixed(in2_kpp) = cbox(in2_z)

!
!   map yyvarkpp values into cbox
!
2000  continue
      cbox(ih2so4_z)	= yy(ih2so4_kpp)
      cbox(ircooh_z)	= yy(ircooh_kpp)
      cbox(io1d_z)	= yy(io1d_kpp)
      cbox(iso2_z)	= yy(iso2_kpp)
      cbox(ic2h5oh_z)	= yy(ic2h5oh_kpp)
      cbox(ih2o2_z)	= yy(ih2o2_kpp)
      cbox(ic2h6_z)	= yy(ic2h6_kpp)
      cbox(ipan_z)	= yy(ipan_kpp)
      cbox(in2o5_z)	= yy(in2o5_kpp)
      cbox(ich3oh_z)	= yy(ich3oh_kpp)
      cbox(ico_z)	= yy(ico_kpp)
      cbox(ihno4_z)	= yy(ihno4_kpp)
      cbox(ihono_z)	= yy(ihono_kpp)
      cbox(ich3ooh_z)	= yy(ich3ooh_kpp)
      cbox(iethooh_z)	= yy(iethooh_kpp)
      cbox(io3p_z)	= yy(io3p_kpp)
      cbox(ihno3_z)	= yy(ihno3_kpp)
      cbox(ihcho_z)	= yy(ihcho_kpp)
      cbox(iethp_z)	= yy(iethp_kpp)
      cbox(iald2_z)	= yy(iald2_kpp)
      cbox(ich3o2_z)	= yy(ich3o2_kpp)
      cbox(ic2o3_z)	= yy(ic2o3_kpp)
      cbox(ino3_z)	= yy(ino3_kpp)
      cbox(iho2_z)	= yy(iho2_kpp)
      cbox(ioh_z)	= yy(ioh_kpp)
      cbox(io3_z)	= yy(io3_kpp)
      cbox(ino_z)	= yy(ino_kpp)
      cbox(ino2_z)	= yy(ino2_kpp)

      return
      end subroutine cbmz_v02r01_mapconcs                                


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r01_maprates(   &
          rk_m1,   &
          rk_m2,   &
          rk_m3,   &
          rk_m4,   &
          rconst )
!
!   maps reaction rate constants (host code array --> kpp rconst array)
!   for mechanism-version-regime = cbmz_v02r01
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     all are host-code reaction-rate-constant arrays [input]
      real rk_m1(*)
      real rk_m2(*)
      real rk_m3(*)
      real rk_m4(*)
      real rconst(nreact_kppmax)

!   local variables
      integer i

      do i = 1, nreact_kppmax
          rconst(i) = 0.
      end do


      rconst(1) = (rk_m1(1))
      rconst(2) = (rk_m1(2))
      rconst(3) = (rk_m1(3))
      rconst(4) = (rk_m1(4))
      rconst(5) = (rk_m1(5))
      rconst(6) = (rk_m1(6))
      rconst(7) = (rk_m1(7))
      rconst(8) = (rk_m1(8))
      rconst(9) = (rk_m1(9))
      rconst(10) = (rk_m1(10))
      rconst(11) = (rk_m1(11))
      rconst(12) = (rk_m1(12))
      rconst(13) = (rk_m1(13))
      rconst(14) = (rk_m1(14))
      rconst(15) = (rk_m1(15))
      rconst(16) = (rk_m1(16))
      rconst(17) = (rk_m1(17))
      rconst(18) = (rk_m1(18))
      rconst(19) = (rk_m1(19))
      rconst(20) = (rk_m1(20))
      rconst(21) = (rk_m1(21))
      rconst(22) = (rk_m1(22))
      rconst(23) = (rk_m1(23))
      rconst(24) = (rk_m1(24))
      rconst(25) = (rk_m1(25))
      rconst(26) = (rk_m1(26))
      rconst(27) = (rk_m1(27))
      rconst(28) = (rk_m1(28))
      rconst(29) = (rk_m1(29))
      rconst(30) = (rk_m1(30))
      rconst(31) = (rk_m1(31))
      rconst(32) = (rk_m1(32))
      rconst(33) = (rk_m1(33))
      rconst(34) = (rk_m1(34))
      rconst(35) = (rk_m1(35))
      rconst(36) = (rk_m1(36))
      rconst(37) = (rk_m1(37))
      rconst(38) = (rk_m1(38))
      rconst(39) = (rk_m1(39))
      rconst(40) = (rk_m1(40))
      rconst(41) = (rk_m1(41))
      rconst(42) = (rk_m1(42))
      rconst(43) = (rk_m1(43))
      rconst(44) = (rk_m1(44))
      rconst(45) = (rk_m1(45))
      rconst(46) = (rk_m1(46))
      rconst(47) = (rk_m1(47))
      rconst(48) = (rk_m1(48))
      rconst(49) = (rk_m1(49))
      rconst(50) = (rk_m1(50))
      rconst(51) = (rk_m1(51))
      rconst(52) = (rk_m1(52))
      rconst(53) = (rk_m1(53))
      rconst(54) = (rk_m1(54))
      rconst(55) = (rk_m1(55))
      rconst(56) = (rk_m1(56))
      rconst(57) = (rk_m1(57))
      rconst(58) = (rk_m1(58))
      rconst(59) = (rk_m1(59))
      rconst(60) = (rk_m1(60))
      rconst(61) = (rk_m1(61))
      rconst(62) = (rk_m1(62))
      rconst(63) = (rk_m1(63))
      rconst(64) = (rk_m1(64))
      rconst(65) = (rk_m1(65))
      rconst(66) = (rk_m2(2))
      rconst(67) = (rk_m2(3))
      rconst(68) = (rk_m2(4))
      rconst(69) = (rk_m2(31))
      rconst(70) = (rk_m2(32))
      rconst(71) = (rk_m2(34))
      rconst(72) = (rk_m2(39))
      rconst(73) = (rk_m2(44))
      rconst(74) = (rk_m2(49))
      return
      end subroutine cbmz_v02r01_maprates 


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r01_dydt( nvardum, tdum, v, a_var, f, rconst )
!
!   computes rates of change for mechanism-version-regime = cbmz_v02r01
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     nvardum = number of variable species [input]
      integer nvardum
!     tdum = time [input]
      real tdum
!     v = concentrations of variable species [input]
      real v(nvar_r01_kpp)
!     a_var = dydt for each species [output]
      real a_var(nvar_r01_kpp)
!     f = concentrations of fixed species [input]
      real f(nfixed_kppmax)
!     rconst = reaction rate constants [input]
      real rconst(nreact_r01_kpp)

!   local variables
!     a = rate for each reaction
      real a(nreact_r01_kpp)

! computation of equation rates
      a(1) = rconst(1)*v(28)
      a(2) = rconst(2)*v(23)
      a(3) = rconst(3)*v(13)
      a(4) = rconst(4)*v(17)
      a(5) = rconst(5)*v(12)
      a(6) = rconst(6)*v(9)
      a(7) = rconst(7)*v(26)
      a(8) = rconst(8)*v(26)
      a(9) = rconst(9)*v(6)
      a(10) = rconst(10)*v(3)*f(4)
      a(11) = rconst(11)*v(3)*f(5)
      a(12) = rconst(12)*v(3)*f(2)
      a(13) = rconst(13)*v(16)*f(4)
      a(14) = rconst(14)*v(16)*v(26)
      a(15) = rconst(15)*v(16)*v(28)
      a(16) = rconst(16)*v(16)*v(28)
      a(17) = rconst(17)*v(16)*v(27)
      a(18) = rconst(18)*v(26)*v(27)
      a(19) = rconst(19)*v(26)*v(28)
      a(20) = rconst(20)*v(25)*v(26)
      a(21) = rconst(21)*v(24)*v(26)
      a(22) = rconst(22)*v(25)*f(3)
      a(23) = rconst(23)*v(25)*v(27)
      a(24) = rconst(24)*v(25)*v(28)
      a(25) = rconst(25)*v(23)*v(25)
      a(26) = rconst(26)*v(13)*v(25)
      a(27) = rconst(27)*v(17)*v(25)
      a(28) = rconst(28)*v(12)*v(25)
      a(29) = rconst(29)*v(24)*v(25)
      a(30) = rconst(30)*v(6)*v(25)
      a(31) = rconst(31)*v(24)*v(24)
      a(32) = rconst(32)*v(24)*v(24)*f(2)
      a(33) = rconst(33)*v(24)*v(27)
      a(34) = rconst(34)*v(24)*v(28)
      a(35) = rconst(35)*v(24)*v(28)
      a(36) = rconst(36)*v(12)
      a(37) = rconst(37)*v(23)*v(27)
      a(38) = rconst(38)*v(23)*v(28)
      a(39) = rconst(39)*v(23)*v(28)
      a(40) = rconst(40)*v(23)*v(23)
      a(41) = rconst(41)*v(23)*v(24)
      a(42) = rconst(42)*v(9)*f(2)
      a(43) = rconst(43)*v(9)
      a(44) = rconst(44)*v(11)*v(25)
      a(45) = rconst(45)*v(4)*v(25)
      a(46) = rconst(46)*v(25)*f(1)
      a(47) = rconst(47)*v(7)*v(25)
      a(48) = rconst(48)*v(10)*v(25)
      a(49) = rconst(49)*v(18)
      a(50) = rconst(50)*v(18)
      a(51) = rconst(51)*v(18)*v(25)
      a(52) = rconst(52)*v(18)*v(23)
      a(53) = rconst(53)*v(14)
      a(54) = rconst(54)*v(15)
      a(55) = rconst(55)*v(14)*v(25)
      a(56) = rconst(56)*v(15)*v(25)
      a(57) = rconst(57)*v(21)*v(27)
      a(58) = rconst(58)*v(19)*v(27)
      a(59) = rconst(59)*v(21)*v(23)
      a(60) = rconst(60)*v(19)*v(23)
      a(61) = rconst(61)*v(21)*v(24)
      a(62) = rconst(62)*v(19)*v(24)
      a(63) = rconst(63)*v(21)
      a(64) = rconst(64)*v(19)
      a(65) = rconst(65)*v(5)*v(25)
      a(66) = rconst(66)*v(20)
      a(67) = rconst(67)*v(20)*v(25)
      a(68) = rconst(68)*v(20)*v(23)
      a(69) = rconst(69)*v(22)*v(28)
      a(70) = rconst(70)*v(8)
      a(71) = rconst(71)*v(22)*v(27)
      a(72) = rconst(72)*v(22)*v(23)
      a(73) = rconst(73)*v(22)*v(24)
      a(74) = rconst(74)*v(22)

! aggregate function
      a_var(1) = a(45)
      a_var(2) = 0.4*a(73)
      a_var(3) = a(8)-a(10)-a(11)-a(12)
      a_var(4) = -a(45)
      a_var(5) = -a(65)
      a_var(6) = -a(9)-a(30)+a(31)+a(32)
      a_var(7) = -a(47)+0.2*a(64)
      a_var(8) = a(69)-a(70)
      a_var(9) = -a(6)+a(39)-a(42)-a(43)
      a_var(10) = -a(48)+0.34*a(63)
      a_var(11) = -a(44)+a(49)+a(50)+a(51)+a(52)+a(66)
      a_var(12) = -a(5)-a(28)+a(34)-a(36)
      a_var(13) = -a(3)+a(23)-a(26)+a(35)
      a_var(14) = -a(53)-a(55)+a(61)
      a_var(15) = -a(54)-a(56)+a(62)
      a_var(16) = a(1)+0.89*a(2)+a(7)+a(10)+a(11)-a(13)-a(14)-a(15)   &
                 -a(16)-a(17)
      a_var(17) = -a(4)+a(24)-a(27)+0.3*a(41)+2*a(42)+a(52)+a(68)
      a_var(18) = a(48)-a(49)-a(50)-a(51)-a(52)+a(53)+0.3*a(55)+a(57)   &
                 +a(59)+0.66*a(63)
      a_var(19) = a(47)+0.5*a(56)-a(58)-a(60)-a(62)-a(64)
      a_var(20) = a(54)+0.5*a(56)+a(58)+a(60)+0.8*a(64)+a(65)-a(66)   &
                 -a(67)-a(68)
      a_var(21) = a(46)+0.7*a(55)-a(57)-a(59)-a(61)-a(63)+a(66)+a(71)   &
                 +a(72)+a(74)
      a_var(22) = a(67)+a(68)-a(69)+a(70)-a(71)-a(72)-a(73)-a(74)
      a_var(23) = -a(2)+a(6)+a(16)+a(19)-a(25)+a(27)-a(37)-a(38)-a(39)   &
                 -2*a(40)-a(41)+a(43)-a(52)-a(59)-a(60)-a(68)-a(72)
      a_var(24) = a(5)+a(20)-a(21)+a(22)+a(25)-a(29)+a(30)-2*a(31)-2   &
                 *a(32)-a(33)-a(34)-a(35)+a(36)-a(41)+a(44)+a(45)   &
                 +a(48)+2*a(49)+a(51)+a(52)+a(53)+a(54)+a(57)+a(58)   &
                 +a(59)+a(60)-a(61)-a(62)+0.32*a(63)+0.6*a(64)+a(65)   &
                 +a(66)-a(73)
      a_var(25) = a(3)+a(4)+2*a(9)+2*a(12)-a(20)+a(21)-a(22)-a(23)   &
                 -a(24)-a(25)-a(26)-a(27)-a(28)-a(29)-a(30)+a(33)+0.7   &
                 *a(41)-a(44)-a(45)-a(46)-a(47)-a(48)-a(51)+a(53)   &
                 +a(54)-0.7*a(55)-0.5*a(56)-a(65)-a(67)
      a_var(26) = -a(7)-a(8)+a(13)-a(14)-a(18)-a(19)-a(20)-a(21)+0.4   &
                 *a(73)
      a_var(27) = a(1)+0.11*a(2)+a(3)+a(15)-a(17)-a(18)-a(23)-a(33)   &
                 -a(37)+a(38)-a(57)-a(58)-a(71)
      a_var(28) = -a(1)+0.89*a(2)+a(4)+a(5)+a(6)-a(15)-a(16)+a(17)   &
                 +a(18)-a(19)-a(24)+a(25)+a(26)+a(28)+a(33)-a(34)   &
                 -a(35)+a(36)+2*a(37)-a(39)+2*a(40)+0.7*a(41)+a(43)   &
                 +a(57)+a(58)+a(59)+a(60)-a(69)+a(70)+a(71)+a(72)
      return
      end subroutine cbmz_v02r01_dydt                                      


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r01_jacob( nvardum, tdum, v, jvs, f, rconst )
!
!   computes jacobian for mechanism-version-regime = cbmz_v02r01
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     nvardum = number of variable species [input]
      integer nvardum
!     tdum = time [input]
      real tdum
!     v = concentrations of variable species [input]
      real v(nvar_r01_kpp)
!     jvs = non-zero jacobian elements [output]
      real jvs(lu_nonzero_v_r01_kpp)
!     f = concentrations of fixed species [input]
      real f(nfixed_kppmax)
!     rconst = reaction rate constants [input]
      real rconst(nreact_r01_kpp)

!   local variables
!     b(i,j) = d[reaction_rate(i)] / d[species_conc(j)]
      real b(nreact_r01_kpp,nvar_r01_kpp)

! computation of b(i,j) = da(i)/dv(j)
      b(1,28) = rconst(1)
      b(2,23) = rconst(2)
      b(3,13) = rconst(3)
      b(4,17) = rconst(4)
      b(5,12) = rconst(5)
      b(6,9) = rconst(6)
      b(7,26) = rconst(7)
      b(8,26) = rconst(8)
      b(9,6) = rconst(9)
      b(10,3) = rconst(10)*f(4)
      b(11,3) = rconst(11)*f(5)
      b(12,3) = rconst(12)*f(2)
      b(13,16) = rconst(13)*f(4)
      b(14,16) = rconst(14)*v(26)
      b(14,26) = rconst(14)*v(16)
      b(15,16) = rconst(15)*v(28)
      b(15,28) = rconst(15)*v(16)
      b(16,16) = rconst(16)*v(28)
      b(16,28) = rconst(16)*v(16)
      b(17,16) = rconst(17)*v(27)
      b(17,27) = rconst(17)*v(16)
      b(18,26) = rconst(18)*v(27)
      b(18,27) = rconst(18)*v(26)
      b(19,26) = rconst(19)*v(28)
      b(19,28) = rconst(19)*v(26)
      b(20,25) = rconst(20)*v(26)
      b(20,26) = rconst(20)*v(25)
      b(21,24) = rconst(21)*v(26)
      b(21,26) = rconst(21)*v(24)
      b(22,25) = rconst(22)*f(3)
      b(23,25) = rconst(23)*v(27)
      b(23,27) = rconst(23)*v(25)
      b(24,25) = rconst(24)*v(28)
      b(24,28) = rconst(24)*v(25)
      b(25,23) = rconst(25)*v(25)
      b(25,25) = rconst(25)*v(23)
      b(26,13) = rconst(26)*v(25)
      b(26,25) = rconst(26)*v(13)
      b(27,17) = rconst(27)*v(25)
      b(27,25) = rconst(27)*v(17)
      b(28,12) = rconst(28)*v(25)
      b(28,25) = rconst(28)*v(12)
      b(29,24) = rconst(29)*v(25)
      b(29,25) = rconst(29)*v(24)
      b(30,6) = rconst(30)*v(25)
      b(30,25) = rconst(30)*v(6)
      b(31,24) = rconst(31)*2*v(24)
      b(32,24) = rconst(32)*2*v(24)*f(2)
      b(33,24) = rconst(33)*v(27)
      b(33,27) = rconst(33)*v(24)
      b(34,24) = rconst(34)*v(28)
      b(34,28) = rconst(34)*v(24)
      b(35,24) = rconst(35)*v(28)
      b(35,28) = rconst(35)*v(24)
      b(36,12) = rconst(36)
      b(37,23) = rconst(37)*v(27)
      b(37,27) = rconst(37)*v(23)
      b(38,23) = rconst(38)*v(28)
      b(38,28) = rconst(38)*v(23)
      b(39,23) = rconst(39)*v(28)
      b(39,28) = rconst(39)*v(23)
      b(40,23) = rconst(40)*2*v(23)
      b(41,23) = rconst(41)*v(24)
      b(41,24) = rconst(41)*v(23)
      b(42,9) = rconst(42)*f(2)
      b(43,9) = rconst(43)
      b(44,11) = rconst(44)*v(25)
      b(44,25) = rconst(44)*v(11)
      b(45,4) = rconst(45)*v(25)
      b(45,25) = rconst(45)*v(4)
      b(46,25) = rconst(46)*f(1)
      b(47,7) = rconst(47)*v(25)
      b(47,25) = rconst(47)*v(7)
      b(48,10) = rconst(48)*v(25)
      b(48,25) = rconst(48)*v(10)
      b(49,18) = rconst(49)
      b(50,18) = rconst(50)
      b(51,18) = rconst(51)*v(25)
      b(51,25) = rconst(51)*v(18)
      b(52,18) = rconst(52)*v(23)
      b(52,23) = rconst(52)*v(18)
      b(53,14) = rconst(53)
      b(54,15) = rconst(54)
      b(55,14) = rconst(55)*v(25)
      b(55,25) = rconst(55)*v(14)
      b(56,15) = rconst(56)*v(25)
      b(56,25) = rconst(56)*v(15)
      b(57,21) = rconst(57)*v(27)
      b(57,27) = rconst(57)*v(21)
      b(58,19) = rconst(58)*v(27)
      b(58,27) = rconst(58)*v(19)
      b(59,21) = rconst(59)*v(23)
      b(59,23) = rconst(59)*v(21)
      b(60,19) = rconst(60)*v(23)
      b(60,23) = rconst(60)*v(19)
      b(61,21) = rconst(61)*v(24)
      b(61,24) = rconst(61)*v(21)
      b(62,19) = rconst(62)*v(24)
      b(62,24) = rconst(62)*v(19)
      b(63,21) = rconst(63)
      b(64,19) = rconst(64)
      b(65,5) = rconst(65)*v(25)
      b(65,25) = rconst(65)*v(5)
      b(66,20) = rconst(66)
      b(67,20) = rconst(67)*v(25)
      b(67,25) = rconst(67)*v(20)
      b(68,20) = rconst(68)*v(23)
      b(68,23) = rconst(68)*v(20)
      b(69,22) = rconst(69)*v(28)
      b(69,28) = rconst(69)*v(22)
      b(70,8) = rconst(70)
      b(71,22) = rconst(71)*v(27)
      b(71,27) = rconst(71)*v(22)
      b(72,22) = rconst(72)*v(23)
      b(72,23) = rconst(72)*v(22)
      b(73,22) = rconst(73)*v(24)
      b(73,24) = rconst(73)*v(22)
      b(74,22) = rconst(74)

! construct the jacobian terms from b's
      jvs(1) = 0
      jvs(2) = b(45,4)
      jvs(3) = b(45,25)
      jvs(4) = 0
      jvs(5) = 0.4*b(73,22)
      jvs(6) = 0.4*b(73,24)
      jvs(7) = -b(10,3)-b(11,3)-b(12,3)
      jvs(8) = b(8,26)
      jvs(9) = -b(45,4)
      jvs(10) = -b(45,25)
      jvs(11) = -b(65,5)
      jvs(12) = -b(65,25)
      jvs(13) = -b(9,6)-b(30,6)
      jvs(14) = b(31,24)+b(32,24)
      jvs(15) = -b(30,25)
      jvs(16) = -b(47,7)
      jvs(17) = 0.2*b(64,19)
      jvs(18) = -b(47,25)
      jvs(19) = -b(70,8)
      jvs(20) = b(69,22)
      jvs(21) = b(69,28)
      jvs(22) = -b(6,9)-b(42,9)-b(43,9)
      jvs(23) = b(39,23)
      jvs(24) = b(39,28)
      jvs(25) = -b(48,10)
      jvs(26) = 0.34*b(63,21)
      jvs(27) = -b(48,25)
      jvs(28) = -b(44,11)
      jvs(29) = b(49,18)+b(50,18)+b(51,18)+b(52,18)
      jvs(30) = b(66,20)
      jvs(31) = b(52,23)
      jvs(32) = -b(44,25)+b(51,25)
      jvs(33) = -b(5,12)-b(28,12)-b(36,12)
      jvs(34) = b(34,24)
      jvs(35) = -b(28,25)
      jvs(36) = b(34,28)
      jvs(37) = -b(3,13)-b(26,13)
      jvs(38) = b(35,24)
      jvs(39) = b(23,25)-b(26,25)
      jvs(40) = b(23,27)
      jvs(41) = b(35,28)
      jvs(42) = -b(53,14)-b(55,14)
      jvs(43) = b(61,21)
      jvs(44) = b(61,24)
      jvs(45) = -b(55,25)
      jvs(46) = -b(54,15)-b(56,15)
      jvs(47) = b(62,19)
      jvs(48) = b(62,24)
      jvs(49) = -b(56,25)
      jvs(50) = b(10,3)+b(11,3)
      jvs(51) = -b(13,16)-b(14,16)-b(15,16)-b(16,16)-b(17,16)
      jvs(52) = 0.89*b(2,23)
      jvs(53) = b(7,26)-b(14,26)
      jvs(54) = -b(17,27)
      jvs(55) = b(1,28)-b(15,28)-b(16,28)
      jvs(56) = 2*b(42,9)
      jvs(57) = -b(4,17)-b(27,17)
      jvs(58) = b(52,18)
      jvs(59) = b(68,20)
      jvs(60) = 0.3*b(41,23)+b(52,23)+b(68,23)
      jvs(61) = 0.3*b(41,24)
      jvs(62) = b(24,25)-b(27,25)
      jvs(63) = b(24,28)
      jvs(64) = b(48,10)
      jvs(65) = b(53,14)+0.3*b(55,14)
      jvs(66) = -b(49,18)-b(50,18)-b(51,18)-b(52,18)
      jvs(67) = b(57,21)+b(59,21)+0.66*b(63,21)
      jvs(68) = -b(52,23)+b(59,23)
      jvs(69) = 0
      jvs(70) = b(48,25)-b(51,25)+0.3*b(55,25)
      jvs(71) = b(57,27)
      jvs(72) = b(47,7)
      jvs(73) = 0.5*b(56,15)
      jvs(74) = -b(58,19)-b(60,19)-b(62,19)-b(64,19)
      jvs(75) = -b(60,23)
      jvs(76) = -b(62,24)
      jvs(77) = b(47,25)+0.5*b(56,25)
      jvs(78) = -b(58,27)
      jvs(79) = b(65,5)
      jvs(80) = b(54,15)+0.5*b(56,15)
      jvs(81) = b(58,19)+b(60,19)+0.8*b(64,19)
      jvs(82) = -b(66,20)-b(67,20)-b(68,20)
      jvs(83) = b(60,23)-b(68,23)
      jvs(84) = 0
      jvs(85) = 0.5*b(56,25)+b(65,25)-b(67,25)
      jvs(86) = b(58,27)
      jvs(87) = 0.7*b(55,14)
      jvs(88) = b(66,20)
      jvs(89) = -b(57,21)-b(59,21)-b(61,21)-b(63,21)
      jvs(90) = b(71,22)+b(72,22)+b(74,22)
      jvs(91) = -b(59,23)+b(72,23)
      jvs(92) = -b(61,24)
      jvs(93) = b(46,25)+0.7*b(55,25)
      jvs(94) = -b(57,27)+b(71,27)
      jvs(95) = b(70,8)
      jvs(96) = b(67,20)+b(68,20)
      jvs(97) = -b(69,22)-b(71,22)-b(72,22)-b(73,22)-b(74,22)
      jvs(98) = b(68,23)-b(72,23)
      jvs(99) = -b(73,24)
      jvs(100) = b(67,25)
      jvs(101) = -b(71,27)
      jvs(102) = -b(69,28)
      jvs(103) = b(6,9)+b(43,9)
      jvs(104) = b(16,16)
      jvs(105) = b(27,17)
      jvs(106) = -b(52,18)
      jvs(107) = -b(60,19)
      jvs(108) = -b(68,20)
      jvs(109) = -b(59,21)
      jvs(110) = -b(72,22)
      jvs(111) = -b(2,23)-b(25,23)-b(37,23)-b(38,23)-b(39,23)-2   &
                *b(40,23)-b(41,23)-b(52,23)-b(59,23)-b(60,23)-b(68,23)   &
                -b(72,23)
      jvs(112) = -b(41,24)
      jvs(113) = -b(25,25)+b(27,25)
      jvs(114) = b(19,26)
      jvs(115) = -b(37,27)
      jvs(116) = b(16,28)+b(19,28)-b(38,28)-b(39,28)
      jvs(117) = b(45,4)
      jvs(118) = b(65,5)
      jvs(119) = b(30,6)
      jvs(120) = b(48,10)
      jvs(121) = b(44,11)
      jvs(122) = b(5,12)+b(36,12)
      jvs(123) = b(53,14)
      jvs(124) = b(54,15)
      jvs(125) = 2*b(49,18)+b(51,18)+b(52,18)
      jvs(126) = b(58,19)+b(60,19)-b(62,19)+0.6*b(64,19)
      jvs(127) = b(66,20)
      jvs(128) = b(57,21)+b(59,21)-b(61,21)+0.32*b(63,21)
      jvs(129) = -b(73,22)
      jvs(130) = b(25,23)-b(41,23)+b(52,23)+b(59,23)+b(60,23)
      jvs(131) = -b(21,24)-b(29,24)-2*b(31,24)-2*b(32,24)-b(33,24)   &
                -b(34,24)-b(35,24)-b(41,24)-b(61,24)-b(62,24)-b(73,24)
      jvs(132) = b(20,25)+b(22,25)+b(25,25)-b(29,25)+b(30,25)+b(44,25)   &
                +b(45,25)+b(48,25)+b(51,25)+b(65,25)
      jvs(133) = b(20,26)-b(21,26)
      jvs(134) = -b(33,27)+b(57,27)+b(58,27)
      jvs(135) = -b(34,28)-b(35,28)
      jvs(136) = 2*b(12,3)
      jvs(137) = -b(45,4)
      jvs(138) = -b(65,5)
      jvs(139) = 2*b(9,6)-b(30,6)
      jvs(140) = -b(47,7)
      jvs(141) = -b(48,10)
      jvs(142) = -b(44,11)
      jvs(143) = -b(28,12)
      jvs(144) = b(3,13)-b(26,13)
      jvs(145) = b(53,14)-0.7*b(55,14)
      jvs(146) = b(54,15)-0.5*b(56,15)
      jvs(147) = b(4,17)-b(27,17)
      jvs(148) = -b(51,18)
      jvs(149) = 0
      jvs(150) = -b(67,20)
      jvs(151) = 0
      jvs(152) = 0
      jvs(153) = -b(25,23)+0.7*b(41,23)
      jvs(154) = b(21,24)-b(29,24)+b(33,24)+0.7*b(41,24)
      jvs(155) = -b(20,25)-b(22,25)-b(23,25)-b(24,25)-b(25,25)   &
                -b(26,25)-b(27,25)-b(28,25)-b(29,25)-b(30,25)-b(44,25)   &
                -b(45,25)-b(46,25)-b(47,25)-b(48,25)-b(51,25)-0.7   &
                *b(55,25)-0.5*b(56,25)-b(65,25)-b(67,25)
      jvs(156) = -b(20,26)+b(21,26)
      jvs(157) = -b(23,27)+b(33,27)
      jvs(158) = -b(24,28)
      jvs(159) = b(13,16)-b(14,16)
      jvs(160) = 0.4*b(73,22)
      jvs(161) = 0
      jvs(162) = -b(21,24)+0.4*b(73,24)
      jvs(163) = -b(20,25)
      jvs(164) = -b(7,26)-b(8,26)-b(14,26)-b(18,26)-b(19,26)-b(20,26)   &
                -b(21,26)
      jvs(165) = -b(18,27)
      jvs(166) = -b(19,28)
      jvs(167) = b(3,13)
      jvs(168) = b(15,16)-b(17,16)
      jvs(169) = -b(58,19)
      jvs(170) = -b(57,21)
      jvs(171) = -b(71,22)
      jvs(172) = 0.11*b(2,23)-b(37,23)+b(38,23)
      jvs(173) = -b(33,24)
      jvs(174) = -b(23,25)
      jvs(175) = -b(18,26)
      jvs(176) = -b(17,27)-b(18,27)-b(23,27)-b(33,27)-b(37,27)   &
                -b(57,27)-b(58,27)-b(71,27)
      jvs(177) = b(1,28)+b(15,28)+b(38,28)
      jvs(178) = b(70,8)
      jvs(179) = b(6,9)+b(43,9)
      jvs(180) = b(5,12)+b(28,12)+b(36,12)
      jvs(181) = b(26,13)
      jvs(182) = -b(15,16)-b(16,16)+b(17,16)
      jvs(183) = b(4,17)
      jvs(184) = 0
      jvs(185) = b(58,19)+b(60,19)
      jvs(186) = 0
      jvs(187) = b(57,21)+b(59,21)
      jvs(188) = -b(69,22)+b(71,22)+b(72,22)
      jvs(189) = 0.89*b(2,23)+b(25,23)+2*b(37,23)-b(39,23)+2*b(40,23)   &
                +0.7*b(41,23)+b(59,23)+b(60,23)+b(72,23)
      jvs(190) = b(33,24)-b(34,24)-b(35,24)+0.7*b(41,24)
      jvs(191) = -b(24,25)+b(25,25)+b(26,25)+b(28,25)
      jvs(192) = b(18,26)-b(19,26)
      jvs(193) = b(17,27)+b(18,27)+b(33,27)+2*b(37,27)+b(57,27)   &
                +b(58,27)+b(71,27)
      jvs(194) = -b(1,28)-b(15,28)-b(16,28)-b(19,28)-b(24,28)-b(34,28)   &
                -b(35,28)-b(39,28)-b(69,28)
      return
      end subroutine cbmz_v02r01_jacob                                    


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r01_decomp( n, v, ier,   &
          lu_crow_v, lu_diag_v, lu_icol_v )
!
!   computes l-u-decomposition of sparse jacobian
!   for mechanism-version-regime = cbmz_v02r01
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     n = number of variable species [input]
      integer n
!     ier = status flag [output]
!         0 = success    other = failure [output]
      integer ier

!     v = non-zero elements of the sparse jacobian [input]
      real v(lu_nonzero_v_r01_kpp)

      integer lu_crow_v(nvar_r01_kpp + 1)
      integer lu_diag_v(nvar_r01_kpp + 1)
      integer lu_icol_v(lu_nonzero_v_r01_kpp)

!   local variables
      integer k, kk, j, jj
      real a, w(nvar_r01_kpp + 1)

      ier = 0
      do k=1,n
        if ( v( lu_diag_v(k) ) .eq. 0. ) then
            ier = k
            return
        end if
        do kk = lu_crow_v(k), lu_crow_v(k+1)-1
              w( lu_icol_v(kk) ) = v(kk)
        end do
        do kk = lu_crow_v(k), lu_diag_v(k)-1
            j = lu_icol_v(kk)
            a = -w(j) / v( lu_diag_v(j) )
            w(j) = -a
            do jj = lu_diag_v(j)+1, lu_crow_v(j+1)-1
               w( lu_icol_v(jj) ) = w( lu_icol_v(jj) ) + a*v(jj)
            end do
         end do
         do kk = lu_crow_v(k), lu_crow_v(k+1)-1
            v(kk) = w( lu_icol_v(kk) )
         end do
      end do
      return
      end subroutine cbmz_v02r01_decomp            


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r01_solve( jvs, x )
!
!   does back-solve for mechanism-version-regime = cbmz_v02r01
!
      implicit none

!   subr parameters
!     jvs = the non-zero elements of the l-u-decomposition
!           of the augmented jacobian [input]
      real jvs(*)
!     x = the right-hand side of the linear equation set being solved [on input]
!     x = concentrations of variable species [on output]
      real x(*)


      x(16) = x(16)-jvs(50)*x(3)
      x(17) = x(17)-jvs(56)*x(9)
      x(18) = x(18)-jvs(64)*x(10)-jvs(65)*x(14)
      x(19) = x(19)-jvs(72)*x(7)-jvs(73)*x(15)
      x(20) = x(20)-jvs(79)*x(5)-jvs(80)*x(15)-jvs(81)*x(19)
      x(21) = x(21)-jvs(87)*x(14)-jvs(88)*x(20)
      x(22) = x(22)-jvs(95)*x(8)-jvs(96)*x(20)
      x(23) = x(23)-jvs(103)*x(9)-jvs(104)*x(16)-jvs(105)*x(17)   &
             -jvs(106)*x(18)-jvs(107)*x(19)-jvs(108)*x(20)-jvs(109)   &
             *x(21)-jvs(110)*x(22)
      x(24) = x(24)-jvs(117)*x(4)-jvs(118)*x(5)-jvs(119)*x(6)-jvs(120)   &
             *x(10)-jvs(121)*x(11)-jvs(122)*x(12)-jvs(123)*x(14)   &
             -jvs(124)*x(15)-jvs(125)*x(18)-jvs(126)*x(19)-jvs(127)   &
             *x(20)-jvs(128)*x(21)-jvs(129)*x(22)-jvs(130)*x(23)
      x(25) = x(25)-jvs(136)*x(3)-jvs(137)*x(4)-jvs(138)*x(5)-jvs(139)   &
             *x(6)-jvs(140)*x(7)-jvs(141)*x(10)-jvs(142)*x(11)   &
             -jvs(143)*x(12)-jvs(144)*x(13)-jvs(145)*x(14)-jvs(146)   &
             *x(15)-jvs(147)*x(17)-jvs(148)*x(18)-jvs(149)*x(19)   &
             -jvs(150)*x(20)-jvs(151)*x(21)-jvs(152)*x(22)-jvs(153)   &
             *x(23)-jvs(154)*x(24)
      x(26) = x(26)-jvs(159)*x(16)-jvs(160)*x(22)-jvs(161)*x(23)   &
             -jvs(162)*x(24)-jvs(163)*x(25)
      x(27) = x(27)-jvs(167)*x(13)-jvs(168)*x(16)-jvs(169)*x(19)   &
             -jvs(170)*x(21)-jvs(171)*x(22)-jvs(172)*x(23)-jvs(173)   &
             *x(24)-jvs(174)*x(25)-jvs(175)*x(26)
      x(28) = x(28)-jvs(178)*x(8)-jvs(179)*x(9)-jvs(180)*x(12)   &
             -jvs(181)*x(13)-jvs(182)*x(16)-jvs(183)*x(17)-jvs(184)   &
             *x(18)-jvs(185)*x(19)-jvs(186)*x(20)-jvs(187)*x(21)   &
             -jvs(188)*x(22)-jvs(189)*x(23)-jvs(190)*x(24)-jvs(191)   &
             *x(25)-jvs(192)*x(26)-jvs(193)*x(27)
      x(28) = x(28)/jvs(194)
      x(27) = (x(27)-jvs(177)*x(28))/(jvs(176))
      x(26) = (x(26)-jvs(165)*x(27)-jvs(166)*x(28))/(jvs(164))
      x(25) = (x(25)-jvs(156)*x(26)-jvs(157)*x(27)-jvs(158)*x(28))/   &
             (jvs(155))
      x(24) = (x(24)-jvs(132)*x(25)-jvs(133)*x(26)-jvs(134)*x(27)   &
             -jvs(135)*x(28))/(jvs(131))
      x(23) = (x(23)-jvs(112)*x(24)-jvs(113)*x(25)-jvs(114)*x(26)   &
             -jvs(115)*x(27)-jvs(116)*x(28))/(jvs(111))
      x(22) = (x(22)-jvs(98)*x(23)-jvs(99)*x(24)-jvs(100)*x(25)   &
             -jvs(101)*x(27)-jvs(102)*x(28))/(jvs(97))
      x(21) = (x(21)-jvs(90)*x(22)-jvs(91)*x(23)-jvs(92)*x(24)-jvs(93)   &
             *x(25)-jvs(94)*x(27))/(jvs(89))
      x(20) = (x(20)-jvs(83)*x(23)-jvs(84)*x(24)-jvs(85)*x(25)-jvs(86)   &
             *x(27))/(jvs(82))
      x(19) = (x(19)-jvs(75)*x(23)-jvs(76)*x(24)-jvs(77)*x(25)-jvs(78)   &
             *x(27))/(jvs(74))
      x(18) = (x(18)-jvs(67)*x(21)-jvs(68)*x(23)-jvs(69)*x(24)-jvs(70)   &
             *x(25)-jvs(71)*x(27))/(jvs(66))
      x(17) = (x(17)-jvs(58)*x(18)-jvs(59)*x(20)-jvs(60)*x(23)-jvs(61)   &
             *x(24)-jvs(62)*x(25)-jvs(63)*x(28))/(jvs(57))
      x(16) = (x(16)-jvs(52)*x(23)-jvs(53)*x(26)-jvs(54)*x(27)-jvs(55)   &
             *x(28))/(jvs(51))
      x(15) = (x(15)-jvs(47)*x(19)-jvs(48)*x(24)-jvs(49)*x(25))/   &
             (jvs(46))
      x(14) = (x(14)-jvs(43)*x(21)-jvs(44)*x(24)-jvs(45)*x(25))/   &
             (jvs(42))
      x(13) = (x(13)-jvs(38)*x(24)-jvs(39)*x(25)-jvs(40)*x(27)-jvs(41)   &
             *x(28))/(jvs(37))
      x(12) = (x(12)-jvs(34)*x(24)-jvs(35)*x(25)-jvs(36)*x(28))/   &
             (jvs(33))
      x(11) = (x(11)-jvs(29)*x(18)-jvs(30)*x(20)-jvs(31)*x(23)-jvs(32)   &
             *x(25))/(jvs(28))
      x(10) = (x(10)-jvs(26)*x(21)-jvs(27)*x(25))/(jvs(25))
      x(9) = (x(9)-jvs(23)*x(23)-jvs(24)*x(28))/(jvs(22))
      x(8) = (x(8)-jvs(20)*x(22)-jvs(21)*x(28))/(jvs(19))
      x(7) = (x(7)-jvs(17)*x(19)-jvs(18)*x(25))/(jvs(16))
      x(6) = (x(6)-jvs(14)*x(24)-jvs(15)*x(25))/(jvs(13))
      x(5) = (x(5)-jvs(12)*x(25))/(jvs(11))
      x(4) = (x(4)-jvs(10)*x(25))/(jvs(9))
      x(3) = (x(3)-jvs(8)*x(26))/(jvs(7))
      x(2) = (x(2)-jvs(5)*x(22)-jvs(6)*x(24))/(jvs(4))
      x(1) = (x(1)-jvs(2)*x(4)-jvs(3)*x(25))/(jvs(1))
      return
      end subroutine cbmz_v02r01_solve          


!   cbmz_v02r02_torodas.f - created on 17-nov-2003 from previous
!        cbmz_v02r02_torodas.f        cbmz_v02r02_mapconcs.f
!        cbmz_v02r02_maprates.f       cbmz_v02r02_dydt.f
!        cbmz_v02r02_jacob.f          cbmz_v02r02_decomp.f
!        cbmz_v02r02_solve.f
!   so now everything is in a single file
!-----------------------------------------------------------------------

      subroutine cbmz_v02r02_torodas(   &
          ngas, taa, tzz,   &
          stot, atol, rtol, yposlimit, yneglimit,   &
          sfixedkpp, rconstkpp,   &
          hmin, hstart,   &
          info_rodas, iok, lunerr, idydt_sngldble )
!
!   interfaces to rodas3 solver formechanism-version-regime =cbmz_v02r02
!
!   *** do not include any pegasus common blocks ***
!
      use module_data_cbmz
      use module_cbmz_rodas3_solver, only:  rodas3_ff_x2
      implicit none

!   subr parameters
      integer ngas, iok, lunerr, idydt_sngldble
      integer info_rodas(6)
      real taa, tzz, hmin, hstart
      real stot(ngas), atol(ngas), rtol(ngas)
      real yposlimit(ngas), yneglimit(ngas)
      real sfixedkpp(nfixed_kppmax), rconstkpp(nreact_kppmax)

!   local variables

!     external cbmz_v02r02_dydt
!     external cbmz_v02r02_jacob
!     external cbmz_v02r02_decomp
!     external cbmz_v02r02_solve

      integer i

      real hmax

      integer lu_crow_v(nvar_r02_kpp + 1)
      save    lu_crow_v
      integer lu_diag_v(nvar_r02_kpp + 1)
      save    lu_diag_v
      integer lu_icol_v(lu_nonzero_v_r02_kpp)
      save    lu_icol_v

      data( lu_icol_v(i), i = 1, 252 ) /   &
        1,  5, 44,  2, 20, 30, 43,  3, 30, 33, 42, 43,   &
       45,  4, 43,  5, 44,  6, 44,  7, 42, 44,  8, 35,   &
       44,  9, 45, 47, 10, 44, 11, 47, 48, 12, 44, 12,   &
       13, 25, 44, 14, 23, 44, 47, 48, 15, 42, 44, 47,   &
       10, 12, 16, 44, 46, 17, 38, 42, 44, 18, 35, 42,   &
       44, 19, 42, 44, 46, 47, 20, 43, 44, 21, 30, 33,   &
       38, 43, 44,  4, 22, 43, 46, 47, 48, 10, 12, 23,   &
       44, 48, 20, 24, 27, 30, 31, 33, 34, 37, 43, 44,   &
       48, 13, 25, 28, 30, 33, 36, 40, 41, 43, 44, 46,   &
       48, 11, 23, 26, 31, 34, 37, 42, 44, 47, 48, 16,   &
       23, 27, 43, 44, 46, 48, 28, 39, 40, 42, 44, 28,   &
       29, 33, 39, 40, 41, 42, 43, 44, 46, 48, 30, 43,   &
       44, 48, 17, 20, 21, 27, 30, 31, 33, 36, 38, 39,   &
       42, 43, 44, 46, 48, 10, 12, 20, 23, 27, 28, 30,   &
       32, 33, 34, 39, 40, 41, 42, 43, 44, 46, 48, 33,   &
       43, 44, 48, 12, 27, 28, 30, 33, 34, 39, 40, 42,   &
       43, 44, 46, 48,  8, 18, 28, 30, 33, 35, 39, 40,   &
       41, 42, 43, 44, 46, 48, 30, 33, 36, 41, 42, 43,   &
       44, 46, 48,  6, 18, 20, 27, 28, 30, 33, 35, 36,   &
       37, 39, 40, 41, 42, 43, 44, 46, 48, 17, 29, 30,   &
       33, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 48 /

      data( lu_icol_v(i), i = 253, 459 ) /   &
       29, 33, 39, 40, 41, 42, 43, 44, 46, 48, 13, 25,   &
       28, 30, 33, 36, 39, 40, 41, 42, 43, 44, 46, 48,   &
       14, 16, 23, 36, 40, 41, 42, 43, 44, 46, 47, 48,   &
        5,  6,  7, 10, 12, 15, 16, 17, 18, 20, 21, 23,   &
       24, 27, 28, 30, 31, 32, 33, 34, 35, 36, 37, 38,   &
       39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 20, 22,   &
       27, 30, 33, 42, 43, 44, 45, 46, 47, 48,  4,  5,   &
        6,  7,  8, 10, 12, 13, 15, 17, 18, 19, 20, 21,   &
       23, 24, 25, 26, 27, 28, 29, 30, 31, 33, 34, 35,   &
       36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,   &
       48,  9, 27, 29, 30, 33, 34, 37, 39, 40, 41, 42,   &
       43, 44, 45, 46, 47, 48, 16, 19, 22, 32, 33, 34,   &
       35, 36, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,   &
       48,  9, 11, 14, 15, 16, 19, 22, 23, 26, 31, 32,   &
       33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44,   &
       45, 46, 47, 48, 11, 22, 23, 26, 30, 31, 32, 33,   &
       34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,   &
       46, 47, 48 /

      data lu_crow_v /   &
        1,  4,  8, 14, 16, 18, 20, 23, 26, 29, 31, 34,   &
       36, 40, 45, 49, 54, 58, 62, 67, 70, 76, 82, 87,   &
       98,110,120,127,132,143,147,162,180,184,197,211,   &
      220,238,253,263,277,289,323,335,374,391,410,437,   &
      460 /

      data lu_diag_v /   &
        1,  4,  8, 14, 16, 18, 20, 23, 26, 29, 31, 34,   &
       37, 40, 45, 51, 54, 58, 62, 67, 70, 77, 84, 88,   &
       99,112,122,127,133,143,152,169,180,189,202,213,   &
      229,243,255,270,282,316,329,369,387,407,435,459,   &
      460 /



      info_rodas(1) = 1
      do i = 2, 6
          info_rodas(i) = 0
      end do
      hmax = tzz - taa

!   do not integrate if hmax is less/equal to hmin
!   because hmin is generally 0.1 s or less
      if (hmax .le. 1.001*hmin) then
          iok = 11
          return
      end if

      call rodas3_ff_x2(   &
           nvar_r02_kpp, taa, tzz, hmin, hmax, hstart,   &
           stot, atol, rtol, yposlimit, yneglimit,   &
           sfixedkpp, rconstkpp,   &
           lu_nonzero_v_r02_kpp, lu_crow_v, lu_diag_v, lu_icol_v,   &
           info_rodas, iok, lunerr,   &
           cbmz_v02r02_dydt,   &
           cbmz_v02r02_jacob,   &
           cbmz_v02r02_decomp,   &
           cbmz_v02r02_solve )

      return
      end subroutine cbmz_v02r02_torodas 


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r02_mapconcs( imap, nyy, yy, yyfixed, cbox )
!
!   maps species concentrations (gaschemistry cbox array <--> kpp yy array)
!   for mechanism-version-regime = cbmz_v02r02
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     imap = mapping direction flag [input]
!         0 = map cbox --> yy and yyfixed
!         1 = map yy --> cbox
      integer imap
!     nyy = number of kpp "variable" species [output]
      integer nyy
!     yy = kpp species concentrations array [input/output]
      real yy(nvar_r02_kpp)
!     yyfixed = kpp species concentrations array [input/output]
      real yyfixed(nfixed_kppmax)
!     cbox = main gaschemistry species conc array [input/output]
      real cbox(ngas_z)

!   local variables
      integer ih2so4_kpp
      parameter ( ih2so4_kpp = 1 )
      integer ihcooh_kpp
      parameter ( ihcooh_kpp = 2 )
      integer ircooh_kpp
      parameter ( ircooh_kpp = 3 )
      integer io1d_kpp
      parameter ( io1d_kpp = 4 )
      integer iso2_kpp
      parameter ( iso2_kpp = 5 )
      integer ic2h5oh_kpp
      parameter ( ic2h5oh_kpp = 6 )
      integer ih2o2_kpp
      parameter ( ih2o2_kpp = 7 )
      integer ic2h6_kpp
      parameter ( ic2h6_kpp = 8 )
      integer ipan_kpp
      parameter ( ipan_kpp = 9 )
      integer itol_kpp
      parameter ( itol_kpp = 10 )
      integer in2o5_kpp
      parameter ( in2o5_kpp = 11 )
      integer ixyl_kpp
      parameter ( ixyl_kpp = 12 )
      integer ipar_kpp
      parameter ( ipar_kpp = 13 )
      integer icro_kpp
      parameter ( icro_kpp = 14 )
      integer ihno4_kpp
      parameter ( ihno4_kpp = 15 )
      integer ito2_kpp
      parameter ( ito2_kpp = 16 )
      integer ich3ooh_kpp
      parameter ( ich3ooh_kpp = 17 )
      integer iethooh_kpp
      parameter ( iethooh_kpp = 18 )
      integer ihono_kpp
      parameter ( ihono_kpp = 19 )
      integer ieth_kpp
      parameter ( ieth_kpp = 20 )
      integer ich3oh_kpp
      parameter ( ich3oh_kpp = 21 )
      integer io3p_kpp
      parameter ( io3p_kpp = 22 )
      integer icres_kpp
      parameter ( icres_kpp = 23 )
      integer ico_kpp
      parameter ( ico_kpp = 24 )
      integer ixpar_kpp
      parameter ( ixpar_kpp = 25 )
      integer ihno3_kpp
      parameter ( ihno3_kpp = 26 )
      integer iopen_kpp
      parameter ( iopen_kpp = 27 )
      integer irooh_kpp
      parameter ( irooh_kpp = 28 )
      integer iaone_kpp
      parameter ( iaone_kpp = 29 )
      integer iolet_kpp
      parameter ( iolet_kpp = 30 )
      integer ihcho_kpp
      parameter ( ihcho_kpp = 31 )
      integer ixo2_kpp
      parameter ( ixo2_kpp = 32 )
      integer iolei_kpp
      parameter ( iolei_kpp = 33 )
      integer imgly_kpp
      parameter ( imgly_kpp = 34 )
      integer iethp_kpp
      parameter ( iethp_kpp = 35 )
      integer inap_kpp
      parameter ( inap_kpp = 36 )
      integer iald2_kpp
      parameter ( iald2_kpp = 37 )
      integer ich3o2_kpp
      parameter ( ich3o2_kpp = 38 )
      integer iano2_kpp
      parameter ( iano2_kpp = 39 )
      integer iro2_kpp
      parameter ( iro2_kpp = 40 )
      integer ionit_kpp
      parameter ( ionit_kpp = 41 )
      integer iho2_kpp
      parameter ( iho2_kpp = 42 )
      integer io3_kpp
      parameter ( io3_kpp = 43 )
      integer ioh_kpp
      parameter ( ioh_kpp = 44 )
      integer ic2o3_kpp
      parameter ( ic2o3_kpp = 45 )
      integer ino_kpp
      parameter ( ino_kpp = 46 )
      integer ino2_kpp
      parameter ( ino2_kpp = 47 )
      integer ino3_kpp
      parameter ( ino3_kpp = 48 )

! indexes declaration for fixed species

      integer ich4_kpp
      parameter ( ich4_kpp = 1 )
      integer ih2o_kpp
      parameter ( ih2o_kpp = 2 )
      integer ih2_kpp
      parameter ( ih2_kpp = 3 )
      integer io2_kpp
      parameter ( io2_kpp = 4 )
      integer in2_kpp
      parameter ( in2_kpp = 5 )


      nyy = nvar_r02_kpp

      if (imap .le. 0) goto 1000
      if (imap .ge. 1) goto 2000


!
!   map cbox values into yyvarkpp and yyfixkpp
!
1000  continue
      yy(ih2so4_kpp)	= cbox(ih2so4_z)
      yy(ihcooh_kpp)	= cbox(ihcooh_z)
      yy(ircooh_kpp)	= cbox(ircooh_z)
      yy(io1d_kpp)	= cbox(io1d_z)
      yy(iso2_kpp)	= cbox(iso2_z)
      yy(ic2h5oh_kpp)	= cbox(ic2h5oh_z)
      yy(ih2o2_kpp)	= cbox(ih2o2_z)
      yy(ic2h6_kpp)	= cbox(ic2h6_z)
      yy(ipan_kpp)	= cbox(ipan_z)
      yy(itol_kpp)	= cbox(itol_z)
      yy(in2o5_kpp)	= cbox(in2o5_z)
      yy(ixyl_kpp)	= cbox(ixyl_z)
      yy(ipar_kpp)	= cbox(ipar_z)
      yy(icro_kpp)	= cbox(icro_z)
      yy(ihno4_kpp)	= cbox(ihno4_z)
      yy(ito2_kpp)	= cbox(ito2_z)
      yy(ich3ooh_kpp)	= cbox(ich3ooh_z)
      yy(iethooh_kpp)	= cbox(iethooh_z)
      yy(ihono_kpp)	= cbox(ihono_z)
      yy(ieth_kpp)	= cbox(ieth_z)
      yy(ich3oh_kpp)	= cbox(ich3oh_z)
      yy(io3p_kpp)	= cbox(io3p_z)
      yy(icres_kpp)	= cbox(icres_z)
      yy(ico_kpp)	= cbox(ico_z)
      yy(ixpar_kpp)	= cbox(ixpar_z)
      yy(ihno3_kpp)	= cbox(ihno3_z)
      yy(iopen_kpp)	= cbox(iopen_z)
      yy(irooh_kpp)	= cbox(irooh_z)
      yy(iaone_kpp)	= cbox(iaone_z)
      yy(iolet_kpp)	= cbox(iolet_z)
      yy(ihcho_kpp)	= cbox(ihcho_z)
      yy(ixo2_kpp)	= cbox(ixo2_z)
      yy(iolei_kpp)	= cbox(iolei_z)
      yy(imgly_kpp)	= cbox(imgly_z)
      yy(iethp_kpp)	= cbox(iethp_z)
      yy(inap_kpp)	= cbox(inap_z)
      yy(iald2_kpp)	= cbox(iald2_z)
      yy(ich3o2_kpp)	= cbox(ich3o2_z)
      yy(iano2_kpp)	= cbox(iano2_z)
      yy(iro2_kpp)	= cbox(iro2_z)
      yy(ionit_kpp)	= cbox(ionit_z)
      yy(iho2_kpp)	= cbox(iho2_z)
      yy(io3_kpp)	= cbox(io3_z)
      yy(ioh_kpp)	= cbox(ioh_z)
      yy(ic2o3_kpp)	= cbox(ic2o3_z)
      yy(ino_kpp)	= cbox(ino_z)
      yy(ino2_kpp)	= cbox(ino2_z)
      yy(ino3_kpp)	= cbox(ino3_z)

      yyfixed(ich4_kpp) = cbox(ich4_z)
      yyfixed(ih2o_kpp) = cbox(ih2o_z)
      yyfixed(ih2_kpp) = cbox(ih2_z)
      yyfixed(io2_kpp) = cbox(io2_z)
      yyfixed(in2_kpp) = cbox(in2_z)

!
!   map yyvarkpp values into cbox
!
2000  continue
      cbox(ih2so4_z)	= yy(ih2so4_kpp)
      cbox(ihcooh_z)	= yy(ihcooh_kpp)
      cbox(ircooh_z)	= yy(ircooh_kpp)
      cbox(io1d_z)	= yy(io1d_kpp)
      cbox(iso2_z)	= yy(iso2_kpp)
      cbox(ic2h5oh_z)	= yy(ic2h5oh_kpp)
      cbox(ih2o2_z)	= yy(ih2o2_kpp)
      cbox(ic2h6_z)	= yy(ic2h6_kpp)
      cbox(ipan_z)	= yy(ipan_kpp)
      cbox(itol_z)	= yy(itol_kpp)
      cbox(in2o5_z)	= yy(in2o5_kpp)
      cbox(ixyl_z)	= yy(ixyl_kpp)
      cbox(ipar_z)	= yy(ipar_kpp)
      cbox(icro_z)	= yy(icro_kpp)
      cbox(ihno4_z)	= yy(ihno4_kpp)
      cbox(ito2_z)	= yy(ito2_kpp)
      cbox(ich3ooh_z)	= yy(ich3ooh_kpp)
      cbox(iethooh_z)	= yy(iethooh_kpp)
      cbox(ihono_z)	= yy(ihono_kpp)
      cbox(ieth_z)	= yy(ieth_kpp)
      cbox(ich3oh_z)	= yy(ich3oh_kpp)
      cbox(io3p_z)	= yy(io3p_kpp)
      cbox(icres_z)	= yy(icres_kpp)
      cbox(ico_z)	= yy(ico_kpp)
      cbox(ixpar_z)	= yy(ixpar_kpp)
      cbox(ihno3_z)	= yy(ihno3_kpp)
      cbox(iopen_z)	= yy(iopen_kpp)
      cbox(irooh_z)	= yy(irooh_kpp)
      cbox(iaone_z)	= yy(iaone_kpp)
      cbox(iolet_z)	= yy(iolet_kpp)
      cbox(ihcho_z)	= yy(ihcho_kpp)
      cbox(ixo2_z)	= yy(ixo2_kpp)
      cbox(iolei_z)	= yy(iolei_kpp)
      cbox(imgly_z)	= yy(imgly_kpp)
      cbox(iethp_z)	= yy(iethp_kpp)
      cbox(inap_z)	= yy(inap_kpp)
      cbox(iald2_z)	= yy(iald2_kpp)
      cbox(ich3o2_z)	= yy(ich3o2_kpp)
      cbox(iano2_z)	= yy(iano2_kpp)
      cbox(iro2_z)	= yy(iro2_kpp)
      cbox(ionit_z)	= yy(ionit_kpp)
      cbox(iho2_z)	= yy(iho2_kpp)
      cbox(io3_z)	= yy(io3_kpp)
      cbox(ioh_z)	= yy(ioh_kpp)
      cbox(ic2o3_z)	= yy(ic2o3_kpp)
      cbox(ino_z)	= yy(ino_kpp)
      cbox(ino2_z)	= yy(ino2_kpp)
      cbox(ino3_z)	= yy(ino3_kpp)

      return
      end subroutine cbmz_v02r02_mapconcs                                


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r02_maprates(   &
          rk_m1,   &
          rk_m2,   &
          rk_m3,   &
          rk_m4,   &
          rconst )
!
!   maps reaction rate constants (host code array --> kpp rconst array)
!   for mechanism-version-regime = cbmz_v02r02
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     all are host-code reaction-rate-constant arrays [input]
      real rk_m1(*)
      real rk_m2(*)
      real rk_m3(*)
      real rk_m4(*)

      real rconst(nreact_kppmax)

!   local variables
      integer i

      do i = 1, nreact_kppmax
          rconst(i) = 0.
      end do


      rconst(1) = (rk_m1(1))
      rconst(2) = (rk_m1(2))
      rconst(3) = (rk_m1(3))
      rconst(4) = (rk_m1(4))
      rconst(5) = (rk_m1(5))
      rconst(6) = (rk_m1(6))
      rconst(7) = (rk_m1(7))
      rconst(8) = (rk_m1(8))
      rconst(9) = (rk_m1(9))
      rconst(10) = (rk_m1(10))
      rconst(11) = (rk_m1(11))
      rconst(12) = (rk_m1(12))
      rconst(13) = (rk_m1(13))
      rconst(14) = (rk_m1(14))
      rconst(15) = (rk_m1(15))
      rconst(16) = (rk_m1(16))
      rconst(17) = (rk_m1(17))
      rconst(18) = (rk_m1(18))
      rconst(19) = (rk_m1(19))
      rconst(20) = (rk_m1(20))
      rconst(21) = (rk_m1(21))
      rconst(22) = (rk_m1(22))
      rconst(23) = (rk_m1(23))
      rconst(24) = (rk_m1(24))
      rconst(25) = (rk_m1(25))
      rconst(26) = (rk_m1(26))
      rconst(27) = (rk_m1(27))
      rconst(28) = (rk_m1(28))
      rconst(29) = (rk_m1(29))
      rconst(30) = (rk_m1(30))
      rconst(31) = (rk_m1(31))
      rconst(32) = (rk_m1(32))
      rconst(33) = (rk_m1(33))
      rconst(34) = (rk_m1(34))
      rconst(35) = (rk_m1(35))
      rconst(36) = (rk_m1(36))
      rconst(37) = (rk_m1(37))
      rconst(38) = (rk_m1(38))
      rconst(39) = (rk_m1(39))
      rconst(40) = (rk_m1(40))
      rconst(41) = (rk_m1(41))
      rconst(42) = (rk_m1(42))
      rconst(43) = (rk_m1(43))
      rconst(44) = (rk_m1(44))
      rconst(45) = (rk_m1(45))
      rconst(46) = (rk_m1(46))
      rconst(47) = (rk_m1(47))
      rconst(48) = (rk_m1(48))
      rconst(49) = (rk_m1(49))
      rconst(50) = (rk_m1(50))
      rconst(51) = (rk_m1(51))
      rconst(52) = (rk_m1(52))
      rconst(53) = (rk_m1(53))
      rconst(54) = (rk_m1(54))
      rconst(55) = (rk_m1(55))
      rconst(56) = (rk_m1(56))
      rconst(57) = (rk_m1(57))
      rconst(58) = (rk_m1(58))
      rconst(59) = (rk_m1(59))
      rconst(60) = (rk_m1(60))
      rconst(61) = (rk_m1(61))
      rconst(62) = (rk_m1(62))
      rconst(63) = (rk_m1(63))
      rconst(64) = (rk_m1(64))
      rconst(65) = (rk_m1(65))
      rconst(66) = (rk_m2(2))
      rconst(67) = (rk_m2(3))
      rconst(68) = (rk_m2(4))
      rconst(69) = (rk_m2(31))
      rconst(70) = (rk_m2(32))
      rconst(71) = (rk_m2(34))
      rconst(72) = (rk_m2(39))
      rconst(73) = (rk_m2(44))
      rconst(74) = (rk_m2(49))
      rconst(75) = (rk_m2(1))
      rconst(76) = (rk_m2(5))
      rconst(77) = (rk_m2(6))
      rconst(78) = (rk_m2(7))
      rconst(79) = (rk_m2(8))
      rconst(80) = (rk_m2(9))
      rconst(81) = (rk_m2(10))
      rconst(82) = (rk_m2(11))
      rconst(83) = (rk_m2(12))
      rconst(84) = (rk_m2(13))
      rconst(85) = (rk_m2(14))
      rconst(86) = (rk_m2(15))
      rconst(87) = (rk_m2(16))
      rconst(88) = (rk_m2(17))
      rconst(89) = (rk_m2(18))
      rconst(90) = (rk_m2(19))
      rconst(91) = (rk_m2(20))
      rconst(92) = (rk_m2(21))
      rconst(93) = (rk_m2(22))
      rconst(94) = (rk_m2(23))
      rconst(95) = (rk_m2(24))
      rconst(96) = (rk_m2(25))
      rconst(97) = (rk_m2(26))
      rconst(98) = (rk_m2(27))
      rconst(99) = (rk_m2(28))
      rconst(100) = (rk_m2(29))
      rconst(101) = (rk_m2(30))
      rconst(102) = (rk_m2(33))
      rconst(103) = (rk_m2(35))
      rconst(104) = (rk_m2(36))
      rconst(105) = (rk_m2(37))
      rconst(106) = (rk_m2(38))
      rconst(107) = (rk_m2(40))
      rconst(108) = (rk_m2(41))
      rconst(109) = (rk_m2(42))
      rconst(110) = (rk_m2(43))
      rconst(111) = (rk_m2(45))
      rconst(112) = (rk_m2(46))
      rconst(113) = (rk_m2(47))
      rconst(114) = (rk_m2(48))
      rconst(115) = (rk_m2(50))
      rconst(116) = (rk_m2(51))
      rconst(117) = (rk_m2(52))
      rconst(118) = (rk_m2(53))
      return
      end subroutine cbmz_v02r02_maprates 


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r02_dydt( nvardum, tdum, v, a_var, f, rconst )
!
!   computes rates of change for mechanism-version-regime = cbmz_v02r02
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     nvardum = number of variable species [input]
      integer nvardum
!     tdum = time [input]
      real tdum
!     v = concentrations of variable species [input]
      real v(nvar_r02_kpp)
!     a_var = dydt for each species [output]
      real a_var(nvar_r02_kpp)
!     f = concentrations of fixed species [input]
      real f(nfixed_kppmax)
!     rconst = reaction rate constants [input]
      real rconst(nreact_r02_kpp)

!   local variables
!     a = rate for each reaction
      real a(nreact_r02_kpp)

! computation of equation rates
      a(1) = rconst(1)*v(47)
      a(2) = rconst(2)*v(48)
      a(3) = rconst(3)*v(19)
      a(4) = rconst(4)*v(26)
      a(5) = rconst(5)*v(15)
      a(6) = rconst(6)*v(11)
      a(7) = rconst(7)*v(43)
      a(8) = rconst(8)*v(43)
      a(9) = rconst(9)*v(7)
      a(10) = rconst(10)*v(4)*f(4)
      a(11) = rconst(11)*v(4)*f(5)
      a(12) = rconst(12)*v(4)*f(2)
      a(13) = rconst(13)*v(22)*f(4)
      a(14) = rconst(14)*v(22)*v(43)
      a(15) = rconst(15)*v(22)*v(47)
      a(16) = rconst(16)*v(22)*v(47)
      a(17) = rconst(17)*v(22)*v(46)
      a(18) = rconst(18)*v(43)*v(46)
      a(19) = rconst(19)*v(43)*v(47)
      a(20) = rconst(20)*v(43)*v(44)
      a(21) = rconst(21)*v(42)*v(43)
      a(22) = rconst(22)*v(44)*f(3)
      a(23) = rconst(23)*v(44)*v(46)
      a(24) = rconst(24)*v(44)*v(47)
      a(25) = rconst(25)*v(44)*v(48)
      a(26) = rconst(26)*v(19)*v(44)
      a(27) = rconst(27)*v(26)*v(44)
      a(28) = rconst(28)*v(15)*v(44)
      a(29) = rconst(29)*v(42)*v(44)
      a(30) = rconst(30)*v(7)*v(44)
      a(31) = rconst(31)*v(42)*v(42)
      a(32) = rconst(32)*v(42)*v(42)*f(2)
      a(33) = rconst(33)*v(42)*v(46)
      a(34) = rconst(34)*v(42)*v(47)
      a(35) = rconst(35)*v(42)*v(47)
      a(36) = rconst(36)*v(15)
      a(37) = rconst(37)*v(46)*v(48)
      a(38) = rconst(38)*v(47)*v(48)
      a(39) = rconst(39)*v(47)*v(48)
      a(40) = rconst(40)*v(48)*v(48)
      a(41) = rconst(41)*v(42)*v(48)
      a(42) = rconst(42)*v(11)*f(2)
      a(43) = rconst(43)*v(11)
      a(44) = rconst(44)*v(24)*v(44)
      a(45) = rconst(45)*v(5)*v(44)
      a(46) = rconst(46)*v(44)*f(1)
      a(47) = rconst(47)*v(8)*v(44)
      a(48) = rconst(48)*v(21)*v(44)
      a(49) = rconst(49)*v(31)
      a(50) = rconst(50)*v(31)
      a(51) = rconst(51)*v(31)*v(44)
      a(52) = rconst(52)*v(31)*v(48)
      a(53) = rconst(53)*v(17)
      a(54) = rconst(54)*v(18)
      a(55) = rconst(55)*v(17)*v(44)
      a(56) = rconst(56)*v(18)*v(44)
      a(57) = rconst(57)*v(38)*v(46)
      a(58) = rconst(58)*v(35)*v(46)
      a(59) = rconst(59)*v(38)*v(48)
      a(60) = rconst(60)*v(35)*v(48)
      a(61) = rconst(61)*v(38)*v(42)
      a(62) = rconst(62)*v(35)*v(42)
      a(63) = rconst(63)*v(38)
      a(64) = rconst(64)*v(35)
      a(65) = rconst(65)*v(6)*v(44)
      a(66) = rconst(66)*v(37)
      a(67) = rconst(67)*v(37)*v(44)
      a(68) = rconst(68)*v(37)*v(48)
      a(69) = rconst(69)*v(45)*v(47)
      a(70) = rconst(70)*v(9)
      a(71) = rconst(71)*v(45)*v(46)
      a(72) = rconst(72)*v(45)*v(48)
      a(73) = rconst(73)*v(42)*v(45)
      a(74) = rconst(74)*v(45)
      a(75) = rconst(75)*v(13)*v(44)
      a(76) = rconst(76)*v(29)
      a(77) = rconst(77)*v(29)*v(44)
      a(78) = rconst(78)*v(34)
      a(79) = rconst(79)*v(34)*v(44)
      a(80) = rconst(80)*v(34)*v(48)
      a(81) = rconst(81)*v(20)*v(43)
      a(82) = rconst(82)*v(20)*v(44)
      a(83) = rconst(83)*v(30)*v(43)
      a(84) = rconst(84)*v(33)*v(43)
      a(85) = rconst(85)*v(30)*v(44)
      a(86) = rconst(86)*v(33)*v(44)
      a(87) = rconst(87)*v(30)*v(48)
      a(88) = rconst(88)*v(33)*v(48)
      a(89) = rconst(89)*v(10)*v(44)
      a(90) = rconst(90)*v(12)*v(44)
      a(91) = rconst(91)*v(16)*v(46)
      a(92) = rconst(92)*v(23)*v(44)
      a(93) = rconst(93)*v(23)*v(48)
      a(94) = rconst(94)*v(14)*v(47)
      a(95) = rconst(95)*v(27)*v(44)
      a(96) = rconst(96)*v(27)
      a(97) = rconst(97)*v(27)*v(43)
      a(98) = rconst(98)*v(28)
      a(99) = rconst(99)*v(28)*v(44)
      a(100) = rconst(100)*v(41)*v(44)
      a(101) = rconst(101)*v(41)
      a(102) = rconst(102)*v(40)*v(46)
      a(103) = rconst(103)*v(39)*v(46)
      a(104) = rconst(104)*v(36)*v(46)
      a(105) = rconst(105)*v(32)*v(46)
      a(106) = rconst(106)*v(40)*v(48)
      a(107) = rconst(107)*v(39)*v(48)
      a(108) = rconst(108)*v(36)*v(48)
      a(109) = rconst(109)*v(32)*v(48)
      a(110) = rconst(110)*v(40)*v(42)
      a(111) = rconst(111)*v(39)*v(42)
      a(112) = rconst(112)*v(36)*v(42)
      a(113) = rconst(113)*v(32)*v(42)
      a(114) = rconst(114)*v(40)
      a(115) = rconst(115)*v(39)
      a(116) = rconst(116)*v(36)
      a(117) = rconst(117)*v(32)
      a(118) = rconst(118)*v(13)*v(25)

! aggregate function
      a_var(1) = a(45)
      a_var(2) = 0.52*a(81)+0.22*a(83)
      a_var(3) = 0.4*a(73)+0.09*a(83)+0.16*a(84)
      a_var(4) = a(8)-a(10)-a(11)-a(12)
      a_var(5) = -a(45)
      a_var(6) = -a(65)
      a_var(7) = -a(9)-a(30)+a(31)+a(32)
      a_var(8) = -a(47)+0.2*a(64)
      a_var(9) = a(69)-a(70)
      a_var(10) = -a(89)
      a_var(11) = -a(6)+a(39)-a(42)-a(43)
      a_var(12) = -a(90)
      a_var(13) = -a(75)+1.1*a(90)-a(118)
      a_var(14) = 0.4*a(92)+a(93)-a(94)
      a_var(15) = -a(5)-a(28)+a(34)-a(36)
      a_var(16) = 0.8*a(89)+0.45*a(90)-a(91)
      a_var(17) = -a(53)-a(55)+a(61)
      a_var(18) = -a(54)-a(56)+a(62)
      a_var(19) = -a(3)+a(23)-a(26)+a(35)
      a_var(20) = -a(81)-a(82)
      a_var(21) = -a(48)+0.34*a(63)+0.03*a(83)+0.04*a(84)
      a_var(22) = a(1)+0.89*a(2)+a(7)+a(10)+a(11)-a(13)-a(14)-a(15)   &
                 -a(16)-a(17)
      a_var(23) = 0.12*a(89)+0.05*a(90)-a(92)-a(93)
      a_var(24) = -a(44)+a(49)+a(50)+a(51)+a(52)+a(66)+a(78)+a(80)   &
                 +0.24*a(81)+0.31*a(83)+0.3*a(84)+2*a(95)+a(96)+0.69   &
                 *a(97)
      a_var(25) = 1.06*a(83)+2.26*a(84)+a(85)+2.23*a(86)+1.98*a(98)   &
                 +0.42*a(99)+1.98*a(101)+1.68*a(102)+a(104)+1.98   &
                 *a(106)+a(108)+1.25*a(114)+a(116)-a(118)
      a_var(26) = -a(4)+a(24)-a(27)+0.3*a(41)+2*a(42)+a(52)+a(68)   &
                 +a(80)+a(93)
      a_var(27) = 0.95*a(91)+0.3*a(92)-a(95)-a(96)-a(97)
      a_var(28) = -a(98)-a(99)+a(110)+a(111)
      a_var(29) = -a(76)-a(77)+0.07*a(84)+0.23*a(86)+0.74*a(98)+0.74   &
                 *a(101)+0.62*a(102)+0.74*a(106)+0.57*a(114)+0.15   &
                 *a(115)
      a_var(30) = -a(83)-a(85)-a(87)
      a_var(31) = a(48)-a(49)-a(50)-a(51)-a(52)+a(53)+0.3*a(55)+a(57)   &
                 +a(59)+0.66*a(63)+a(81)+1.56*a(82)+0.57*a(83)+a(85)   &
                 +a(95)+0.7*a(97)+a(103)+0.5*a(104)+a(107)+0.5*a(108)   &
                 +0.7*a(115)+0.5*a(116)
      a_var(32) = a(79)+a(82)+a(85)+a(86)+0.08*a(89)+0.5*a(90)+0.6   &
                 *a(92)+a(95)+0.03*a(97)+0.4*a(98)+0.4*a(101)+0.34   &
                 *a(102)-a(105)+0.4*a(106)-a(109)-a(113)+0.24*a(114)   &
                 -a(117)
      a_var(33) = -a(84)-a(86)-a(88)
      a_var(34) = -a(78)-a(79)-a(80)+0.04*a(83)+0.07*a(84)+0.8*a(90)   &
                 +0.2*a(97)+0.19*a(99)+0.15*a(115)
      a_var(35) = a(47)+0.5*a(56)-a(58)-a(60)-a(62)-a(64)+0.06*a(83)   &
                 +0.05*a(84)+0.1*a(98)+0.1*a(101)+0.08*a(102)+0.1   &
                 *a(106)+0.06*a(114)
      a_var(36) = a(87)+a(88)+a(100)-a(104)-a(108)-a(112)-a(116)
      a_var(37) = a(54)+0.5*a(56)+a(58)+a(60)+0.8*a(64)+a(65)-a(66)   &
                 -a(67)-a(68)+0.22*a(82)+0.47*a(83)+1.03*a(84)+a(85)   &
                 +1.77*a(86)+0.03*a(97)+0.3*a(98)+0.04*a(99)+0.3   &
                 *a(101)+0.25*a(102)+0.5*a(104)+0.3*a(106)+0.5*a(108)   &
                 +0.21*a(114)+0.5*a(116)
      a_var(38) = a(46)+0.7*a(55)-a(57)-a(59)-a(61)-a(63)+a(66)+a(71)   &
                 +a(72)+a(74)+a(76)+0.07*a(83)+0.1*a(84)
      a_var(39) = a(77)+0.11*a(84)-a(103)-a(107)-a(111)-a(115)
      a_var(40) = a(75)+0.03*a(83)+0.09*a(84)+0.77*a(99)-a(102)-a(106)   &
                 -a(110)-a(114)
      a_var(41) = 0.05*a(91)+a(94)-a(100)-a(101)+0.16*a(102)+0.5   &
                 *a(104)+0.5*a(108)+a(112)+0.5*a(116)
      a_var(42) = a(5)+a(20)-a(21)+a(22)+a(25)-a(29)+a(30)-2*a(31)-2   &
                 *a(32)-a(33)-a(34)-a(35)+a(36)-a(41)+a(44)+a(45)   &
                 +a(48)+2*a(49)+a(51)+a(52)+a(53)+a(54)+a(57)+a(58)   &
                 +a(59)+a(60)-a(61)-a(62)+0.32*a(63)+0.6*a(64)+a(65)   &
                 +a(66)-a(73)+a(78)+0.22*a(81)+a(82)+0.26*a(83)+0.22   &
                 *a(84)+a(85)+a(86)+0.2*a(89)+0.55*a(90)+0.95*a(91)   &
                 +0.6*a(92)+2*a(95)+a(96)+0.76*a(97)+0.9*a(98)+0.9   &
                 *a(101)+0.76*a(102)+0.5*a(104)+0.9*a(106)+0.5*a(108)   &
                 -a(110)-a(111)-a(112)-a(113)+0.54*a(114)
      a_var(43) = -a(7)-a(8)+a(13)-a(14)-a(18)-a(19)-a(20)-a(21)+0.4   &
                 *a(73)-a(81)-a(83)-a(84)-a(97)
      a_var(44) = a(3)+a(4)+2*a(9)+2*a(12)-a(20)+a(21)-a(22)-a(23)   &
                 -a(24)-a(25)-a(26)-a(27)-a(28)-a(29)-a(30)+a(33)+0.7   &
                 *a(41)-a(44)-a(45)-a(46)-a(47)-a(48)-a(51)+a(53)   &
                 +a(54)-0.7*a(55)-0.5*a(56)-a(65)-a(67)-a(75)-a(77)   &
                 -a(79)+0.12*a(81)-a(82)+0.33*a(83)+0.6*a(84)-a(85)   &
                 -a(86)-a(89)-a(90)-a(92)-a(95)+0.08*a(97)+a(98)-0.77   &
                 *a(99)-a(100)
      a_var(45) = a(67)+a(68)-a(69)+a(70)-a(71)-a(72)-a(73)-a(74)   &
                 +a(76)+a(78)+a(79)+a(80)+0.13*a(83)+0.19*a(84)+a(95)   &
                 +a(96)+0.62*a(97)+a(103)+a(107)+0.7*a(115)
      a_var(46) = a(1)+0.11*a(2)+a(3)+a(15)-a(17)-a(18)-a(23)-a(33)   &
                 -a(37)+a(38)-a(57)-a(58)-a(71)-a(91)-a(102)-a(103)   &
                 -a(104)-a(105)
      a_var(47) = -a(1)+0.89*a(2)+a(4)+a(5)+a(6)-a(15)-a(16)+a(17)   &
                 +a(18)-a(19)-a(24)+a(25)+a(26)+a(28)+a(33)-a(34)   &
                 -a(35)+a(36)+2*a(37)-a(39)+2*a(40)+0.7*a(41)+a(43)   &
                 +a(57)+a(58)+a(59)+a(60)-a(69)+a(70)+a(71)+a(72)+0.95   &
                 *a(91)-a(94)+a(101)+0.84*a(102)+a(103)+1.5*a(104)   &
                 +a(105)+a(106)+a(107)+1.5*a(108)+a(109)+0.5*a(116)
      a_var(48) = -a(2)+a(6)+a(16)+a(19)-a(25)+a(27)-a(37)-a(38)-a(39)   &
                 -2*a(40)-a(41)+a(43)-a(52)-a(59)-a(60)-a(68)-a(72)   &
                 -a(80)-a(87)-a(88)-a(93)-a(106)-a(107)-a(108)-a(109)
      return
      end subroutine cbmz_v02r02_dydt                                      


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r02_jacob( nvardum, tdum, v, jvs, f, rconst )
!
!   computes jacobian for mechanism-version-regime = cbmz_v02r02
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     nvardum = number of variable species [input]
      integer nvardum
!     tdum = time [input]
      real tdum
!     v = concentrations of variable species [input]
      real v(nvar_r02_kpp)
!     jvs = non-zero jacobian elements [output]
      real jvs(lu_nonzero_v_r02_kpp)
!     f = concentrations of fixed species [input]
      real f(nfixed_kppmax)
!     rconst = reaction rate constants [input]
      real rconst(nreact_r02_kpp)

!   local variables
!     b(i,j) = d[reaction_rate(i)] / d[species_conc(j)]
      real b(nreact_r02_kpp,nvar_r02_kpp)

! computation of b(i,j) = da(i)/dv(j)
      b(1,47) = rconst(1)
      b(2,48) = rconst(2)
      b(3,19) = rconst(3)
      b(4,26) = rconst(4)
      b(5,15) = rconst(5)
      b(6,11) = rconst(6)
      b(7,43) = rconst(7)
      b(8,43) = rconst(8)
      b(9,7) = rconst(9)
      b(10,4) = rconst(10)*f(4)
      b(11,4) = rconst(11)*f(5)
      b(12,4) = rconst(12)*f(2)
      b(13,22) = rconst(13)*f(4)
      b(14,22) = rconst(14)*v(43)
      b(14,43) = rconst(14)*v(22)
      b(15,22) = rconst(15)*v(47)
      b(15,47) = rconst(15)*v(22)
      b(16,22) = rconst(16)*v(47)
      b(16,47) = rconst(16)*v(22)
      b(17,22) = rconst(17)*v(46)
      b(17,46) = rconst(17)*v(22)
      b(18,43) = rconst(18)*v(46)
      b(18,46) = rconst(18)*v(43)
      b(19,43) = rconst(19)*v(47)
      b(19,47) = rconst(19)*v(43)
      b(20,43) = rconst(20)*v(44)
      b(20,44) = rconst(20)*v(43)
      b(21,42) = rconst(21)*v(43)
      b(21,43) = rconst(21)*v(42)
      b(22,44) = rconst(22)*f(3)
      b(23,44) = rconst(23)*v(46)
      b(23,46) = rconst(23)*v(44)
      b(24,44) = rconst(24)*v(47)
      b(24,47) = rconst(24)*v(44)
      b(25,44) = rconst(25)*v(48)
      b(25,48) = rconst(25)*v(44)
      b(26,19) = rconst(26)*v(44)
      b(26,44) = rconst(26)*v(19)
      b(27,26) = rconst(27)*v(44)
      b(27,44) = rconst(27)*v(26)
      b(28,15) = rconst(28)*v(44)
      b(28,44) = rconst(28)*v(15)
      b(29,42) = rconst(29)*v(44)
      b(29,44) = rconst(29)*v(42)
      b(30,7) = rconst(30)*v(44)
      b(30,44) = rconst(30)*v(7)
      b(31,42) = rconst(31)*2*v(42)
      b(32,42) = rconst(32)*2*v(42)*f(2)
      b(33,42) = rconst(33)*v(46)
      b(33,46) = rconst(33)*v(42)
      b(34,42) = rconst(34)*v(47)
      b(34,47) = rconst(34)*v(42)
      b(35,42) = rconst(35)*v(47)
      b(35,47) = rconst(35)*v(42)
      b(36,15) = rconst(36)
      b(37,46) = rconst(37)*v(48)
      b(37,48) = rconst(37)*v(46)
      b(38,47) = rconst(38)*v(48)
      b(38,48) = rconst(38)*v(47)
      b(39,47) = rconst(39)*v(48)
      b(39,48) = rconst(39)*v(47)
      b(40,48) = rconst(40)*2*v(48)
      b(41,42) = rconst(41)*v(48)
      b(41,48) = rconst(41)*v(42)
      b(42,11) = rconst(42)*f(2)
      b(43,11) = rconst(43)
      b(44,24) = rconst(44)*v(44)
      b(44,44) = rconst(44)*v(24)
      b(45,5) = rconst(45)*v(44)
      b(45,44) = rconst(45)*v(5)
      b(46,44) = rconst(46)*f(1)
      b(47,8) = rconst(47)*v(44)
      b(47,44) = rconst(47)*v(8)
      b(48,21) = rconst(48)*v(44)
      b(48,44) = rconst(48)*v(21)
      b(49,31) = rconst(49)
      b(50,31) = rconst(50)
      b(51,31) = rconst(51)*v(44)
      b(51,44) = rconst(51)*v(31)
      b(52,31) = rconst(52)*v(48)
      b(52,48) = rconst(52)*v(31)
      b(53,17) = rconst(53)
      b(54,18) = rconst(54)
      b(55,17) = rconst(55)*v(44)
      b(55,44) = rconst(55)*v(17)
      b(56,18) = rconst(56)*v(44)
      b(56,44) = rconst(56)*v(18)
      b(57,38) = rconst(57)*v(46)
      b(57,46) = rconst(57)*v(38)
      b(58,35) = rconst(58)*v(46)
      b(58,46) = rconst(58)*v(35)
      b(59,38) = rconst(59)*v(48)
      b(59,48) = rconst(59)*v(38)
      b(60,35) = rconst(60)*v(48)
      b(60,48) = rconst(60)*v(35)
      b(61,38) = rconst(61)*v(42)
      b(61,42) = rconst(61)*v(38)
      b(62,35) = rconst(62)*v(42)
      b(62,42) = rconst(62)*v(35)
      b(63,38) = rconst(63)
      b(64,35) = rconst(64)
      b(65,6) = rconst(65)*v(44)
      b(65,44) = rconst(65)*v(6)
      b(66,37) = rconst(66)
      b(67,37) = rconst(67)*v(44)
      b(67,44) = rconst(67)*v(37)
      b(68,37) = rconst(68)*v(48)
      b(68,48) = rconst(68)*v(37)
      b(69,45) = rconst(69)*v(47)
      b(69,47) = rconst(69)*v(45)
      b(70,9) = rconst(70)
      b(71,45) = rconst(71)*v(46)
      b(71,46) = rconst(71)*v(45)
      b(72,45) = rconst(72)*v(48)
      b(72,48) = rconst(72)*v(45)
      b(73,42) = rconst(73)*v(45)
      b(73,45) = rconst(73)*v(42)
      b(74,45) = rconst(74)
      b(75,13) = rconst(75)*v(44)
      b(75,44) = rconst(75)*v(13)
      b(76,29) = rconst(76)
      b(77,29) = rconst(77)*v(44)
      b(77,44) = rconst(77)*v(29)
      b(78,34) = rconst(78)
      b(79,34) = rconst(79)*v(44)
      b(79,44) = rconst(79)*v(34)
      b(80,34) = rconst(80)*v(48)
      b(80,48) = rconst(80)*v(34)
      b(81,20) = rconst(81)*v(43)
      b(81,43) = rconst(81)*v(20)
      b(82,20) = rconst(82)*v(44)
      b(82,44) = rconst(82)*v(20)
      b(83,30) = rconst(83)*v(43)
      b(83,43) = rconst(83)*v(30)
      b(84,33) = rconst(84)*v(43)
      b(84,43) = rconst(84)*v(33)
      b(85,30) = rconst(85)*v(44)
      b(85,44) = rconst(85)*v(30)
      b(86,33) = rconst(86)*v(44)
      b(86,44) = rconst(86)*v(33)
      b(87,30) = rconst(87)*v(48)
      b(87,48) = rconst(87)*v(30)
      b(88,33) = rconst(88)*v(48)
      b(88,48) = rconst(88)*v(33)
      b(89,10) = rconst(89)*v(44)
      b(89,44) = rconst(89)*v(10)
      b(90,12) = rconst(90)*v(44)
      b(90,44) = rconst(90)*v(12)
      b(91,16) = rconst(91)*v(46)
      b(91,46) = rconst(91)*v(16)
      b(92,23) = rconst(92)*v(44)
      b(92,44) = rconst(92)*v(23)
      b(93,23) = rconst(93)*v(48)
      b(93,48) = rconst(93)*v(23)
      b(94,14) = rconst(94)*v(47)
      b(94,47) = rconst(94)*v(14)
      b(95,27) = rconst(95)*v(44)
      b(95,44) = rconst(95)*v(27)
      b(96,27) = rconst(96)
      b(97,27) = rconst(97)*v(43)
      b(97,43) = rconst(97)*v(27)
      b(98,28) = rconst(98)
      b(99,28) = rconst(99)*v(44)
      b(99,44) = rconst(99)*v(28)
      b(100,41) = rconst(100)*v(44)
      b(100,44) = rconst(100)*v(41)
      b(101,41) = rconst(101)
      b(102,40) = rconst(102)*v(46)
      b(102,46) = rconst(102)*v(40)
      b(103,39) = rconst(103)*v(46)
      b(103,46) = rconst(103)*v(39)
      b(104,36) = rconst(104)*v(46)
      b(104,46) = rconst(104)*v(36)
      b(105,32) = rconst(105)*v(46)
      b(105,46) = rconst(105)*v(32)
      b(106,40) = rconst(106)*v(48)
      b(106,48) = rconst(106)*v(40)
      b(107,39) = rconst(107)*v(48)
      b(107,48) = rconst(107)*v(39)
      b(108,36) = rconst(108)*v(48)
      b(108,48) = rconst(108)*v(36)
      b(109,32) = rconst(109)*v(48)
      b(109,48) = rconst(109)*v(32)
      b(110,40) = rconst(110)*v(42)
      b(110,42) = rconst(110)*v(40)
      b(111,39) = rconst(111)*v(42)
      b(111,42) = rconst(111)*v(39)
      b(112,36) = rconst(112)*v(42)
      b(112,42) = rconst(112)*v(36)
      b(113,32) = rconst(113)*v(42)
      b(113,42) = rconst(113)*v(32)
      b(114,40) = rconst(114)
      b(115,39) = rconst(115)
      b(116,36) = rconst(116)
      b(117,32) = rconst(117)
      b(118,13) = rconst(118)*v(25)
      b(118,25) = rconst(118)*v(13)

! construct the jacobian terms from b's
      jvs(1) = 0
      jvs(2) = b(45,5)
      jvs(3) = b(45,44)
      jvs(4) = 0
      jvs(5) = 0.52*b(81,20)
      jvs(6) = 0.22*b(83,30)
      jvs(7) = 0.52*b(81,43)+0.22*b(83,43)
      jvs(8) = 0
      jvs(9) = 0.09*b(83,30)
      jvs(10) = 0.16*b(84,33)
      jvs(11) = 0.4*b(73,42)
      jvs(12) = 0.09*b(83,43)+0.16*b(84,43)
      jvs(13) = 0.4*b(73,45)
      jvs(14) = -b(10,4)-b(11,4)-b(12,4)
      jvs(15) = b(8,43)
      jvs(16) = -b(45,5)
      jvs(17) = -b(45,44)
      jvs(18) = -b(65,6)
      jvs(19) = -b(65,44)
      jvs(20) = -b(9,7)-b(30,7)
      jvs(21) = b(31,42)+b(32,42)
      jvs(22) = -b(30,44)
      jvs(23) = -b(47,8)
      jvs(24) = 0.2*b(64,35)
      jvs(25) = -b(47,44)
      jvs(26) = -b(70,9)
      jvs(27) = b(69,45)
      jvs(28) = b(69,47)
      jvs(29) = -b(89,10)
      jvs(30) = -b(89,44)
      jvs(31) = -b(6,11)-b(42,11)-b(43,11)
      jvs(32) = b(39,47)
      jvs(33) = b(39,48)
      jvs(34) = -b(90,12)
      jvs(35) = -b(90,44)
      jvs(36) = 1.1*b(90,12)
      jvs(37) = -b(75,13)-b(118,13)
      jvs(38) = -b(118,25)
      jvs(39) = -b(75,44)+1.1*b(90,44)
      jvs(40) = -b(94,14)
      jvs(41) = 0.4*b(92,23)+b(93,23)
      jvs(42) = 0.4*b(92,44)
      jvs(43) = -b(94,47)
      jvs(44) = b(93,48)
      jvs(45) = -b(5,15)-b(28,15)-b(36,15)
      jvs(46) = b(34,42)
      jvs(47) = -b(28,44)
      jvs(48) = b(34,47)
      jvs(49) = 0.8*b(89,10)
      jvs(50) = 0.45*b(90,12)
      jvs(51) = -b(91,16)
      jvs(52) = 0.8*b(89,44)+0.45*b(90,44)
      jvs(53) = -b(91,46)
      jvs(54) = -b(53,17)-b(55,17)
      jvs(55) = b(61,38)
      jvs(56) = b(61,42)
      jvs(57) = -b(55,44)
      jvs(58) = -b(54,18)-b(56,18)
      jvs(59) = b(62,35)
      jvs(60) = b(62,42)
      jvs(61) = -b(56,44)
      jvs(62) = -b(3,19)-b(26,19)
      jvs(63) = b(35,42)
      jvs(64) = b(23,44)-b(26,44)
      jvs(65) = b(23,46)
      jvs(66) = b(35,47)
      jvs(67) = -b(81,20)-b(82,20)
      jvs(68) = -b(81,43)
      jvs(69) = -b(82,44)
      jvs(70) = -b(48,21)
      jvs(71) = 0.03*b(83,30)
      jvs(72) = 0.04*b(84,33)
      jvs(73) = 0.34*b(63,38)
      jvs(74) = 0.03*b(83,43)+0.04*b(84,43)
      jvs(75) = -b(48,44)
      jvs(76) = b(10,4)+b(11,4)
      jvs(77) = -b(13,22)-b(14,22)-b(15,22)-b(16,22)-b(17,22)
      jvs(78) = b(7,43)-b(14,43)
      jvs(79) = -b(17,46)
      jvs(80) = b(1,47)-b(15,47)-b(16,47)
      jvs(81) = 0.89*b(2,48)
      jvs(82) = 0.12*b(89,10)
      jvs(83) = 0.05*b(90,12)
      jvs(84) = -b(92,23)-b(93,23)
      jvs(85) = 0.12*b(89,44)+0.05*b(90,44)-b(92,44)
      jvs(86) = -b(93,48)
      jvs(87) = 0.24*b(81,20)
      jvs(88) = -b(44,24)
      jvs(89) = 2*b(95,27)+b(96,27)+0.69*b(97,27)
      jvs(90) = 0.31*b(83,30)
      jvs(91) = b(49,31)+b(50,31)+b(51,31)+b(52,31)
      jvs(92) = 0.3*b(84,33)
      jvs(93) = b(78,34)+b(80,34)
      jvs(94) = b(66,37)
      jvs(95) = 0.24*b(81,43)+0.31*b(83,43)+0.3*b(84,43)+0.69*b(97,43)
      jvs(96) = -b(44,44)+b(51,44)+2*b(95,44)
      jvs(97) = b(52,48)+b(80,48)
      jvs(98) = -b(118,13)
      jvs(99) = -b(118,25)
      jvs(100) = 1.98*b(98,28)+0.42*b(99,28)
      jvs(101) = 1.06*b(83,30)+b(85,30)
      jvs(102) = 2.26*b(84,33)+2.23*b(86,33)
      jvs(103) = b(104,36)+b(108,36)+b(116,36)
      jvs(104) = 1.68*b(102,40)+1.98*b(106,40)+1.25*b(114,40)
      jvs(105) = 1.98*b(101,41)
      jvs(106) = 1.06*b(83,43)+2.26*b(84,43)
      jvs(107) = b(85,44)+2.23*b(86,44)+0.42*b(99,44)
      jvs(108) = 1.68*b(102,46)+b(104,46)
      jvs(109) = 1.98*b(106,48)+b(108,48)
      jvs(110) = 2*b(42,11)
      jvs(111) = b(93,23)
      jvs(112) = -b(4,26)-b(27,26)
      jvs(113) = b(52,31)
      jvs(114) = b(80,34)
      jvs(115) = b(68,37)
      jvs(116) = 0.3*b(41,42)
      jvs(117) = b(24,44)-b(27,44)
      jvs(118) = b(24,47)
      jvs(119) = 0.3*b(41,48)+b(52,48)+b(68,48)+b(80,48)+b(93,48)
      jvs(120) = 0.95*b(91,16)
      jvs(121) = 0.3*b(92,23)
      jvs(122) = -b(95,27)-b(96,27)-b(97,27)
      jvs(123) = -b(97,43)
      jvs(124) = 0.3*b(92,44)-b(95,44)
      jvs(125) = 0.95*b(91,46)
      jvs(126) = 0
      jvs(127) = -b(98,28)-b(99,28)
      jvs(128) = b(111,39)
      jvs(129) = b(110,40)
      jvs(130) = b(110,42)+b(111,42)
      jvs(131) = -b(99,44)
      jvs(132) = 0.74*b(98,28)
      jvs(133) = -b(76,29)-b(77,29)
      jvs(134) = 0.07*b(84,33)+0.23*b(86,33)
      jvs(135) = 0.15*b(115,39)
      jvs(136) = 0.62*b(102,40)+0.74*b(106,40)+0.57*b(114,40)
      jvs(137) = 0.74*b(101,41)
      jvs(138) = 0
      jvs(139) = 0.07*b(84,43)
      jvs(140) = -b(77,44)+0.23*b(86,44)
      jvs(141) = 0.62*b(102,46)
      jvs(142) = 0.74*b(106,48)
      jvs(143) = -b(83,30)-b(85,30)-b(87,30)
      jvs(144) = -b(83,43)
      jvs(145) = -b(85,44)
      jvs(146) = -b(87,48)
      jvs(147) = b(53,17)+0.3*b(55,17)
      jvs(148) = b(81,20)+1.56*b(82,20)
      jvs(149) = b(48,21)
      jvs(150) = b(95,27)+0.7*b(97,27)
      jvs(151) = 0.57*b(83,30)+b(85,30)
      jvs(152) = -b(49,31)-b(50,31)-b(51,31)-b(52,31)
      jvs(153) = 0
      jvs(154) = 0.5*b(104,36)+0.5*b(108,36)+0.5*b(116,36)
      jvs(155) = b(57,38)+b(59,38)+0.66*b(63,38)
      jvs(156) = b(103,39)+b(107,39)+0.7*b(115,39)
      jvs(157) = 0
      jvs(158) = b(81,43)+0.57*b(83,43)+0.7*b(97,43)
      jvs(159) = b(48,44)-b(51,44)+0.3*b(55,44)+1.56*b(82,44)+b(85,44)   &
                +b(95,44)
      jvs(160) = b(57,46)+b(103,46)+0.5*b(104,46)
      jvs(161) = -b(52,48)+b(59,48)+b(107,48)+0.5*b(108,48)
      jvs(162) = 0.08*b(89,10)
      jvs(163) = 0.5*b(90,12)
      jvs(164) = b(82,20)
      jvs(165) = 0.6*b(92,23)
      jvs(166) = b(95,27)+0.03*b(97,27)
      jvs(167) = 0.4*b(98,28)
      jvs(168) = b(85,30)
      jvs(169) = -b(105,32)-b(109,32)-b(113,32)-b(117,32)
      jvs(170) = b(86,33)
      jvs(171) = b(79,34)
      jvs(172) = 0
      jvs(173) = 0.34*b(102,40)+0.4*b(106,40)+0.24*b(114,40)
      jvs(174) = 0.4*b(101,41)
      jvs(175) = -b(113,42)
      jvs(176) = 0.03*b(97,43)
      jvs(177) = b(79,44)+b(82,44)+b(85,44)+b(86,44)+0.08*b(89,44)+0.5   &
                *b(90,44)+0.6*b(92,44)+b(95,44)
      jvs(178) = 0.34*b(102,46)-b(105,46)
      jvs(179) = 0.4*b(106,48)-b(109,48)
      jvs(180) = -b(84,33)-b(86,33)-b(88,33)
      jvs(181) = -b(84,43)
      jvs(182) = -b(86,44)
      jvs(183) = -b(88,48)
      jvs(184) = 0.8*b(90,12)
      jvs(185) = 0.2*b(97,27)
      jvs(186) = 0.19*b(99,28)
      jvs(187) = 0.04*b(83,30)
      jvs(188) = 0.07*b(84,33)
      jvs(189) = -b(78,34)-b(79,34)-b(80,34)
      jvs(190) = 0.15*b(115,39)
      jvs(191) = 0
      jvs(192) = 0
      jvs(193) = 0.04*b(83,43)+0.07*b(84,43)+0.2*b(97,43)
      jvs(194) = -b(79,44)+0.8*b(90,44)+0.19*b(99,44)
      jvs(195) = 0
      jvs(196) = -b(80,48)
      jvs(197) = b(47,8)
      jvs(198) = 0.5*b(56,18)
      jvs(199) = 0.1*b(98,28)
      jvs(200) = 0.06*b(83,30)
      jvs(201) = 0.05*b(84,33)
      jvs(202) = -b(58,35)-b(60,35)-b(62,35)-b(64,35)
      jvs(203) = 0
      jvs(204) = 0.08*b(102,40)+0.1*b(106,40)+0.06*b(114,40)
      jvs(205) = 0.1*b(101,41)
      jvs(206) = -b(62,42)
      jvs(207) = 0.06*b(83,43)+0.05*b(84,43)
      jvs(208) = b(47,44)+0.5*b(56,44)
      jvs(209) = -b(58,46)+0.08*b(102,46)
      jvs(210) = -b(60,48)+0.1*b(106,48)
      jvs(211) = b(87,30)
      jvs(212) = b(88,33)
      jvs(213) = -b(104,36)-b(108,36)-b(112,36)-b(116,36)
      jvs(214) = b(100,41)
      jvs(215) = -b(112,42)
      jvs(216) = 0
      jvs(217) = b(100,44)
      jvs(218) = -b(104,46)
      jvs(219) = b(87,48)+b(88,48)-b(108,48)
      jvs(220) = b(65,6)
      jvs(221) = b(54,18)+0.5*b(56,18)
      jvs(222) = 0.22*b(82,20)
      jvs(223) = 0.03*b(97,27)
      jvs(224) = 0.3*b(98,28)+0.04*b(99,28)
      jvs(225) = 0.47*b(83,30)+b(85,30)
      jvs(226) = 1.03*b(84,33)+1.77*b(86,33)
      jvs(227) = b(58,35)+b(60,35)+0.8*b(64,35)
      jvs(228) = 0.5*b(104,36)+0.5*b(108,36)+0.5*b(116,36)
      jvs(229) = -b(66,37)-b(67,37)-b(68,37)
      jvs(230) = 0
      jvs(231) = 0.25*b(102,40)+0.3*b(106,40)+0.21*b(114,40)
      jvs(232) = 0.3*b(101,41)
      jvs(233) = 0
      jvs(234) = 0.47*b(83,43)+1.03*b(84,43)+0.03*b(97,43)
      jvs(235) = 0.5*b(56,44)+b(65,44)-b(67,44)+0.22*b(82,44)+b(85,44)   &
                +1.77*b(86,44)+0.04*b(99,44)
      jvs(236) = b(58,46)+0.25*b(102,46)+0.5*b(104,46)
      jvs(237) = b(60,48)-b(68,48)+0.3*b(106,48)+0.5*b(108,48)
      jvs(238) = 0.7*b(55,17)
      jvs(239) = b(76,29)
      jvs(240) = 0.07*b(83,30)
      jvs(241) = 0.1*b(84,33)
      jvs(242) = b(66,37)
      jvs(243) = -b(57,38)-b(59,38)-b(61,38)-b(63,38)
      jvs(244) = 0
      jvs(245) = 0
      jvs(246) = 0
      jvs(247) = -b(61,42)
      jvs(248) = 0.07*b(83,43)+0.1*b(84,43)
      jvs(249) = b(46,44)+0.7*b(55,44)
      jvs(250) = b(71,45)+b(72,45)+b(74,45)
      jvs(251) = -b(57,46)+b(71,46)
      jvs(252) = -b(59,48)+b(72,48)
      jvs(253) = b(77,29)
      jvs(254) = 0.11*b(84,33)
      jvs(255) = -b(103,39)-b(107,39)-b(111,39)-b(115,39)
      jvs(256) = 0
      jvs(257) = 0
      jvs(258) = -b(111,42)
      jvs(259) = 0.11*b(84,43)
      jvs(260) = b(77,44)
      jvs(261) = -b(103,46)
      jvs(262) = -b(107,48)
      jvs(263) = b(75,13)
      jvs(264) = 0
      jvs(265) = 0.77*b(99,28)
      jvs(266) = 0.03*b(83,30)
      jvs(267) = 0.09*b(84,33)
      jvs(268) = 0
      jvs(269) = 0
      jvs(270) = -b(102,40)-b(106,40)-b(110,40)-b(114,40)
      jvs(271) = 0
      jvs(272) = -b(110,42)
      jvs(273) = 0.03*b(83,43)+0.09*b(84,43)
      jvs(274) = b(75,44)+0.77*b(99,44)
      jvs(275) = -b(102,46)
      jvs(276) = -b(106,48)
      jvs(277) = b(94,14)
      jvs(278) = 0.05*b(91,16)
      jvs(279) = 0
      jvs(280) = 0.5*b(104,36)+0.5*b(108,36)+b(112,36)+0.5*b(116,36)
      jvs(281) = 0.16*b(102,40)
      jvs(282) = -b(100,41)-b(101,41)
      jvs(283) = b(112,42)
      jvs(284) = 0
      jvs(285) = -b(100,44)
      jvs(286) = 0.05*b(91,46)+0.16*b(102,46)+0.5*b(104,46)
      jvs(287) = b(94,47)
      jvs(288) = 0.5*b(108,48)
      jvs(289) = b(45,5)
      jvs(290) = b(65,6)
      jvs(291) = b(30,7)
      jvs(292) = 0.2*b(89,10)
      jvs(293) = 0.55*b(90,12)
      jvs(294) = b(5,15)+b(36,15)
      jvs(295) = 0.95*b(91,16)
      jvs(296) = b(53,17)
      jvs(297) = b(54,18)
      jvs(298) = 0.22*b(81,20)+b(82,20)
      jvs(299) = b(48,21)
      jvs(300) = 0.6*b(92,23)
      jvs(301) = b(44,24)
      jvs(302) = 2*b(95,27)+b(96,27)+0.76*b(97,27)
      jvs(303) = 0.9*b(98,28)
      jvs(304) = 0.26*b(83,30)+b(85,30)
      jvs(305) = 2*b(49,31)+b(51,31)+b(52,31)
      jvs(306) = -b(113,32)
      jvs(307) = 0.22*b(84,33)+b(86,33)
      jvs(308) = b(78,34)
      jvs(309) = b(58,35)+b(60,35)-b(62,35)+0.6*b(64,35)
      jvs(310) = 0.5*b(104,36)+0.5*b(108,36)-b(112,36)
      jvs(311) = b(66,37)
      jvs(312) = b(57,38)+b(59,38)-b(61,38)+0.32*b(63,38)
      jvs(313) = -b(111,39)
      jvs(314) = 0.76*b(102,40)+0.9*b(106,40)-b(110,40)+0.54*b(114,40)
      jvs(315) = 0.9*b(101,41)
      jvs(316) = -b(21,42)-b(29,42)-2*b(31,42)-2*b(32,42)-b(33,42)   &
                -b(34,42)-b(35,42)-b(41,42)-b(61,42)-b(62,42)-b(73,42)   &
                -b(110,42)-b(111,42)-b(112,42)-b(113,42)
      jvs(317) = b(20,43)-b(21,43)+0.22*b(81,43)+0.26*b(83,43)+0.22   &
                *b(84,43)+0.76*b(97,43)
      jvs(318) = b(20,44)+b(22,44)+b(25,44)-b(29,44)+b(30,44)+b(44,44)   &
                +b(45,44)+b(48,44)+b(51,44)+b(65,44)+b(82,44)+b(85,44)   &
                +b(86,44)+0.2*b(89,44)+0.55*b(90,44)+0.6*b(92,44)+2   &
                *b(95,44)
      jvs(319) = -b(73,45)
      jvs(320) = -b(33,46)+b(57,46)+b(58,46)+0.95*b(91,46)+0.76   &
                *b(102,46)+0.5*b(104,46)
      jvs(321) = -b(34,47)-b(35,47)
      jvs(322) = b(25,48)-b(41,48)+b(52,48)+b(59,48)+b(60,48)+0.9   &
                *b(106,48)+0.5*b(108,48)
      jvs(323) = -b(81,20)
      jvs(324) = b(13,22)-b(14,22)
      jvs(325) = -b(97,27)
      jvs(326) = -b(83,30)
      jvs(327) = -b(84,33)
      jvs(328) = -b(21,42)+0.4*b(73,42)
      jvs(329) = -b(7,43)-b(8,43)-b(14,43)-b(18,43)-b(19,43)-b(20,43)   &
                -b(21,43)-b(81,43)-b(83,43)-b(84,43)-b(97,43)
      jvs(330) = -b(20,44)
      jvs(331) = 0.4*b(73,45)
      jvs(332) = -b(18,46)
      jvs(333) = -b(19,47)
      jvs(334) = 0
      jvs(335) = 2*b(12,4)
      jvs(336) = -b(45,5)
      jvs(337) = -b(65,6)
      jvs(338) = 2*b(9,7)-b(30,7)
      jvs(339) = -b(47,8)
      jvs(340) = -b(89,10)
      jvs(341) = -b(90,12)
      jvs(342) = -b(75,13)
      jvs(343) = -b(28,15)
      jvs(344) = b(53,17)-0.7*b(55,17)
      jvs(345) = b(54,18)-0.5*b(56,18)
      jvs(346) = b(3,19)-b(26,19)
      jvs(347) = 0.12*b(81,20)-b(82,20)
      jvs(348) = -b(48,21)
      jvs(349) = -b(92,23)
      jvs(350) = -b(44,24)
      jvs(351) = 0
      jvs(352) = b(4,26)-b(27,26)
      jvs(353) = -b(95,27)+0.08*b(97,27)
      jvs(354) = b(98,28)-0.77*b(99,28)
      jvs(355) = -b(77,29)
      jvs(356) = 0.33*b(83,30)-b(85,30)
      jvs(357) = -b(51,31)
      jvs(358) = 0.6*b(84,33)-b(86,33)
      jvs(359) = -b(79,34)
      jvs(360) = 0
      jvs(361) = 0
      jvs(362) = -b(67,37)
      jvs(363) = 0
      jvs(364) = 0
      jvs(365) = 0
      jvs(366) = -b(100,41)
      jvs(367) = b(21,42)-b(29,42)+b(33,42)+0.7*b(41,42)
      jvs(368) = -b(20,43)+b(21,43)+0.12*b(81,43)+0.33*b(83,43)+0.6   &
                *b(84,43)+0.08*b(97,43)
      jvs(369) = -b(20,44)-b(22,44)-b(23,44)-b(24,44)-b(25,44)   &
                -b(26,44)-b(27,44)-b(28,44)-b(29,44)-b(30,44)-b(44,44)   &
                -b(45,44)-b(46,44)-b(47,44)-b(48,44)-b(51,44)-0.7   &
                *b(55,44)-0.5*b(56,44)-b(65,44)-b(67,44)-b(75,44)   &
                -b(77,44)-b(79,44)-b(82,44)-b(85,44)-b(86,44)-b(89,44)   &
                -b(90,44)-b(92,44)-b(95,44)-0.77*b(99,44)-b(100,44)
      jvs(370) = 0
      jvs(371) = -b(23,46)+b(33,46)
      jvs(372) = -b(24,47)
      jvs(373) = -b(25,48)+0.7*b(41,48)
      jvs(374) = b(70,9)
      jvs(375) = b(95,27)+b(96,27)+0.62*b(97,27)
      jvs(376) = b(76,29)
      jvs(377) = 0.13*b(83,30)
      jvs(378) = 0.19*b(84,33)
      jvs(379) = b(78,34)+b(79,34)+b(80,34)
      jvs(380) = b(67,37)+b(68,37)
      jvs(381) = b(103,39)+b(107,39)+0.7*b(115,39)
      jvs(382) = 0
      jvs(383) = 0
      jvs(384) = -b(73,42)
      jvs(385) = 0.13*b(83,43)+0.19*b(84,43)+0.62*b(97,43)
      jvs(386) = b(67,44)+b(79,44)+b(95,44)
      jvs(387) = -b(69,45)-b(71,45)-b(72,45)-b(73,45)-b(74,45)
      jvs(388) = -b(71,46)+b(103,46)
      jvs(389) = -b(69,47)
      jvs(390) = b(68,48)-b(72,48)+b(80,48)+b(107,48)
      jvs(391) = -b(91,16)
      jvs(392) = b(3,19)
      jvs(393) = b(15,22)-b(17,22)
      jvs(394) = -b(105,32)
      jvs(395) = 0
      jvs(396) = 0
      jvs(397) = -b(58,35)
      jvs(398) = -b(104,36)
      jvs(399) = -b(57,38)
      jvs(400) = -b(103,39)
      jvs(401) = -b(102,40)
      jvs(402) = 0
      jvs(403) = -b(33,42)
      jvs(404) = -b(18,43)
      jvs(405) = -b(23,44)
      jvs(406) = -b(71,45)
      jvs(407) = -b(17,46)-b(18,46)-b(23,46)-b(33,46)-b(37,46)   &
                -b(57,46)-b(58,46)-b(71,46)-b(91,46)-b(102,46)   &
                -b(103,46)-b(104,46)-b(105,46)
      jvs(408) = b(1,47)+b(15,47)+b(38,47)
      jvs(409) = 0.11*b(2,48)-b(37,48)+b(38,48)
      jvs(410) = b(70,9)
      jvs(411) = b(6,11)+b(43,11)
      jvs(412) = -b(94,14)
      jvs(413) = b(5,15)+b(28,15)+b(36,15)
      jvs(414) = 0.95*b(91,16)
      jvs(415) = b(26,19)
      jvs(416) = -b(15,22)-b(16,22)+b(17,22)
      jvs(417) = 0
      jvs(418) = b(4,26)
      jvs(419) = 0
      jvs(420) = b(105,32)+b(109,32)
      jvs(421) = 0
      jvs(422) = 0
      jvs(423) = b(58,35)+b(60,35)
      jvs(424) = 1.5*b(104,36)+1.5*b(108,36)+0.5*b(116,36)
      jvs(425) = 0
      jvs(426) = b(57,38)+b(59,38)
      jvs(427) = b(103,39)+b(107,39)
      jvs(428) = 0.84*b(102,40)+b(106,40)
      jvs(429) = b(101,41)
      jvs(430) = b(33,42)-b(34,42)-b(35,42)+0.7*b(41,42)
      jvs(431) = b(18,43)-b(19,43)
      jvs(432) = -b(24,44)+b(25,44)+b(26,44)+b(28,44)
      jvs(433) = -b(69,45)+b(71,45)+b(72,45)
      jvs(434) = b(17,46)+b(18,46)+b(33,46)+2*b(37,46)+b(57,46)   &
                +b(58,46)+b(71,46)+0.95*b(91,46)+0.84*b(102,46)   &
                +b(103,46)+1.5*b(104,46)+b(105,46)
      jvs(435) = -b(1,47)-b(15,47)-b(16,47)-b(19,47)-b(24,47)-b(34,47)   &
                -b(35,47)-b(39,47)-b(69,47)-b(94,47)
      jvs(436) = 0.89*b(2,48)+b(25,48)+2*b(37,48)-b(39,48)+2*b(40,48)   &
                +0.7*b(41,48)+b(59,48)+b(60,48)+b(72,48)+b(106,48)   &
                +b(107,48)+1.5*b(108,48)+b(109,48)
      jvs(437) = b(6,11)+b(43,11)
      jvs(438) = b(16,22)
      jvs(439) = -b(93,23)
      jvs(440) = b(27,26)
      jvs(441) = -b(87,30)
      jvs(442) = -b(52,31)
      jvs(443) = -b(109,32)
      jvs(444) = -b(88,33)
      jvs(445) = -b(80,34)
      jvs(446) = -b(60,35)
      jvs(447) = -b(108,36)
      jvs(448) = -b(68,37)
      jvs(449) = -b(59,38)
      jvs(450) = -b(107,39)
      jvs(451) = -b(106,40)
      jvs(452) = 0
      jvs(453) = -b(41,42)
      jvs(454) = b(19,43)
      jvs(455) = -b(25,44)+b(27,44)
      jvs(456) = -b(72,45)
      jvs(457) = -b(37,46)
      jvs(458) = b(16,47)+b(19,47)-b(38,47)-b(39,47)
      jvs(459) = -b(2,48)-b(25,48)-b(37,48)-b(38,48)-b(39,48)-2   &
                *b(40,48)-b(41,48)-b(52,48)-b(59,48)-b(60,48)-b(68,48)   &
                -b(72,48)-b(80,48)-b(87,48)-b(88,48)-b(93,48)   &
                -b(106,48)-b(107,48)-b(108,48)-b(109,48)
      return
      end subroutine cbmz_v02r02_jacob                                    


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r02_decomp( n, v, ier,   &
          lu_crow_v, lu_diag_v, lu_icol_v )
!
!   computes l-u-decomposition of sparse jacobian
!   for mechanism-version-regime = cbmz_v02r02
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     n = number of variable species [input]
      integer n
!     ier = status flag [output]
!         0 = success    other = failure [output]
      integer ier

!     v = non-zero elements of the sparse jacobian [input]
      real v(lu_nonzero_v_r02_kpp)

      integer lu_crow_v(nvar_r02_kpp + 1)
      integer lu_diag_v(nvar_r02_kpp + 1)
      integer lu_icol_v(lu_nonzero_v_r02_kpp)

!   local variables
      integer k, kk, j, jj
      real a, w(nvar_r02_kpp + 1)

      ier = 0
      do k=1,n
        if ( v( lu_diag_v(k) ) .eq. 0. ) then
            ier = k
            return
        end if
        do kk = lu_crow_v(k), lu_crow_v(k+1)-1
              w( lu_icol_v(kk) ) = v(kk)
        end do
        do kk = lu_crow_v(k), lu_diag_v(k)-1
            j = lu_icol_v(kk)
            a = -w(j) / v( lu_diag_v(j) )
            w(j) = -a
            do jj = lu_diag_v(j)+1, lu_crow_v(j+1)-1
               w( lu_icol_v(jj) ) = w( lu_icol_v(jj) ) + a*v(jj)
            end do
         end do
         do kk = lu_crow_v(k), lu_crow_v(k+1)-1
            v(kk) = w( lu_icol_v(kk) )
         end do
      end do
      return
      end subroutine cbmz_v02r02_decomp            


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r02_solve( jvs, x )
!
!   does back-solve for mechanism-version-regime = cbmz_v02r02
!
      implicit none

!   subr parameters
!     jvs = the non-zero elements of the l-u-decomposition
!           of the augmented jacobian [input]
      real jvs(*)
!     x = the right-hand side of the linear equation set being solved [on input]
!     x = concentrations of variable species [on output]
      real x(*)


      x(13) = x(13)-jvs(36)*x(12)
      x(16) = x(16)-jvs(49)*x(10)-jvs(50)*x(12)
      x(22) = x(22)-jvs(76)*x(4)
      x(23) = x(23)-jvs(82)*x(10)-jvs(83)*x(12)
      x(24) = x(24)-jvs(87)*x(20)
      x(25) = x(25)-jvs(98)*x(13)
      x(26) = x(26)-jvs(110)*x(11)-jvs(111)*x(23)
      x(27) = x(27)-jvs(120)*x(16)-jvs(121)*x(23)
      x(29) = x(29)-jvs(132)*x(28)
      x(31) = x(31)-jvs(147)*x(17)-jvs(148)*x(20)-jvs(149)*x(21)   &
             -jvs(150)*x(27)-jvs(151)*x(30)
      x(32) = x(32)-jvs(162)*x(10)-jvs(163)*x(12)-jvs(164)*x(20)   &
             -jvs(165)*x(23)-jvs(166)*x(27)-jvs(167)*x(28)-jvs(168)   &
             *x(30)
      x(34) = x(34)-jvs(184)*x(12)-jvs(185)*x(27)-jvs(186)*x(28)   &
             -jvs(187)*x(30)-jvs(188)*x(33)
      x(35) = x(35)-jvs(197)*x(8)-jvs(198)*x(18)-jvs(199)*x(28)   &
             -jvs(200)*x(30)-jvs(201)*x(33)
      x(36) = x(36)-jvs(211)*x(30)-jvs(212)*x(33)
      x(37) = x(37)-jvs(220)*x(6)-jvs(221)*x(18)-jvs(222)*x(20)   &
             -jvs(223)*x(27)-jvs(224)*x(28)-jvs(225)*x(30)-jvs(226)   &
             *x(33)-jvs(227)*x(35)-jvs(228)*x(36)
      x(38) = x(38)-jvs(238)*x(17)-jvs(239)*x(29)-jvs(240)*x(30)   &
             -jvs(241)*x(33)-jvs(242)*x(37)
      x(39) = x(39)-jvs(253)*x(29)-jvs(254)*x(33)
      x(40) = x(40)-jvs(263)*x(13)-jvs(264)*x(25)-jvs(265)*x(28)   &
             -jvs(266)*x(30)-jvs(267)*x(33)-jvs(268)*x(36)-jvs(269)   &
             *x(39)
      x(41) = x(41)-jvs(277)*x(14)-jvs(278)*x(16)-jvs(279)*x(23)   &
             -jvs(280)*x(36)-jvs(281)*x(40)
      x(42) = x(42)-jvs(289)*x(5)-jvs(290)*x(6)-jvs(291)*x(7)-jvs(292)   &
             *x(10)-jvs(293)*x(12)-jvs(294)*x(15)-jvs(295)*x(16)   &
             -jvs(296)*x(17)-jvs(297)*x(18)-jvs(298)*x(20)-jvs(299)   &
             *x(21)-jvs(300)*x(23)-jvs(301)*x(24)-jvs(302)*x(27)   &
             -jvs(303)*x(28)-jvs(304)*x(30)-jvs(305)*x(31)-jvs(306)   &
             *x(32)-jvs(307)*x(33)-jvs(308)*x(34)-jvs(309)*x(35)   &
             -jvs(310)*x(36)-jvs(311)*x(37)-jvs(312)*x(38)-jvs(313)   &
             *x(39)-jvs(314)*x(40)-jvs(315)*x(41)
      x(43) = x(43)-jvs(323)*x(20)-jvs(324)*x(22)-jvs(325)*x(27)   &
             -jvs(326)*x(30)-jvs(327)*x(33)-jvs(328)*x(42)
      x(44) = x(44)-jvs(335)*x(4)-jvs(336)*x(5)-jvs(337)*x(6)-jvs(338)   &
             *x(7)-jvs(339)*x(8)-jvs(340)*x(10)-jvs(341)*x(12)   &
             -jvs(342)*x(13)-jvs(343)*x(15)-jvs(344)*x(17)-jvs(345)   &
             *x(18)-jvs(346)*x(19)-jvs(347)*x(20)-jvs(348)*x(21)   &
             -jvs(349)*x(23)-jvs(350)*x(24)-jvs(351)*x(25)-jvs(352)   &
             *x(26)-jvs(353)*x(27)-jvs(354)*x(28)-jvs(355)*x(29)   &
             -jvs(356)*x(30)-jvs(357)*x(31)-jvs(358)*x(33)-jvs(359)   &
             *x(34)-jvs(360)*x(35)-jvs(361)*x(36)-jvs(362)*x(37)   &
             -jvs(363)*x(38)-jvs(364)*x(39)-jvs(365)*x(40)-jvs(366)   &
             *x(41)-jvs(367)*x(42)-jvs(368)*x(43)
      x(45) = x(45)-jvs(374)*x(9)-jvs(375)*x(27)-jvs(376)*x(29)   &
             -jvs(377)*x(30)-jvs(378)*x(33)-jvs(379)*x(34)-jvs(380)   &
             *x(37)-jvs(381)*x(39)-jvs(382)*x(40)-jvs(383)*x(41)   &
             -jvs(384)*x(42)-jvs(385)*x(43)-jvs(386)*x(44)
      x(46) = x(46)-jvs(391)*x(16)-jvs(392)*x(19)-jvs(393)*x(22)   &
             -jvs(394)*x(32)-jvs(395)*x(33)-jvs(396)*x(34)-jvs(397)   &
             *x(35)-jvs(398)*x(36)-jvs(399)*x(38)-jvs(400)*x(39)   &
             -jvs(401)*x(40)-jvs(402)*x(41)-jvs(403)*x(42)-jvs(404)   &
             *x(43)-jvs(405)*x(44)-jvs(406)*x(45)
      x(47) = x(47)-jvs(410)*x(9)-jvs(411)*x(11)-jvs(412)*x(14)   &
             -jvs(413)*x(15)-jvs(414)*x(16)-jvs(415)*x(19)-jvs(416)   &
             *x(22)-jvs(417)*x(23)-jvs(418)*x(26)-jvs(419)*x(31)   &
             -jvs(420)*x(32)-jvs(421)*x(33)-jvs(422)*x(34)-jvs(423)   &
             *x(35)-jvs(424)*x(36)-jvs(425)*x(37)-jvs(426)*x(38)   &
             -jvs(427)*x(39)-jvs(428)*x(40)-jvs(429)*x(41)-jvs(430)   &
             *x(42)-jvs(431)*x(43)-jvs(432)*x(44)-jvs(433)*x(45)   &
             -jvs(434)*x(46)
      x(48) = x(48)-jvs(437)*x(11)-jvs(438)*x(22)-jvs(439)*x(23)   &
             -jvs(440)*x(26)-jvs(441)*x(30)-jvs(442)*x(31)-jvs(443)   &
             *x(32)-jvs(444)*x(33)-jvs(445)*x(34)-jvs(446)*x(35)   &
             -jvs(447)*x(36)-jvs(448)*x(37)-jvs(449)*x(38)-jvs(450)   &
             *x(39)-jvs(451)*x(40)-jvs(452)*x(41)-jvs(453)*x(42)   &
             -jvs(454)*x(43)-jvs(455)*x(44)-jvs(456)*x(45)-jvs(457)   &
             *x(46)-jvs(458)*x(47)
      x(48) = x(48)/jvs(459)
      x(47) = (x(47)-jvs(436)*x(48))/(jvs(435))
      x(46) = (x(46)-jvs(408)*x(47)-jvs(409)*x(48))/(jvs(407))
      x(45) = (x(45)-jvs(388)*x(46)-jvs(389)*x(47)-jvs(390)*x(48))/   &
             (jvs(387))
      x(44) = (x(44)-jvs(370)*x(45)-jvs(371)*x(46)-jvs(372)*x(47)   &
             -jvs(373)*x(48))/(jvs(369))
      x(43) = (x(43)-jvs(330)*x(44)-jvs(331)*x(45)-jvs(332)*x(46)   &
             -jvs(333)*x(47)-jvs(334)*x(48))/(jvs(329))
      x(42) = (x(42)-jvs(317)*x(43)-jvs(318)*x(44)-jvs(319)*x(45)   &
             -jvs(320)*x(46)-jvs(321)*x(47)-jvs(322)*x(48))/(jvs(316))
      x(41) = (x(41)-jvs(283)*x(42)-jvs(284)*x(43)-jvs(285)*x(44)   &
             -jvs(286)*x(46)-jvs(287)*x(47)-jvs(288)*x(48))/(jvs(282))
      x(40) = (x(40)-jvs(271)*x(41)-jvs(272)*x(42)-jvs(273)*x(43)   &
             -jvs(274)*x(44)-jvs(275)*x(46)-jvs(276)*x(48))/(jvs(270))
      x(39) = (x(39)-jvs(256)*x(40)-jvs(257)*x(41)-jvs(258)*x(42)   &
             -jvs(259)*x(43)-jvs(260)*x(44)-jvs(261)*x(46)-jvs(262)   &
             *x(48))/(jvs(255))
      x(38) = (x(38)-jvs(244)*x(39)-jvs(245)*x(40)-jvs(246)*x(41)   &
             -jvs(247)*x(42)-jvs(248)*x(43)-jvs(249)*x(44)-jvs(250)   &
             *x(45)-jvs(251)*x(46)-jvs(252)*x(48))/(jvs(243))
      x(37) = (x(37)-jvs(230)*x(39)-jvs(231)*x(40)-jvs(232)*x(41)   &
             -jvs(233)*x(42)-jvs(234)*x(43)-jvs(235)*x(44)-jvs(236)   &
             *x(46)-jvs(237)*x(48))/(jvs(229))
      x(36) = (x(36)-jvs(214)*x(41)-jvs(215)*x(42)-jvs(216)*x(43)   &
             -jvs(217)*x(44)-jvs(218)*x(46)-jvs(219)*x(48))/(jvs(213))
      x(35) = (x(35)-jvs(203)*x(39)-jvs(204)*x(40)-jvs(205)*x(41)   &
             -jvs(206)*x(42)-jvs(207)*x(43)-jvs(208)*x(44)-jvs(209)   &
             *x(46)-jvs(210)*x(48))/(jvs(202))
      x(34) = (x(34)-jvs(190)*x(39)-jvs(191)*x(40)-jvs(192)*x(42)   &
             -jvs(193)*x(43)-jvs(194)*x(44)-jvs(195)*x(46)-jvs(196)   &
             *x(48))/(jvs(189))
      x(33) = (x(33)-jvs(181)*x(43)-jvs(182)*x(44)-jvs(183)*x(48))/   &
             (jvs(180))
      x(32) = (x(32)-jvs(170)*x(33)-jvs(171)*x(34)-jvs(172)*x(39)   &
             -jvs(173)*x(40)-jvs(174)*x(41)-jvs(175)*x(42)-jvs(176)   &
             *x(43)-jvs(177)*x(44)-jvs(178)*x(46)-jvs(179)*x(48))/   &
             (jvs(169))
      x(31) = (x(31)-jvs(153)*x(33)-jvs(154)*x(36)-jvs(155)*x(38)   &
             -jvs(156)*x(39)-jvs(157)*x(42)-jvs(158)*x(43)-jvs(159)   &
             *x(44)-jvs(160)*x(46)-jvs(161)*x(48))/(jvs(152))
      x(30) = (x(30)-jvs(144)*x(43)-jvs(145)*x(44)-jvs(146)*x(48))/   &
             (jvs(143))
      x(29) = (x(29)-jvs(134)*x(33)-jvs(135)*x(39)-jvs(136)*x(40)   &
             -jvs(137)*x(41)-jvs(138)*x(42)-jvs(139)*x(43)-jvs(140)   &
             *x(44)-jvs(141)*x(46)-jvs(142)*x(48))/(jvs(133))
      x(28) = (x(28)-jvs(128)*x(39)-jvs(129)*x(40)-jvs(130)*x(42)   &
             -jvs(131)*x(44))/(jvs(127))
      x(27) = (x(27)-jvs(123)*x(43)-jvs(124)*x(44)-jvs(125)*x(46)   &
             -jvs(126)*x(48))/(jvs(122))
      x(26) = (x(26)-jvs(113)*x(31)-jvs(114)*x(34)-jvs(115)*x(37)   &
             -jvs(116)*x(42)-jvs(117)*x(44)-jvs(118)*x(47)-jvs(119)   &
             *x(48))/(jvs(112))
      x(25) = (x(25)-jvs(100)*x(28)-jvs(101)*x(30)-jvs(102)*x(33)   &
             -jvs(103)*x(36)-jvs(104)*x(40)-jvs(105)*x(41)-jvs(106)   &
             *x(43)-jvs(107)*x(44)-jvs(108)*x(46)-jvs(109)*x(48))/   &
             (jvs(99))
      x(24) = (x(24)-jvs(89)*x(27)-jvs(90)*x(30)-jvs(91)*x(31)-jvs(92)   &
             *x(33)-jvs(93)*x(34)-jvs(94)*x(37)-jvs(95)*x(43)-jvs(96)   &
             *x(44)-jvs(97)*x(48))/(jvs(88))
      x(23) = (x(23)-jvs(85)*x(44)-jvs(86)*x(48))/(jvs(84))
      x(22) = (x(22)-jvs(78)*x(43)-jvs(79)*x(46)-jvs(80)*x(47)-jvs(81)   &
             *x(48))/(jvs(77))
      x(21) = (x(21)-jvs(71)*x(30)-jvs(72)*x(33)-jvs(73)*x(38)-jvs(74)   &
             *x(43)-jvs(75)*x(44))/(jvs(70))
      x(20) = (x(20)-jvs(68)*x(43)-jvs(69)*x(44))/(jvs(67))
      x(19) = (x(19)-jvs(63)*x(42)-jvs(64)*x(44)-jvs(65)*x(46)-jvs(66)   &
             *x(47))/(jvs(62))
      x(18) = (x(18)-jvs(59)*x(35)-jvs(60)*x(42)-jvs(61)*x(44))/   &
             (jvs(58))
      x(17) = (x(17)-jvs(55)*x(38)-jvs(56)*x(42)-jvs(57)*x(44))/   &
             (jvs(54))
      x(16) = (x(16)-jvs(52)*x(44)-jvs(53)*x(46))/(jvs(51))
      x(15) = (x(15)-jvs(46)*x(42)-jvs(47)*x(44)-jvs(48)*x(47))/   &
             (jvs(45))
      x(14) = (x(14)-jvs(41)*x(23)-jvs(42)*x(44)-jvs(43)*x(47)-jvs(44)   &
             *x(48))/(jvs(40))
      x(13) = (x(13)-jvs(38)*x(25)-jvs(39)*x(44))/(jvs(37))
      x(12) = (x(12)-jvs(35)*x(44))/(jvs(34))
      x(11) = (x(11)-jvs(32)*x(47)-jvs(33)*x(48))/(jvs(31))
      x(10) = (x(10)-jvs(30)*x(44))/(jvs(29))
      x(9) = (x(9)-jvs(27)*x(45)-jvs(28)*x(47))/(jvs(26))
      x(8) = (x(8)-jvs(24)*x(35)-jvs(25)*x(44))/(jvs(23))
      x(7) = (x(7)-jvs(21)*x(42)-jvs(22)*x(44))/(jvs(20))
      x(6) = (x(6)-jvs(19)*x(44))/(jvs(18))
      x(5) = (x(5)-jvs(17)*x(44))/(jvs(16))
      x(4) = (x(4)-jvs(15)*x(43))/(jvs(14))
      x(3) = (x(3)-jvs(9)*x(30)-jvs(10)*x(33)-jvs(11)*x(42)-jvs(12)   &
            *x(43)-jvs(13)*x(45))/(jvs(8))
      x(2) = (x(2)-jvs(5)*x(20)-jvs(6)*x(30)-jvs(7)*x(43))/(jvs(4))
      x(1) = (x(1)-jvs(2)*x(5)-jvs(3)*x(44))/(jvs(1))
      return
      end subroutine cbmz_v02r02_solve          


!   cbmz_v02r03_torodas.f - created on 17-nov-2003 from previous
!        cbmz_v02r03_torodas.f        cbmz_v02r03_mapconcs.f
!        cbmz_v02r03_maprates.f       cbmz_v02r03_dydt.f
!        cbmz_v02r03_jacob.f          cbmz_v02r03_decomp.f
!        cbmz_v02r03_solve.f
!   so now everything is in a single file
!-----------------------------------------------------------------------

      subroutine cbmz_v02r03_torodas(   &
          ngas, taa, tzz,   &
          stot, atol, rtol, yposlimit, yneglimit,   &
          sfixedkpp, rconstkpp,   &
          hmin, hstart,   &
          info_rodas, iok, lunerr, idydt_sngldble )
!
!   interfaces to rodas3 solver formechanism-version-regime =cbmz_v02r03
!
!   *** do not include any pegasus common blocks ***
!
      use module_data_cbmz
      use module_cbmz_rodas3_solver, only:  rodas3_ff_x2
      implicit none

!   subr parameters
      integer ngas, iok, lunerr, idydt_sngldble
      integer info_rodas(6)
      real taa, tzz, hmin, hstart
      real stot(ngas), atol(ngas), rtol(ngas)
      real yposlimit(ngas), yneglimit(ngas)
      real sfixedkpp(nfixed_kppmax), rconstkpp(nreact_kppmax)

!   local variables

!     external cbmz_v02r03_dydt
!     external cbmz_v02r03_jacob
!     external cbmz_v02r03_decomp
!     external cbmz_v02r03_solve

      integer i

      real hmax
      integer lu_crow_v(nvar_r03_kpp + 1)
      save    lu_crow_v
      integer lu_diag_v(nvar_r03_kpp + 1)
      save    lu_diag_v
      integer lu_icol_v(lu_nonzero_v_r03_kpp)
      save    lu_icol_v

      data( lu_icol_v(i), i = 1, 252 ) /   &
        1,  5, 48,  2, 20, 31, 32, 42, 49,  3, 31, 36,   &
       47, 49, 50,  4, 49,  5, 48,  6, 48,  7, 48, 50,   &
        8, 39, 48,  9, 47, 52, 10, 48, 11, 52, 53, 12,   &
       48, 13, 23, 48, 52, 53, 14, 48, 50, 52, 10, 12,   &
       15, 48, 51, 16, 26, 31, 36, 37, 44, 45, 46, 48,   &
       49, 51, 53, 17, 41, 48, 50, 18, 39, 48, 50, 19,   &
       48, 50, 51, 52, 20, 48, 49, 21, 31, 36, 41, 48,   &
       49,  4, 22, 49, 51, 52, 53, 10, 12, 23, 48, 53,   &
       11, 23, 24, 33, 38, 40, 42, 48, 50, 52, 53, 20,   &
       25, 27, 30, 31, 32, 33, 36, 38, 40, 42, 48, 49,   &
       51, 53, 12, 16, 26, 28, 29, 31, 36, 37, 42, 44,   &
       45, 46, 48, 49, 50, 51, 53, 15, 23, 27, 48, 49,   &
       51, 53, 28, 32, 50, 51, 53, 29, 32, 48, 50, 51,   &
       30, 42, 48, 50, 51, 31, 48, 49, 53, 32, 48, 49,   &
       53, 17, 20, 21, 27, 29, 30, 31, 32, 33, 36, 37,   &
       41, 42, 43, 48, 49, 50, 51, 53, 10, 12, 20, 23,   &
       27, 31, 32, 34, 36, 38, 42, 44, 45, 46, 48, 49,   &
       50, 51, 53, 30, 35, 36, 42, 43, 44, 45, 46, 48,   &
       49, 50, 51, 53, 36, 48, 49, 53, 31, 36, 37, 46,   &
       48, 49, 50, 51, 53, 12, 27, 30, 31, 36, 38, 42,   &
       43, 44, 48, 49, 50, 51, 53,  8, 18, 31, 36, 39 /

      data( lu_icol_v(i), i = 253, 504 ) /   &
       44, 45, 46, 48, 49, 50, 51, 53,  6, 18, 20, 27,   &
       28, 30, 31, 32, 36, 37, 39, 40, 42, 44, 45, 46,   &
       48, 49, 50, 51, 53, 17, 31, 35, 36, 40, 41, 42,   &
       43, 44, 45, 46, 47, 48, 49, 50, 51, 53, 28, 29,   &
       32, 42, 48, 49, 50, 51, 53, 35, 36, 42, 43, 44,   &
       45, 46, 48, 49, 50, 51, 53, 29, 30, 32, 42, 43,   &
       44, 45, 46, 48, 49, 50, 51, 53, 26, 28, 29, 31,   &
       32, 36, 37, 42, 44, 45, 46, 48, 49, 50, 51, 53,   &
       13, 15, 23, 28, 29, 32, 37, 42, 45, 46, 48, 49,   &
       50, 51, 52, 53,  9, 27, 31, 32, 35, 36, 38, 40,   &
       42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53,   &
        4,  5,  6,  7,  8, 10, 12, 14, 17, 18, 19, 20,   &
       21, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,   &
       35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46,   &
       47, 48, 49, 50, 51, 52, 53, 20, 22, 27, 31, 32,   &
       36, 42, 47, 48, 49, 50, 51, 52, 53,  5,  6,  7,   &
       10, 12, 14, 15, 17, 18, 20, 21, 23, 25, 27, 28,   &
       29, 30, 31, 32, 33, 34, 36, 37, 38, 39, 40, 41,   &
       42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53,   &
       15, 19, 22, 28, 29, 30, 32, 34, 36, 37, 38, 39,   &
       41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52 /

      data( lu_icol_v(i), i = 505, 564 ) /   &
       53,  9, 11, 13, 14, 15, 19, 22, 23, 24, 28, 29,   &
       30, 32, 33, 34, 36, 37, 38, 39, 40, 41, 42, 43,   &
       44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 11, 22,   &
       23, 24, 31, 32, 33, 34, 36, 37, 38, 39, 40, 41,   &
       42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53 /

      data lu_crow_v /   &
        1,  4, 10, 16, 18, 20, 22, 25, 28, 31, 33, 36,   &
       38, 43, 47, 52, 64, 68, 72, 77, 80, 86, 92, 97,   &
      108,123,140,147,152,157,162,166,170,189,208,221,   &
      225,234,248,261,282,299,308,320,333,349,365,385,   &
      428,442,481,506,539,565 /

      data lu_diag_v /   &
        1,  4, 10, 16, 18, 20, 22, 25, 28, 31, 33, 36,   &
       38, 43, 49, 52, 64, 68, 72, 77, 80, 87, 94, 99,   &
      109,125,142,147,152,157,162,166,178,196,209,221,   &
      227,239,252,272,287,302,311,325,342,358,378,422,   &
      437,477,503,537,564,565 /



      info_rodas(1) = 1
      do i = 2, 6
          info_rodas(i) = 0
      end do
      hmax = tzz - taa

!   do not integrate if hmax is less/equal to hmin
!   because hmin is generally 0.1 s or less
      if (hmax .le. 1.001*hmin) then
          iok = 11
          return
      end if

      call rodas3_ff_x2(   &
           nvar_r03_kpp, taa, tzz, hmin, hmax, hstart,   &
           stot, atol, rtol, yposlimit, yneglimit,   &
           sfixedkpp, rconstkpp,   &
           lu_nonzero_v_r03_kpp, lu_crow_v, lu_diag_v, lu_icol_v,   &
           info_rodas, iok, lunerr,   &
           cbmz_v02r03_dydt,   &
           cbmz_v02r03_jacob,   &
           cbmz_v02r03_decomp,   &
           cbmz_v02r03_solve )

      return
      end subroutine cbmz_v02r03_torodas 


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r03_mapconcs( imap, nyy, yy, yyfixed, cbox )
!
!   maps species concentrations (gaschemistry cbox array <--> kpp yy array)
!   for mechanism-version-regime = cbmz_v02r03
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     imap = mapping direction flag [input]
!         0 = map cbox --> yy and yyfixed
!         1 = map yy --> cbox
      integer imap
!     nyy = number of kpp "variable" species [output]
      integer nyy
!     yy = kpp species concentrations array [input/output]
      real yy(nvar_r03_kpp)
!     yyfixed = kpp species concentrations array [input/output]
      real yyfixed(nfixed_kppmax)
!     cbox = main gaschemistry species conc array [input/output]
      real cbox(ngas_z)

!   local variables
      integer ih2so4_kpp
      parameter ( ih2so4_kpp = 1 )
      integer ihcooh_kpp
      parameter ( ihcooh_kpp = 2 )
      integer ircooh_kpp
      parameter ( ircooh_kpp = 3 )
      integer io1d_kpp
      parameter ( io1d_kpp = 4 )
      integer iso2_kpp
      parameter ( iso2_kpp = 5 )
      integer ic2h5oh_kpp
      parameter ( ic2h5oh_kpp = 6 )
      integer ih2o2_kpp
      parameter ( ih2o2_kpp = 7 )
      integer ic2h6_kpp
      parameter ( ic2h6_kpp = 8 )
      integer ipan_kpp
      parameter ( ipan_kpp = 9 )
      integer itol_kpp
      parameter ( itol_kpp = 10 )
      integer in2o5_kpp
      parameter ( in2o5_kpp = 11 )
      integer ixyl_kpp
      parameter ( ixyl_kpp = 12 )
      integer icro_kpp
      parameter ( icro_kpp = 13 )
      integer ihno4_kpp
      parameter ( ihno4_kpp = 14 )
      integer ito2_kpp
      parameter ( ito2_kpp = 15 )
      integer ixpar_kpp
      parameter ( ixpar_kpp = 16 )
      integer ich3ooh_kpp
      parameter ( ich3ooh_kpp = 17 )
      integer iethooh_kpp
      parameter ( iethooh_kpp = 18 )
      integer ihono_kpp
      parameter ( ihono_kpp = 19 )
      integer ieth_kpp
      parameter ( ieth_kpp = 20 )
      integer ich3oh_kpp
      parameter ( ich3oh_kpp = 21 )
      integer io3p_kpp
      parameter ( io3p_kpp = 22 )
      integer icres_kpp
      parameter ( icres_kpp = 23 )
      integer ihno3_kpp
      parameter ( ihno3_kpp = 24 )
      integer ico_kpp
      parameter ( ico_kpp = 25 )
      integer ipar_kpp
      parameter ( ipar_kpp = 26 )
      integer iopen_kpp
      parameter ( iopen_kpp = 27 )
      integer iisopn_kpp
      parameter ( iisopn_kpp = 28 )
      integer iisopp_kpp
      parameter ( iisopp_kpp = 29 )
      integer iisopo2_kpp
      parameter ( iisopo2_kpp = 30 )
      integer iolet_kpp
      parameter ( iolet_kpp = 31 )
      integer iisop_kpp
      parameter ( iisop_kpp = 32 )
      integer ihcho_kpp
      parameter ( ihcho_kpp = 33 )
      integer ixo2_kpp
      parameter ( ixo2_kpp = 34 )
      integer iaone_kpp
      parameter ( iaone_kpp = 35 )
      integer iolei_kpp
      parameter ( iolei_kpp = 36 )
      integer inap_kpp
      parameter ( inap_kpp = 37 )
      integer imgly_kpp
      parameter ( imgly_kpp = 38 )
      integer iethp_kpp
      parameter ( iethp_kpp = 39 )
      integer iald2_kpp
      parameter ( iald2_kpp = 40 )
      integer ich3o2_kpp
      parameter ( ich3o2_kpp = 41 )
      integer iisoprd_kpp
      parameter ( iisoprd_kpp = 42 )
      integer iano2_kpp
      parameter ( iano2_kpp = 43 )
      integer irooh_kpp
      parameter ( irooh_kpp = 44 )
      integer iro2_kpp
      parameter ( iro2_kpp = 45 )
      integer ionit_kpp
      parameter ( ionit_kpp = 46 )
      integer ic2o3_kpp
      parameter ( ic2o3_kpp = 47 )
      integer ioh_kpp
      parameter ( ioh_kpp = 48 )
      integer io3_kpp
      parameter ( io3_kpp = 49 )
      integer iho2_kpp
      parameter ( iho2_kpp = 50 )
      integer ino_kpp
      parameter ( ino_kpp = 51 )
      integer ino2_kpp
      parameter ( ino2_kpp = 52 )
      integer ino3_kpp
      parameter ( ino3_kpp = 53 )

! indexes declaration for fixed species

      integer ich4_kpp
      parameter ( ich4_kpp = 1 )
      integer ih2o_kpp
      parameter ( ih2o_kpp = 2 )
      integer ih2_kpp
      parameter ( ih2_kpp = 3 )
      integer io2_kpp
      parameter ( io2_kpp = 4 )
      integer in2_kpp
      parameter ( in2_kpp = 5 )


      nyy = nvar_r03_kpp

      if (imap .le. 0) goto 1000
      if (imap .ge. 1) goto 2000


!
!   map cbox values into yyvarkpp and yyfixkpp
!
1000  continue
      yy(ih2so4_kpp)	= cbox(ih2so4_z)
      yy(ihcooh_kpp)	= cbox(ihcooh_z)
      yy(ircooh_kpp)	= cbox(ircooh_z)
      yy(io1d_kpp)	= cbox(io1d_z)
      yy(iso2_kpp)	= cbox(iso2_z)
      yy(ic2h5oh_kpp)	= cbox(ic2h5oh_z)
      yy(ih2o2_kpp)	= cbox(ih2o2_z)
      yy(ic2h6_kpp)	= cbox(ic2h6_z)
      yy(ipan_kpp)	= cbox(ipan_z)
      yy(itol_kpp)	= cbox(itol_z)
      yy(in2o5_kpp)	= cbox(in2o5_z)
      yy(ixyl_kpp)	= cbox(ixyl_z)
      yy(icro_kpp)	= cbox(icro_z)
      yy(ihno4_kpp)	= cbox(ihno4_z)
      yy(ito2_kpp)	= cbox(ito2_z)
      yy(ixpar_kpp)	= cbox(ixpar_z)
      yy(ich3ooh_kpp)	= cbox(ich3ooh_z)
      yy(iethooh_kpp)	= cbox(iethooh_z)
      yy(ihono_kpp)	= cbox(ihono_z)
      yy(ieth_kpp)	= cbox(ieth_z)
      yy(ich3oh_kpp)	= cbox(ich3oh_z)
      yy(io3p_kpp)	= cbox(io3p_z)
      yy(icres_kpp)	= cbox(icres_z)
      yy(ihno3_kpp)	= cbox(ihno3_z)
      yy(ico_kpp)	= cbox(ico_z)
      yy(ipar_kpp)	= cbox(ipar_z)
      yy(iopen_kpp)	= cbox(iopen_z)
      yy(iisopn_kpp)	= cbox(iisopn_z)
      yy(iisopp_kpp)	= cbox(iisopp_z)
      yy(iisopo2_kpp)	= cbox(iisopo2_z)
      yy(iolet_kpp)	= cbox(iolet_z)
      yy(iisop_kpp)	= cbox(iisop_z)
      yy(ihcho_kpp)	= cbox(ihcho_z)
      yy(ixo2_kpp)	= cbox(ixo2_z)
      yy(iaone_kpp)	= cbox(iaone_z)
      yy(iolei_kpp)	= cbox(iolei_z)
      yy(inap_kpp)	= cbox(inap_z)
      yy(imgly_kpp)	= cbox(imgly_z)
      yy(iethp_kpp)	= cbox(iethp_z)
      yy(iald2_kpp)	= cbox(iald2_z)
      yy(ich3o2_kpp)	= cbox(ich3o2_z)
      yy(iisoprd_kpp)	= cbox(iisoprd_z)
      yy(iano2_kpp)	= cbox(iano2_z)
      yy(irooh_kpp)	= cbox(irooh_z)
      yy(iro2_kpp)	= cbox(iro2_z)
      yy(ionit_kpp)	= cbox(ionit_z)
      yy(ic2o3_kpp)	= cbox(ic2o3_z)
      yy(ioh_kpp)	= cbox(ioh_z)
      yy(io3_kpp)	= cbox(io3_z)
      yy(iho2_kpp)	= cbox(iho2_z)
      yy(ino_kpp)	= cbox(ino_z)
      yy(ino2_kpp)	= cbox(ino2_z)
      yy(ino3_kpp)	= cbox(ino3_z)

      yyfixed(ich4_kpp)	= cbox(ich4_z)
      yyfixed(ih2o_kpp)	= cbox(ih2o_z)
      yyfixed(ih2_kpp)	= cbox(ih2_z)
      yyfixed(io2_kpp)	= cbox(io2_z)
      yyfixed(in2_kpp)	= cbox(in2_z)

!
!   map yyvarkpp values into cbox
!
2000  continue
      cbox(ih2so4_z)	= yy(ih2so4_kpp)
      cbox(ihcooh_z)	= yy(ihcooh_kpp)
      cbox(ircooh_z)	= yy(ircooh_kpp)
      cbox(io1d_z)	= yy(io1d_kpp)
      cbox(iso2_z)	= yy(iso2_kpp)
      cbox(ic2h5oh_z)	= yy(ic2h5oh_kpp)
      cbox(ih2o2_z)	= yy(ih2o2_kpp)
      cbox(ic2h6_z)	= yy(ic2h6_kpp)
      cbox(ipan_z)	= yy(ipan_kpp)
      cbox(itol_z)	= yy(itol_kpp)
      cbox(in2o5_z)	= yy(in2o5_kpp)
      cbox(ixyl_z)	= yy(ixyl_kpp)
      cbox(icro_z)	= yy(icro_kpp)
      cbox(ihno4_z)	= yy(ihno4_kpp)
      cbox(ito2_z)	= yy(ito2_kpp)
      cbox(ixpar_z)	= yy(ixpar_kpp)
      cbox(ich3ooh_z)	= yy(ich3ooh_kpp)
      cbox(iethooh_z)	= yy(iethooh_kpp)
      cbox(ihono_z)	= yy(ihono_kpp)
      cbox(ieth_z)	= yy(ieth_kpp)
      cbox(ich3oh_z)	= yy(ich3oh_kpp)
      cbox(io3p_z)	= yy(io3p_kpp)
      cbox(icres_z)	= yy(icres_kpp)
      cbox(ihno3_z)	= yy(ihno3_kpp)
      cbox(ico_z)	= yy(ico_kpp)
      cbox(ipar_z)	= yy(ipar_kpp)
      cbox(iopen_z)	= yy(iopen_kpp)
      cbox(iisopn_z)	= yy(iisopn_kpp)
      cbox(iisopp_z)	= yy(iisopp_kpp)
      cbox(iisopo2_z)	= yy(iisopo2_kpp)
      cbox(iolet_z)	= yy(iolet_kpp)
      cbox(iisop_z)	= yy(iisop_kpp)
      cbox(ihcho_z)	= yy(ihcho_kpp)
      cbox(ixo2_z)	= yy(ixo2_kpp)
      cbox(iaone_z)	= yy(iaone_kpp)
      cbox(iolei_z)	= yy(iolei_kpp)
      cbox(inap_z)	= yy(inap_kpp)
      cbox(imgly_z)	= yy(imgly_kpp)
      cbox(iethp_z)	= yy(iethp_kpp)
      cbox(iald2_z)	= yy(iald2_kpp)
      cbox(ich3o2_z)	= yy(ich3o2_kpp)
      cbox(iisoprd_z)	= yy(iisoprd_kpp)
      cbox(iano2_z)	= yy(iano2_kpp)
      cbox(irooh_z)	= yy(irooh_kpp)
      cbox(iro2_z)	= yy(iro2_kpp)
      cbox(ionit_z)	= yy(ionit_kpp)
      cbox(ic2o3_z)	= yy(ic2o3_kpp)
      cbox(ioh_z)	= yy(ioh_kpp)
      cbox(io3_z)	= yy(io3_kpp)
      cbox(iho2_z)	= yy(iho2_kpp)
      cbox(ino_z)	= yy(ino_kpp)
      cbox(ino2_z)	= yy(ino2_kpp)
      cbox(ino3_z)	= yy(ino3_kpp)

      return
      end subroutine cbmz_v02r03_mapconcs                                


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r03_maprates(   &
          rk_m1,   &
          rk_m2,   &
          rk_m3,   &
          rk_m4,   &
          rconst )
!
!   maps reaction rate constants (host code array --> kpp rconst array)
!   for mechanism-version-regime = cbmz_v02r03
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     all are host-code reaction-rate-constant arrays [input]
      real rk_m1(*)
      real rk_m2(*)
      real rk_m3(*)
      real rk_m4(*)

      real rconst(nreact_kppmax)

!   local variables
      integer i

      do i = 1, nreact_kppmax
          rconst(i) = 0.
      end do


      rconst(1) = (rk_m1(1))
      rconst(2) = (rk_m1(2))
      rconst(3) = (rk_m1(3))
      rconst(4) = (rk_m1(4))
      rconst(5) = (rk_m1(5))
      rconst(6) = (rk_m1(6))
      rconst(7) = (rk_m1(7))
      rconst(8) = (rk_m1(8))
      rconst(9) = (rk_m1(9))
      rconst(10) = (rk_m1(10))
      rconst(11) = (rk_m1(11))
      rconst(12) = (rk_m1(12))
      rconst(13) = (rk_m1(13))
      rconst(14) = (rk_m1(14))
      rconst(15) = (rk_m1(15))
      rconst(16) = (rk_m1(16))
      rconst(17) = (rk_m1(17))
      rconst(18) = (rk_m1(18))
      rconst(19) = (rk_m1(19))
      rconst(20) = (rk_m1(20))
      rconst(21) = (rk_m1(21))
      rconst(22) = (rk_m1(22))
      rconst(23) = (rk_m1(23))
      rconst(24) = (rk_m1(24))
      rconst(25) = (rk_m1(25))
      rconst(26) = (rk_m1(26))
      rconst(27) = (rk_m1(27))
      rconst(28) = (rk_m1(28))
      rconst(29) = (rk_m1(29))
      rconst(30) = (rk_m1(30))
      rconst(31) = (rk_m1(31))
      rconst(32) = (rk_m1(32))
      rconst(33) = (rk_m1(33))
      rconst(34) = (rk_m1(34))
      rconst(35) = (rk_m1(35))
      rconst(36) = (rk_m1(36))
      rconst(37) = (rk_m1(37))
      rconst(38) = (rk_m1(38))
      rconst(39) = (rk_m1(39))
      rconst(40) = (rk_m1(40))
      rconst(41) = (rk_m1(41))
      rconst(42) = (rk_m1(42))
      rconst(43) = (rk_m1(43))
      rconst(44) = (rk_m1(44))
      rconst(45) = (rk_m1(45))
      rconst(46) = (rk_m1(46))
      rconst(47) = (rk_m1(47))
      rconst(48) = (rk_m1(48))
      rconst(49) = (rk_m1(49))
      rconst(50) = (rk_m1(50))
      rconst(51) = (rk_m1(51))
      rconst(52) = (rk_m1(52))
      rconst(53) = (rk_m1(53))
      rconst(54) = (rk_m1(54))
      rconst(55) = (rk_m1(55))
      rconst(56) = (rk_m1(56))
      rconst(57) = (rk_m1(57))
      rconst(58) = (rk_m1(58))
      rconst(59) = (rk_m1(59))
      rconst(60) = (rk_m1(60))
      rconst(61) = (rk_m1(61))
      rconst(62) = (rk_m1(62))
      rconst(63) = (rk_m1(63))
      rconst(64) = (rk_m1(64))
      rconst(65) = (rk_m1(65))
      rconst(66) = (rk_m2(2))
      rconst(67) = (rk_m2(3))
      rconst(68) = (rk_m2(4))
      rconst(69) = (rk_m2(31))
      rconst(70) = (rk_m2(32))
      rconst(71) = (rk_m2(34))
      rconst(72) = (rk_m2(39))
      rconst(73) = (rk_m2(44))
      rconst(74) = (rk_m2(49))
      rconst(75) = (rk_m2(1))
      rconst(76) = (rk_m2(5))
      rconst(77) = (rk_m2(6))
      rconst(78) = (rk_m2(7))
      rconst(79) = (rk_m2(8))
      rconst(80) = (rk_m2(9))
      rconst(81) = (rk_m2(10))
      rconst(82) = (rk_m2(11))
      rconst(83) = (rk_m2(12))
      rconst(84) = (rk_m2(13))
      rconst(85) = (rk_m2(14))
      rconst(86) = (rk_m2(15))
      rconst(87) = (rk_m2(16))
      rconst(88) = (rk_m2(17))
      rconst(89) = (rk_m2(18))
      rconst(90) = (rk_m2(19))
      rconst(91) = (rk_m2(20))
      rconst(92) = (rk_m2(21))
      rconst(93) = (rk_m2(22))
      rconst(94) = (rk_m2(23))
      rconst(95) = (rk_m2(24))
      rconst(96) = (rk_m2(25))
      rconst(97) = (rk_m2(26))
      rconst(98) = (rk_m2(27))
      rconst(99) = (rk_m2(28))
      rconst(100) = (rk_m2(29))
      rconst(101) = (rk_m2(30))
      rconst(102) = (rk_m2(33))
      rconst(103) = (rk_m2(35))
      rconst(104) = (rk_m2(36))
      rconst(105) = (rk_m2(37))
      rconst(106) = (rk_m2(38))
      rconst(107) = (rk_m2(40))
      rconst(108) = (rk_m2(41))
      rconst(109) = (rk_m2(42))
      rconst(110) = (rk_m2(43))
      rconst(111) = (rk_m2(45))
      rconst(112) = (rk_m2(46))
      rconst(113) = (rk_m2(47))
      rconst(114) = (rk_m2(48))
      rconst(115) = (rk_m2(50))
      rconst(116) = (rk_m2(51))
      rconst(117) = (rk_m2(52))
      rconst(118) = (rk_m2(53))
      rconst(119) = (rk_m3(1))
      rconst(120) = (rk_m3(2))
      rconst(121) = (rk_m3(3))
      rconst(122) = (rk_m3(4))
      rconst(123) = (rk_m3(5))
      rconst(124) = (rk_m3(6))
      rconst(125) = (rk_m3(7))
      rconst(126) = (rk_m3(8))
      rconst(127) = (rk_m3(9))
      rconst(128) = (rk_m3(10))
      rconst(129) = (rk_m3(11))
      rconst(130) = (rk_m3(12))
      rconst(131) = (rk_m3(13))
      rconst(132) = (rk_m3(14))
      rconst(133) = (rk_m3(15))
      rconst(134) = (rk_m3(16))
      return
      end subroutine cbmz_v02r03_maprates 


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r03_dydt( nvardum, tdum, v, a_var, f, rconst )
!
!   computes rates of change for mechanism-version-regime = cbmz_v02r03
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     nvardum = number of variable species [input]
      integer nvardum
!     tdum = time [input]
      real tdum
!     v = concentrations of variable species [input]
      real v(nvar_r03_kpp)
!     a_var = dydt for each species [output]
      real a_var(nvar_r03_kpp)
!     f = concentrations of fixed species [input]
      real f(nfixed_kppmax)
!     rconst = reaction rate constants [input]
      real rconst(nreact_r03_kpp)

!   local variables
!     a = rate for each reaction
      real a(nreact_r03_kpp)

! computation of equation rates
      a(1) = rconst(1)*v(52)
      a(2) = rconst(2)*v(53)
      a(3) = rconst(3)*v(19)
      a(4) = rconst(4)*v(24)
      a(5) = rconst(5)*v(14)
      a(6) = rconst(6)*v(11)
      a(7) = rconst(7)*v(49)
      a(8) = rconst(8)*v(49)
      a(9) = rconst(9)*v(7)
      a(10) = rconst(10)*v(4)*f(4)
      a(11) = rconst(11)*v(4)*f(5)
      a(12) = rconst(12)*v(4)*f(2)
      a(13) = rconst(13)*v(22)*f(4)
      a(14) = rconst(14)*v(22)*v(49)
      a(15) = rconst(15)*v(22)*v(52)
      a(16) = rconst(16)*v(22)*v(52)
      a(17) = rconst(17)*v(22)*v(51)
      a(18) = rconst(18)*v(49)*v(51)
      a(19) = rconst(19)*v(49)*v(52)
      a(20) = rconst(20)*v(48)*v(49)
      a(21) = rconst(21)*v(49)*v(50)
      a(22) = rconst(22)*v(48)*f(3)
      a(23) = rconst(23)*v(48)*v(51)
      a(24) = rconst(24)*v(48)*v(52)
      a(25) = rconst(25)*v(48)*v(53)
      a(26) = rconst(26)*v(19)*v(48)
      a(27) = rconst(27)*v(24)*v(48)
      a(28) = rconst(28)*v(14)*v(48)
      a(29) = rconst(29)*v(48)*v(50)
      a(30) = rconst(30)*v(7)*v(48)
      a(31) = rconst(31)*v(50)*v(50)
      a(32) = rconst(32)*v(50)*v(50)*f(2)
      a(33) = rconst(33)*v(50)*v(51)
      a(34) = rconst(34)*v(50)*v(52)
      a(35) = rconst(35)*v(50)*v(52)
      a(36) = rconst(36)*v(14)
      a(37) = rconst(37)*v(51)*v(53)
      a(38) = rconst(38)*v(52)*v(53)
      a(39) = rconst(39)*v(52)*v(53)
      a(40) = rconst(40)*v(53)*v(53)
      a(41) = rconst(41)*v(50)*v(53)
      a(42) = rconst(42)*v(11)*f(2)
      a(43) = rconst(43)*v(11)
      a(44) = rconst(44)*v(25)*v(48)
      a(45) = rconst(45)*v(5)*v(48)
      a(46) = rconst(46)*v(48)*f(1)
      a(47) = rconst(47)*v(8)*v(48)
      a(48) = rconst(48)*v(21)*v(48)
      a(49) = rconst(49)*v(33)
      a(50) = rconst(50)*v(33)
      a(51) = rconst(51)*v(33)*v(48)
      a(52) = rconst(52)*v(33)*v(53)
      a(53) = rconst(53)*v(17)
      a(54) = rconst(54)*v(18)
      a(55) = rconst(55)*v(17)*v(48)
      a(56) = rconst(56)*v(18)*v(48)
      a(57) = rconst(57)*v(41)*v(51)
      a(58) = rconst(58)*v(39)*v(51)
      a(59) = rconst(59)*v(41)*v(53)
      a(60) = rconst(60)*v(39)*v(53)
      a(61) = rconst(61)*v(41)*v(50)
      a(62) = rconst(62)*v(39)*v(50)
      a(63) = rconst(63)*v(41)
      a(64) = rconst(64)*v(39)
      a(65) = rconst(65)*v(6)*v(48)
      a(66) = rconst(66)*v(40)
      a(67) = rconst(67)*v(40)*v(48)
      a(68) = rconst(68)*v(40)*v(53)
      a(69) = rconst(69)*v(47)*v(52)
      a(70) = rconst(70)*v(9)
      a(71) = rconst(71)*v(47)*v(51)
      a(72) = rconst(72)*v(47)*v(53)
      a(73) = rconst(73)*v(47)*v(50)
      a(74) = rconst(74)*v(47)
      a(75) = rconst(75)*v(26)*v(48)
      a(76) = rconst(76)*v(35)
      a(77) = rconst(77)*v(35)*v(48)
      a(78) = rconst(78)*v(38)
      a(79) = rconst(79)*v(38)*v(48)
      a(80) = rconst(80)*v(38)*v(53)
      a(81) = rconst(81)*v(20)*v(49)
      a(82) = rconst(82)*v(20)*v(48)
      a(83) = rconst(83)*v(31)*v(49)
      a(84) = rconst(84)*v(36)*v(49)
      a(85) = rconst(85)*v(31)*v(48)
      a(86) = rconst(86)*v(36)*v(48)
      a(87) = rconst(87)*v(31)*v(53)
      a(88) = rconst(88)*v(36)*v(53)
      a(89) = rconst(89)*v(10)*v(48)
      a(90) = rconst(90)*v(12)*v(48)
      a(91) = rconst(91)*v(15)*v(51)
      a(92) = rconst(92)*v(23)*v(48)
      a(93) = rconst(93)*v(23)*v(53)
      a(94) = rconst(94)*v(13)*v(52)
      a(95) = rconst(95)*v(27)*v(48)
      a(96) = rconst(96)*v(27)
      a(97) = rconst(97)*v(27)*v(49)
      a(98) = rconst(98)*v(44)
      a(99) = rconst(99)*v(44)*v(48)
      a(100) = rconst(100)*v(46)*v(48)
      a(101) = rconst(101)*v(46)
      a(102) = rconst(102)*v(45)*v(51)
      a(103) = rconst(103)*v(43)*v(51)
      a(104) = rconst(104)*v(37)*v(51)
      a(105) = rconst(105)*v(34)*v(51)
      a(106) = rconst(106)*v(45)*v(53)
      a(107) = rconst(107)*v(43)*v(53)
      a(108) = rconst(108)*v(37)*v(53)
      a(109) = rconst(109)*v(34)*v(53)
      a(110) = rconst(110)*v(45)*v(50)
      a(111) = rconst(111)*v(43)*v(50)
      a(112) = rconst(112)*v(37)*v(50)
      a(113) = rconst(113)*v(34)*v(50)
      a(114) = rconst(114)*v(45)
      a(115) = rconst(115)*v(43)
      a(116) = rconst(116)*v(37)
      a(117) = rconst(117)*v(34)
      a(118) = rconst(118)*v(16)*v(26)
      a(119) = rconst(119)*v(32)*v(48)
      a(120) = rconst(120)*v(32)*v(49)
      a(121) = rconst(121)*v(32)*v(53)
      a(122) = rconst(122)*v(42)
      a(123) = rconst(123)*v(42)*v(48)
      a(124) = rconst(124)*v(42)*v(49)
      a(125) = rconst(125)*v(42)*v(53)
      a(126) = rconst(126)*v(29)*v(51)
      a(127) = rconst(127)*v(28)*v(51)
      a(128) = rconst(128)*v(30)*v(51)
      a(129) = rconst(129)*v(29)*v(50)
      a(130) = rconst(130)*v(28)*v(50)
      a(131) = rconst(131)*v(30)*v(50)
      a(132) = rconst(132)*v(29)
      a(133) = rconst(133)*v(28)
      a(134) = rconst(134)*v(30)

! aggregate function
      a_var(1) = a(45)
      a_var(2) = 0.52*a(81)+0.22*a(83)+0.39*a(120)+0.46*a(124)
      a_var(3) = 0.4*a(73)+0.09*a(83)+0.16*a(84)
      a_var(4) = a(8)-a(10)-a(11)-a(12)
      a_var(5) = -a(45)
      a_var(6) = -a(65)
      a_var(7) = -a(9)-a(30)+a(31)+a(32)
      a_var(8) = -a(47)+0.2*a(64)
      a_var(9) = a(69)-a(70)
      a_var(10) = -a(89)
      a_var(11) = -a(6)+a(39)-a(42)-a(43)
      a_var(12) = -a(90)
      a_var(13) = 0.4*a(92)+a(93)-a(94)
      a_var(14) = -a(5)-a(28)+a(34)-a(36)
      a_var(15) = 0.8*a(89)+0.45*a(90)-a(91)
      a_var(16) = 1.06*a(83)+2.26*a(84)+a(85)+2.23*a(86)+1.98*a(98)   &
                 +0.42*a(99)+1.98*a(101)+1.68*a(102)+a(104)+1.98   &
                 *a(106)+a(108)+1.25*a(114)+a(116)-a(118)
      a_var(17) = -a(53)-a(55)+a(61)
      a_var(18) = -a(54)-a(56)+a(62)
      a_var(19) = -a(3)+a(23)-a(26)+a(35)
      a_var(20) = -a(81)-a(82)
      a_var(21) = -a(48)+0.34*a(63)+0.03*a(83)+0.04*a(84)
      a_var(22) = a(1)+0.89*a(2)+a(7)+a(10)+a(11)-a(13)-a(14)-a(15)   &
                 -a(16)-a(17)
      a_var(23) = 0.12*a(89)+0.05*a(90)-a(92)-a(93)
      a_var(24) = -a(4)+a(24)-a(27)+0.3*a(41)+2*a(42)+a(52)+a(68)   &
                 +a(80)+a(93)+0.07*a(125)
      a_var(25) = -a(44)+a(49)+a(50)+a(51)+a(52)+a(66)+a(78)+a(80)   &
                 +0.24*a(81)+0.31*a(83)+0.3*a(84)+2*a(95)+a(96)+0.69   &
                 *a(97)+0.07*a(120)+0.33*a(122)+0.16*a(124)+0.64   &
                 *a(125)+0.59*a(128)
      a_var(26) = -a(75)+1.1*a(90)-a(118)+1.86*a(125)+0.18*a(126)+1.6   &
                 *a(127)+2*a(130)+2*a(133)
      a_var(27) = 0.95*a(91)+0.3*a(92)-a(95)-a(96)-a(97)
      a_var(28) = a(121)-a(127)-a(130)-a(133)
      a_var(29) = a(119)-a(126)-a(129)-a(132)
      a_var(30) = 0.5*a(123)-a(128)-a(131)-a(134)
      a_var(31) = -a(83)-a(85)-a(87)
      a_var(32) = -a(119)-a(120)-a(121)
      a_var(33) = a(48)-a(49)-a(50)-a(51)-a(52)+a(53)+0.3*a(55)+a(57)   &
                 +a(59)+0.66*a(63)+a(81)+1.56*a(82)+0.57*a(83)+a(85)   &
                 +a(95)+0.7*a(97)+a(103)+0.5*a(104)+a(107)+0.5*a(108)   &
                 +0.7*a(115)+0.5*a(116)+0.6*a(120)+0.2*a(122)+0.15   &
                 *a(124)+0.28*a(125)+0.63*a(126)+0.25*a(128)
      a_var(34) = a(79)+a(82)+a(85)+a(86)+0.08*a(89)+0.5*a(90)+0.6   &
                 *a(92)+a(95)+0.03*a(97)+0.4*a(98)+0.4*a(101)+0.34   &
                 *a(102)-a(105)+0.4*a(106)-a(109)-a(113)+0.24*a(114)   &
                 -a(117)+0.08*a(119)+0.2*a(120)+0.2*a(123)+0.07*a(124)   &
                 +0.93*a(125)
      a_var(35) = -a(76)-a(77)+0.07*a(84)+0.23*a(86)+0.74*a(98)+0.74   &
                 *a(101)+0.62*a(102)+0.74*a(106)+0.57*a(114)+0.15   &
                 *a(115)+0.03*a(122)+0.09*a(124)+0.63*a(128)+0.5   &
                 *a(134)
      a_var(36) = -a(84)-a(86)-a(88)
      a_var(37) = a(87)+a(88)+a(100)-a(104)-a(108)-a(112)-a(116)
      a_var(38) = -a(78)-a(79)-a(80)+0.04*a(83)+0.07*a(84)+0.8*a(90)   &
                 +0.2*a(97)+0.19*a(99)+0.15*a(115)+0.85*a(124)+0.34   &
                 *a(128)
      a_var(39) = a(47)+0.5*a(56)-a(58)-a(60)-a(62)-a(64)+0.06*a(83)   &
                 +0.05*a(84)+0.1*a(98)+0.1*a(101)+0.08*a(102)+0.1   &
                 *a(106)+0.06*a(114)
      a_var(40) = a(54)+0.5*a(56)+a(58)+a(60)+0.8*a(64)+a(65)-a(66)   &
                 -a(67)-a(68)+0.22*a(82)+0.47*a(83)+1.03*a(84)+a(85)   &
                 +1.77*a(86)+0.03*a(97)+0.3*a(98)+0.04*a(99)+0.3   &
                 *a(101)+0.25*a(102)+0.5*a(104)+0.3*a(106)+0.5*a(108)   &
                 +0.21*a(114)+0.5*a(116)+0.15*a(120)+0.07*a(122)+0.02   &
                 *a(124)+0.28*a(125)+0.8*a(127)+0.55*a(128)+a(133)+0.5   &
                 *a(134)
      a_var(41) = a(46)+0.7*a(55)-a(57)-a(59)-a(61)-a(63)+a(66)+a(71)   &
                 +a(72)+a(74)+a(76)+0.07*a(83)+0.1*a(84)+0.7*a(122)   &
                 +0.05*a(124)
      a_var(42) = 0.65*a(120)-a(122)-a(123)-a(124)-a(125)+0.91*a(126)   &
                 +0.2*a(127)+a(132)
      a_var(43) = a(77)+0.11*a(84)-a(103)-a(107)-a(111)-a(115)
      a_var(44) = -a(98)-a(99)+a(110)+a(111)+a(129)+a(131)
      a_var(45) = a(75)+0.03*a(83)+0.09*a(84)+0.77*a(99)-a(102)-a(106)   &
                 -a(110)-a(114)
      a_var(46) = 0.05*a(91)+a(94)-a(100)-a(101)+0.16*a(102)+0.5   &
                 *a(104)+0.5*a(108)+a(112)+0.5*a(116)+0.93*a(125)+0.09   &
                 *a(126)+0.8*a(127)+a(130)+a(133)
      a_var(47) = a(67)+a(68)-a(69)+a(70)-a(71)-a(72)-a(73)-a(74)   &
                 +a(76)+a(78)+a(79)+a(80)+0.13*a(83)+0.19*a(84)+a(95)   &
                 +a(96)+0.62*a(97)+a(103)+a(107)+0.7*a(115)+0.2*a(120)   &
                 +0.97*a(122)+0.5*a(123)+0.11*a(124)+0.07*a(125)
      a_var(48) = a(3)+a(4)+2*a(9)+2*a(12)-a(20)+a(21)-a(22)-a(23)   &
                 -a(24)-a(25)-a(26)-a(27)-a(28)-a(29)-a(30)+a(33)+0.7   &
                 *a(41)-a(44)-a(45)-a(46)-a(47)-a(48)-a(51)+a(53)   &
                 +a(54)-0.7*a(55)-0.5*a(56)-a(65)-a(67)-a(75)-a(77)   &
                 -a(79)+0.12*a(81)-a(82)+0.33*a(83)+0.6*a(84)-a(85)   &
                 -a(86)-a(89)-a(90)-a(92)-a(95)+0.08*a(97)+a(98)-0.77   &
                 *a(99)-a(100)-a(119)+0.27*a(120)-a(123)+0.27*a(124)
      a_var(49) = -a(7)-a(8)+a(13)-a(14)-a(18)-a(19)-a(20)-a(21)+0.4   &
                 *a(73)-a(81)-a(83)-a(84)-a(97)-a(120)-a(124)
      a_var(50) = a(5)+a(20)-a(21)+a(22)+a(25)-a(29)+a(30)-2*a(31)-2   &
                 *a(32)-a(33)-a(34)-a(35)+a(36)-a(41)+a(44)+a(45)   &
                 +a(48)+2*a(49)+a(51)+a(52)+a(53)+a(54)+a(57)+a(58)   &
                 +a(59)+a(60)-a(61)-a(62)+0.32*a(63)+0.6*a(64)+a(65)   &
                 +a(66)-a(73)+a(78)+0.22*a(81)+a(82)+0.26*a(83)+0.22   &
                 *a(84)+a(85)+a(86)+0.2*a(89)+0.55*a(90)+0.95*a(91)   &
                 +0.6*a(92)+2*a(95)+a(96)+0.76*a(97)+0.9*a(98)+0.9   &
                 *a(101)+0.76*a(102)+0.5*a(104)+0.9*a(106)+0.5*a(108)   &
                 -a(110)-a(111)-a(112)-a(113)+0.54*a(114)+0.07*a(120)   &
                 +0.33*a(122)+0.1*a(124)+0.93*a(125)+0.91*a(126)+0.8   &
                 *a(127)+a(128)-a(129)-a(130)-a(131)
      a_var(51) = a(1)+0.11*a(2)+a(3)+a(15)-a(17)-a(18)-a(23)-a(33)   &
                 -a(37)+a(38)-a(57)-a(58)-a(71)-a(91)-a(102)-a(103)   &
                 -a(104)-a(105)-a(126)-a(127)-a(128)
      a_var(52) = -a(1)+0.89*a(2)+a(4)+a(5)+a(6)-a(15)-a(16)+a(17)   &
                 +a(18)-a(19)-a(24)+a(25)+a(26)+a(28)+a(33)-a(34)   &
                 -a(35)+a(36)+2*a(37)-a(39)+2*a(40)+0.7*a(41)+a(43)   &
                 +a(57)+a(58)+a(59)+a(60)-a(69)+a(70)+a(71)+a(72)+0.95   &
                 *a(91)-a(94)+a(101)+0.84*a(102)+a(103)+1.5*a(104)   &
                 +a(105)+a(106)+a(107)+1.5*a(108)+a(109)+0.5*a(116)   &
                 +0.91*a(126)+1.2*a(127)+a(128)
      a_var(53) = -a(2)+a(6)+a(16)+a(19)-a(25)+a(27)-a(37)-a(38)-a(39)   &
                 -2*a(40)-a(41)+a(43)-a(52)-a(59)-a(60)-a(68)-a(72)   &
                 -a(80)-a(87)-a(88)-a(93)-a(106)-a(107)-a(108)-a(109)   &
                 -a(121)-a(125)
      return
      end subroutine cbmz_v02r03_dydt                                      


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r03_jacob( nvardum, tdum, v, jvs, f, rconst )
!
!   computes jacobian for mechanism-version-regime = cbmz_v02r03
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     nvardum = number of variable species [input]
      integer nvardum
!     tdum = time [input]
      real tdum
!     v = concentrations of variable species [input]
      real v(nvar_r03_kpp)
!     jvs = non-zero jacobian elements [output]
      real jvs(lu_nonzero_v_r03_kpp)
!     f = concentrations of fixed species [input]
      real f(nfixed_kppmax)
!     rconst = reaction rate constants [input]
      real rconst(nreact_r03_kpp)

!   local variables
!     b(i,j) = d[reaction_rate(i)] / d[species_conc(j)]
      real b(nreact_r03_kpp,nvar_r03_kpp)

! computation of b(i,j) = da(i)/dv(j)
      b(1,52) = rconst(1)
      b(2,53) = rconst(2)
      b(3,19) = rconst(3)
      b(4,24) = rconst(4)
      b(5,14) = rconst(5)
      b(6,11) = rconst(6)
      b(7,49) = rconst(7)
      b(8,49) = rconst(8)
      b(9,7) = rconst(9)
      b(10,4) = rconst(10)*f(4)
      b(11,4) = rconst(11)*f(5)
      b(12,4) = rconst(12)*f(2)
      b(13,22) = rconst(13)*f(4)
      b(14,22) = rconst(14)*v(49)
      b(14,49) = rconst(14)*v(22)
      b(15,22) = rconst(15)*v(52)
      b(15,52) = rconst(15)*v(22)
      b(16,22) = rconst(16)*v(52)
      b(16,52) = rconst(16)*v(22)
      b(17,22) = rconst(17)*v(51)
      b(17,51) = rconst(17)*v(22)
      b(18,49) = rconst(18)*v(51)
      b(18,51) = rconst(18)*v(49)
      b(19,49) = rconst(19)*v(52)
      b(19,52) = rconst(19)*v(49)
      b(20,48) = rconst(20)*v(49)
      b(20,49) = rconst(20)*v(48)
      b(21,49) = rconst(21)*v(50)
      b(21,50) = rconst(21)*v(49)
      b(22,48) = rconst(22)*f(3)
      b(23,48) = rconst(23)*v(51)
      b(23,51) = rconst(23)*v(48)
      b(24,48) = rconst(24)*v(52)
      b(24,52) = rconst(24)*v(48)
      b(25,48) = rconst(25)*v(53)
      b(25,53) = rconst(25)*v(48)
      b(26,19) = rconst(26)*v(48)
      b(26,48) = rconst(26)*v(19)
      b(27,24) = rconst(27)*v(48)
      b(27,48) = rconst(27)*v(24)
      b(28,14) = rconst(28)*v(48)
      b(28,48) = rconst(28)*v(14)
      b(29,48) = rconst(29)*v(50)
      b(29,50) = rconst(29)*v(48)
      b(30,7) = rconst(30)*v(48)
      b(30,48) = rconst(30)*v(7)
      b(31,50) = rconst(31)*2*v(50)
      b(32,50) = rconst(32)*2*v(50)*f(2)
      b(33,50) = rconst(33)*v(51)
      b(33,51) = rconst(33)*v(50)
      b(34,50) = rconst(34)*v(52)
      b(34,52) = rconst(34)*v(50)
      b(35,50) = rconst(35)*v(52)
      b(35,52) = rconst(35)*v(50)
      b(36,14) = rconst(36)
      b(37,51) = rconst(37)*v(53)
      b(37,53) = rconst(37)*v(51)
      b(38,52) = rconst(38)*v(53)
      b(38,53) = rconst(38)*v(52)
      b(39,52) = rconst(39)*v(53)
      b(39,53) = rconst(39)*v(52)
      b(40,53) = rconst(40)*2*v(53)
      b(41,50) = rconst(41)*v(53)
      b(41,53) = rconst(41)*v(50)
      b(42,11) = rconst(42)*f(2)
      b(43,11) = rconst(43)
      b(44,25) = rconst(44)*v(48)
      b(44,48) = rconst(44)*v(25)
      b(45,5) = rconst(45)*v(48)
      b(45,48) = rconst(45)*v(5)
      b(46,48) = rconst(46)*f(1)
      b(47,8) = rconst(47)*v(48)
      b(47,48) = rconst(47)*v(8)
      b(48,21) = rconst(48)*v(48)
      b(48,48) = rconst(48)*v(21)
      b(49,33) = rconst(49)
      b(50,33) = rconst(50)
      b(51,33) = rconst(51)*v(48)
      b(51,48) = rconst(51)*v(33)
      b(52,33) = rconst(52)*v(53)
      b(52,53) = rconst(52)*v(33)
      b(53,17) = rconst(53)
      b(54,18) = rconst(54)
      b(55,17) = rconst(55)*v(48)
      b(55,48) = rconst(55)*v(17)
      b(56,18) = rconst(56)*v(48)
      b(56,48) = rconst(56)*v(18)
      b(57,41) = rconst(57)*v(51)
      b(57,51) = rconst(57)*v(41)
      b(58,39) = rconst(58)*v(51)
      b(58,51) = rconst(58)*v(39)
      b(59,41) = rconst(59)*v(53)
      b(59,53) = rconst(59)*v(41)
      b(60,39) = rconst(60)*v(53)
      b(60,53) = rconst(60)*v(39)
      b(61,41) = rconst(61)*v(50)
      b(61,50) = rconst(61)*v(41)
      b(62,39) = rconst(62)*v(50)
      b(62,50) = rconst(62)*v(39)
      b(63,41) = rconst(63)
      b(64,39) = rconst(64)
      b(65,6) = rconst(65)*v(48)
      b(65,48) = rconst(65)*v(6)
      b(66,40) = rconst(66)
      b(67,40) = rconst(67)*v(48)
      b(67,48) = rconst(67)*v(40)
      b(68,40) = rconst(68)*v(53)
      b(68,53) = rconst(68)*v(40)
      b(69,47) = rconst(69)*v(52)
      b(69,52) = rconst(69)*v(47)
      b(70,9) = rconst(70)
      b(71,47) = rconst(71)*v(51)
      b(71,51) = rconst(71)*v(47)
      b(72,47) = rconst(72)*v(53)
      b(72,53) = rconst(72)*v(47)
      b(73,47) = rconst(73)*v(50)
      b(73,50) = rconst(73)*v(47)
      b(74,47) = rconst(74)
      b(75,26) = rconst(75)*v(48)
      b(75,48) = rconst(75)*v(26)
      b(76,35) = rconst(76)
      b(77,35) = rconst(77)*v(48)
      b(77,48) = rconst(77)*v(35)
      b(78,38) = rconst(78)
      b(79,38) = rconst(79)*v(48)
      b(79,48) = rconst(79)*v(38)
      b(80,38) = rconst(80)*v(53)
      b(80,53) = rconst(80)*v(38)
      b(81,20) = rconst(81)*v(49)
      b(81,49) = rconst(81)*v(20)
      b(82,20) = rconst(82)*v(48)
      b(82,48) = rconst(82)*v(20)
      b(83,31) = rconst(83)*v(49)
      b(83,49) = rconst(83)*v(31)
      b(84,36) = rconst(84)*v(49)
      b(84,49) = rconst(84)*v(36)
      b(85,31) = rconst(85)*v(48)
      b(85,48) = rconst(85)*v(31)
      b(86,36) = rconst(86)*v(48)
      b(86,48) = rconst(86)*v(36)
      b(87,31) = rconst(87)*v(53)
      b(87,53) = rconst(87)*v(31)
      b(88,36) = rconst(88)*v(53)
      b(88,53) = rconst(88)*v(36)
      b(89,10) = rconst(89)*v(48)
      b(89,48) = rconst(89)*v(10)
      b(90,12) = rconst(90)*v(48)
      b(90,48) = rconst(90)*v(12)
      b(91,15) = rconst(91)*v(51)
      b(91,51) = rconst(91)*v(15)
      b(92,23) = rconst(92)*v(48)
      b(92,48) = rconst(92)*v(23)
      b(93,23) = rconst(93)*v(53)
      b(93,53) = rconst(93)*v(23)
      b(94,13) = rconst(94)*v(52)
      b(94,52) = rconst(94)*v(13)
      b(95,27) = rconst(95)*v(48)
      b(95,48) = rconst(95)*v(27)
      b(96,27) = rconst(96)
      b(97,27) = rconst(97)*v(49)
      b(97,49) = rconst(97)*v(27)
      b(98,44) = rconst(98)
      b(99,44) = rconst(99)*v(48)
      b(99,48) = rconst(99)*v(44)
      b(100,46) = rconst(100)*v(48)
      b(100,48) = rconst(100)*v(46)
      b(101,46) = rconst(101)
      b(102,45) = rconst(102)*v(51)
      b(102,51) = rconst(102)*v(45)
      b(103,43) = rconst(103)*v(51)
      b(103,51) = rconst(103)*v(43)
      b(104,37) = rconst(104)*v(51)
      b(104,51) = rconst(104)*v(37)
      b(105,34) = rconst(105)*v(51)
      b(105,51) = rconst(105)*v(34)
      b(106,45) = rconst(106)*v(53)
      b(106,53) = rconst(106)*v(45)
      b(107,43) = rconst(107)*v(53)
      b(107,53) = rconst(107)*v(43)
      b(108,37) = rconst(108)*v(53)
      b(108,53) = rconst(108)*v(37)
      b(109,34) = rconst(109)*v(53)
      b(109,53) = rconst(109)*v(34)
      b(110,45) = rconst(110)*v(50)
      b(110,50) = rconst(110)*v(45)
      b(111,43) = rconst(111)*v(50)
      b(111,50) = rconst(111)*v(43)
      b(112,37) = rconst(112)*v(50)
      b(112,50) = rconst(112)*v(37)
      b(113,34) = rconst(113)*v(50)
      b(113,50) = rconst(113)*v(34)
      b(114,45) = rconst(114)
      b(115,43) = rconst(115)
      b(116,37) = rconst(116)
      b(117,34) = rconst(117)
      b(118,16) = rconst(118)*v(26)
      b(118,26) = rconst(118)*v(16)
      b(119,32) = rconst(119)*v(48)
      b(119,48) = rconst(119)*v(32)
      b(120,32) = rconst(120)*v(49)
      b(120,49) = rconst(120)*v(32)
      b(121,32) = rconst(121)*v(53)
      b(121,53) = rconst(121)*v(32)
      b(122,42) = rconst(122)
      b(123,42) = rconst(123)*v(48)
      b(123,48) = rconst(123)*v(42)
      b(124,42) = rconst(124)*v(49)
      b(124,49) = rconst(124)*v(42)
      b(125,42) = rconst(125)*v(53)
      b(125,53) = rconst(125)*v(42)
      b(126,29) = rconst(126)*v(51)
      b(126,51) = rconst(126)*v(29)
      b(127,28) = rconst(127)*v(51)
      b(127,51) = rconst(127)*v(28)
      b(128,30) = rconst(128)*v(51)
      b(128,51) = rconst(128)*v(30)
      b(129,29) = rconst(129)*v(50)
      b(129,50) = rconst(129)*v(29)
      b(130,28) = rconst(130)*v(50)
      b(130,50) = rconst(130)*v(28)
      b(131,30) = rconst(131)*v(50)
      b(131,50) = rconst(131)*v(30)
      b(132,29) = rconst(132)
      b(133,28) = rconst(133)
      b(134,30) = rconst(134)

! construct the jacobian terms from b's
      jvs(1) = 0
      jvs(2) = b(45,5)
      jvs(3) = b(45,48)
      jvs(4) = 0
      jvs(5) = 0.52*b(81,20)
      jvs(6) = 0.22*b(83,31)
      jvs(7) = 0.39*b(120,32)
      jvs(8) = 0.46*b(124,42)
      jvs(9) = 0.52*b(81,49)+0.22*b(83,49)+0.39*b(120,49)+0.46   &
              *b(124,49)
      jvs(10) = 0
      jvs(11) = 0.09*b(83,31)
      jvs(12) = 0.16*b(84,36)
      jvs(13) = 0.4*b(73,47)
      jvs(14) = 0.09*b(83,49)+0.16*b(84,49)
      jvs(15) = 0.4*b(73,50)
      jvs(16) = -b(10,4)-b(11,4)-b(12,4)
      jvs(17) = b(8,49)
      jvs(18) = -b(45,5)
      jvs(19) = -b(45,48)
      jvs(20) = -b(65,6)
      jvs(21) = -b(65,48)
      jvs(22) = -b(9,7)-b(30,7)
      jvs(23) = -b(30,48)
      jvs(24) = b(31,50)+b(32,50)
      jvs(25) = -b(47,8)
      jvs(26) = 0.2*b(64,39)
      jvs(27) = -b(47,48)
      jvs(28) = -b(70,9)
      jvs(29) = b(69,47)
      jvs(30) = b(69,52)
      jvs(31) = -b(89,10)
      jvs(32) = -b(89,48)
      jvs(33) = -b(6,11)-b(42,11)-b(43,11)
      jvs(34) = b(39,52)
      jvs(35) = b(39,53)
      jvs(36) = -b(90,12)
      jvs(37) = -b(90,48)
      jvs(38) = -b(94,13)
      jvs(39) = 0.4*b(92,23)+b(93,23)
      jvs(40) = 0.4*b(92,48)
      jvs(41) = -b(94,52)
      jvs(42) = b(93,53)
      jvs(43) = -b(5,14)-b(28,14)-b(36,14)
      jvs(44) = -b(28,48)
      jvs(45) = b(34,50)
      jvs(46) = b(34,52)
      jvs(47) = 0.8*b(89,10)
      jvs(48) = 0.45*b(90,12)
      jvs(49) = -b(91,15)
      jvs(50) = 0.8*b(89,48)+0.45*b(90,48)
      jvs(51) = -b(91,51)
      jvs(52) = -b(118,16)
      jvs(53) = -b(118,26)
      jvs(54) = 1.06*b(83,31)+b(85,31)
      jvs(55) = 2.26*b(84,36)+2.23*b(86,36)
      jvs(56) = b(104,37)+b(108,37)+b(116,37)
      jvs(57) = 1.98*b(98,44)+0.42*b(99,44)
      jvs(58) = 1.68*b(102,45)+1.98*b(106,45)+1.25*b(114,45)
      jvs(59) = 1.98*b(101,46)
      jvs(60) = b(85,48)+2.23*b(86,48)+0.42*b(99,48)
      jvs(61) = 1.06*b(83,49)+2.26*b(84,49)
      jvs(62) = 1.68*b(102,51)+b(104,51)
      jvs(63) = 1.98*b(106,53)+b(108,53)
      jvs(64) = -b(53,17)-b(55,17)
      jvs(65) = b(61,41)
      jvs(66) = -b(55,48)
      jvs(67) = b(61,50)
      jvs(68) = -b(54,18)-b(56,18)
      jvs(69) = b(62,39)
      jvs(70) = -b(56,48)
      jvs(71) = b(62,50)
      jvs(72) = -b(3,19)-b(26,19)
      jvs(73) = b(23,48)-b(26,48)
      jvs(74) = b(35,50)
      jvs(75) = b(23,51)
      jvs(76) = b(35,52)
      jvs(77) = -b(81,20)-b(82,20)
      jvs(78) = -b(82,48)
      jvs(79) = -b(81,49)
      jvs(80) = -b(48,21)
      jvs(81) = 0.03*b(83,31)
      jvs(82) = 0.04*b(84,36)
      jvs(83) = 0.34*b(63,41)
      jvs(84) = -b(48,48)
      jvs(85) = 0.03*b(83,49)+0.04*b(84,49)
      jvs(86) = b(10,4)+b(11,4)
      jvs(87) = -b(13,22)-b(14,22)-b(15,22)-b(16,22)-b(17,22)
      jvs(88) = b(7,49)-b(14,49)
      jvs(89) = -b(17,51)
      jvs(90) = b(1,52)-b(15,52)-b(16,52)
      jvs(91) = 0.89*b(2,53)
      jvs(92) = 0.12*b(89,10)
      jvs(93) = 0.05*b(90,12)
      jvs(94) = -b(92,23)-b(93,23)
      jvs(95) = 0.12*b(89,48)+0.05*b(90,48)-b(92,48)
      jvs(96) = -b(93,53)
      jvs(97) = 2*b(42,11)
      jvs(98) = b(93,23)
      jvs(99) = -b(4,24)-b(27,24)
      jvs(100) = b(52,33)
      jvs(101) = b(80,38)
      jvs(102) = b(68,40)
      jvs(103) = 0.07*b(125,42)
      jvs(104) = b(24,48)-b(27,48)
      jvs(105) = 0.3*b(41,50)
      jvs(106) = b(24,52)
      jvs(107) = 0.3*b(41,53)+b(52,53)+b(68,53)+b(80,53)+b(93,53)+0.07   &
                *b(125,53)
      jvs(108) = 0.24*b(81,20)
      jvs(109) = -b(44,25)
      jvs(110) = 2*b(95,27)+b(96,27)+0.69*b(97,27)
      jvs(111) = 0.59*b(128,30)
      jvs(112) = 0.31*b(83,31)
      jvs(113) = 0.07*b(120,32)
      jvs(114) = b(49,33)+b(50,33)+b(51,33)+b(52,33)
      jvs(115) = 0.3*b(84,36)
      jvs(116) = b(78,38)+b(80,38)
      jvs(117) = b(66,40)
      jvs(118) = 0.33*b(122,42)+0.16*b(124,42)+0.64*b(125,42)
      jvs(119) = -b(44,48)+b(51,48)+2*b(95,48)
      jvs(120) = 0.24*b(81,49)+0.31*b(83,49)+0.3*b(84,49)+0.69   &
                *b(97,49)+0.07*b(120,49)+0.16*b(124,49)
      jvs(121) = 0.59*b(128,51)
      jvs(122) = b(52,53)+b(80,53)+0.64*b(125,53)
      jvs(123) = 1.1*b(90,12)
      jvs(124) = -b(118,16)
      jvs(125) = -b(75,26)-b(118,26)
      jvs(126) = 1.6*b(127,28)+2*b(130,28)+2*b(133,28)
      jvs(127) = 0.18*b(126,29)
      jvs(128) = 0
      jvs(129) = 0
      jvs(130) = 0
      jvs(131) = 1.86*b(125,42)
      jvs(132) = 0
      jvs(133) = 0
      jvs(134) = 0
      jvs(135) = -b(75,48)+1.1*b(90,48)
      jvs(136) = 0
      jvs(137) = 2*b(130,50)
      jvs(138) = 0.18*b(126,51)+1.6*b(127,51)
      jvs(139) = 1.86*b(125,53)
      jvs(140) = 0.95*b(91,15)
      jvs(141) = 0.3*b(92,23)
      jvs(142) = -b(95,27)-b(96,27)-b(97,27)
      jvs(143) = 0.3*b(92,48)-b(95,48)
      jvs(144) = -b(97,49)
      jvs(145) = 0.95*b(91,51)
      jvs(146) = 0
      jvs(147) = -b(127,28)-b(130,28)-b(133,28)
      jvs(148) = b(121,32)
      jvs(149) = -b(130,50)
      jvs(150) = -b(127,51)
      jvs(151) = b(121,53)
      jvs(152) = -b(126,29)-b(129,29)-b(132,29)
      jvs(153) = b(119,32)
      jvs(154) = b(119,48)
      jvs(155) = -b(129,50)
      jvs(156) = -b(126,51)
      jvs(157) = -b(128,30)-b(131,30)-b(134,30)
      jvs(158) = 0.5*b(123,42)
      jvs(159) = 0.5*b(123,48)
      jvs(160) = -b(131,50)
      jvs(161) = -b(128,51)
      jvs(162) = -b(83,31)-b(85,31)-b(87,31)
      jvs(163) = -b(85,48)
      jvs(164) = -b(83,49)
      jvs(165) = -b(87,53)
      jvs(166) = -b(119,32)-b(120,32)-b(121,32)
      jvs(167) = -b(119,48)
      jvs(168) = -b(120,49)
      jvs(169) = -b(121,53)
      jvs(170) = b(53,17)+0.3*b(55,17)
      jvs(171) = b(81,20)+1.56*b(82,20)
      jvs(172) = b(48,21)
      jvs(173) = b(95,27)+0.7*b(97,27)
      jvs(174) = 0.63*b(126,29)
      jvs(175) = 0.25*b(128,30)
      jvs(176) = 0.57*b(83,31)+b(85,31)
      jvs(177) = 0.6*b(120,32)
      jvs(178) = -b(49,33)-b(50,33)-b(51,33)-b(52,33)
      jvs(179) = 0
      jvs(180) = 0.5*b(104,37)+0.5*b(108,37)+0.5*b(116,37)
      jvs(181) = b(57,41)+b(59,41)+0.66*b(63,41)
      jvs(182) = 0.2*b(122,42)+0.15*b(124,42)+0.28*b(125,42)
      jvs(183) = b(103,43)+b(107,43)+0.7*b(115,43)
      jvs(184) = b(48,48)-b(51,48)+0.3*b(55,48)+1.56*b(82,48)+b(85,48)   &
                +b(95,48)
      jvs(185) = b(81,49)+0.57*b(83,49)+0.7*b(97,49)+0.6*b(120,49)   &
                +0.15*b(124,49)
      jvs(186) = 0
      jvs(187) = b(57,51)+b(103,51)+0.5*b(104,51)+0.63*b(126,51)+0.25   &
                *b(128,51)
      jvs(188) = -b(52,53)+b(59,53)+b(107,53)+0.5*b(108,53)+0.28   &
                *b(125,53)
      jvs(189) = 0.08*b(89,10)
      jvs(190) = 0.5*b(90,12)
      jvs(191) = b(82,20)
      jvs(192) = 0.6*b(92,23)
      jvs(193) = b(95,27)+0.03*b(97,27)
      jvs(194) = b(85,31)
      jvs(195) = 0.08*b(119,32)+0.2*b(120,32)
      jvs(196) = -b(105,34)-b(109,34)-b(113,34)-b(117,34)
      jvs(197) = b(86,36)
      jvs(198) = b(79,38)
      jvs(199) = 0.2*b(123,42)+0.07*b(124,42)+0.93*b(125,42)
      jvs(200) = 0.4*b(98,44)
      jvs(201) = 0.34*b(102,45)+0.4*b(106,45)+0.24*b(114,45)
      jvs(202) = 0.4*b(101,46)
      jvs(203) = b(79,48)+b(82,48)+b(85,48)+b(86,48)+0.08*b(89,48)+0.5   &
                *b(90,48)+0.6*b(92,48)+b(95,48)+0.08*b(119,48)+0.2   &
                *b(123,48)
      jvs(204) = 0.03*b(97,49)+0.2*b(120,49)+0.07*b(124,49)
      jvs(205) = -b(113,50)
      jvs(206) = 0.34*b(102,51)-b(105,51)
      jvs(207) = 0.4*b(106,53)-b(109,53)+0.93*b(125,53)
      jvs(208) = 0.63*b(128,30)+0.5*b(134,30)
      jvs(209) = -b(76,35)-b(77,35)
      jvs(210) = 0.07*b(84,36)+0.23*b(86,36)
      jvs(211) = 0.03*b(122,42)+0.09*b(124,42)
      jvs(212) = 0.15*b(115,43)
      jvs(213) = 0.74*b(98,44)
      jvs(214) = 0.62*b(102,45)+0.74*b(106,45)+0.57*b(114,45)
      jvs(215) = 0.74*b(101,46)
      jvs(216) = -b(77,48)+0.23*b(86,48)
      jvs(217) = 0.07*b(84,49)+0.09*b(124,49)
      jvs(218) = 0
      jvs(219) = 0.62*b(102,51)+0.63*b(128,51)
      jvs(220) = 0.74*b(106,53)
      jvs(221) = -b(84,36)-b(86,36)-b(88,36)
      jvs(222) = -b(86,48)
      jvs(223) = -b(84,49)
      jvs(224) = -b(88,53)
      jvs(225) = b(87,31)
      jvs(226) = b(88,36)
      jvs(227) = -b(104,37)-b(108,37)-b(112,37)-b(116,37)
      jvs(228) = b(100,46)
      jvs(229) = b(100,48)
      jvs(230) = 0
      jvs(231) = -b(112,50)
      jvs(232) = -b(104,51)
      jvs(233) = b(87,53)+b(88,53)-b(108,53)
      jvs(234) = 0.8*b(90,12)
      jvs(235) = 0.2*b(97,27)
      jvs(236) = 0.34*b(128,30)
      jvs(237) = 0.04*b(83,31)
      jvs(238) = 0.07*b(84,36)
      jvs(239) = -b(78,38)-b(79,38)-b(80,38)
      jvs(240) = 0.85*b(124,42)
      jvs(241) = 0.15*b(115,43)
      jvs(242) = 0.19*b(99,44)
      jvs(243) = -b(79,48)+0.8*b(90,48)+0.19*b(99,48)
      jvs(244) = 0.04*b(83,49)+0.07*b(84,49)+0.2*b(97,49)+0.85   &
                *b(124,49)
      jvs(245) = 0
      jvs(246) = 0.34*b(128,51)
      jvs(247) = -b(80,53)
      jvs(248) = b(47,8)
      jvs(249) = 0.5*b(56,18)
      jvs(250) = 0.06*b(83,31)
      jvs(251) = 0.05*b(84,36)
      jvs(252) = -b(58,39)-b(60,39)-b(62,39)-b(64,39)
      jvs(253) = 0.1*b(98,44)
      jvs(254) = 0.08*b(102,45)+0.1*b(106,45)+0.06*b(114,45)
      jvs(255) = 0.1*b(101,46)
      jvs(256) = b(47,48)+0.5*b(56,48)
      jvs(257) = 0.06*b(83,49)+0.05*b(84,49)
      jvs(258) = -b(62,50)
      jvs(259) = -b(58,51)+0.08*b(102,51)
      jvs(260) = -b(60,53)+0.1*b(106,53)
      jvs(261) = b(65,6)
      jvs(262) = b(54,18)+0.5*b(56,18)
      jvs(263) = 0.22*b(82,20)
      jvs(264) = 0.03*b(97,27)
      jvs(265) = 0.8*b(127,28)+b(133,28)
      jvs(266) = 0.55*b(128,30)+0.5*b(134,30)
      jvs(267) = 0.47*b(83,31)+b(85,31)
      jvs(268) = 0.15*b(120,32)
      jvs(269) = 1.03*b(84,36)+1.77*b(86,36)
      jvs(270) = 0.5*b(104,37)+0.5*b(108,37)+0.5*b(116,37)
      jvs(271) = b(58,39)+b(60,39)+0.8*b(64,39)
      jvs(272) = -b(66,40)-b(67,40)-b(68,40)
      jvs(273) = 0.07*b(122,42)+0.02*b(124,42)+0.28*b(125,42)
      jvs(274) = 0.3*b(98,44)+0.04*b(99,44)
      jvs(275) = 0.25*b(102,45)+0.3*b(106,45)+0.21*b(114,45)
      jvs(276) = 0.3*b(101,46)
      jvs(277) = 0.5*b(56,48)+b(65,48)-b(67,48)+0.22*b(82,48)+b(85,48)   &
                +1.77*b(86,48)+0.04*b(99,48)
      jvs(278) = 0.47*b(83,49)+1.03*b(84,49)+0.03*b(97,49)+0.15   &
                *b(120,49)+0.02*b(124,49)
      jvs(279) = 0
      jvs(280) = b(58,51)+0.25*b(102,51)+0.5*b(104,51)+0.8*b(127,51)   &
                +0.55*b(128,51)
      jvs(281) = b(60,53)-b(68,53)+0.3*b(106,53)+0.5*b(108,53)+0.28   &
                *b(125,53)
      jvs(282) = 0.7*b(55,17)
      jvs(283) = 0.07*b(83,31)
      jvs(284) = b(76,35)
      jvs(285) = 0.1*b(84,36)
      jvs(286) = b(66,40)
      jvs(287) = -b(57,41)-b(59,41)-b(61,41)-b(63,41)
      jvs(288) = 0.7*b(122,42)+0.05*b(124,42)
      jvs(289) = 0
      jvs(290) = 0
      jvs(291) = 0
      jvs(292) = 0
      jvs(293) = b(71,47)+b(72,47)+b(74,47)
      jvs(294) = b(46,48)+0.7*b(55,48)
      jvs(295) = 0.07*b(83,49)+0.1*b(84,49)+0.05*b(124,49)
      jvs(296) = -b(61,50)
      jvs(297) = -b(57,51)+b(71,51)
      jvs(298) = -b(59,53)+b(72,53)
      jvs(299) = 0.2*b(127,28)
      jvs(300) = 0.91*b(126,29)+b(132,29)
      jvs(301) = 0.65*b(120,32)
      jvs(302) = -b(122,42)-b(123,42)-b(124,42)-b(125,42)
      jvs(303) = -b(123,48)
      jvs(304) = 0.65*b(120,49)-b(124,49)
      jvs(305) = 0
      jvs(306) = 0.91*b(126,51)+0.2*b(127,51)
      jvs(307) = -b(125,53)
      jvs(308) = b(77,35)
      jvs(309) = 0.11*b(84,36)
      jvs(310) = 0
      jvs(311) = -b(103,43)-b(107,43)-b(111,43)-b(115,43)
      jvs(312) = 0
      jvs(313) = 0
      jvs(314) = 0
      jvs(315) = b(77,48)
      jvs(316) = 0.11*b(84,49)
      jvs(317) = -b(111,50)
      jvs(318) = -b(103,51)
      jvs(319) = -b(107,53)
      jvs(320) = b(129,29)
      jvs(321) = b(131,30)
      jvs(322) = 0
      jvs(323) = 0
      jvs(324) = b(111,43)
      jvs(325) = -b(98,44)-b(99,44)
      jvs(326) = b(110,45)
      jvs(327) = 0
      jvs(328) = -b(99,48)
      jvs(329) = 0
      jvs(330) = b(110,50)+b(111,50)+b(129,50)+b(131,50)
      jvs(331) = 0
      jvs(332) = 0
      jvs(333) = b(75,26)
      jvs(334) = 0
      jvs(335) = 0
      jvs(336) = 0.03*b(83,31)
      jvs(337) = 0
      jvs(338) = 0.09*b(84,36)
      jvs(339) = 0
      jvs(340) = 0
      jvs(341) = 0.77*b(99,44)
      jvs(342) = -b(102,45)-b(106,45)-b(110,45)-b(114,45)
      jvs(343) = 0
      jvs(344) = b(75,48)+0.77*b(99,48)
      jvs(345) = 0.03*b(83,49)+0.09*b(84,49)
      jvs(346) = -b(110,50)
      jvs(347) = -b(102,51)
      jvs(348) = -b(106,53)
      jvs(349) = b(94,13)
      jvs(350) = 0.05*b(91,15)
      jvs(351) = 0
      jvs(352) = 0.8*b(127,28)+b(130,28)+b(133,28)
      jvs(353) = 0.09*b(126,29)
      jvs(354) = 0
      jvs(355) = 0.5*b(104,37)+0.5*b(108,37)+b(112,37)+0.5*b(116,37)
      jvs(356) = 0.93*b(125,42)
      jvs(357) = 0.16*b(102,45)
      jvs(358) = -b(100,46)-b(101,46)
      jvs(359) = -b(100,48)
      jvs(360) = 0
      jvs(361) = b(112,50)+b(130,50)
      jvs(362) = 0.05*b(91,51)+0.16*b(102,51)+0.5*b(104,51)+0.09   &
                *b(126,51)+0.8*b(127,51)
      jvs(363) = b(94,52)
      jvs(364) = 0.5*b(108,53)+0.93*b(125,53)
      jvs(365) = b(70,9)
      jvs(366) = b(95,27)+b(96,27)+0.62*b(97,27)
      jvs(367) = 0.13*b(83,31)
      jvs(368) = 0.2*b(120,32)
      jvs(369) = b(76,35)
      jvs(370) = 0.19*b(84,36)
      jvs(371) = b(78,38)+b(79,38)+b(80,38)
      jvs(372) = b(67,40)+b(68,40)
      jvs(373) = 0.97*b(122,42)+0.5*b(123,42)+0.11*b(124,42)+0.07   &
                *b(125,42)
      jvs(374) = b(103,43)+b(107,43)+0.7*b(115,43)
      jvs(375) = 0
      jvs(376) = 0
      jvs(377) = 0
      jvs(378) = -b(69,47)-b(71,47)-b(72,47)-b(73,47)-b(74,47)
      jvs(379) = b(67,48)+b(79,48)+b(95,48)+0.5*b(123,48)
      jvs(380) = 0.13*b(83,49)+0.19*b(84,49)+0.62*b(97,49)+0.2   &
                *b(120,49)+0.11*b(124,49)
      jvs(381) = -b(73,50)
      jvs(382) = -b(71,51)+b(103,51)
      jvs(383) = -b(69,52)
      jvs(384) = b(68,53)-b(72,53)+b(80,53)+b(107,53)+0.07*b(125,53)
      jvs(385) = 2*b(12,4)
      jvs(386) = -b(45,5)
      jvs(387) = -b(65,6)
      jvs(388) = 2*b(9,7)-b(30,7)
      jvs(389) = -b(47,8)
      jvs(390) = -b(89,10)
      jvs(391) = -b(90,12)
      jvs(392) = -b(28,14)
      jvs(393) = b(53,17)-0.7*b(55,17)
      jvs(394) = b(54,18)-0.5*b(56,18)
      jvs(395) = b(3,19)-b(26,19)
      jvs(396) = 0.12*b(81,20)-b(82,20)
      jvs(397) = -b(48,21)
      jvs(398) = -b(92,23)
      jvs(399) = b(4,24)-b(27,24)
      jvs(400) = -b(44,25)
      jvs(401) = -b(75,26)
      jvs(402) = -b(95,27)+0.08*b(97,27)
      jvs(403) = 0
      jvs(404) = 0
      jvs(405) = 0
      jvs(406) = 0.33*b(83,31)-b(85,31)
      jvs(407) = -b(119,32)+0.27*b(120,32)
      jvs(408) = -b(51,33)
      jvs(409) = -b(77,35)
      jvs(410) = 0.6*b(84,36)-b(86,36)
      jvs(411) = 0
      jvs(412) = -b(79,38)
      jvs(413) = 0
      jvs(414) = -b(67,40)
      jvs(415) = 0
      jvs(416) = -b(123,42)+0.27*b(124,42)
      jvs(417) = 0
      jvs(418) = b(98,44)-0.77*b(99,44)
      jvs(419) = 0
      jvs(420) = -b(100,46)
      jvs(421) = 0
      jvs(422) = -b(20,48)-b(22,48)-b(23,48)-b(24,48)-b(25,48)   &
                -b(26,48)-b(27,48)-b(28,48)-b(29,48)-b(30,48)-b(44,48)   &
                -b(45,48)-b(46,48)-b(47,48)-b(48,48)-b(51,48)-0.7   &
                *b(55,48)-0.5*b(56,48)-b(65,48)-b(67,48)-b(75,48)   &
                -b(77,48)-b(79,48)-b(82,48)-b(85,48)-b(86,48)-b(89,48)   &
                -b(90,48)-b(92,48)-b(95,48)-0.77*b(99,48)-b(100,48)   &
                -b(119,48)-b(123,48)
      jvs(423) = -b(20,49)+b(21,49)+0.12*b(81,49)+0.33*b(83,49)+0.6   &
                *b(84,49)+0.08*b(97,49)+0.27*b(120,49)+0.27*b(124,49)
      jvs(424) = b(21,50)-b(29,50)+b(33,50)+0.7*b(41,50)
      jvs(425) = -b(23,51)+b(33,51)
      jvs(426) = -b(24,52)
      jvs(427) = -b(25,53)+0.7*b(41,53)
      jvs(428) = -b(81,20)
      jvs(429) = b(13,22)-b(14,22)
      jvs(430) = -b(97,27)
      jvs(431) = -b(83,31)
      jvs(432) = -b(120,32)
      jvs(433) = -b(84,36)
      jvs(434) = -b(124,42)
      jvs(435) = 0.4*b(73,47)
      jvs(436) = -b(20,48)
      jvs(437) = -b(7,49)-b(8,49)-b(14,49)-b(18,49)-b(19,49)-b(20,49)   &
                -b(21,49)-b(81,49)-b(83,49)-b(84,49)-b(97,49)   &
                -b(120,49)-b(124,49)
      jvs(438) = -b(21,50)+0.4*b(73,50)
      jvs(439) = -b(18,51)
      jvs(440) = -b(19,52)
      jvs(441) = 0
      jvs(442) = b(45,5)
      jvs(443) = b(65,6)
      jvs(444) = b(30,7)
      jvs(445) = 0.2*b(89,10)
      jvs(446) = 0.55*b(90,12)
      jvs(447) = b(5,14)+b(36,14)
      jvs(448) = 0.95*b(91,15)
      jvs(449) = b(53,17)
      jvs(450) = b(54,18)
      jvs(451) = 0.22*b(81,20)+b(82,20)
      jvs(452) = b(48,21)
      jvs(453) = 0.6*b(92,23)
      jvs(454) = b(44,25)
      jvs(455) = 2*b(95,27)+b(96,27)+0.76*b(97,27)
      jvs(456) = 0.8*b(127,28)-b(130,28)
      jvs(457) = 0.91*b(126,29)-b(129,29)
      jvs(458) = b(128,30)-b(131,30)
      jvs(459) = 0.26*b(83,31)+b(85,31)
      jvs(460) = 0.07*b(120,32)
      jvs(461) = 2*b(49,33)+b(51,33)+b(52,33)
      jvs(462) = -b(113,34)
      jvs(463) = 0.22*b(84,36)+b(86,36)
      jvs(464) = 0.5*b(104,37)+0.5*b(108,37)-b(112,37)
      jvs(465) = b(78,38)
      jvs(466) = b(58,39)+b(60,39)-b(62,39)+0.6*b(64,39)
      jvs(467) = b(66,40)
      jvs(468) = b(57,41)+b(59,41)-b(61,41)+0.32*b(63,41)
      jvs(469) = 0.33*b(122,42)+0.1*b(124,42)+0.93*b(125,42)
      jvs(470) = -b(111,43)
      jvs(471) = 0.9*b(98,44)
      jvs(472) = 0.76*b(102,45)+0.9*b(106,45)-b(110,45)+0.54*b(114,45)
      jvs(473) = 0.9*b(101,46)
      jvs(474) = -b(73,47)
      jvs(475) = b(20,48)+b(22,48)+b(25,48)-b(29,48)+b(30,48)+b(44,48)   &
                +b(45,48)+b(48,48)+b(51,48)+b(65,48)+b(82,48)+b(85,48)   &
                +b(86,48)+0.2*b(89,48)+0.55*b(90,48)+0.6*b(92,48)+2   &
                *b(95,48)
      jvs(476) = b(20,49)-b(21,49)+0.22*b(81,49)+0.26*b(83,49)+0.22   &
                *b(84,49)+0.76*b(97,49)+0.07*b(120,49)+0.1*b(124,49)
      jvs(477) = -b(21,50)-b(29,50)-2*b(31,50)-2*b(32,50)-b(33,50)   &
                -b(34,50)-b(35,50)-b(41,50)-b(61,50)-b(62,50)-b(73,50)   &
                -b(110,50)-b(111,50)-b(112,50)-b(113,50)-b(129,50)   &
                -b(130,50)-b(131,50)
      jvs(478) = -b(33,51)+b(57,51)+b(58,51)+0.95*b(91,51)+0.76   &
                *b(102,51)+0.5*b(104,51)+0.91*b(126,51)+0.8*b(127,51)   &
                +b(128,51)
      jvs(479) = -b(34,52)-b(35,52)
      jvs(480) = b(25,53)-b(41,53)+b(52,53)+b(59,53)+b(60,53)+0.9   &
                *b(106,53)+0.5*b(108,53)+0.93*b(125,53)
      jvs(481) = -b(91,15)
      jvs(482) = b(3,19)
      jvs(483) = b(15,22)-b(17,22)
      jvs(484) = -b(127,28)
      jvs(485) = -b(126,29)
      jvs(486) = -b(128,30)
      jvs(487) = 0
      jvs(488) = -b(105,34)
      jvs(489) = 0
      jvs(490) = -b(104,37)
      jvs(491) = 0
      jvs(492) = -b(58,39)
      jvs(493) = -b(57,41)
      jvs(494) = 0
      jvs(495) = -b(103,43)
      jvs(496) = 0
      jvs(497) = -b(102,45)
      jvs(498) = 0
      jvs(499) = -b(71,47)
      jvs(500) = -b(23,48)
      jvs(501) = -b(18,49)
      jvs(502) = -b(33,50)
      jvs(503) = -b(17,51)-b(18,51)-b(23,51)-b(33,51)-b(37,51)   &
                -b(57,51)-b(58,51)-b(71,51)-b(91,51)-b(102,51)   &
                -b(103,51)-b(104,51)-b(105,51)-b(126,51)-b(127,51)   &
                -b(128,51)
      jvs(504) = b(1,52)+b(15,52)+b(38,52)
      jvs(505) = 0.11*b(2,53)-b(37,53)+b(38,53)
      jvs(506) = b(70,9)
      jvs(507) = b(6,11)+b(43,11)
      jvs(508) = -b(94,13)
      jvs(509) = b(5,14)+b(28,14)+b(36,14)
      jvs(510) = 0.95*b(91,15)
      jvs(511) = b(26,19)
      jvs(512) = -b(15,22)-b(16,22)+b(17,22)
      jvs(513) = 0
      jvs(514) = b(4,24)
      jvs(515) = 1.2*b(127,28)
      jvs(516) = 0.91*b(126,29)
      jvs(517) = b(128,30)
      jvs(518) = 0
      jvs(519) = 0
      jvs(520) = b(105,34)+b(109,34)
      jvs(521) = 0
      jvs(522) = 1.5*b(104,37)+1.5*b(108,37)+0.5*b(116,37)
      jvs(523) = 0
      jvs(524) = b(58,39)+b(60,39)
      jvs(525) = 0
      jvs(526) = b(57,41)+b(59,41)
      jvs(527) = 0
      jvs(528) = b(103,43)+b(107,43)
      jvs(529) = 0
      jvs(530) = 0.84*b(102,45)+b(106,45)
      jvs(531) = b(101,46)
      jvs(532) = -b(69,47)+b(71,47)+b(72,47)
      jvs(533) = -b(24,48)+b(25,48)+b(26,48)+b(28,48)
      jvs(534) = b(18,49)-b(19,49)
      jvs(535) = b(33,50)-b(34,50)-b(35,50)+0.7*b(41,50)
      jvs(536) = b(17,51)+b(18,51)+b(33,51)+2*b(37,51)+b(57,51)   &
                +b(58,51)+b(71,51)+0.95*b(91,51)+0.84*b(102,51)   &
                +b(103,51)+1.5*b(104,51)+b(105,51)+0.91*b(126,51)+1.2   &
                *b(127,51)+b(128,51)
      jvs(537) = -b(1,52)-b(15,52)-b(16,52)-b(19,52)-b(24,52)-b(34,52)   &
                -b(35,52)-b(39,52)-b(69,52)-b(94,52)
      jvs(538) = 0.89*b(2,53)+b(25,53)+2*b(37,53)-b(39,53)+2*b(40,53)   &
                +0.7*b(41,53)+b(59,53)+b(60,53)+b(72,53)+b(106,53)   &
                +b(107,53)+1.5*b(108,53)+b(109,53)
      jvs(539) = b(6,11)+b(43,11)
      jvs(540) = b(16,22)
      jvs(541) = -b(93,23)
      jvs(542) = b(27,24)
      jvs(543) = -b(87,31)
      jvs(544) = -b(121,32)
      jvs(545) = -b(52,33)
      jvs(546) = -b(109,34)
      jvs(547) = -b(88,36)
      jvs(548) = -b(108,37)
      jvs(549) = -b(80,38)
      jvs(550) = -b(60,39)
      jvs(551) = -b(68,40)
      jvs(552) = -b(59,41)
      jvs(553) = -b(125,42)
      jvs(554) = -b(107,43)
      jvs(555) = 0
      jvs(556) = -b(106,45)
      jvs(557) = 0
      jvs(558) = -b(72,47)
      jvs(559) = -b(25,48)+b(27,48)
      jvs(560) = b(19,49)
      jvs(561) = -b(41,50)
      jvs(562) = -b(37,51)
      jvs(563) = b(16,52)+b(19,52)-b(38,52)-b(39,52)
      jvs(564) = -b(2,53)-b(25,53)-b(37,53)-b(38,53)-b(39,53)-2   &
                *b(40,53)-b(41,53)-b(52,53)-b(59,53)-b(60,53)-b(68,53)   &
                -b(72,53)-b(80,53)-b(87,53)-b(88,53)-b(93,53)   &
                -b(106,53)-b(107,53)-b(108,53)-b(109,53)-b(121,53)   &
                -b(125,53)
      return
      end subroutine cbmz_v02r03_jacob                                    


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r03_decomp( n, v, ier,   &
          lu_crow_v, lu_diag_v, lu_icol_v )
!
!   computes l-u-decomposition of sparse jacobian
!   for mechanism-version-regime = cbmz_v02r03
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     n = number of variable species [input]
      integer n
!     ier = status flag [output]
!         0 = success    other = failure [output]
      integer ier

!     v = non-zero elements of the sparse jacobian [input]
      real v(lu_nonzero_v_r03_kpp)

      integer lu_crow_v(nvar_r03_kpp + 1)
      integer lu_diag_v(nvar_r03_kpp + 1)
      integer lu_icol_v(lu_nonzero_v_r03_kpp)

!   local variables
      integer k, kk, j, jj
      real a, w(nvar_r03_kpp + 1)

      ier = 0
      do k=1,n
        if ( v( lu_diag_v(k) ) .eq. 0. ) then
            ier = k
            return
        end if
        do kk = lu_crow_v(k), lu_crow_v(k+1)-1
              w( lu_icol_v(kk) ) = v(kk)
        end do
        do kk = lu_crow_v(k), lu_diag_v(k)-1
            j = lu_icol_v(kk)
            a = -w(j) / v( lu_diag_v(j) )
            w(j) = -a
            do jj = lu_diag_v(j)+1, lu_crow_v(j+1)-1
               w( lu_icol_v(jj) ) = w( lu_icol_v(jj) ) + a*v(jj)
            end do
         end do
         do kk = lu_crow_v(k), lu_crow_v(k+1)-1
            v(kk) = w( lu_icol_v(kk) )
         end do
      end do
      return
      end subroutine cbmz_v02r03_decomp            


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r03_solve( jvs, x )
!
!   does back-solve for mechanism-version-regime = cbmz_v02r03
!
      implicit none

!   subr parameters
!     jvs = the non-zero elements of the l-u-decomposition
!           of the augmented jacobian [input]
      real jvs(*)
!     x = the right-hand side of the linear equation set being solved [on input]
!     x = concentrations of variable species [on output]
      real x(*)


      x(15) = x(15)-jvs(47)*x(10)-jvs(48)*x(12)
      x(22) = x(22)-jvs(86)*x(4)
      x(23) = x(23)-jvs(92)*x(10)-jvs(93)*x(12)
      x(24) = x(24)-jvs(97)*x(11)-jvs(98)*x(23)
      x(25) = x(25)-jvs(108)*x(20)
      x(26) = x(26)-jvs(123)*x(12)-jvs(124)*x(16)
      x(27) = x(27)-jvs(140)*x(15)-jvs(141)*x(23)
      x(33) = x(33)-jvs(170)*x(17)-jvs(171)*x(20)-jvs(172)*x(21)   &
             -jvs(173)*x(27)-jvs(174)*x(29)-jvs(175)*x(30)-jvs(176)   &
             *x(31)-jvs(177)*x(32)
      x(34) = x(34)-jvs(189)*x(10)-jvs(190)*x(12)-jvs(191)*x(20)   &
             -jvs(192)*x(23)-jvs(193)*x(27)-jvs(194)*x(31)-jvs(195)   &
             *x(32)
      x(35) = x(35)-jvs(208)*x(30)
      x(37) = x(37)-jvs(225)*x(31)-jvs(226)*x(36)
      x(38) = x(38)-jvs(234)*x(12)-jvs(235)*x(27)-jvs(236)*x(30)   &
             -jvs(237)*x(31)-jvs(238)*x(36)
      x(39) = x(39)-jvs(248)*x(8)-jvs(249)*x(18)-jvs(250)*x(31)   &
             -jvs(251)*x(36)
      x(40) = x(40)-jvs(261)*x(6)-jvs(262)*x(18)-jvs(263)*x(20)   &
             -jvs(264)*x(27)-jvs(265)*x(28)-jvs(266)*x(30)-jvs(267)   &
             *x(31)-jvs(268)*x(32)-jvs(269)*x(36)-jvs(270)*x(37)   &
             -jvs(271)*x(39)
      x(41) = x(41)-jvs(282)*x(17)-jvs(283)*x(31)-jvs(284)*x(35)   &
             -jvs(285)*x(36)-jvs(286)*x(40)
      x(42) = x(42)-jvs(299)*x(28)-jvs(300)*x(29)-jvs(301)*x(32)
      x(43) = x(43)-jvs(308)*x(35)-jvs(309)*x(36)-jvs(310)*x(42)
      x(44) = x(44)-jvs(320)*x(29)-jvs(321)*x(30)-jvs(322)*x(32)   &
             -jvs(323)*x(42)-jvs(324)*x(43)
      x(45) = x(45)-jvs(333)*x(26)-jvs(334)*x(28)-jvs(335)*x(29)   &
             -jvs(336)*x(31)-jvs(337)*x(32)-jvs(338)*x(36)-jvs(339)   &
             *x(37)-jvs(340)*x(42)-jvs(341)*x(44)
      x(46) = x(46)-jvs(349)*x(13)-jvs(350)*x(15)-jvs(351)*x(23)   &
             -jvs(352)*x(28)-jvs(353)*x(29)-jvs(354)*x(32)-jvs(355)   &
             *x(37)-jvs(356)*x(42)-jvs(357)*x(45)
      x(47) = x(47)-jvs(365)*x(9)-jvs(366)*x(27)-jvs(367)*x(31)   &
             -jvs(368)*x(32)-jvs(369)*x(35)-jvs(370)*x(36)-jvs(371)   &
             *x(38)-jvs(372)*x(40)-jvs(373)*x(42)-jvs(374)*x(43)   &
             -jvs(375)*x(44)-jvs(376)*x(45)-jvs(377)*x(46)
      x(48) = x(48)-jvs(385)*x(4)-jvs(386)*x(5)-jvs(387)*x(6)-jvs(388)   &
             *x(7)-jvs(389)*x(8)-jvs(390)*x(10)-jvs(391)*x(12)   &
             -jvs(392)*x(14)-jvs(393)*x(17)-jvs(394)*x(18)-jvs(395)   &
             *x(19)-jvs(396)*x(20)-jvs(397)*x(21)-jvs(398)*x(23)   &
             -jvs(399)*x(24)-jvs(400)*x(25)-jvs(401)*x(26)-jvs(402)   &
             *x(27)-jvs(403)*x(28)-jvs(404)*x(29)-jvs(405)*x(30)   &
             -jvs(406)*x(31)-jvs(407)*x(32)-jvs(408)*x(33)-jvs(409)   &
             *x(35)-jvs(410)*x(36)-jvs(411)*x(37)-jvs(412)*x(38)   &
             -jvs(413)*x(39)-jvs(414)*x(40)-jvs(415)*x(41)-jvs(416)   &
             *x(42)-jvs(417)*x(43)-jvs(418)*x(44)-jvs(419)*x(45)   &
             -jvs(420)*x(46)-jvs(421)*x(47)
      x(49) = x(49)-jvs(428)*x(20)-jvs(429)*x(22)-jvs(430)*x(27)   &
             -jvs(431)*x(31)-jvs(432)*x(32)-jvs(433)*x(36)-jvs(434)   &
             *x(42)-jvs(435)*x(47)-jvs(436)*x(48)
      x(50) = x(50)-jvs(442)*x(5)-jvs(443)*x(6)-jvs(444)*x(7)-jvs(445)   &
             *x(10)-jvs(446)*x(12)-jvs(447)*x(14)-jvs(448)*x(15)   &
             -jvs(449)*x(17)-jvs(450)*x(18)-jvs(451)*x(20)-jvs(452)   &
             *x(21)-jvs(453)*x(23)-jvs(454)*x(25)-jvs(455)*x(27)   &
             -jvs(456)*x(28)-jvs(457)*x(29)-jvs(458)*x(30)-jvs(459)   &
             *x(31)-jvs(460)*x(32)-jvs(461)*x(33)-jvs(462)*x(34)   &
             -jvs(463)*x(36)-jvs(464)*x(37)-jvs(465)*x(38)-jvs(466)   &
             *x(39)-jvs(467)*x(40)-jvs(468)*x(41)-jvs(469)*x(42)   &
             -jvs(470)*x(43)-jvs(471)*x(44)-jvs(472)*x(45)-jvs(473)   &
             *x(46)-jvs(474)*x(47)-jvs(475)*x(48)-jvs(476)*x(49)
      x(51) = x(51)-jvs(481)*x(15)-jvs(482)*x(19)-jvs(483)*x(22)   &
             -jvs(484)*x(28)-jvs(485)*x(29)-jvs(486)*x(30)-jvs(487)   &
             *x(32)-jvs(488)*x(34)-jvs(489)*x(36)-jvs(490)*x(37)   &
             -jvs(491)*x(38)-jvs(492)*x(39)-jvs(493)*x(41)-jvs(494)   &
             *x(42)-jvs(495)*x(43)-jvs(496)*x(44)-jvs(497)*x(45)   &
             -jvs(498)*x(46)-jvs(499)*x(47)-jvs(500)*x(48)-jvs(501)   &
             *x(49)-jvs(502)*x(50)
      x(52) = x(52)-jvs(506)*x(9)-jvs(507)*x(11)-jvs(508)*x(13)   &
             -jvs(509)*x(14)-jvs(510)*x(15)-jvs(511)*x(19)-jvs(512)   &
             *x(22)-jvs(513)*x(23)-jvs(514)*x(24)-jvs(515)*x(28)   &
             -jvs(516)*x(29)-jvs(517)*x(30)-jvs(518)*x(32)-jvs(519)   &
             *x(33)-jvs(520)*x(34)-jvs(521)*x(36)-jvs(522)*x(37)   &
             -jvs(523)*x(38)-jvs(524)*x(39)-jvs(525)*x(40)-jvs(526)   &
             *x(41)-jvs(527)*x(42)-jvs(528)*x(43)-jvs(529)*x(44)   &
             -jvs(530)*x(45)-jvs(531)*x(46)-jvs(532)*x(47)-jvs(533)   &
             *x(48)-jvs(534)*x(49)-jvs(535)*x(50)-jvs(536)*x(51)
      x(53) = x(53)-jvs(539)*x(11)-jvs(540)*x(22)-jvs(541)*x(23)   &
             -jvs(542)*x(24)-jvs(543)*x(31)-jvs(544)*x(32)-jvs(545)   &
             *x(33)-jvs(546)*x(34)-jvs(547)*x(36)-jvs(548)*x(37)   &
             -jvs(549)*x(38)-jvs(550)*x(39)-jvs(551)*x(40)-jvs(552)   &
             *x(41)-jvs(553)*x(42)-jvs(554)*x(43)-jvs(555)*x(44)   &
             -jvs(556)*x(45)-jvs(557)*x(46)-jvs(558)*x(47)-jvs(559)   &
             *x(48)-jvs(560)*x(49)-jvs(561)*x(50)-jvs(562)*x(51)   &
             -jvs(563)*x(52)
      x(53) = x(53)/jvs(564)
      x(52) = (x(52)-jvs(538)*x(53))/(jvs(537))
      x(51) = (x(51)-jvs(504)*x(52)-jvs(505)*x(53))/(jvs(503))
      x(50) = (x(50)-jvs(478)*x(51)-jvs(479)*x(52)-jvs(480)*x(53))/   &
             (jvs(477))
      x(49) = (x(49)-jvs(438)*x(50)-jvs(439)*x(51)-jvs(440)*x(52)   &
             -jvs(441)*x(53))/(jvs(437))
      x(48) = (x(48)-jvs(423)*x(49)-jvs(424)*x(50)-jvs(425)*x(51)   &
             -jvs(426)*x(52)-jvs(427)*x(53))/(jvs(422))
      x(47) = (x(47)-jvs(379)*x(48)-jvs(380)*x(49)-jvs(381)*x(50)   &
             -jvs(382)*x(51)-jvs(383)*x(52)-jvs(384)*x(53))/(jvs(378))
      x(46) = (x(46)-jvs(359)*x(48)-jvs(360)*x(49)-jvs(361)*x(50)   &
             -jvs(362)*x(51)-jvs(363)*x(52)-jvs(364)*x(53))/(jvs(358))
      x(45) = (x(45)-jvs(343)*x(46)-jvs(344)*x(48)-jvs(345)*x(49)   &
             -jvs(346)*x(50)-jvs(347)*x(51)-jvs(348)*x(53))/(jvs(342))
      x(44) = (x(44)-jvs(326)*x(45)-jvs(327)*x(46)-jvs(328)*x(48)   &
             -jvs(329)*x(49)-jvs(330)*x(50)-jvs(331)*x(51)-jvs(332)   &
             *x(53))/(jvs(325))
      x(43) = (x(43)-jvs(312)*x(44)-jvs(313)*x(45)-jvs(314)*x(46)   &
             -jvs(315)*x(48)-jvs(316)*x(49)-jvs(317)*x(50)-jvs(318)   &
             *x(51)-jvs(319)*x(53))/(jvs(311))
      x(42) = (x(42)-jvs(303)*x(48)-jvs(304)*x(49)-jvs(305)*x(50)   &
             -jvs(306)*x(51)-jvs(307)*x(53))/(jvs(302))
      x(41) = (x(41)-jvs(288)*x(42)-jvs(289)*x(43)-jvs(290)*x(44)   &
             -jvs(291)*x(45)-jvs(292)*x(46)-jvs(293)*x(47)-jvs(294)   &
             *x(48)-jvs(295)*x(49)-jvs(296)*x(50)-jvs(297)*x(51)   &
             -jvs(298)*x(53))/(jvs(287))
      x(40) = (x(40)-jvs(273)*x(42)-jvs(274)*x(44)-jvs(275)*x(45)   &
             -jvs(276)*x(46)-jvs(277)*x(48)-jvs(278)*x(49)-jvs(279)   &
             *x(50)-jvs(280)*x(51)-jvs(281)*x(53))/(jvs(272))
      x(39) = (x(39)-jvs(253)*x(44)-jvs(254)*x(45)-jvs(255)*x(46)   &
             -jvs(256)*x(48)-jvs(257)*x(49)-jvs(258)*x(50)-jvs(259)   &
             *x(51)-jvs(260)*x(53))/(jvs(252))
      x(38) = (x(38)-jvs(240)*x(42)-jvs(241)*x(43)-jvs(242)*x(44)   &
             -jvs(243)*x(48)-jvs(244)*x(49)-jvs(245)*x(50)-jvs(246)   &
             *x(51)-jvs(247)*x(53))/(jvs(239))
      x(37) = (x(37)-jvs(228)*x(46)-jvs(229)*x(48)-jvs(230)*x(49)   &
             -jvs(231)*x(50)-jvs(232)*x(51)-jvs(233)*x(53))/(jvs(227))
      x(36) = (x(36)-jvs(222)*x(48)-jvs(223)*x(49)-jvs(224)*x(53))/   &
             (jvs(221))
      x(35) = (x(35)-jvs(210)*x(36)-jvs(211)*x(42)-jvs(212)*x(43)   &
             -jvs(213)*x(44)-jvs(214)*x(45)-jvs(215)*x(46)-jvs(216)   &
             *x(48)-jvs(217)*x(49)-jvs(218)*x(50)-jvs(219)*x(51)   &
             -jvs(220)*x(53))/(jvs(209))
      x(34) = (x(34)-jvs(197)*x(36)-jvs(198)*x(38)-jvs(199)*x(42)   &
             -jvs(200)*x(44)-jvs(201)*x(45)-jvs(202)*x(46)-jvs(203)   &
             *x(48)-jvs(204)*x(49)-jvs(205)*x(50)-jvs(206)*x(51)   &
             -jvs(207)*x(53))/(jvs(196))
      x(33) = (x(33)-jvs(179)*x(36)-jvs(180)*x(37)-jvs(181)*x(41)   &
             -jvs(182)*x(42)-jvs(183)*x(43)-jvs(184)*x(48)-jvs(185)   &
             *x(49)-jvs(186)*x(50)-jvs(187)*x(51)-jvs(188)*x(53))/   &
             (jvs(178))
      x(32) = (x(32)-jvs(167)*x(48)-jvs(168)*x(49)-jvs(169)*x(53))/   &
             (jvs(166))
      x(31) = (x(31)-jvs(163)*x(48)-jvs(164)*x(49)-jvs(165)*x(53))/   &
             (jvs(162))
      x(30) = (x(30)-jvs(158)*x(42)-jvs(159)*x(48)-jvs(160)*x(50)   &
             -jvs(161)*x(51))/(jvs(157))
      x(29) = (x(29)-jvs(153)*x(32)-jvs(154)*x(48)-jvs(155)*x(50)   &
             -jvs(156)*x(51))/(jvs(152))
      x(28) = (x(28)-jvs(148)*x(32)-jvs(149)*x(50)-jvs(150)*x(51)   &
             -jvs(151)*x(53))/(jvs(147))
      x(27) = (x(27)-jvs(143)*x(48)-jvs(144)*x(49)-jvs(145)*x(51)   &
             -jvs(146)*x(53))/(jvs(142))
      x(26) = (x(26)-jvs(126)*x(28)-jvs(127)*x(29)-jvs(128)*x(31)   &
             -jvs(129)*x(36)-jvs(130)*x(37)-jvs(131)*x(42)-jvs(132)   &
             *x(44)-jvs(133)*x(45)-jvs(134)*x(46)-jvs(135)*x(48)   &
             -jvs(136)*x(49)-jvs(137)*x(50)-jvs(138)*x(51)-jvs(139)   &
             *x(53))/(jvs(125))
      x(25) = (x(25)-jvs(110)*x(27)-jvs(111)*x(30)-jvs(112)*x(31)   &
             -jvs(113)*x(32)-jvs(114)*x(33)-jvs(115)*x(36)-jvs(116)   &
             *x(38)-jvs(117)*x(40)-jvs(118)*x(42)-jvs(119)*x(48)   &
             -jvs(120)*x(49)-jvs(121)*x(51)-jvs(122)*x(53))/(jvs(109))
      x(24) = (x(24)-jvs(100)*x(33)-jvs(101)*x(38)-jvs(102)*x(40)   &
             -jvs(103)*x(42)-jvs(104)*x(48)-jvs(105)*x(50)-jvs(106)   &
             *x(52)-jvs(107)*x(53))/(jvs(99))
      x(23) = (x(23)-jvs(95)*x(48)-jvs(96)*x(53))/(jvs(94))
      x(22) = (x(22)-jvs(88)*x(49)-jvs(89)*x(51)-jvs(90)*x(52)-jvs(91)   &
             *x(53))/(jvs(87))
      x(21) = (x(21)-jvs(81)*x(31)-jvs(82)*x(36)-jvs(83)*x(41)-jvs(84)   &
             *x(48)-jvs(85)*x(49))/(jvs(80))
      x(20) = (x(20)-jvs(78)*x(48)-jvs(79)*x(49))/(jvs(77))
      x(19) = (x(19)-jvs(73)*x(48)-jvs(74)*x(50)-jvs(75)*x(51)-jvs(76)   &
             *x(52))/(jvs(72))
      x(18) = (x(18)-jvs(69)*x(39)-jvs(70)*x(48)-jvs(71)*x(50))/   &
             (jvs(68))
      x(17) = (x(17)-jvs(65)*x(41)-jvs(66)*x(48)-jvs(67)*x(50))/   &
             (jvs(64))
      x(16) = (x(16)-jvs(53)*x(26)-jvs(54)*x(31)-jvs(55)*x(36)-jvs(56)   &
             *x(37)-jvs(57)*x(44)-jvs(58)*x(45)-jvs(59)*x(46)-jvs(60)   &
             *x(48)-jvs(61)*x(49)-jvs(62)*x(51)-jvs(63)*x(53))/   &
             (jvs(52))
      x(15) = (x(15)-jvs(50)*x(48)-jvs(51)*x(51))/(jvs(49))
      x(14) = (x(14)-jvs(44)*x(48)-jvs(45)*x(50)-jvs(46)*x(52))/   &
             (jvs(43))
      x(13) = (x(13)-jvs(39)*x(23)-jvs(40)*x(48)-jvs(41)*x(52)-jvs(42)   &
             *x(53))/(jvs(38))
      x(12) = (x(12)-jvs(37)*x(48))/(jvs(36))
      x(11) = (x(11)-jvs(34)*x(52)-jvs(35)*x(53))/(jvs(33))
      x(10) = (x(10)-jvs(32)*x(48))/(jvs(31))
      x(9) = (x(9)-jvs(29)*x(47)-jvs(30)*x(52))/(jvs(28))
      x(8) = (x(8)-jvs(26)*x(39)-jvs(27)*x(48))/(jvs(25))
      x(7) = (x(7)-jvs(23)*x(48)-jvs(24)*x(50))/(jvs(22))
      x(6) = (x(6)-jvs(21)*x(48))/(jvs(20))
      x(5) = (x(5)-jvs(19)*x(48))/(jvs(18))
      x(4) = (x(4)-jvs(17)*x(49))/(jvs(16))
      x(3) = (x(3)-jvs(11)*x(31)-jvs(12)*x(36)-jvs(13)*x(47)-jvs(14)   &
            *x(49)-jvs(15)*x(50))/(jvs(10))
      x(2) = (x(2)-jvs(5)*x(20)-jvs(6)*x(31)-jvs(7)*x(32)-jvs(8)*x(42)   &
            -jvs(9)*x(49))/(jvs(4))
      x(1) = (x(1)-jvs(2)*x(5)-jvs(3)*x(48))/(jvs(1))
      return
      end subroutine cbmz_v02r03_solve          


!   cbmz_v02r04_torodas.f - created on 18-nov-2003 from previous
!        cbmz_v02r04_torodas.f        cbmz_v02r04_mapconcs.f
!        cbmz_v02r04_maprates.f       cbmz_v02r04_dydt.f
!        cbmz_v02r04_jacob.f          cbmz_v02r04_decomp.f
!        cbmz_v02r04_solve.f
!   so now everything is in a single file
!-----------------------------------------------------------------------

      subroutine cbmz_v02r04_torodas(   &
          ngas, taa, tzz,   &
          stot, atol, rtol, yposlimit, yneglimit,   &
          sfixedkpp, rconstkpp,   &
          hmin, hstart,   &
          info_rodas, iok, lunerr, idydt_sngldble )
!
!   interfaces to rodas3 solver formechanism-version-regime =cbmz_v02r04
!
!   *** do not include any pegasus common blocks ***
!
      use module_data_cbmz
      use module_cbmz_rodas3_solver, only:  rodas3_ff_x2
      implicit none

!   subr parameters
      integer ngas, iok, lunerr, idydt_sngldble
      integer info_rodas(6)
      real taa, tzz, hmin, hstart
      real stot(ngas), atol(ngas), rtol(ngas)
      real yposlimit(ngas), yneglimit(ngas)
      real sfixedkpp(nfixed_kppmax), rconstkpp(nreact_kppmax)

!   local variables

!     external cbmz_v02r04_dydt
!     external cbmz_v02r04_jacob
!     external cbmz_v02r04_decomp
!     external cbmz_v02r04_solve

      integer i

      real hmax

      integer lu_crow_v(nvar_r04_kpp + 1)
      save    lu_crow_v
      integer lu_diag_v(nvar_r04_kpp + 1)
      save    lu_diag_v
      integer lu_icol_v(lu_nonzero_v_r04_kpp)
      save    lu_icol_v

      data( lu_icol_v(i), i = 1, 252 ) /   &
        1,  7, 33, 39,  2, 25, 34,  3, 28, 29, 32, 33,   &
       34, 37, 38, 39,  4, 27,  5, 31,  6, 39,  7, 32,   &
       39,  8, 21, 39,  9, 25, 37, 10, 16, 39, 11, 29,   &
       34, 39, 12, 35, 39, 13, 36, 37, 14, 34, 37, 39,   &
       15, 22, 28, 33, 36, 39, 16, 26, 39, 17, 21, 34,   &
       39, 18, 33, 34, 37, 38, 39, 19, 29, 34, 35, 39,   &
       20, 32, 35, 38,  8, 17, 21, 34, 36, 38, 39,  6,   &
       17, 21, 22, 34, 36, 38, 39, 10, 16, 23, 26, 35,   &
       38, 39, 13, 22, 24, 26, 28, 29, 33, 34, 36, 37,   &
       38, 39,  9, 22, 25, 34, 36, 37, 38, 39, 26, 30,   &
       36, 39, 26, 27, 30, 32, 35, 36, 38, 39, 12, 19,   &
       20, 23, 26, 27, 28, 29, 30, 32, 33, 34, 35, 36,   &
       38, 39, 16, 26, 29, 30, 33, 34, 35, 36, 39,  5,   &
       26, 30, 31, 36, 37, 38, 39, 25, 30, 31, 32, 34,   &
       36, 37, 38, 39, 20, 23, 26, 27, 29, 30, 31, 32,   &
       33, 34, 35, 36, 37, 38, 39, 20, 27, 28, 29, 30,   &
       31, 32, 33, 34, 35, 36, 37, 38, 39,  6,  7, 11,   &
       12, 14, 15, 16, 17, 19, 20, 21, 22, 23, 25, 26,   &
       27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,   &
       39, 16, 19, 20, 22, 23, 25, 26, 27, 29, 30, 31,   &
       32, 33, 34, 35, 36, 37, 38, 39, 13, 21, 22, 24 /

      data( lu_icol_v(i), i = 253, 334 ) /   &
       25, 26, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37,   &
       38, 39,  9, 13, 14, 18, 20, 21, 23, 24, 25, 26,   &
       27, 28, 29, 30, 31, 32, 33, 34, 35, 37, 38, 39,   &
       18, 20, 21, 23, 25, 26, 27, 30, 31, 32, 33, 34,   &
       35, 36, 37, 38, 39,  5,  6,  7,  8, 10, 11, 12,   &
       14, 15, 16, 17, 18, 19, 21, 22, 24, 26, 28, 29,   &
       30, 31, 32, 33, 34, 35, 36, 37, 38, 39 /

      data lu_crow_v /   &
        1,  5,  8, 17, 19, 21, 23, 26, 29, 32, 35, 39,   &
       42, 45, 49, 55, 58, 62, 68, 73, 77, 84, 92, 99,   &
      111,119,123,131,147,156,164,173,188,202,230,249,   &
      267,289,306,335 /

      data lu_diag_v /   &
        1,  5,  8, 17, 19, 21, 23, 26, 29, 32, 35, 39,   &
       42, 45, 49, 55, 58, 62, 68, 73, 79, 87, 94,101,   &
      113,119,124,137,149,158,166,180,195,224,244,263,   &
      286,304,334,335 /



      info_rodas(1) = 1
      do i = 2, 6
          info_rodas(i) = 0
      end do
      hmax = tzz - taa

!   do not integrate if hmax is less/equal to hmin
!   because hmin is generally 0.1 s or less
      if (hmax .le. 1.001*hmin) then
          iok = 11
          return
      end if

      call rodas3_ff_x2(   &
           nvar_r04_kpp, taa, tzz, hmin, hmax, hstart,   &
           stot, atol, rtol, yposlimit, yneglimit,   &
           sfixedkpp, rconstkpp,   &
           lu_nonzero_v_r04_kpp, lu_crow_v, lu_diag_v, lu_icol_v,   &
           info_rodas, iok, lunerr,   &
           cbmz_v02r04_dydt,   &
           cbmz_v02r04_jacob,   &
           cbmz_v02r04_decomp,   &
           cbmz_v02r04_solve )

      return
      end subroutine cbmz_v02r04_torodas 


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r04_mapconcs( imap, nyy, yy, yyfixed, cbox )
!
!   maps species concentrations (gaschemistry cbox array <--> kpp yy array)
!   for mechanism-version-regime = cbmz_v02r04
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     imap = mapping direction flag [input]
!         0 = map cbox --> yy and yyfixed
!         1 = map yy --> cbox
      integer imap
!     nyy = number of kpp "variable" species [output]
      integer nyy
!     yy = kpp species concentrations array [input/output]
      real yy(nvar_r04_kpp)
!     yyfixed = kpp species concentrations array [input/output]
      real yyfixed(nfixed_kppmax)
!     cbox = main gaschemistry species conc array [input/output]
      real cbox(ngas_z)

!   local variables
      integer ih2so4_kpp
      parameter ( ih2so4_kpp = 1 )
      integer ircooh_kpp
      parameter ( ircooh_kpp = 2 )
      integer imsa_kpp
      parameter ( imsa_kpp = 3 )
      integer imtf_kpp
      parameter ( imtf_kpp = 4 )
      integer io1d_kpp
      parameter ( io1d_kpp = 5 )
      integer ic2h5oh_kpp
      parameter ( ic2h5oh_kpp = 6 )
      integer iso2_kpp
      parameter ( iso2_kpp = 7 )
      integer ic2h6_kpp
      parameter ( ic2h6_kpp = 8 )
      integer ipan_kpp
      parameter ( ipan_kpp = 9 )
      integer idmso2_kpp
      parameter ( idmso2_kpp = 10 )
      integer ih2o2_kpp
      parameter ( ih2o2_kpp = 11 )
      integer ich3oh_kpp
      parameter ( ich3oh_kpp = 12 )
      integer in2o5_kpp
      parameter ( in2o5_kpp = 13 )
      integer ihno4_kpp
      parameter ( ihno4_kpp = 14 )
      integer ico_kpp
      parameter ( ico_kpp = 15 )
      integer idmso_kpp
      parameter ( idmso_kpp = 16 )
      integer iethooh_kpp
      parameter ( iethooh_kpp = 17 )
      integer ihono_kpp
      parameter ( ihono_kpp = 18 )
      integer ich3ooh_kpp
      parameter ( ich3ooh_kpp = 19 )
      integer ich3so2oo_kpp
      parameter ( ich3so2oo_kpp = 20 )
      integer iethp_kpp
      parameter ( iethp_kpp = 21 )
      integer iald2_kpp
      parameter ( iald2_kpp = 22 )
      integer ich3so2ch2oo_kpp
      parameter ( ich3so2ch2oo_kpp = 23 )
      integer ihno3_kpp
      parameter ( ihno3_kpp = 24 )
      integer ic2o3_kpp
      parameter ( ic2o3_kpp = 25 )
      integer idms_kpp
      parameter ( idms_kpp = 26 )
      integer ich3sch2oo_kpp
      parameter ( ich3sch2oo_kpp = 27 )
      integer ihcho_kpp
      parameter ( ihcho_kpp = 28 )
      integer ich3so2h_kpp
      parameter ( ich3so2h_kpp = 29 )
      integer io3p_kpp
      parameter ( io3p_kpp = 30 )
      integer io3_kpp
      parameter ( io3_kpp = 31 )
      integer ich3so2_kpp
      parameter ( ich3so2_kpp = 32 )
      integer ich3so3_kpp
      parameter ( ich3so3_kpp = 33 )
      integer iho2_kpp
      parameter ( iho2_kpp = 34 )
      integer ich3o2_kpp
      parameter ( ich3o2_kpp = 35 )
      integer ino3_kpp
      parameter ( ino3_kpp = 36 )
      integer ino2_kpp
      parameter ( ino2_kpp = 37 )
      integer ino_kpp
      parameter ( ino_kpp = 38 )
      integer ioh_kpp
      parameter ( ioh_kpp = 39 )

! indexes declaration for fixed species
      integer ich4_kpp
      parameter ( ich4_kpp = 1 )
      integer ih2o_kpp
      parameter ( ih2o_kpp = 2 )
      integer ih2_kpp
      parameter ( ih2_kpp = 3 )
      integer io2_kpp
      parameter ( io2_kpp = 4 )
      integer in2_kpp
      parameter ( in2_kpp = 5 )


      nyy = nvar_r04_kpp

      if (imap .le. 0) goto 1000
      if (imap .ge. 1) goto 2000


!
!   map cbox values into yyvarkpp and yyfixkpp
!
1000  continue
      yy(ih2so4_kpp)	= cbox(ih2so4_z)
      yy(ircooh_kpp)	= cbox(ircooh_z)
      yy(imsa_kpp)	= cbox(imsa_z)
      yy(imtf_kpp)	= cbox(imtf_z)
      yy(io1d_kpp)	= cbox(io1d_z)
      yy(ic2h5oh_kpp)	= cbox(ic2h5oh_z)
      yy(iso2_kpp)	= cbox(iso2_z)
      yy(ic2h6_kpp)	= cbox(ic2h6_z)
      yy(ipan_kpp)	= cbox(ipan_z)
      yy(idmso2_kpp)	= cbox(idmso2_z)
      yy(ih2o2_kpp)	= cbox(ih2o2_z)
      yy(ich3oh_kpp)	= cbox(ich3oh_z)
      yy(in2o5_kpp)	= cbox(in2o5_z)
      yy(ihno4_kpp)	= cbox(ihno4_z)
      yy(ico_kpp)	= cbox(ico_z)
      yy(idmso_kpp)	= cbox(idmso_z)
      yy(iethooh_kpp)	= cbox(iethooh_z)
      yy(ihono_kpp)	= cbox(ihono_z)
      yy(ich3ooh_kpp)	= cbox(ich3ooh_z)
      yy(ich3so2oo_kpp)	= cbox(ich3so2oo_z)
      yy(iethp_kpp)	= cbox(iethp_z)
      yy(iald2_kpp)	= cbox(iald2_z)
      yy(ich3so2ch2oo_kpp)	= cbox(ich3so2ch2oo_z)
      yy(ihno3_kpp)	= cbox(ihno3_z)
      yy(ic2o3_kpp)	= cbox(ic2o3_z)
      yy(idms_kpp)	= cbox(idms_z)
      yy(ich3sch2oo_kpp)	= cbox(ich3sch2oo_z)
      yy(ihcho_kpp)	= cbox(ihcho_z)
      yy(ich3so2h_kpp)	= cbox(ich3so2h_z)
      yy(io3p_kpp)	= cbox(io3p_z)
      yy(io3_kpp)	= cbox(io3_z)
      yy(ich3so2_kpp)	= cbox(ich3so2_z)
      yy(ich3so3_kpp)	= cbox(ich3so3_z)
      yy(iho2_kpp)	= cbox(iho2_z)
      yy(ich3o2_kpp)	= cbox(ich3o2_z)
      yy(ino3_kpp)	= cbox(ino3_z)
      yy(ino2_kpp)	= cbox(ino2_z)
      yy(ino_kpp)	= cbox(ino_z)
      yy(ioh_kpp)	= cbox(ioh_z)

      yyfixed(ich4_kpp)	= cbox(ich4_z)
      yyfixed(ih2o_kpp)	= cbox(ih2o_z)
      yyfixed(ih2_kpp)	= cbox(ih2_z)
      yyfixed(io2_kpp)	= cbox(io2_z)
      yyfixed(in2_kpp)	= cbox(in2_z)

!
!   map yyvarkpp values into cbox
!
2000  continue
      cbox(ih2so4_z)	= yy(ih2so4_kpp)
      cbox(ircooh_z)	= yy(ircooh_kpp)
      cbox(imsa_z)	= yy(imsa_kpp)
      cbox(imtf_z)	= yy(imtf_kpp)
      cbox(io1d_z)	= yy(io1d_kpp)
      cbox(ic2h5oh_z)	= yy(ic2h5oh_kpp)
      cbox(iso2_z)	= yy(iso2_kpp)
      cbox(ic2h6_z)	= yy(ic2h6_kpp)
      cbox(ipan_z)	= yy(ipan_kpp)
      cbox(idmso2_z)	= yy(idmso2_kpp)
      cbox(ih2o2_z)	= yy(ih2o2_kpp)
      cbox(ich3oh_z)	= yy(ich3oh_kpp)
      cbox(in2o5_z)	= yy(in2o5_kpp)
      cbox(ihno4_z)	= yy(ihno4_kpp)
      cbox(ico_z)	= yy(ico_kpp)
      cbox(idmso_z)	= yy(idmso_kpp)
      cbox(iethooh_z)	= yy(iethooh_kpp)
      cbox(ihono_z)	= yy(ihono_kpp)
      cbox(ich3ooh_z)	= yy(ich3ooh_kpp)
      cbox(ich3so2oo_z)	= yy(ich3so2oo_kpp)
      cbox(iethp_z)	= yy(iethp_kpp)
      cbox(iald2_z)	= yy(iald2_kpp)
      cbox(ich3so2ch2oo_z)	= yy(ich3so2ch2oo_kpp)
      cbox(ihno3_z)	= yy(ihno3_kpp)
      cbox(ic2o3_z)	= yy(ic2o3_kpp)
      cbox(idms_z)	= yy(idms_kpp)
      cbox(ich3sch2oo_z)	= yy(ich3sch2oo_kpp)
      cbox(ihcho_z)	= yy(ihcho_kpp)
      cbox(ich3so2h_z)	= yy(ich3so2h_kpp)
      cbox(io3p_z)	= yy(io3p_kpp)
      cbox(io3_z)	= yy(io3_kpp)
      cbox(ich3so2_z)	= yy(ich3so2_kpp)
      cbox(ich3so3_z)	= yy(ich3so3_kpp)
      cbox(iho2_z)	= yy(iho2_kpp)
      cbox(ich3o2_z)	= yy(ich3o2_kpp)
      cbox(ino3_z)	= yy(ino3_kpp)
      cbox(ino2_z)	= yy(ino2_kpp)
      cbox(ino_z)	= yy(ino_kpp)
      cbox(ioh_z)	= yy(ioh_kpp)

      return
      end subroutine cbmz_v02r04_mapconcs                                


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r04_maprates(   &
          rk_m1,   &
          rk_m2,   &
          rk_m3,   &
          rk_m4,   &
          rconst )
!
!   maps reaction rate constants (host code array --> kpp rconst array)
!   for mechanism-version-regime = cbmz_v02r04
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     all are host-code reaction-rate-constant arrays [input]
      real rk_m1(*)
      real rk_m2(*)
      real rk_m3(*)
      real rk_m4(*)
      real rconst(nreact_kppmax)

!   local variables
      integer i

      do i = 1, nreact_kppmax
          rconst(i) = 0.
      end do


      rconst(1) = (rk_m1(1))
      rconst(2) = (rk_m1(2))
      rconst(3) = (rk_m1(3))
      rconst(4) = (rk_m1(4))
      rconst(5) = (rk_m1(5))
      rconst(6) = (rk_m1(6))
      rconst(7) = (rk_m1(7))
      rconst(8) = (rk_m1(8))
      rconst(9) = (rk_m1(9))
      rconst(10) = (rk_m1(10))
      rconst(11) = (rk_m1(11))
      rconst(12) = (rk_m1(12))
      rconst(13) = (rk_m1(13))
      rconst(14) = (rk_m1(14))
      rconst(15) = (rk_m1(15))
      rconst(16) = (rk_m1(16))
      rconst(17) = (rk_m1(17))
      rconst(18) = (rk_m1(18))
      rconst(19) = (rk_m1(19))
      rconst(20) = (rk_m1(20))
      rconst(21) = (rk_m1(21))
      rconst(22) = (rk_m1(22))
      rconst(23) = (rk_m1(23))
      rconst(24) = (rk_m1(24))
      rconst(25) = (rk_m1(25))
      rconst(26) = (rk_m1(26))
      rconst(27) = (rk_m1(27))
      rconst(28) = (rk_m1(28))
      rconst(29) = (rk_m1(29))
      rconst(30) = (rk_m1(30))
      rconst(31) = (rk_m1(31))
      rconst(32) = (rk_m1(32))
      rconst(33) = (rk_m1(33))
      rconst(34) = (rk_m1(34))
      rconst(35) = (rk_m1(35))
      rconst(36) = (rk_m1(36))
      rconst(37) = (rk_m1(37))
      rconst(38) = (rk_m1(38))
      rconst(39) = (rk_m1(39))
      rconst(40) = (rk_m1(40))
      rconst(41) = (rk_m1(41))
      rconst(42) = (rk_m1(42))
      rconst(43) = (rk_m1(43))
      rconst(44) = (rk_m1(44))
      rconst(45) = (rk_m1(45))
      rconst(46) = (rk_m1(46))
      rconst(47) = (rk_m1(47))
      rconst(48) = (rk_m1(48))
      rconst(49) = (rk_m1(49))
      rconst(50) = (rk_m1(50))
      rconst(51) = (rk_m1(51))
      rconst(52) = (rk_m1(52))
      rconst(53) = (rk_m1(53))
      rconst(54) = (rk_m1(54))
      rconst(55) = (rk_m1(55))
      rconst(56) = (rk_m1(56))
      rconst(57) = (rk_m1(57))
      rconst(58) = (rk_m1(58))
      rconst(59) = (rk_m1(59))
      rconst(60) = (rk_m1(60))
      rconst(61) = (rk_m1(61))
      rconst(62) = (rk_m1(62))
      rconst(63) = (rk_m1(63))
      rconst(64) = (rk_m1(64))
      rconst(65) = (rk_m1(65))
      rconst(66) = (rk_m2(2))
      rconst(67) = (rk_m2(3))
      rconst(68) = (rk_m2(4))
      rconst(69) = (rk_m2(31))
      rconst(70) = (rk_m2(32))
      rconst(71) = (rk_m2(34))
      rconst(72) = (rk_m2(39))
      rconst(73) = (rk_m2(44))
      rconst(74) = (rk_m2(49))
      rconst(75) = (rk_m4(1))
      rconst(76) = (rk_m4(2))
      rconst(77) = (rk_m4(3))
      rconst(78) = (rk_m4(4))
      rconst(79) = (rk_m4(5))
      rconst(80) = (rk_m4(6))
      rconst(81) = (rk_m4(7))
      rconst(82) = (rk_m4(8))
      rconst(83) = (rk_m4(9))
      rconst(84) = (rk_m4(10))
      rconst(85) = (rk_m4(11))
      rconst(86) = (rk_m4(12))
      rconst(87) = (rk_m4(13))
      rconst(88) = (rk_m4(14))
      rconst(89) = (rk_m4(15))
      rconst(90) = (rk_m4(16))
      rconst(91) = (rk_m4(17))
      rconst(92) = (rk_m4(18))
      rconst(93) = (rk_m4(19))
      rconst(94) = (rk_m4(20))
      rconst(95) = (rk_m4(21))
      rconst(96) = (rk_m4(22))
      rconst(97) = (rk_m4(23))
      rconst(98) = (rk_m4(24))
      rconst(99) = (rk_m4(25))
      rconst(100) = (rk_m4(26))
      rconst(101) = (rk_m4(27))
      rconst(102) = (rk_m4(28))
      rconst(103) = (rk_m4(29))
      rconst(104) = (rk_m4(30))
      rconst(105) = (rk_m4(31))
      rconst(106) = (rk_m4(32))
      return
      end subroutine cbmz_v02r04_maprates 


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r04_dydt( nvardum, tdum, v, a_var, f, rconst )
!
!   computes rates of change for mechanism-version-regime = cbmz_v02r04
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     nvardum = number of variable species [input]
      integer nvardum
!     tdum = time [input]
      real tdum
!     v = concentrations of variable species [input]
      real v(nvar_r04_kpp)
!     a_var = dydt for each species [output]
      real a_var(nvar_r04_kpp)
!     f = concentrations of fixed species [input]
      real f(nfixed_kppmax)
!     rconst = reaction rate constants [input]
      real rconst(nreact_r04_kpp)

!   local variables
!     a = rate for each reaction
      real a(nreact_r04_kpp)

! computation of equation rates
      a(1) = rconst(1)*v(37)
      a(2) = rconst(2)*v(36)
      a(3) = rconst(3)*v(18)
      a(4) = rconst(4)*v(24)
      a(5) = rconst(5)*v(14)
      a(6) = rconst(6)*v(13)
      a(7) = rconst(7)*v(31)
      a(8) = rconst(8)*v(31)
      a(9) = rconst(9)*v(11)
      a(10) = rconst(10)*v(5)*f(4)
      a(11) = rconst(11)*v(5)*f(5)
      a(12) = rconst(12)*v(5)*f(2)
      a(13) = rconst(13)*v(30)*f(4)
      a(14) = rconst(14)*v(30)*v(31)
      a(15) = rconst(15)*v(30)*v(37)
      a(16) = rconst(16)*v(30)*v(37)
      a(17) = rconst(17)*v(30)*v(38)
      a(18) = rconst(18)*v(31)*v(38)
      a(19) = rconst(19)*v(31)*v(37)
      a(20) = rconst(20)*v(31)*v(39)
      a(21) = rconst(21)*v(31)*v(34)
      a(22) = rconst(22)*v(39)*f(3)
      a(23) = rconst(23)*v(38)*v(39)
      a(24) = rconst(24)*v(37)*v(39)
      a(25) = rconst(25)*v(36)*v(39)
      a(26) = rconst(26)*v(18)*v(39)
      a(27) = rconst(27)*v(24)*v(39)
      a(28) = rconst(28)*v(14)*v(39)
      a(29) = rconst(29)*v(34)*v(39)
      a(30) = rconst(30)*v(11)*v(39)
      a(31) = rconst(31)*v(34)*v(34)
      a(32) = rconst(32)*v(34)*v(34)*f(2)
      a(33) = rconst(33)*v(34)*v(38)
      a(34) = rconst(34)*v(34)*v(37)
      a(35) = rconst(35)*v(34)*v(37)
      a(36) = rconst(36)*v(14)
      a(37) = rconst(37)*v(36)*v(38)
      a(38) = rconst(38)*v(36)*v(37)
      a(39) = rconst(39)*v(36)*v(37)
      a(40) = rconst(40)*v(36)*v(36)
      a(41) = rconst(41)*v(34)*v(36)
      a(42) = rconst(42)*v(13)*f(2)
      a(43) = rconst(43)*v(13)
      a(44) = rconst(44)*v(15)*v(39)
      a(45) = rconst(45)*v(7)*v(39)
      a(46) = rconst(46)*v(39)*f(1)
      a(47) = rconst(47)*v(8)*v(39)
      a(48) = rconst(48)*v(12)*v(39)
      a(49) = rconst(49)*v(28)
      a(50) = rconst(50)*v(28)
      a(51) = rconst(51)*v(28)*v(39)
      a(52) = rconst(52)*v(28)*v(36)
      a(53) = rconst(53)*v(19)
      a(54) = rconst(54)*v(17)
      a(55) = rconst(55)*v(19)*v(39)
      a(56) = rconst(56)*v(17)*v(39)
      a(57) = rconst(57)*v(35)*v(38)
      a(58) = rconst(58)*v(21)*v(38)
      a(59) = rconst(59)*v(35)*v(36)
      a(60) = rconst(60)*v(21)*v(36)
      a(61) = rconst(61)*v(34)*v(35)
      a(62) = rconst(62)*v(21)*v(34)
      a(63) = rconst(63)*v(35)
      a(64) = rconst(64)*v(21)
      a(65) = rconst(65)*v(6)*v(39)
      a(66) = rconst(66)*v(22)
      a(67) = rconst(67)*v(22)*v(39)
      a(68) = rconst(68)*v(22)*v(36)
      a(69) = rconst(69)*v(25)*v(37)
      a(70) = rconst(70)*v(9)
      a(71) = rconst(71)*v(25)*v(38)
      a(72) = rconst(72)*v(25)*v(36)
      a(73) = rconst(73)*v(25)*v(34)
      a(74) = rconst(74)*v(25)
      a(75) = rconst(75)*v(26)*v(39)
      a(76) = rconst(76)*v(26)*v(36)
      a(77) = rconst(77)*v(26)*v(30)
      a(78) = rconst(78)*v(26)*v(39)
      a(79) = rconst(79)*v(27)*v(38)
      a(80) = rconst(80)*v(27)*v(35)
      a(81) = rconst(81)*v(27)*v(32)
      a(82) = rconst(82)*v(27)*v(27)
      a(83) = rconst(83)*v(16)*v(39)
      a(84) = rconst(84)*v(10)*v(39)
      a(85) = rconst(85)*v(23)*v(38)
      a(86) = rconst(86)*v(23)*v(35)
      a(87) = rconst(87)*v(29)*v(34)
      a(88) = rconst(88)*v(29)*v(36)
      a(89) = rconst(89)*v(29)*v(35)
      a(90) = rconst(90)*v(29)*v(39)
      a(91) = rconst(91)*v(29)*v(33)
      a(92) = rconst(92)*v(32)
      a(93) = rconst(93)*v(32)*v(37)
      a(94) = rconst(94)*v(31)*v(32)
      a(95) = rconst(95)*v(32)*v(34)
      a(96) = rconst(96)*v(32)*v(35)
      a(97) = rconst(97)*v(32)*v(39)
      a(98) = rconst(98)*v(32)*f(4)
      a(99) = rconst(99)*v(20)
      a(100) = rconst(100)*v(20)*v(38)
      a(101) = rconst(101)*v(20)*v(35)
      a(102) = rconst(102)*v(33)
      a(103) = rconst(103)*v(33)*v(37)
      a(104) = rconst(104)*v(33)*v(38)
      a(105) = rconst(105)*v(33)*v(34)
      a(106) = rconst(106)*v(28)*v(33)

! aggregate function
      a_var(1) = a(45)+a(102)
      a_var(2) = 0.4*a(73)
      a_var(3) = a(91)+a(97)+a(103)+a(104)+a(105)+a(106)
      a_var(4) = 0.15*a(82)
      a_var(5) = a(8)-a(10)-a(11)-a(12)
      a_var(6) = -a(65)
      a_var(7) = -a(45)+a(92)
      a_var(8) = -a(47)+0.2*a(64)
      a_var(9) = a(69)-a(70)
      a_var(10) = 0.27*a(83)-a(84)
      a_var(11) = -a(9)-a(30)+a(31)+a(32)+a(87)
      a_var(12) = -a(48)+0.34*a(63)
      a_var(13) = -a(6)+a(39)-a(42)-a(43)
      a_var(14) = -a(5)-a(28)+a(34)-a(36)
      a_var(15) = -a(44)+a(49)+a(50)+a(51)+a(52)+a(66)+a(106)
      a_var(16) = 0.965*a(78)-a(83)
      a_var(17) = -a(54)-a(56)+a(62)
      a_var(18) = -a(3)+a(23)-a(26)+a(35)+a(104)
      a_var(19) = -a(53)-a(55)+a(61)+a(89)
      a_var(20) = a(98)-a(99)-a(100)-a(101)
      a_var(21) = a(47)+0.5*a(56)-a(58)-a(60)-a(62)-a(64)
      a_var(22) = a(54)+0.5*a(56)+a(58)+a(60)+0.8*a(64)+a(65)-a(66)   &
                 -a(67)-a(68)
      a_var(23) = a(84)-a(85)-a(86)
      a_var(24) = -a(4)+a(24)-a(27)+0.3*a(41)+2*a(42)+a(52)+a(68)   &
                 +a(76)+a(88)+a(103)
      a_var(25) = a(67)+a(68)-a(69)+a(70)-a(71)-a(72)-a(73)-a(74)
      a_var(26) = -a(75)-a(76)-a(77)-a(78)
      a_var(27) = a(75)+a(76)-a(79)-a(80)-a(81)-2*a(82)
      a_var(28) = a(48)-a(49)-a(50)-a(51)-a(52)+a(53)+0.3*a(55)+a(57)   &
                 +a(59)+0.66*a(63)+a(79)+2*a(80)+a(81)+a(85)+2*a(86)   &
                 +a(96)+a(101)-a(106)
      a_var(29) = 0.73*a(83)-a(87)-a(88)-a(89)-a(90)-a(91)
      a_var(30) = a(1)+0.89*a(2)+a(7)+a(10)+a(11)-a(13)-a(14)-a(15)   &
                 -a(16)-a(17)-a(77)
      a_var(31) = -a(7)-a(8)+a(13)-a(14)-a(18)-a(19)-a(20)-a(21)+0.4   &
                 *a(73)-a(94)
      a_var(32) = a(77)+0.035*a(78)+a(79)+a(80)+1.85*a(82)+a(85)+a(86)   &
                 +a(87)+a(88)+a(89)+a(90)+a(91)-a(92)-a(93)-a(94)   &
                 -a(95)-a(96)-a(97)-a(98)+a(99)
      a_var(33) = a(81)-a(91)+a(93)+a(94)+a(95)+a(96)+a(100)+a(101)   &
                 -a(102)-a(103)-a(104)-a(105)-a(106)
      a_var(34) = a(5)+a(20)-a(21)+a(22)+a(25)-a(29)+a(30)-2*a(31)-2   &
                 *a(32)-a(33)-a(34)-a(35)+a(36)-a(41)+a(44)+a(45)   &
                 +a(48)+2*a(49)+a(51)+a(52)+a(53)+a(54)+a(57)+a(58)   &
                 +a(59)+a(60)-a(61)-a(62)+0.32*a(63)+0.6*a(64)+a(65)   &
                 +a(66)-a(73)+0.965*a(78)+a(80)+0.27*a(83)+a(86)-a(87)   &
                 -a(95)+a(96)+a(101)-a(105)+a(106)
      a_var(35) = a(46)+0.7*a(55)-a(57)-a(59)-a(61)-a(63)+a(66)+a(71)   &
                 +a(72)+a(74)+a(77)+0.035*a(78)-a(80)+0.73*a(83)-a(86)   &
                 -a(89)+a(92)-a(96)-a(101)+a(102)
      a_var(36) = -a(2)+a(6)+a(16)+a(19)-a(25)+a(27)-a(37)-a(38)-a(39)   &
                 -2*a(40)-a(41)+a(43)-a(52)-a(59)-a(60)-a(68)-a(72)   &
                 -a(76)-a(88)
      a_var(37) = -a(1)+0.89*a(2)+a(4)+a(5)+a(6)-a(15)-a(16)+a(17)   &
                 +a(18)-a(19)-a(24)+a(25)+a(26)+a(28)+a(33)-a(34)   &
                 -a(35)+a(36)+2*a(37)-a(39)+2*a(40)+0.7*a(41)+a(43)   &
                 +a(57)+a(58)+a(59)+a(60)-a(69)+a(70)+a(71)+a(72)   &
                 +a(79)+a(85)-a(93)+a(100)-a(103)
      a_var(38) = a(1)+0.11*a(2)+a(3)+a(15)-a(17)-a(18)-a(23)-a(33)   &
                 -a(37)+a(38)-a(57)-a(58)-a(71)-a(79)-a(85)+a(93)   &
                 -a(100)-a(104)
      a_var(39) = a(3)+a(4)+2*a(9)+2*a(12)-a(20)+a(21)-a(22)-a(23)   &
                 -a(24)-a(25)-a(26)-a(27)-a(28)-a(29)-a(30)+a(33)+0.7   &
                 *a(41)-a(44)-a(45)-a(46)-a(47)-a(48)-a(51)+a(53)   &
                 +a(54)-0.7*a(55)-0.5*a(56)-a(65)-a(67)-a(75)-a(78)   &
                 -a(83)-a(84)-a(90)+a(95)-a(97)
      return
      end subroutine cbmz_v02r04_dydt                                      


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r04_jacob( nvardum, tdum, v, jvs, f, rconst )
!
!   computes jacobian for mechanism-version-regime = cbmz_v02r04
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     nvardum = number of variable species [input]
      integer nvardum
!     tdum = time [input]
      real tdum
!     v = concentrations of variable species [input]
      real v(nvar_r04_kpp)
!     jvs = non-zero jacobian elements [output]
      real jvs(lu_nonzero_v_r04_kpp)
!     f = concentrations of fixed species [input]
      real f(nfixed_kppmax)
!     rconst = reaction rate constants [input]
      real rconst(nreact_r04_kpp)

!   local variables
!     b(i,j) = d[reaction_rate(i)] / d[species_conc(j)]
      real b(nreact_r04_kpp,nvar_r04_kpp)

! computation of b(i,j) = da(i)/dv(j)
      b(1,37) = rconst(1)
      b(2,36) = rconst(2)
      b(3,18) = rconst(3)
      b(4,24) = rconst(4)
      b(5,14) = rconst(5)
      b(6,13) = rconst(6)
      b(7,31) = rconst(7)
      b(8,31) = rconst(8)
      b(9,11) = rconst(9)
      b(10,5) = rconst(10)*f(4)
      b(11,5) = rconst(11)*f(5)
      b(12,5) = rconst(12)*f(2)
      b(13,30) = rconst(13)*f(4)
      b(14,30) = rconst(14)*v(31)
      b(14,31) = rconst(14)*v(30)
      b(15,30) = rconst(15)*v(37)
      b(15,37) = rconst(15)*v(30)
      b(16,30) = rconst(16)*v(37)
      b(16,37) = rconst(16)*v(30)
      b(17,30) = rconst(17)*v(38)
      b(17,38) = rconst(17)*v(30)
      b(18,31) = rconst(18)*v(38)
      b(18,38) = rconst(18)*v(31)
      b(19,31) = rconst(19)*v(37)
      b(19,37) = rconst(19)*v(31)
      b(20,31) = rconst(20)*v(39)
      b(20,39) = rconst(20)*v(31)
      b(21,31) = rconst(21)*v(34)
      b(21,34) = rconst(21)*v(31)
      b(22,39) = rconst(22)*f(3)
      b(23,38) = rconst(23)*v(39)
      b(23,39) = rconst(23)*v(38)
      b(24,37) = rconst(24)*v(39)
      b(24,39) = rconst(24)*v(37)
      b(25,36) = rconst(25)*v(39)
      b(25,39) = rconst(25)*v(36)
      b(26,18) = rconst(26)*v(39)
      b(26,39) = rconst(26)*v(18)
      b(27,24) = rconst(27)*v(39)
      b(27,39) = rconst(27)*v(24)
      b(28,14) = rconst(28)*v(39)
      b(28,39) = rconst(28)*v(14)
      b(29,34) = rconst(29)*v(39)
      b(29,39) = rconst(29)*v(34)
      b(30,11) = rconst(30)*v(39)
      b(30,39) = rconst(30)*v(11)
      b(31,34) = rconst(31)*2*v(34)
      b(32,34) = rconst(32)*2*v(34)*f(2)
      b(33,34) = rconst(33)*v(38)
      b(33,38) = rconst(33)*v(34)
      b(34,34) = rconst(34)*v(37)
      b(34,37) = rconst(34)*v(34)
      b(35,34) = rconst(35)*v(37)
      b(35,37) = rconst(35)*v(34)
      b(36,14) = rconst(36)
      b(37,36) = rconst(37)*v(38)
      b(37,38) = rconst(37)*v(36)
      b(38,36) = rconst(38)*v(37)
      b(38,37) = rconst(38)*v(36)
      b(39,36) = rconst(39)*v(37)
      b(39,37) = rconst(39)*v(36)
      b(40,36) = rconst(40)*2*v(36)
      b(41,34) = rconst(41)*v(36)
      b(41,36) = rconst(41)*v(34)
      b(42,13) = rconst(42)*f(2)
      b(43,13) = rconst(43)
      b(44,15) = rconst(44)*v(39)
      b(44,39) = rconst(44)*v(15)
      b(45,7) = rconst(45)*v(39)
      b(45,39) = rconst(45)*v(7)
      b(46,39) = rconst(46)*f(1)
      b(47,8) = rconst(47)*v(39)
      b(47,39) = rconst(47)*v(8)
      b(48,12) = rconst(48)*v(39)
      b(48,39) = rconst(48)*v(12)
      b(49,28) = rconst(49)
      b(50,28) = rconst(50)
      b(51,28) = rconst(51)*v(39)
      b(51,39) = rconst(51)*v(28)
      b(52,28) = rconst(52)*v(36)
      b(52,36) = rconst(52)*v(28)
      b(53,19) = rconst(53)
      b(54,17) = rconst(54)
      b(55,19) = rconst(55)*v(39)
      b(55,39) = rconst(55)*v(19)
      b(56,17) = rconst(56)*v(39)
      b(56,39) = rconst(56)*v(17)
      b(57,35) = rconst(57)*v(38)
      b(57,38) = rconst(57)*v(35)
      b(58,21) = rconst(58)*v(38)
      b(58,38) = rconst(58)*v(21)
      b(59,35) = rconst(59)*v(36)
      b(59,36) = rconst(59)*v(35)
      b(60,21) = rconst(60)*v(36)
      b(60,36) = rconst(60)*v(21)
      b(61,34) = rconst(61)*v(35)
      b(61,35) = rconst(61)*v(34)
      b(62,21) = rconst(62)*v(34)
      b(62,34) = rconst(62)*v(21)
      b(63,35) = rconst(63)
      b(64,21) = rconst(64)
      b(65,6) = rconst(65)*v(39)
      b(65,39) = rconst(65)*v(6)
      b(66,22) = rconst(66)
      b(67,22) = rconst(67)*v(39)
      b(67,39) = rconst(67)*v(22)
      b(68,22) = rconst(68)*v(36)
      b(68,36) = rconst(68)*v(22)
      b(69,25) = rconst(69)*v(37)
      b(69,37) = rconst(69)*v(25)
      b(70,9) = rconst(70)
      b(71,25) = rconst(71)*v(38)
      b(71,38) = rconst(71)*v(25)
      b(72,25) = rconst(72)*v(36)
      b(72,36) = rconst(72)*v(25)
      b(73,25) = rconst(73)*v(34)
      b(73,34) = rconst(73)*v(25)
      b(74,25) = rconst(74)
      b(75,26) = rconst(75)*v(39)
      b(75,39) = rconst(75)*v(26)
      b(76,26) = rconst(76)*v(36)
      b(76,36) = rconst(76)*v(26)
      b(77,26) = rconst(77)*v(30)
      b(77,30) = rconst(77)*v(26)
      b(78,26) = rconst(78)*v(39)
      b(78,39) = rconst(78)*v(26)
      b(79,27) = rconst(79)*v(38)
      b(79,38) = rconst(79)*v(27)
      b(80,27) = rconst(80)*v(35)
      b(80,35) = rconst(80)*v(27)
      b(81,27) = rconst(81)*v(32)
      b(81,32) = rconst(81)*v(27)
      b(82,27) = rconst(82)*2*v(27)
      b(83,16) = rconst(83)*v(39)
      b(83,39) = rconst(83)*v(16)
      b(84,10) = rconst(84)*v(39)
      b(84,39) = rconst(84)*v(10)
      b(85,23) = rconst(85)*v(38)
      b(85,38) = rconst(85)*v(23)
      b(86,23) = rconst(86)*v(35)
      b(86,35) = rconst(86)*v(23)
      b(87,29) = rconst(87)*v(34)
      b(87,34) = rconst(87)*v(29)
      b(88,29) = rconst(88)*v(36)
      b(88,36) = rconst(88)*v(29)
      b(89,29) = rconst(89)*v(35)
      b(89,35) = rconst(89)*v(29)
      b(90,29) = rconst(90)*v(39)
      b(90,39) = rconst(90)*v(29)
      b(91,29) = rconst(91)*v(33)
      b(91,33) = rconst(91)*v(29)
      b(92,32) = rconst(92)
      b(93,32) = rconst(93)*v(37)
      b(93,37) = rconst(93)*v(32)
      b(94,31) = rconst(94)*v(32)
      b(94,32) = rconst(94)*v(31)
      b(95,32) = rconst(95)*v(34)
      b(95,34) = rconst(95)*v(32)
      b(96,32) = rconst(96)*v(35)
      b(96,35) = rconst(96)*v(32)
      b(97,32) = rconst(97)*v(39)
      b(97,39) = rconst(97)*v(32)
      b(98,32) = rconst(98)*f(4)
      b(99,20) = rconst(99)
      b(100,20) = rconst(100)*v(38)
      b(100,38) = rconst(100)*v(20)
      b(101,20) = rconst(101)*v(35)
      b(101,35) = rconst(101)*v(20)
      b(102,33) = rconst(102)
      b(103,33) = rconst(103)*v(37)
      b(103,37) = rconst(103)*v(33)
      b(104,33) = rconst(104)*v(38)
      b(104,38) = rconst(104)*v(33)
      b(105,33) = rconst(105)*v(34)
      b(105,34) = rconst(105)*v(33)
      b(106,28) = rconst(106)*v(33)
      b(106,33) = rconst(106)*v(28)

! construct the jacobian terms from b's
      jvs(1) = 0
      jvs(2) = b(45,7)
      jvs(3) = b(102,33)
      jvs(4) = b(45,39)
      jvs(5) = 0
      jvs(6) = 0.4*b(73,25)
      jvs(7) = 0.4*b(73,34)
      jvs(8) = 0
      jvs(9) = b(106,28)
      jvs(10) = b(91,29)
      jvs(11) = b(97,32)
      jvs(12) = b(91,33)+b(103,33)+b(104,33)+b(105,33)+b(106,33)
      jvs(13) = b(105,34)
      jvs(14) = b(103,37)
      jvs(15) = b(104,38)
      jvs(16) = b(97,39)
      jvs(17) = 0
      jvs(18) = 0.15*b(82,27)
      jvs(19) = -b(10,5)-b(11,5)-b(12,5)
      jvs(20) = b(8,31)
      jvs(21) = -b(65,6)
      jvs(22) = -b(65,39)
      jvs(23) = -b(45,7)
      jvs(24) = b(92,32)
      jvs(25) = -b(45,39)
      jvs(26) = -b(47,8)
      jvs(27) = 0.2*b(64,21)
      jvs(28) = -b(47,39)
      jvs(29) = -b(70,9)
      jvs(30) = b(69,25)
      jvs(31) = b(69,37)
      jvs(32) = -b(84,10)
      jvs(33) = 0.27*b(83,16)
      jvs(34) = 0.27*b(83,39)-b(84,39)
      jvs(35) = -b(9,11)-b(30,11)
      jvs(36) = b(87,29)
      jvs(37) = b(31,34)+b(32,34)+b(87,34)
      jvs(38) = -b(30,39)
      jvs(39) = -b(48,12)
      jvs(40) = 0.34*b(63,35)
      jvs(41) = -b(48,39)
      jvs(42) = -b(6,13)-b(42,13)-b(43,13)
      jvs(43) = b(39,36)
      jvs(44) = b(39,37)
      jvs(45) = -b(5,14)-b(28,14)-b(36,14)
      jvs(46) = b(34,34)
      jvs(47) = b(34,37)
      jvs(48) = -b(28,39)
      jvs(49) = -b(44,15)
      jvs(50) = b(66,22)
      jvs(51) = b(49,28)+b(50,28)+b(51,28)+b(52,28)+b(106,28)
      jvs(52) = b(106,33)
      jvs(53) = b(52,36)
      jvs(54) = -b(44,39)+b(51,39)
      jvs(55) = -b(83,16)
      jvs(56) = 0.965*b(78,26)
      jvs(57) = 0.965*b(78,39)-b(83,39)
      jvs(58) = -b(54,17)-b(56,17)
      jvs(59) = b(62,21)
      jvs(60) = b(62,34)
      jvs(61) = -b(56,39)
      jvs(62) = -b(3,18)-b(26,18)
      jvs(63) = b(104,33)
      jvs(64) = b(35,34)
      jvs(65) = b(35,37)
      jvs(66) = b(23,38)+b(104,38)
      jvs(67) = b(23,39)-b(26,39)
      jvs(68) = -b(53,19)-b(55,19)
      jvs(69) = b(89,29)
      jvs(70) = b(61,34)
      jvs(71) = b(61,35)+b(89,35)
      jvs(72) = -b(55,39)
      jvs(73) = -b(99,20)-b(100,20)-b(101,20)
      jvs(74) = b(98,32)
      jvs(75) = -b(101,35)
      jvs(76) = -b(100,38)
      jvs(77) = b(47,8)
      jvs(78) = 0.5*b(56,17)
      jvs(79) = -b(58,21)-b(60,21)-b(62,21)-b(64,21)
      jvs(80) = -b(62,34)
      jvs(81) = -b(60,36)
      jvs(82) = -b(58,38)
      jvs(83) = b(47,39)+0.5*b(56,39)
      jvs(84) = b(65,6)
      jvs(85) = b(54,17)+0.5*b(56,17)
      jvs(86) = b(58,21)+b(60,21)+0.8*b(64,21)
      jvs(87) = -b(66,22)-b(67,22)-b(68,22)
      jvs(88) = 0
      jvs(89) = b(60,36)-b(68,36)
      jvs(90) = b(58,38)
      jvs(91) = 0.5*b(56,39)+b(65,39)-b(67,39)
      jvs(92) = b(84,10)
      jvs(93) = 0
      jvs(94) = -b(85,23)-b(86,23)
      jvs(95) = 0
      jvs(96) = -b(86,35)
      jvs(97) = -b(85,38)
      jvs(98) = b(84,39)
      jvs(99) = 2*b(42,13)
      jvs(100) = b(68,22)
      jvs(101) = -b(4,24)-b(27,24)
      jvs(102) = b(76,26)
      jvs(103) = b(52,28)
      jvs(104) = b(88,29)
      jvs(105) = b(103,33)
      jvs(106) = 0.3*b(41,34)
      jvs(107) = 0.3*b(41,36)+b(52,36)+b(68,36)+b(76,36)+b(88,36)
      jvs(108) = b(24,37)+b(103,37)
      jvs(109) = 0
      jvs(110) = b(24,39)-b(27,39)
      jvs(111) = b(70,9)
      jvs(112) = b(67,22)+b(68,22)
      jvs(113) = -b(69,25)-b(71,25)-b(72,25)-b(73,25)-b(74,25)
      jvs(114) = -b(73,34)
      jvs(115) = b(68,36)-b(72,36)
      jvs(116) = -b(69,37)
      jvs(117) = -b(71,38)
      jvs(118) = b(67,39)
      jvs(119) = -b(75,26)-b(76,26)-b(77,26)-b(78,26)
      jvs(120) = -b(77,30)
      jvs(121) = -b(76,36)
      jvs(122) = -b(75,39)-b(78,39)
      jvs(123) = b(75,26)+b(76,26)
      jvs(124) = -b(79,27)-b(80,27)-b(81,27)-2*b(82,27)
      jvs(125) = 0
      jvs(126) = -b(81,32)
      jvs(127) = -b(80,35)
      jvs(128) = b(76,36)
      jvs(129) = -b(79,38)
      jvs(130) = b(75,39)
      jvs(131) = b(48,12)
      jvs(132) = b(53,19)+0.3*b(55,19)
      jvs(133) = b(101,20)
      jvs(134) = b(85,23)+2*b(86,23)
      jvs(135) = 0
      jvs(136) = b(79,27)+2*b(80,27)+b(81,27)
      jvs(137) = -b(49,28)-b(50,28)-b(51,28)-b(52,28)-b(106,28)
      jvs(138) = 0
      jvs(139) = 0
      jvs(140) = b(81,32)+b(96,32)
      jvs(141) = -b(106,33)
      jvs(142) = 0
      jvs(143) = b(57,35)+b(59,35)+0.66*b(63,35)+2*b(80,35)+2*b(86,35)   &
                +b(96,35)+b(101,35)
      jvs(144) = -b(52,36)+b(59,36)
      jvs(145) = b(57,38)+b(79,38)+b(85,38)
      jvs(146) = b(48,39)-b(51,39)+0.3*b(55,39)
      jvs(147) = 0.73*b(83,16)
      jvs(148) = 0
      jvs(149) = -b(87,29)-b(88,29)-b(89,29)-b(90,29)-b(91,29)
      jvs(150) = 0
      jvs(151) = -b(91,33)
      jvs(152) = -b(87,34)
      jvs(153) = -b(89,35)
      jvs(154) = -b(88,36)
      jvs(155) = 0.73*b(83,39)-b(90,39)
      jvs(156) = b(10,5)+b(11,5)
      jvs(157) = -b(77,26)
      jvs(158) = -b(13,30)-b(14,30)-b(15,30)-b(16,30)-b(17,30)   &
                -b(77,30)
      jvs(159) = b(7,31)-b(14,31)
      jvs(160) = 0.89*b(2,36)
      jvs(161) = b(1,37)-b(15,37)-b(16,37)
      jvs(162) = -b(17,38)
      jvs(163) = 0
      jvs(164) = 0.4*b(73,25)
      jvs(165) = b(13,30)-b(14,30)
      jvs(166) = -b(7,31)-b(8,31)-b(14,31)-b(18,31)-b(19,31)-b(20,31)   &
                -b(21,31)-b(94,31)
      jvs(167) = -b(94,32)
      jvs(168) = -b(21,34)+0.4*b(73,34)
      jvs(169) = 0
      jvs(170) = -b(19,37)
      jvs(171) = -b(18,38)
      jvs(172) = -b(20,39)
      jvs(173) = b(99,20)
      jvs(174) = b(85,23)+b(86,23)
      jvs(175) = b(77,26)+0.035*b(78,26)
      jvs(176) = b(79,27)+b(80,27)+1.85*b(82,27)
      jvs(177) = b(87,29)+b(88,29)+b(89,29)+b(90,29)+b(91,29)
      jvs(178) = b(77,30)
      jvs(179) = -b(94,31)
      jvs(180) = -b(92,32)-b(93,32)-b(94,32)-b(95,32)-b(96,32)   &
                -b(97,32)-b(98,32)
      jvs(181) = b(91,33)
      jvs(182) = b(87,34)-b(95,34)
      jvs(183) = b(80,35)+b(86,35)+b(89,35)-b(96,35)
      jvs(184) = b(88,36)
      jvs(185) = -b(93,37)
      jvs(186) = b(79,38)+b(85,38)
      jvs(187) = 0.035*b(78,39)+b(90,39)-b(97,39)
      jvs(188) = b(100,20)+b(101,20)
      jvs(189) = b(81,27)
      jvs(190) = -b(106,28)
      jvs(191) = -b(91,29)
      jvs(192) = 0
      jvs(193) = b(94,31)
      jvs(194) = b(81,32)+b(93,32)+b(94,32)+b(95,32)+b(96,32)
      jvs(195) = -b(91,33)-b(102,33)-b(103,33)-b(104,33)-b(105,33)   &
                -b(106,33)
      jvs(196) = b(95,34)-b(105,34)
      jvs(197) = b(96,35)+b(101,35)
      jvs(198) = 0
      jvs(199) = b(93,37)-b(103,37)
      jvs(200) = b(100,38)-b(104,38)
      jvs(201) = 0
      jvs(202) = b(65,6)
      jvs(203) = b(45,7)
      jvs(204) = b(30,11)
      jvs(205) = b(48,12)
      jvs(206) = b(5,14)+b(36,14)
      jvs(207) = b(44,15)
      jvs(208) = 0.27*b(83,16)
      jvs(209) = b(54,17)
      jvs(210) = b(53,19)
      jvs(211) = b(101,20)
      jvs(212) = b(58,21)+b(60,21)-b(62,21)+0.6*b(64,21)
      jvs(213) = b(66,22)
      jvs(214) = b(86,23)
      jvs(215) = -b(73,25)
      jvs(216) = 0.965*b(78,26)
      jvs(217) = b(80,27)
      jvs(218) = 2*b(49,28)+b(51,28)+b(52,28)+b(106,28)
      jvs(219) = -b(87,29)
      jvs(220) = 0
      jvs(221) = b(20,31)-b(21,31)
      jvs(222) = -b(95,32)+b(96,32)
      jvs(223) = -b(105,33)+b(106,33)
      jvs(224) = -b(21,34)-b(29,34)-2*b(31,34)-2*b(32,34)-b(33,34)   &
                -b(34,34)-b(35,34)-b(41,34)-b(61,34)-b(62,34)-b(73,34)   &
                -b(87,34)-b(95,34)-b(105,34)
      jvs(225) = b(57,35)+b(59,35)-b(61,35)+0.32*b(63,35)+b(80,35)   &
                +b(86,35)+b(96,35)+b(101,35)
      jvs(226) = b(25,36)-b(41,36)+b(52,36)+b(59,36)+b(60,36)
      jvs(227) = -b(34,37)-b(35,37)
      jvs(228) = -b(33,38)+b(57,38)+b(58,38)
      jvs(229) = b(20,39)+b(22,39)+b(25,39)-b(29,39)+b(30,39)+b(44,39)   &
                +b(45,39)+b(48,39)+b(51,39)+b(65,39)+0.965*b(78,39)   &
                +0.27*b(83,39)
      jvs(230) = 0.73*b(83,16)
      jvs(231) = 0.7*b(55,19)
      jvs(232) = -b(101,20)
      jvs(233) = b(66,22)
      jvs(234) = -b(86,23)
      jvs(235) = b(71,25)+b(72,25)+b(74,25)
      jvs(236) = b(77,26)+0.035*b(78,26)
      jvs(237) = -b(80,27)
      jvs(238) = -b(89,29)
      jvs(239) = b(77,30)
      jvs(240) = 0
      jvs(241) = b(92,32)-b(96,32)
      jvs(242) = b(102,33)
      jvs(243) = -b(61,34)
      jvs(244) = -b(57,35)-b(59,35)-b(61,35)-b(63,35)-b(80,35)   &
                -b(86,35)-b(89,35)-b(96,35)-b(101,35)
      jvs(245) = -b(59,36)+b(72,36)
      jvs(246) = 0
      jvs(247) = -b(57,38)+b(71,38)
      jvs(248) = b(46,39)+0.7*b(55,39)+0.035*b(78,39)+0.73*b(83,39)
      jvs(249) = b(6,13)+b(43,13)
      jvs(250) = -b(60,21)
      jvs(251) = -b(68,22)
      jvs(252) = b(27,24)
      jvs(253) = -b(72,25)
      jvs(254) = -b(76,26)
      jvs(255) = -b(52,28)
      jvs(256) = -b(88,29)
      jvs(257) = b(16,30)
      jvs(258) = b(19,31)
      jvs(259) = 0
      jvs(260) = 0
      jvs(261) = -b(41,34)
      jvs(262) = -b(59,35)
      jvs(263) = 0
      jvs(264) = b(16,37)+b(19,37)-b(38,37)-b(39,37)
      jvs(265) = -b(37,38)
      jvs(266) = -b(25,39)+b(27,39)
      jvs(267) = b(70,9)
      jvs(268) = b(6,13)+b(43,13)
      jvs(269) = b(5,14)+b(28,14)+b(36,14)
      jvs(270) = b(26,18)
      jvs(271) = b(100,20)
      jvs(272) = b(58,21)+b(60,21)
      jvs(273) = b(85,23)
      jvs(274) = b(4,24)
      jvs(275) = -b(69,25)+b(71,25)+b(72,25)
      jvs(276) = 0
      jvs(277) = b(79,27)
      jvs(278) = 0
      jvs(279) = 0
      jvs(280) = -b(15,30)-b(16,30)+b(17,30)
      jvs(281) = b(18,31)-b(19,31)
      jvs(282) = -b(93,32)
      jvs(283) = -b(103,33)
      jvs(284) = b(33,34)-b(34,34)-b(35,34)+0.7*b(41,34)
      jvs(285) = b(57,35)+b(59,35)
      jvs(286) = -b(1,37)-b(15,37)-b(16,37)-b(19,37)-b(24,37)-b(34,37)   &
                -b(35,37)-b(39,37)-b(69,37)-b(93,37)-b(103,37)
      jvs(287) = b(17,38)+b(18,38)+b(33,38)+2*b(37,38)+b(57,38)   &
                +b(58,38)+b(71,38)+b(79,38)+b(85,38)+b(100,38)
      jvs(288) = -b(24,39)+b(25,39)+b(26,39)+b(28,39)
      jvs(289) = b(3,18)
      jvs(290) = -b(100,20)
      jvs(291) = -b(58,21)
      jvs(292) = -b(85,23)
      jvs(293) = -b(71,25)
      jvs(294) = 0
      jvs(295) = -b(79,27)
      jvs(296) = b(15,30)-b(17,30)
      jvs(297) = -b(18,31)
      jvs(298) = b(93,32)
      jvs(299) = -b(104,33)
      jvs(300) = -b(33,34)
      jvs(301) = -b(57,35)
      jvs(302) = 0.11*b(2,36)-b(37,36)+b(38,36)
      jvs(303) = b(1,37)+b(15,37)+b(38,37)+b(93,37)
      jvs(304) = -b(17,38)-b(18,38)-b(23,38)-b(33,38)-b(37,38)   &
                -b(57,38)-b(58,38)-b(71,38)-b(79,38)-b(85,38)   &
                -b(100,38)-b(104,38)
      jvs(305) = -b(23,39)
      jvs(306) = 2*b(12,5)
      jvs(307) = -b(65,6)
      jvs(308) = -b(45,7)
      jvs(309) = -b(47,8)
      jvs(310) = -b(84,10)
      jvs(311) = 2*b(9,11)-b(30,11)
      jvs(312) = -b(48,12)
      jvs(313) = -b(28,14)
      jvs(314) = -b(44,15)
      jvs(315) = -b(83,16)
      jvs(316) = b(54,17)-0.5*b(56,17)
      jvs(317) = b(3,18)-b(26,18)
      jvs(318) = b(53,19)-0.7*b(55,19)
      jvs(319) = 0
      jvs(320) = -b(67,22)
      jvs(321) = b(4,24)-b(27,24)
      jvs(322) = -b(75,26)-b(78,26)
      jvs(323) = -b(51,28)
      jvs(324) = -b(90,29)
      jvs(325) = 0
      jvs(326) = -b(20,31)+b(21,31)
      jvs(327) = b(95,32)-b(97,32)
      jvs(328) = 0
      jvs(329) = b(21,34)-b(29,34)+b(33,34)+0.7*b(41,34)+b(95,34)
      jvs(330) = 0
      jvs(331) = -b(25,36)+0.7*b(41,36)
      jvs(332) = -b(24,37)
      jvs(333) = -b(23,38)+b(33,38)
      jvs(334) = -b(20,39)-b(22,39)-b(23,39)-b(24,39)-b(25,39)   &
                -b(26,39)-b(27,39)-b(28,39)-b(29,39)-b(30,39)-b(44,39)   &
                -b(45,39)-b(46,39)-b(47,39)-b(48,39)-b(51,39)-0.7   &
                *b(55,39)-0.5*b(56,39)-b(65,39)-b(67,39)-b(75,39)   &
                -b(78,39)-b(83,39)-b(84,39)-b(90,39)-b(97,39)
      return
      end subroutine cbmz_v02r04_jacob                                    


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r04_decomp( n, v, ier,   &
          lu_crow_v, lu_diag_v, lu_icol_v )
!
!   computes l-u-decomposition of sparse jacobian
!   for mechanism-version-regime = cbmz_v02r04
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     n = number of variable species [input]
      integer n
!     ier = status flag [output]
!         0 = success    other = failure [output]
      integer ier

!     v = non-zero elements of the sparse jacobian [input]
      real v(lu_nonzero_v_r04_kpp)

      integer lu_crow_v(nvar_r04_kpp + 1)
      integer lu_diag_v(nvar_r04_kpp + 1)
      integer lu_icol_v(lu_nonzero_v_r04_kpp)

!   local variables
      integer k, kk, j, jj
      real a, w(nvar_r04_kpp + 1)

      ier = 0
      do k=1,n
        if ( v( lu_diag_v(k) ) .eq. 0. ) then
            ier = k
            return
        end if
        do kk = lu_crow_v(k), lu_crow_v(k+1)-1
              w( lu_icol_v(kk) ) = v(kk)
        end do
        do kk = lu_crow_v(k), lu_diag_v(k)-1
            j = lu_icol_v(kk)
            a = -w(j) / v( lu_diag_v(j) )
            w(j) = -a
            do jj = lu_diag_v(j)+1, lu_crow_v(j+1)-1
               w( lu_icol_v(jj) ) = w( lu_icol_v(jj) ) + a*v(jj)
            end do
         end do
         do kk = lu_crow_v(k), lu_crow_v(k+1)-1
            v(kk) = w( lu_icol_v(kk) )
         end do
      end do
      return
      end subroutine cbmz_v02r04_decomp            


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r04_solve( jvs, x )
!
!   does back-solve for mechanism-version-regime = cbmz_v02r04
!
      implicit none

!   subr parameters
!     jvs = the non-zero elements of the l-u-decomposition
!           of the augmented jacobian [input]
      real jvs(*)
!     x = the right-hand side of the linear equation set being solved [on input]
!     x = concentrations of variable species [on output]
      real x(*)


      x(21) = x(21)-jvs(77)*x(8)-jvs(78)*x(17)
      x(22) = x(22)-jvs(84)*x(6)-jvs(85)*x(17)-jvs(86)*x(21)
      x(23) = x(23)-jvs(92)*x(10)-jvs(93)*x(16)
      x(24) = x(24)-jvs(99)*x(13)-jvs(100)*x(22)
      x(25) = x(25)-jvs(111)*x(9)-jvs(112)*x(22)
      x(27) = x(27)-jvs(123)*x(26)
      x(28) = x(28)-jvs(131)*x(12)-jvs(132)*x(19)-jvs(133)*x(20)   &
             -jvs(134)*x(23)-jvs(135)*x(26)-jvs(136)*x(27)
      x(29) = x(29)-jvs(147)*x(16)-jvs(148)*x(26)
      x(30) = x(30)-jvs(156)*x(5)-jvs(157)*x(26)
      x(31) = x(31)-jvs(164)*x(25)-jvs(165)*x(30)
      x(32) = x(32)-jvs(173)*x(20)-jvs(174)*x(23)-jvs(175)*x(26)   &
             -jvs(176)*x(27)-jvs(177)*x(29)-jvs(178)*x(30)-jvs(179)   &
             *x(31)
      x(33) = x(33)-jvs(188)*x(20)-jvs(189)*x(27)-jvs(190)*x(28)   &
             -jvs(191)*x(29)-jvs(192)*x(30)-jvs(193)*x(31)-jvs(194)   &
             *x(32)
      x(34) = x(34)-jvs(202)*x(6)-jvs(203)*x(7)-jvs(204)*x(11)   &
             -jvs(205)*x(12)-jvs(206)*x(14)-jvs(207)*x(15)-jvs(208)   &
             *x(16)-jvs(209)*x(17)-jvs(210)*x(19)-jvs(211)*x(20)   &
             -jvs(212)*x(21)-jvs(213)*x(22)-jvs(214)*x(23)-jvs(215)   &
             *x(25)-jvs(216)*x(26)-jvs(217)*x(27)-jvs(218)*x(28)   &
             -jvs(219)*x(29)-jvs(220)*x(30)-jvs(221)*x(31)-jvs(222)   &
             *x(32)-jvs(223)*x(33)
      x(35) = x(35)-jvs(230)*x(16)-jvs(231)*x(19)-jvs(232)*x(20)   &
             -jvs(233)*x(22)-jvs(234)*x(23)-jvs(235)*x(25)-jvs(236)   &
             *x(26)-jvs(237)*x(27)-jvs(238)*x(29)-jvs(239)*x(30)   &
             -jvs(240)*x(31)-jvs(241)*x(32)-jvs(242)*x(33)-jvs(243)   &
             *x(34)
      x(36) = x(36)-jvs(249)*x(13)-jvs(250)*x(21)-jvs(251)*x(22)   &
             -jvs(252)*x(24)-jvs(253)*x(25)-jvs(254)*x(26)-jvs(255)   &
             *x(28)-jvs(256)*x(29)-jvs(257)*x(30)-jvs(258)*x(31)   &
             -jvs(259)*x(32)-jvs(260)*x(33)-jvs(261)*x(34)-jvs(262)   &
             *x(35)
      x(37) = x(37)-jvs(267)*x(9)-jvs(268)*x(13)-jvs(269)*x(14)   &
             -jvs(270)*x(18)-jvs(271)*x(20)-jvs(272)*x(21)-jvs(273)   &
             *x(23)-jvs(274)*x(24)-jvs(275)*x(25)-jvs(276)*x(26)   &
             -jvs(277)*x(27)-jvs(278)*x(28)-jvs(279)*x(29)-jvs(280)   &
             *x(30)-jvs(281)*x(31)-jvs(282)*x(32)-jvs(283)*x(33)   &
             -jvs(284)*x(34)-jvs(285)*x(35)
      x(38) = x(38)-jvs(289)*x(18)-jvs(290)*x(20)-jvs(291)*x(21)   &
             -jvs(292)*x(23)-jvs(293)*x(25)-jvs(294)*x(26)-jvs(295)   &
             *x(27)-jvs(296)*x(30)-jvs(297)*x(31)-jvs(298)*x(32)   &
             -jvs(299)*x(33)-jvs(300)*x(34)-jvs(301)*x(35)-jvs(302)   &
             *x(36)-jvs(303)*x(37)
      x(39) = x(39)-jvs(306)*x(5)-jvs(307)*x(6)-jvs(308)*x(7)-jvs(309)   &
             *x(8)-jvs(310)*x(10)-jvs(311)*x(11)-jvs(312)*x(12)   &
             -jvs(313)*x(14)-jvs(314)*x(15)-jvs(315)*x(16)-jvs(316)   &
             *x(17)-jvs(317)*x(18)-jvs(318)*x(19)-jvs(319)*x(21)   &
             -jvs(320)*x(22)-jvs(321)*x(24)-jvs(322)*x(26)-jvs(323)   &
             *x(28)-jvs(324)*x(29)-jvs(325)*x(30)-jvs(326)*x(31)   &
             -jvs(327)*x(32)-jvs(328)*x(33)-jvs(329)*x(34)-jvs(330)   &
             *x(35)-jvs(331)*x(36)-jvs(332)*x(37)-jvs(333)*x(38)
      x(39) = x(39)/jvs(334)
      x(38) = (x(38)-jvs(305)*x(39))/(jvs(304))
      x(37) = (x(37)-jvs(287)*x(38)-jvs(288)*x(39))/(jvs(286))
      x(36) = (x(36)-jvs(264)*x(37)-jvs(265)*x(38)-jvs(266)*x(39))/   &
             (jvs(263))
      x(35) = (x(35)-jvs(245)*x(36)-jvs(246)*x(37)-jvs(247)*x(38)   &
             -jvs(248)*x(39))/(jvs(244))
      x(34) = (x(34)-jvs(225)*x(35)-jvs(226)*x(36)-jvs(227)*x(37)   &
             -jvs(228)*x(38)-jvs(229)*x(39))/(jvs(224))
      x(33) = (x(33)-jvs(196)*x(34)-jvs(197)*x(35)-jvs(198)*x(36)   &
             -jvs(199)*x(37)-jvs(200)*x(38)-jvs(201)*x(39))/(jvs(195))
      x(32) = (x(32)-jvs(181)*x(33)-jvs(182)*x(34)-jvs(183)*x(35)   &
             -jvs(184)*x(36)-jvs(185)*x(37)-jvs(186)*x(38)-jvs(187)   &
             *x(39))/(jvs(180))
      x(31) = (x(31)-jvs(167)*x(32)-jvs(168)*x(34)-jvs(169)*x(36)   &
             -jvs(170)*x(37)-jvs(171)*x(38)-jvs(172)*x(39))/(jvs(166))
      x(30) = (x(30)-jvs(159)*x(31)-jvs(160)*x(36)-jvs(161)*x(37)   &
             -jvs(162)*x(38)-jvs(163)*x(39))/(jvs(158))
      x(29) = (x(29)-jvs(150)*x(30)-jvs(151)*x(33)-jvs(152)*x(34)   &
             -jvs(153)*x(35)-jvs(154)*x(36)-jvs(155)*x(39))/(jvs(149))
      x(28) = (x(28)-jvs(138)*x(29)-jvs(139)*x(30)-jvs(140)*x(32)   &
             -jvs(141)*x(33)-jvs(142)*x(34)-jvs(143)*x(35)-jvs(144)   &
             *x(36)-jvs(145)*x(38)-jvs(146)*x(39))/(jvs(137))
      x(27) = (x(27)-jvs(125)*x(30)-jvs(126)*x(32)-jvs(127)*x(35)   &
             -jvs(128)*x(36)-jvs(129)*x(38)-jvs(130)*x(39))/(jvs(124))
      x(26) = (x(26)-jvs(120)*x(30)-jvs(121)*x(36)-jvs(122)*x(39))/   &
             (jvs(119))
      x(25) = (x(25)-jvs(114)*x(34)-jvs(115)*x(36)-jvs(116)*x(37)   &
             -jvs(117)*x(38)-jvs(118)*x(39))/(jvs(113))
      x(24) = (x(24)-jvs(102)*x(26)-jvs(103)*x(28)-jvs(104)*x(29)   &
             -jvs(105)*x(33)-jvs(106)*x(34)-jvs(107)*x(36)-jvs(108)   &
             *x(37)-jvs(109)*x(38)-jvs(110)*x(39))/(jvs(101))
      x(23) = (x(23)-jvs(95)*x(26)-jvs(96)*x(35)-jvs(97)*x(38)-jvs(98)   &
             *x(39))/(jvs(94))
      x(22) = (x(22)-jvs(88)*x(34)-jvs(89)*x(36)-jvs(90)*x(38)-jvs(91)   &
             *x(39))/(jvs(87))
      x(21) = (x(21)-jvs(80)*x(34)-jvs(81)*x(36)-jvs(82)*x(38)-jvs(83)   &
             *x(39))/(jvs(79))
      x(20) = (x(20)-jvs(74)*x(32)-jvs(75)*x(35)-jvs(76)*x(38))/   &
             (jvs(73))
      x(19) = (x(19)-jvs(69)*x(29)-jvs(70)*x(34)-jvs(71)*x(35)-jvs(72)   &
             *x(39))/(jvs(68))
      x(18) = (x(18)-jvs(63)*x(33)-jvs(64)*x(34)-jvs(65)*x(37)-jvs(66)   &
             *x(38)-jvs(67)*x(39))/(jvs(62))
      x(17) = (x(17)-jvs(59)*x(21)-jvs(60)*x(34)-jvs(61)*x(39))/   &
             (jvs(58))
      x(16) = (x(16)-jvs(56)*x(26)-jvs(57)*x(39))/(jvs(55))
      x(15) = (x(15)-jvs(50)*x(22)-jvs(51)*x(28)-jvs(52)*x(33)-jvs(53)   &
             *x(36)-jvs(54)*x(39))/(jvs(49))
      x(14) = (x(14)-jvs(46)*x(34)-jvs(47)*x(37)-jvs(48)*x(39))/   &
             (jvs(45))
      x(13) = (x(13)-jvs(43)*x(36)-jvs(44)*x(37))/(jvs(42))
      x(12) = (x(12)-jvs(40)*x(35)-jvs(41)*x(39))/(jvs(39))
      x(11) = (x(11)-jvs(36)*x(29)-jvs(37)*x(34)-jvs(38)*x(39))/   &
             (jvs(35))
      x(10) = (x(10)-jvs(33)*x(16)-jvs(34)*x(39))/(jvs(32))
      x(9) = (x(9)-jvs(30)*x(25)-jvs(31)*x(37))/(jvs(29))
      x(8) = (x(8)-jvs(27)*x(21)-jvs(28)*x(39))/(jvs(26))
      x(7) = (x(7)-jvs(24)*x(32)-jvs(25)*x(39))/(jvs(23))
      x(6) = (x(6)-jvs(22)*x(39))/(jvs(21))
      x(5) = (x(5)-jvs(20)*x(31))/(jvs(19))
      x(4) = (x(4)-jvs(18)*x(27))/(jvs(17))
      x(3) = (x(3)-jvs(9)*x(28)-jvs(10)*x(29)-jvs(11)*x(32)-jvs(12)   &
            *x(33)-jvs(13)*x(34)-jvs(14)*x(37)-jvs(15)*x(38)-jvs(16)   &
            *x(39))/(jvs(8))
      x(2) = (x(2)-jvs(6)*x(25)-jvs(7)*x(34))/(jvs(5))
      x(1) = (x(1)-jvs(2)*x(7)-jvs(3)*x(33)-jvs(4)*x(39))/(jvs(1))
      return
      end subroutine cbmz_v02r04_solve          


!   cbmz_v02r05_torodas.f - created on 18-nov-2003 from previous
!        cbmz_v02r05_torodas.f        cbmz_v02r05_mapconcs.f
!        cbmz_v02r05_maprates.f       cbmz_v02r05_dydt.f
!        cbmz_v02r05_jacob.f          cbmz_v02r05_decomp.f
!        cbmz_v02r05_solve.f
!   so now everything is in a single file
!-----------------------------------------------------------------------

      subroutine cbmz_v02r05_torodas(   &
          ngas, taa, tzz,   &
          stot, atol, rtol, yposlimit, yneglimit,   &
          sfixedkpp, rconstkpp,   &
          hmin, hstart,   &
          info_rodas, iok, lunerr, idydt_sngldble )
!
!   interfaces to rodas3 solver formechanism-version-regime =cbmz_v02r05
!
!   *** do not include any pegasus common blocks ***
!
      use module_data_cbmz
      use module_cbmz_rodas3_solver, only:  rodas3_ff_x2
      implicit none

!   subr parameters
      integer ngas, iok, lunerr, idydt_sngldble
      integer info_rodas(6)
      real taa, tzz, hmin, hstart
      real stot(ngas), atol(ngas), rtol(ngas)
      real yposlimit(ngas), yneglimit(ngas)
      real sfixedkpp(nfixed_kppmax), rconstkpp(nreact_kppmax)

!   local variables

!     external cbmz_v02r05_dydt
!     external cbmz_v02r05_jacob
!     external cbmz_v02r05_decomp
!     external cbmz_v02r05_solve

      integer i

      real hmax

      integer lu_crow_v(nvar_r05_kpp + 1)
      save    lu_crow_v
      integer lu_diag_v(nvar_r05_kpp + 1)
      save    lu_diag_v
      integer lu_icol_v(lu_nonzero_v_r05_kpp)
      save    lu_icol_v

      data( lu_icol_v(i), i = 1, 252 ) /   &
        1,  8, 52, 59,  2, 22, 36, 51,  3, 36, 39, 46,   &
       51, 56,  4, 42, 47, 52, 53, 54, 55, 56, 59,  5,   &
       38,  6, 51,  7, 59,  8, 54, 59,  9, 41, 59, 10,   &
       46, 53, 11, 20, 59, 12, 59, 13, 42, 56, 59, 14,   &
       53, 57, 15, 59, 15, 16, 27, 59, 17, 26, 53, 57,   &
       59, 18, 53, 56, 59, 12, 15, 19, 55, 59, 20, 34,   &
       59, 21, 41, 56, 59, 22, 51, 59, 23, 36, 39, 51,   &
       58, 59, 24, 52, 53, 55, 56, 59, 25, 42, 56, 58,   &
       59, 12, 15, 26, 57, 59, 16, 27, 33, 36, 39, 44,   &
       49, 50, 51, 55, 57, 59, 22, 28, 32, 36, 39, 40,   &
       45, 47, 51, 52, 57, 59, 29, 54, 55, 58, 11, 20,   &
       30, 34, 55, 58, 59, 14, 26, 31, 34, 40, 42, 45,   &
       47, 52, 53, 56, 57, 59, 19, 26, 32, 51, 55, 57,   &
       59, 33, 48, 49, 56, 59, 34, 43, 57, 59, 33, 35,   &
       39, 48, 49, 50, 51, 55, 56, 57, 59, 36, 51, 57,   &
       59, 12, 15, 22, 26, 32, 33, 36, 37, 39, 40, 48,   &
       49, 50, 51, 55, 56, 57, 59, 34, 38, 43, 54, 55,   &
       57, 58, 59, 39, 51, 57, 59, 15, 32, 33, 36, 39,   &
       40, 48, 49, 51, 55, 56, 57, 59,  9, 21, 33, 36,   &
       39, 41, 48, 49, 50, 51, 55, 56, 57, 59, 20, 34,   &
       42, 43, 52, 56, 57, 58, 59,  6, 34, 43, 51, 53 /

      data( lu_icol_v(i), i = 253, 504 ) /   &
       55, 57, 59, 36, 39, 44, 50, 51, 55, 56, 57, 59,   &
        7, 21, 22, 32, 33, 36, 39, 41, 44, 45, 48, 49,   &
       50, 51, 55, 56, 57, 59, 10, 32, 35, 36, 39, 40,   &
       45, 46, 48, 49, 50, 51, 53, 55, 56, 57, 59, 22,   &
       23, 25, 29, 30, 32, 34, 36, 38, 39, 42, 43, 44,   &
       47, 48, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,   &
       35, 39, 48, 49, 50, 51, 55, 56, 57, 59, 16, 27,   &
       33, 36, 39, 44, 48, 49, 50, 51, 55, 56, 57, 59,   &
       17, 19, 26, 44, 49, 50, 51, 53, 55, 56, 57, 59,   &
       22, 32, 36, 39, 43, 46, 48, 49, 50, 51, 53, 54,   &
       55, 56, 57, 59, 29, 38, 42, 43, 47, 48, 49, 50,   &
       51, 52, 53, 54, 55, 56, 57, 58, 59, 10, 14, 17,   &
       18, 19, 24, 26, 29, 30, 31, 34, 37, 38, 39, 40,   &
       41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,   &
       53, 54, 55, 56, 58, 59, 29, 30, 34, 38, 42, 43,   &
       51, 52, 53, 54, 55, 56, 57, 58, 59, 19, 24, 29,   &
       30, 34, 37, 38, 39, 40, 41, 43, 44, 46, 48, 49,   &
       50, 51, 52, 53, 54, 55, 56, 57, 58, 59,  7,  8,   &
       12, 13, 15, 18, 19, 20, 21, 22, 23, 25, 26, 28,   &
       29, 30, 32, 33, 34, 36, 37, 38, 39, 40, 41, 42,   &
       43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54 /

      data( lu_icol_v(i), i = 505, 606 ) /   &
       55, 56, 57, 58, 59, 14, 26, 31, 34, 36, 37, 39,   &
       40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,   &
       52, 53, 54, 55, 56, 57, 58, 59, 20, 25, 29, 30,   &
       34, 35, 36, 38, 39, 42, 43, 45, 46, 48, 49, 50,   &
       51, 52, 53, 54, 55, 56, 57, 58, 59,  6,  7,  8,   &
        9, 11, 12, 13, 15, 16, 18, 20, 21, 22, 23, 24,   &
       25, 26, 27, 28, 31, 32, 33, 34, 35, 36, 39, 40,   &
       41, 42, 43, 44, 45, 47, 48, 49, 50, 51, 52, 53,   &
       54, 55, 56, 57, 58, 59 /

      data lu_crow_v /   &
        1,  5,  9, 15, 24, 26, 28, 30, 33, 36, 39, 42,   &
       44, 48, 51, 53, 57, 62, 66, 71, 74, 78, 81, 87,   &
       93, 98,103,115,127,131,138,151,158,163,167,178,   &
      182,200,208,212,225,239,248,256,265,283,300,325,   &
      335,349,361,377,394,427,442,467,510,537,562,607   &
       /

      data lu_diag_v /   &
        1,  5,  9, 15, 24, 26, 28, 30, 33, 36, 39, 42,   &
       44, 48, 51, 54, 57, 62, 68, 71, 74, 78, 81, 87,   &
       93,100,104,116,127,133,140,153,158,163,168,178,   &
      189,201,208,217,230,241,250,258,274,290,313,327,   &
      342,354,370,386,421,436,462,506,534,560,606,607   &
       /



      info_rodas(1) = 1
      do i = 2, 6
          info_rodas(i) = 0
      end do
      hmax = tzz - taa

!   do not integrate if hmax is less/equal to hmin
!   because hmin is generally 0.1 s or less
      if (hmax .le. 1.001*hmin) then
          iok = 11
          return
      end if

      call rodas3_ff_x2(   &
           nvar_r05_kpp, taa, tzz, hmin, hmax, hstart,   &
           stot, atol, rtol, yposlimit, yneglimit,   &
           sfixedkpp, rconstkpp,   &
           lu_nonzero_v_r05_kpp, lu_crow_v, lu_diag_v, lu_icol_v,   &
           info_rodas, iok, lunerr,   &
           cbmz_v02r05_dydt,   &
           cbmz_v02r05_jacob,   &
           cbmz_v02r05_decomp,   &
           cbmz_v02r05_solve )

      return
      end subroutine cbmz_v02r05_torodas 


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r05_mapconcs( imap, nyy, yy, yyfixed, cbox )
!
!   maps species concentrations (gaschemistry cbox array <--> kpp yy array)
!   for mechanism-version-regime = cbmz_v02r05
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     imap = mapping direction flag [input]
!         0 = map cbox --> yy and yyfixed
!         1 = map yy --> cbox
      integer imap
!     nyy = number of kpp "variable" species [output]
      integer nyy
!     yy = kpp species concentrations array [input/output]
      real yy(nvar_r05_kpp)
!     yyfixed = kpp species concentrations array [input/output]
      real yyfixed(nfixed_kppmax)
!     cbox = main gaschemistry species conc array [input/output]
      real cbox(ngas_z)

!   local variables
      integer ih2so4_kpp
      parameter ( ih2so4_kpp = 1 )
      integer ihcooh_kpp
      parameter ( ihcooh_kpp = 2 )
      integer ircooh_kpp
      parameter ( ircooh_kpp = 3 )
      integer imsa_kpp
      parameter ( imsa_kpp = 4 )
      integer imtf_kpp
      parameter ( imtf_kpp = 5 )
      integer io1d_kpp
      parameter ( io1d_kpp = 6 )
      integer ic2h5oh_kpp
      parameter ( ic2h5oh_kpp = 7 )
      integer iso2_kpp
      parameter ( iso2_kpp = 8 )
      integer ic2h6_kpp
      parameter ( ic2h6_kpp = 9 )
      integer ipan_kpp
      parameter ( ipan_kpp = 10 )
      integer idmso2_kpp
      parameter ( idmso2_kpp = 11 )
      integer itol_kpp
      parameter ( itol_kpp = 12 )
      integer ih2o2_kpp
      parameter ( ih2o2_kpp = 13 )
      integer in2o5_kpp
      parameter ( in2o5_kpp = 14 )
      integer ixyl_kpp
      parameter ( ixyl_kpp = 15 )
      integer ipar_kpp
      parameter ( ipar_kpp = 16 )
      integer icro_kpp
      parameter ( icro_kpp = 17 )
      integer ihno4_kpp
      parameter ( ihno4_kpp = 18 )
      integer ito2_kpp
      parameter ( ito2_kpp = 19 )
      integer idmso_kpp
      parameter ( idmso_kpp = 20 )
      integer iethooh_kpp
      parameter ( iethooh_kpp = 21 )
      integer ieth_kpp
      parameter ( ieth_kpp = 22 )
      integer ich3oh_kpp
      parameter ( ich3oh_kpp = 23 )
      integer ihono_kpp
      parameter ( ihono_kpp = 24 )
      integer ich3ooh_kpp
      parameter ( ich3ooh_kpp = 25 )
      integer icres_kpp
      parameter ( icres_kpp = 26 )
      integer ixpar_kpp
      parameter ( ixpar_kpp = 27 )
      integer ico_kpp
      parameter ( ico_kpp = 28 )
      integer ich3so2oo_kpp
      parameter ( ich3so2oo_kpp = 29 )
      integer ich3so2ch2oo_kpp
      parameter ( ich3so2ch2oo_kpp = 30 )
      integer ihno3_kpp
      parameter ( ihno3_kpp = 31 )
      integer iopen_kpp
      parameter ( iopen_kpp = 32 )
      integer irooh_kpp
      parameter ( irooh_kpp = 33 )
      integer idms_kpp
      parameter ( idms_kpp = 34 )
      integer iaone_kpp
      parameter ( iaone_kpp = 35 )
      integer iolet_kpp
      parameter ( iolet_kpp = 36 )
      integer ixo2_kpp
      parameter ( ixo2_kpp = 37 )
      integer ich3sch2oo_kpp
      parameter ( ich3sch2oo_kpp = 38 )
      integer iolei_kpp
      parameter ( iolei_kpp = 39 )
      integer imgly_kpp
      parameter ( imgly_kpp = 40 )
      integer iethp_kpp
      parameter ( iethp_kpp = 41 )
      integer ich3so2h_kpp
      parameter ( ich3so2h_kpp = 42 )
      integer io3p_kpp
      parameter ( io3p_kpp = 43 )
      integer inap_kpp
      parameter ( inap_kpp = 44 )
      integer iald2_kpp
      parameter ( iald2_kpp = 45 )
      integer ic2o3_kpp
      parameter ( ic2o3_kpp = 46 )
      integer ihcho_kpp
      parameter ( ihcho_kpp = 47 )
      integer iano2_kpp
      parameter ( iano2_kpp = 48 )
      integer iro2_kpp
      parameter ( iro2_kpp = 49 )
      integer ionit_kpp
      parameter ( ionit_kpp = 50 )
      integer io3_kpp
      parameter ( io3_kpp = 51 )
      integer ich3so3_kpp
      parameter ( ich3so3_kpp = 52 )
      integer ino2_kpp
      parameter ( ino2_kpp = 53 )
      integer ich3so2_kpp
      parameter ( ich3so2_kpp = 54 )
      integer ino_kpp
      parameter ( ino_kpp = 55 )
      integer iho2_kpp
      parameter ( iho2_kpp = 56 )
      integer ino3_kpp
      parameter ( ino3_kpp = 57 )
      integer ich3o2_kpp
      parameter ( ich3o2_kpp = 58 )
      integer ioh_kpp
      parameter ( ioh_kpp = 59 )

! indexes declaration for fixed species
      integer ich4_kpp
      parameter ( ich4_kpp = 1 )
      integer ih2o_kpp
      parameter ( ih2o_kpp = 2 )
      integer ih2_kpp
      parameter ( ih2_kpp = 3 )
      integer io2_kpp
      parameter ( io2_kpp = 4 )
      integer in2_kpp
      parameter ( in2_kpp = 5 )


      nyy = nvar_r05_kpp

      if (imap .le. 0) goto 1000
      if (imap .ge. 1) goto 2000


!
!   map cbox values into yyvarkpp and yyfixkpp
!
1000  continue
      yy(ih2so4_kpp)	= cbox(ih2so4_z)
      yy(ihcooh_kpp)	= cbox(ihcooh_z)
      yy(ircooh_kpp)	= cbox(ircooh_z)
      yy(imsa_kpp)	= cbox(imsa_z)
      yy(imtf_kpp)	= cbox(imtf_z)
      yy(io1d_kpp)	= cbox(io1d_z)
      yy(ic2h5oh_kpp)	= cbox(ic2h5oh_z)
      yy(iso2_kpp)	= cbox(iso2_z)
      yy(ic2h6_kpp)	= cbox(ic2h6_z)
      yy(ipan_kpp)	= cbox(ipan_z)
      yy(idmso2_kpp)	= cbox(idmso2_z)
      yy(itol_kpp)	= cbox(itol_z)
      yy(ih2o2_kpp)	= cbox(ih2o2_z)
      yy(in2o5_kpp)	= cbox(in2o5_z)
      yy(ixyl_kpp)	= cbox(ixyl_z)
      yy(ipar_kpp)	= cbox(ipar_z)
      yy(icro_kpp)	= cbox(icro_z)
      yy(ihno4_kpp)	= cbox(ihno4_z)
      yy(ito2_kpp)	= cbox(ito2_z)
      yy(idmso_kpp)	= cbox(idmso_z)
      yy(iethooh_kpp)	= cbox(iethooh_z)
      yy(ieth_kpp)	= cbox(ieth_z)
      yy(ich3oh_kpp)	= cbox(ich3oh_z)
      yy(ihono_kpp)	= cbox(ihono_z)
      yy(ich3ooh_kpp)	= cbox(ich3ooh_z)
      yy(icres_kpp)	= cbox(icres_z)
      yy(ixpar_kpp)	= cbox(ixpar_z)
      yy(ico_kpp)	= cbox(ico_z)
      yy(ich3so2oo_kpp)	= cbox(ich3so2oo_z)
      yy(ich3so2ch2oo_kpp)	= cbox(ich3so2ch2oo_z)
      yy(ihno3_kpp)	= cbox(ihno3_z)
      yy(iopen_kpp)	= cbox(iopen_z)
      yy(irooh_kpp)	= cbox(irooh_z)
      yy(idms_kpp)	= cbox(idms_z)
      yy(iaone_kpp)	= cbox(iaone_z)
      yy(iolet_kpp)	= cbox(iolet_z)
      yy(ixo2_kpp)	= cbox(ixo2_z)
      yy(ich3sch2oo_kpp)	= cbox(ich3sch2oo_z)
      yy(iolei_kpp)	= cbox(iolei_z)
      yy(imgly_kpp)	= cbox(imgly_z)
      yy(iethp_kpp)	= cbox(iethp_z)
      yy(ich3so2h_kpp)	= cbox(ich3so2h_z)
      yy(io3p_kpp)	= cbox(io3p_z)
      yy(inap_kpp)	= cbox(inap_z)
      yy(iald2_kpp)	= cbox(iald2_z)
      yy(ic2o3_kpp)	= cbox(ic2o3_z)
      yy(ihcho_kpp)	= cbox(ihcho_z)
      yy(iano2_kpp)	= cbox(iano2_z)
      yy(iro2_kpp)	= cbox(iro2_z)
      yy(ionit_kpp)	= cbox(ionit_z)
      yy(io3_kpp)	= cbox(io3_z)
      yy(ich3so3_kpp)	= cbox(ich3so3_z)
      yy(ino2_kpp)	= cbox(ino2_z)
      yy(ich3so2_kpp)	= cbox(ich3so2_z)
      yy(ino_kpp)	= cbox(ino_z)
      yy(iho2_kpp)	= cbox(iho2_z)
      yy(ino3_kpp)	= cbox(ino3_z)
      yy(ich3o2_kpp)	= cbox(ich3o2_z)
      yy(ioh_kpp)	= cbox(ioh_z)

      yyfixed(ich4_kpp)	= cbox(ich4_z)
      yyfixed(ih2o_kpp)	= cbox(ih2o_z)
      yyfixed(ih2_kpp)	= cbox(ih2_z)
      yyfixed(io2_kpp)	= cbox(io2_z)
      yyfixed(in2_kpp)	= cbox(in2_z)

!
!   map yyvarkpp values into cbox
!
2000  continue
      cbox(ih2so4_z)	= yy(ih2so4_kpp)
      cbox(ihcooh_z)	= yy(ihcooh_kpp)
      cbox(ircooh_z)	= yy(ircooh_kpp)
      cbox(imsa_z)	= yy(imsa_kpp)
      cbox(imtf_z)	= yy(imtf_kpp)
      cbox(io1d_z)	= yy(io1d_kpp)
      cbox(ic2h5oh_z)	= yy(ic2h5oh_kpp)
      cbox(iso2_z)	= yy(iso2_kpp)
      cbox(ic2h6_z)	= yy(ic2h6_kpp)
      cbox(ipan_z)	= yy(ipan_kpp)
      cbox(idmso2_z)	= yy(idmso2_kpp)
      cbox(itol_z)	= yy(itol_kpp)
      cbox(ih2o2_z)	= yy(ih2o2_kpp)
      cbox(in2o5_z)	= yy(in2o5_kpp)
      cbox(ixyl_z)	= yy(ixyl_kpp)
      cbox(ipar_z)	= yy(ipar_kpp)
      cbox(icro_z)	= yy(icro_kpp)
      cbox(ihno4_z)	= yy(ihno4_kpp)
      cbox(ito2_z)	= yy(ito2_kpp)
      cbox(idmso_z)	= yy(idmso_kpp)
      cbox(iethooh_z)	= yy(iethooh_kpp)
      cbox(ieth_z)	= yy(ieth_kpp)
      cbox(ich3oh_z)	= yy(ich3oh_kpp)
      cbox(ihono_z)	= yy(ihono_kpp)
      cbox(ich3ooh_z)	= yy(ich3ooh_kpp)
      cbox(icres_z)	= yy(icres_kpp)
      cbox(ixpar_z)	= yy(ixpar_kpp)
      cbox(ico_z)	= yy(ico_kpp)
      cbox(ich3so2oo_z)	= yy(ich3so2oo_kpp)
      cbox(ich3so2ch2oo_z)	= yy(ich3so2ch2oo_kpp)
      cbox(ihno3_z)	= yy(ihno3_kpp)
      cbox(iopen_z)	= yy(iopen_kpp)
      cbox(irooh_z)	= yy(irooh_kpp)
      cbox(idms_z)	= yy(idms_kpp)
      cbox(iaone_z)	= yy(iaone_kpp)
      cbox(iolet_z)	= yy(iolet_kpp)
      cbox(ixo2_z)	= yy(ixo2_kpp)
      cbox(ich3sch2oo_z)	= yy(ich3sch2oo_kpp)
      cbox(iolei_z)	= yy(iolei_kpp)
      cbox(imgly_z)	= yy(imgly_kpp)
      cbox(iethp_z)	= yy(iethp_kpp)
      cbox(ich3so2h_z)	= yy(ich3so2h_kpp)
      cbox(io3p_z)	= yy(io3p_kpp)
      cbox(inap_z)	= yy(inap_kpp)
      cbox(iald2_z)	= yy(iald2_kpp)
      cbox(ic2o3_z)	= yy(ic2o3_kpp)
      cbox(ihcho_z)	= yy(ihcho_kpp)
      cbox(iano2_z)	= yy(iano2_kpp)
      cbox(iro2_z)	= yy(iro2_kpp)
      cbox(ionit_z)	= yy(ionit_kpp)
      cbox(io3_z)	= yy(io3_kpp)
      cbox(ich3so3_z)	= yy(ich3so3_kpp)
      cbox(ino2_z)	= yy(ino2_kpp)
      cbox(ich3so2_z)	= yy(ich3so2_kpp)
      cbox(ino_z)	= yy(ino_kpp)
      cbox(iho2_z)	= yy(iho2_kpp)
      cbox(ino3_z)	= yy(ino3_kpp)
      cbox(ich3o2_z)	= yy(ich3o2_kpp)
      cbox(ioh_z)	= yy(ioh_kpp)

      return
      end subroutine cbmz_v02r05_mapconcs                                


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r05_maprates(   &
          rk_m1,   &
          rk_m2,   &
          rk_m3,   &
          rk_m4,   &
          rconst )
!
!   maps reaction rate constants (host code array --> kpp rconst array)
!   for mechanism-version-regime = cbmz_v02r05
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     all are host-code reaction-rate-constant arrays [input]
      real rk_m1(*)
      real rk_m2(*)
      real rk_m3(*)
      real rk_m4(*)
      real rconst(nreact_kppmax)

!   local variables
      integer i

      do i = 1, nreact_kppmax
          rconst(i) = 0.
      end do


      rconst(1) = (rk_m1(1))
      rconst(2) = (rk_m1(2))
      rconst(3) = (rk_m1(3))
      rconst(4) = (rk_m1(4))
      rconst(5) = (rk_m1(5))
      rconst(6) = (rk_m1(6))
      rconst(7) = (rk_m1(7))
      rconst(8) = (rk_m1(8))
      rconst(9) = (rk_m1(9))
      rconst(10) = (rk_m1(10))
      rconst(11) = (rk_m1(11))
      rconst(12) = (rk_m1(12))
      rconst(13) = (rk_m1(13))
      rconst(14) = (rk_m1(14))
      rconst(15) = (rk_m1(15))
      rconst(16) = (rk_m1(16))
      rconst(17) = (rk_m1(17))
      rconst(18) = (rk_m1(18))
      rconst(19) = (rk_m1(19))
      rconst(20) = (rk_m1(20))
      rconst(21) = (rk_m1(21))
      rconst(22) = (rk_m1(22))
      rconst(23) = (rk_m1(23))
      rconst(24) = (rk_m1(24))
      rconst(25) = (rk_m1(25))
      rconst(26) = (rk_m1(26))
      rconst(27) = (rk_m1(27))
      rconst(28) = (rk_m1(28))
      rconst(29) = (rk_m1(29))
      rconst(30) = (rk_m1(30))
      rconst(31) = (rk_m1(31))
      rconst(32) = (rk_m1(32))
      rconst(33) = (rk_m1(33))
      rconst(34) = (rk_m1(34))
      rconst(35) = (rk_m1(35))
      rconst(36) = (rk_m1(36))
      rconst(37) = (rk_m1(37))
      rconst(38) = (rk_m1(38))
      rconst(39) = (rk_m1(39))
      rconst(40) = (rk_m1(40))
      rconst(41) = (rk_m1(41))
      rconst(42) = (rk_m1(42))
      rconst(43) = (rk_m1(43))
      rconst(44) = (rk_m1(44))
      rconst(45) = (rk_m1(45))
      rconst(46) = (rk_m1(46))
      rconst(47) = (rk_m1(47))
      rconst(48) = (rk_m1(48))
      rconst(49) = (rk_m1(49))
      rconst(50) = (rk_m1(50))
      rconst(51) = (rk_m1(51))
      rconst(52) = (rk_m1(52))
      rconst(53) = (rk_m1(53))
      rconst(54) = (rk_m1(54))
      rconst(55) = (rk_m1(55))
      rconst(56) = (rk_m1(56))
      rconst(57) = (rk_m1(57))
      rconst(58) = (rk_m1(58))
      rconst(59) = (rk_m1(59))
      rconst(60) = (rk_m1(60))
      rconst(61) = (rk_m1(61))
      rconst(62) = (rk_m1(62))
      rconst(63) = (rk_m1(63))
      rconst(64) = (rk_m1(64))
      rconst(65) = (rk_m1(65))
      rconst(66) = (rk_m2(2))
      rconst(67) = (rk_m2(3))
      rconst(68) = (rk_m2(4))
      rconst(69) = (rk_m2(31))
      rconst(70) = (rk_m2(32))
      rconst(71) = (rk_m2(34))
      rconst(72) = (rk_m2(39))
      rconst(73) = (rk_m2(44))
      rconst(74) = (rk_m2(49))
      rconst(75) = (rk_m2(1))
      rconst(76) = (rk_m2(5))
      rconst(77) = (rk_m2(6))
      rconst(78) = (rk_m2(7))
      rconst(79) = (rk_m2(8))
      rconst(80) = (rk_m2(9))
      rconst(81) = (rk_m2(10))
      rconst(82) = (rk_m2(11))
      rconst(83) = (rk_m2(12))
      rconst(84) = (rk_m2(13))
      rconst(85) = (rk_m2(14))
      rconst(86) = (rk_m2(15))
      rconst(87) = (rk_m2(16))
      rconst(88) = (rk_m2(17))
      rconst(89) = (rk_m2(18))
      rconst(90) = (rk_m2(19))
      rconst(91) = (rk_m2(20))
      rconst(92) = (rk_m2(21))
      rconst(93) = (rk_m2(22))
      rconst(94) = (rk_m2(23))
      rconst(95) = (rk_m2(24))
      rconst(96) = (rk_m2(25))
      rconst(97) = (rk_m2(26))
      rconst(98) = (rk_m2(27))
      rconst(99) = (rk_m2(28))
      rconst(100) = (rk_m2(29))
      rconst(101) = (rk_m2(30))
      rconst(102) = (rk_m2(33))
      rconst(103) = (rk_m2(35))
      rconst(104) = (rk_m2(36))
      rconst(105) = (rk_m2(37))
      rconst(106) = (rk_m2(38))
      rconst(107) = (rk_m2(40))
      rconst(108) = (rk_m2(41))
      rconst(109) = (rk_m2(42))
      rconst(110) = (rk_m2(43))
      rconst(111) = (rk_m2(45))
      rconst(112) = (rk_m2(46))
      rconst(113) = (rk_m2(47))
      rconst(114) = (rk_m2(48))
      rconst(115) = (rk_m2(50))
      rconst(116) = (rk_m2(51))
      rconst(117) = (rk_m2(52))
      rconst(118) = (rk_m2(53))
      rconst(119) = (rk_m4(1))
      rconst(120) = (rk_m4(2))
      rconst(121) = (rk_m4(3))
      rconst(122) = (rk_m4(4))
      rconst(123) = (rk_m4(5))
      rconst(124) = (rk_m4(6))
      rconst(125) = (rk_m4(7))
      rconst(126) = (rk_m4(8))
      rconst(127) = (rk_m4(9))
      rconst(128) = (rk_m4(10))
      rconst(129) = (rk_m4(11))
      rconst(130) = (rk_m4(12))
      rconst(131) = (rk_m4(13))
      rconst(132) = (rk_m4(14))
      rconst(133) = (rk_m4(15))
      rconst(134) = (rk_m4(16))
      rconst(135) = (rk_m4(17))
      rconst(136) = (rk_m4(18))
      rconst(137) = (rk_m4(19))
      rconst(138) = (rk_m4(20))
      rconst(139) = (rk_m4(21))
      rconst(140) = (rk_m4(22))
      rconst(141) = (rk_m4(23))
      rconst(142) = (rk_m4(24))
      rconst(143) = (rk_m4(25))
      rconst(144) = (rk_m4(26))
      rconst(145) = (rk_m4(27))
      rconst(146) = (rk_m4(28))
      rconst(147) = (rk_m4(29))
      rconst(148) = (rk_m4(30))
      rconst(149) = (rk_m4(31))
      rconst(150) = (rk_m4(32))
      return
      end subroutine cbmz_v02r05_maprates 


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r05_dydt( nvardum, tdum, v, a_var, f, rconst )
!
!   computes rates of change for mechanism-version-regime = cbmz_v02r05
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     nvardum = number of variable species [input]
      integer nvardum
!     tdum = time [input]
      real tdum
!     v = concentrations of variable species [input]
      real v(nvar_r05_kpp)
!     a_var = dydt for each species [output]
      real a_var(nvar_r05_kpp)
!     f = concentrations of fixed species [input]
      real f(nfixed_kppmax)
!     rconst = reaction rate constants [input]
      real rconst(nreact_r05_kpp)

!   local variables
!     a = rate for each reaction
      real a(nreact_r05_kpp)

! computation of equation rates
      a(1) = rconst(1)*v(53)
      a(2) = rconst(2)*v(57)
      a(3) = rconst(3)*v(24)
      a(4) = rconst(4)*v(31)
      a(5) = rconst(5)*v(18)
      a(6) = rconst(6)*v(14)
      a(7) = rconst(7)*v(51)
      a(8) = rconst(8)*v(51)
      a(9) = rconst(9)*v(13)
      a(10) = rconst(10)*v(6)*f(4)
      a(11) = rconst(11)*v(6)*f(5)
      a(12) = rconst(12)*v(6)*f(2)
      a(13) = rconst(13)*v(43)*f(4)
      a(14) = rconst(14)*v(43)*v(51)
      a(15) = rconst(15)*v(43)*v(53)
      a(16) = rconst(16)*v(43)*v(53)
      a(17) = rconst(17)*v(43)*v(55)
      a(18) = rconst(18)*v(51)*v(55)
      a(19) = rconst(19)*v(51)*v(53)
      a(20) = rconst(20)*v(51)*v(59)
      a(21) = rconst(21)*v(51)*v(56)
      a(22) = rconst(22)*v(59)*f(3)
      a(23) = rconst(23)*v(55)*v(59)
      a(24) = rconst(24)*v(53)*v(59)
      a(25) = rconst(25)*v(57)*v(59)
      a(26) = rconst(26)*v(24)*v(59)
      a(27) = rconst(27)*v(31)*v(59)
      a(28) = rconst(28)*v(18)*v(59)
      a(29) = rconst(29)*v(56)*v(59)
      a(30) = rconst(30)*v(13)*v(59)
      a(31) = rconst(31)*v(56)*v(56)
      a(32) = rconst(32)*v(56)*v(56)*f(2)
      a(33) = rconst(33)*v(55)*v(56)
      a(34) = rconst(34)*v(53)*v(56)
      a(35) = rconst(35)*v(53)*v(56)
      a(36) = rconst(36)*v(18)
      a(37) = rconst(37)*v(55)*v(57)
      a(38) = rconst(38)*v(53)*v(57)
      a(39) = rconst(39)*v(53)*v(57)
      a(40) = rconst(40)*v(57)*v(57)
      a(41) = rconst(41)*v(56)*v(57)
      a(42) = rconst(42)*v(14)*f(2)
      a(43) = rconst(43)*v(14)
      a(44) = rconst(44)*v(28)*v(59)
      a(45) = rconst(45)*v(8)*v(59)
      a(46) = rconst(46)*v(59)*f(1)
      a(47) = rconst(47)*v(9)*v(59)
      a(48) = rconst(48)*v(23)*v(59)
      a(49) = rconst(49)*v(47)
      a(50) = rconst(50)*v(47)
      a(51) = rconst(51)*v(47)*v(59)
      a(52) = rconst(52)*v(47)*v(57)
      a(53) = rconst(53)*v(25)
      a(54) = rconst(54)*v(21)
      a(55) = rconst(55)*v(25)*v(59)
      a(56) = rconst(56)*v(21)*v(59)
      a(57) = rconst(57)*v(55)*v(58)
      a(58) = rconst(58)*v(41)*v(55)
      a(59) = rconst(59)*v(57)*v(58)
      a(60) = rconst(60)*v(41)*v(57)
      a(61) = rconst(61)*v(56)*v(58)
      a(62) = rconst(62)*v(41)*v(56)
      a(63) = rconst(63)*v(58)
      a(64) = rconst(64)*v(41)
      a(65) = rconst(65)*v(7)*v(59)
      a(66) = rconst(66)*v(45)
      a(67) = rconst(67)*v(45)*v(59)
      a(68) = rconst(68)*v(45)*v(57)
      a(69) = rconst(69)*v(46)*v(53)
      a(70) = rconst(70)*v(10)
      a(71) = rconst(71)*v(46)*v(55)
      a(72) = rconst(72)*v(46)*v(57)
      a(73) = rconst(73)*v(46)*v(56)
      a(74) = rconst(74)*v(46)
      a(75) = rconst(75)*v(16)*v(59)
      a(76) = rconst(76)*v(35)
      a(77) = rconst(77)*v(35)*v(59)
      a(78) = rconst(78)*v(40)
      a(79) = rconst(79)*v(40)*v(59)
      a(80) = rconst(80)*v(40)*v(57)
      a(81) = rconst(81)*v(22)*v(51)
      a(82) = rconst(82)*v(22)*v(59)
      a(83) = rconst(83)*v(36)*v(51)
      a(84) = rconst(84)*v(39)*v(51)
      a(85) = rconst(85)*v(36)*v(59)
      a(86) = rconst(86)*v(39)*v(59)
      a(87) = rconst(87)*v(36)*v(57)
      a(88) = rconst(88)*v(39)*v(57)
      a(89) = rconst(89)*v(12)*v(59)
      a(90) = rconst(90)*v(15)*v(59)
      a(91) = rconst(91)*v(19)*v(55)
      a(92) = rconst(92)*v(26)*v(59)
      a(93) = rconst(93)*v(26)*v(57)
      a(94) = rconst(94)*v(17)*v(53)
      a(95) = rconst(95)*v(32)*v(59)
      a(96) = rconst(96)*v(32)
      a(97) = rconst(97)*v(32)*v(51)
      a(98) = rconst(98)*v(33)
      a(99) = rconst(99)*v(33)*v(59)
      a(100) = rconst(100)*v(50)*v(59)
      a(101) = rconst(101)*v(50)
      a(102) = rconst(102)*v(49)*v(55)
      a(103) = rconst(103)*v(48)*v(55)
      a(104) = rconst(104)*v(44)*v(55)
      a(105) = rconst(105)*v(37)*v(55)
      a(106) = rconst(106)*v(49)*v(57)
      a(107) = rconst(107)*v(48)*v(57)
      a(108) = rconst(108)*v(44)*v(57)
      a(109) = rconst(109)*v(37)*v(57)
      a(110) = rconst(110)*v(49)*v(56)
      a(111) = rconst(111)*v(48)*v(56)
      a(112) = rconst(112)*v(44)*v(56)
      a(113) = rconst(113)*v(37)*v(56)
      a(114) = rconst(114)*v(49)
      a(115) = rconst(115)*v(48)
      a(116) = rconst(116)*v(44)
      a(117) = rconst(117)*v(37)
      a(118) = rconst(118)*v(16)*v(27)
      a(119) = rconst(119)*v(34)*v(59)
      a(120) = rconst(120)*v(34)*v(57)
      a(121) = rconst(121)*v(34)*v(43)
      a(122) = rconst(122)*v(34)*v(59)
      a(123) = rconst(123)*v(38)*v(55)
      a(124) = rconst(124)*v(38)*v(58)
      a(125) = rconst(125)*v(38)*v(54)
      a(126) = rconst(126)*v(38)*v(38)
      a(127) = rconst(127)*v(20)*v(59)
      a(128) = rconst(128)*v(11)*v(59)
      a(129) = rconst(129)*v(30)*v(55)
      a(130) = rconst(130)*v(30)*v(58)
      a(131) = rconst(131)*v(42)*v(56)
      a(132) = rconst(132)*v(42)*v(57)
      a(133) = rconst(133)*v(42)*v(58)
      a(134) = rconst(134)*v(42)*v(59)
      a(135) = rconst(135)*v(42)*v(52)
      a(136) = rconst(136)*v(54)
      a(137) = rconst(137)*v(53)*v(54)
      a(138) = rconst(138)*v(51)*v(54)
      a(139) = rconst(139)*v(54)*v(56)
      a(140) = rconst(140)*v(54)*v(58)
      a(141) = rconst(141)*v(54)*v(59)
      a(142) = rconst(142)*v(54)*f(4)
      a(143) = rconst(143)*v(29)
      a(144) = rconst(144)*v(29)*v(55)
      a(145) = rconst(145)*v(29)*v(58)
      a(146) = rconst(146)*v(52)
      a(147) = rconst(147)*v(52)*v(53)
      a(148) = rconst(148)*v(52)*v(55)
      a(149) = rconst(149)*v(52)*v(56)
      a(150) = rconst(150)*v(47)*v(52)

! aggregate function
      a_var(1) = a(45)+a(146)
      a_var(2) = 0.52*a(81)+0.22*a(83)
      a_var(3) = 0.4*a(73)+0.09*a(83)+0.16*a(84)
      a_var(4) = a(135)+a(141)+a(147)+a(148)+a(149)+a(150)
      a_var(5) = 0.15*a(126)
      a_var(6) = a(8)-a(10)-a(11)-a(12)
      a_var(7) = -a(65)
      a_var(8) = -a(45)+a(136)
      a_var(9) = -a(47)+0.2*a(64)
      a_var(10) = a(69)-a(70)
      a_var(11) = 0.27*a(127)-a(128)
      a_var(12) = -a(89)
      a_var(13) = -a(9)-a(30)+a(31)+a(32)+a(131)
      a_var(14) = -a(6)+a(39)-a(42)-a(43)
      a_var(15) = -a(90)
      a_var(16) = -a(75)+1.1*a(90)-a(118)
      a_var(17) = 0.4*a(92)+a(93)-a(94)
      a_var(18) = -a(5)-a(28)+a(34)-a(36)
      a_var(19) = 0.8*a(89)+0.45*a(90)-a(91)
      a_var(20) = 0.965*a(122)-a(127)
      a_var(21) = -a(54)-a(56)+a(62)
      a_var(22) = -a(81)-a(82)
      a_var(23) = -a(48)+0.34*a(63)+0.03*a(83)+0.04*a(84)
      a_var(24) = -a(3)+a(23)-a(26)+a(35)+a(148)
      a_var(25) = -a(53)-a(55)+a(61)+a(133)
      a_var(26) = 0.12*a(89)+0.05*a(90)-a(92)-a(93)
      a_var(27) = 1.06*a(83)+2.26*a(84)+a(85)+2.23*a(86)+1.98*a(98)   &
                 +0.42*a(99)+1.98*a(101)+1.68*a(102)+a(104)+1.98   &
                 *a(106)+a(108)+1.25*a(114)+a(116)-a(118)
      a_var(28) = -a(44)+a(49)+a(50)+a(51)+a(52)+a(66)+a(78)+a(80)   &
                 +0.24*a(81)+0.31*a(83)+0.3*a(84)+2*a(95)+a(96)+0.69   &
                 *a(97)+a(150)
      a_var(29) = a(142)-a(143)-a(144)-a(145)
      a_var(30) = a(128)-a(129)-a(130)
      a_var(31) = -a(4)+a(24)-a(27)+0.3*a(41)+2*a(42)+a(52)+a(68)   &
                 +a(80)+a(93)+a(120)+a(132)+a(147)
      a_var(32) = 0.95*a(91)+0.3*a(92)-a(95)-a(96)-a(97)
      a_var(33) = -a(98)-a(99)+a(110)+a(111)
      a_var(34) = -a(119)-a(120)-a(121)-a(122)
      a_var(35) = -a(76)-a(77)+0.07*a(84)+0.23*a(86)+0.74*a(98)+0.74   &
                 *a(101)+0.62*a(102)+0.74*a(106)+0.57*a(114)+0.15   &
                 *a(115)
      a_var(36) = -a(83)-a(85)-a(87)
      a_var(37) = a(79)+a(82)+a(85)+a(86)+0.08*a(89)+0.5*a(90)+0.6   &
                 *a(92)+a(95)+0.03*a(97)+0.4*a(98)+0.4*a(101)+0.34   &
                 *a(102)-a(105)+0.4*a(106)-a(109)-a(113)+0.24*a(114)   &
                 -a(117)
      a_var(38) = a(119)+a(120)-a(123)-a(124)-a(125)-2*a(126)
      a_var(39) = -a(84)-a(86)-a(88)
      a_var(40) = -a(78)-a(79)-a(80)+0.04*a(83)+0.07*a(84)+0.8*a(90)   &
                 +0.2*a(97)+0.19*a(99)+0.15*a(115)
      a_var(41) = a(47)+0.5*a(56)-a(58)-a(60)-a(62)-a(64)+0.06*a(83)   &
                 +0.05*a(84)+0.1*a(98)+0.1*a(101)+0.08*a(102)+0.1   &
                 *a(106)+0.06*a(114)
      a_var(42) = 0.73*a(127)-a(131)-a(132)-a(133)-a(134)-a(135)
      a_var(43) = a(1)+0.89*a(2)+a(7)+a(10)+a(11)-a(13)-a(14)-a(15)   &
                 -a(16)-a(17)-a(121)
      a_var(44) = a(87)+a(88)+a(100)-a(104)-a(108)-a(112)-a(116)
      a_var(45) = a(54)+0.5*a(56)+a(58)+a(60)+0.8*a(64)+a(65)-a(66)   &
                 -a(67)-a(68)+0.22*a(82)+0.47*a(83)+1.03*a(84)+a(85)   &
                 +1.77*a(86)+0.03*a(97)+0.3*a(98)+0.04*a(99)+0.3   &
                 *a(101)+0.25*a(102)+0.5*a(104)+0.3*a(106)+0.5*a(108)   &
                 +0.21*a(114)+0.5*a(116)
      a_var(46) = a(67)+a(68)-a(69)+a(70)-a(71)-a(72)-a(73)-a(74)   &
                 +a(76)+a(78)+a(79)+a(80)+0.13*a(83)+0.19*a(84)+a(95)   &
                 +a(96)+0.62*a(97)+a(103)+a(107)+0.7*a(115)
      a_var(47) = a(48)-a(49)-a(50)-a(51)-a(52)+a(53)+0.3*a(55)+a(57)   &
                 +a(59)+0.66*a(63)+a(81)+1.56*a(82)+0.57*a(83)+a(85)   &
                 +a(95)+0.7*a(97)+a(103)+0.5*a(104)+a(107)+0.5*a(108)   &
                 +0.7*a(115)+0.5*a(116)+a(123)+2*a(124)+a(125)+a(129)   &
                 +2*a(130)+a(140)+a(145)-a(150)
      a_var(48) = a(77)+0.11*a(84)-a(103)-a(107)-a(111)-a(115)
      a_var(49) = a(75)+0.03*a(83)+0.09*a(84)+0.77*a(99)-a(102)-a(106)   &
                 -a(110)-a(114)
      a_var(50) = 0.05*a(91)+a(94)-a(100)-a(101)+0.16*a(102)+0.5   &
                 *a(104)+0.5*a(108)+a(112)+0.5*a(116)
      a_var(51) = -a(7)-a(8)+a(13)-a(14)-a(18)-a(19)-a(20)-a(21)+0.4   &
                 *a(73)-a(81)-a(83)-a(84)-a(97)-a(138)
      a_var(52) = a(125)-a(135)+a(137)+a(138)+a(139)+a(140)+a(144)   &
                 +a(145)-a(146)-a(147)-a(148)-a(149)-a(150)
      a_var(53) = -a(1)+0.89*a(2)+a(4)+a(5)+a(6)-a(15)-a(16)+a(17)   &
                 +a(18)-a(19)-a(24)+a(25)+a(26)+a(28)+a(33)-a(34)   &
                 -a(35)+a(36)+2*a(37)-a(39)+2*a(40)+0.7*a(41)+a(43)   &
                 +a(57)+a(58)+a(59)+a(60)-a(69)+a(70)+a(71)+a(72)+0.95   &
                 *a(91)-a(94)+a(101)+0.84*a(102)+a(103)+1.5*a(104)   &
                 +a(105)+a(106)+a(107)+1.5*a(108)+a(109)+0.5*a(116)   &
                 +a(123)+a(129)-a(137)+a(144)-a(147)
      a_var(54) = a(121)+0.035*a(122)+a(123)+a(124)+1.85*a(126)+a(129)   &
                 +a(130)+a(131)+a(132)+a(133)+a(134)+a(135)-a(136)   &
                 -a(137)-a(138)-a(139)-a(140)-a(141)-a(142)+a(143)
      a_var(55) = a(1)+0.11*a(2)+a(3)+a(15)-a(17)-a(18)-a(23)-a(33)   &
                 -a(37)+a(38)-a(57)-a(58)-a(71)-a(91)-a(102)-a(103)   &
                 -a(104)-a(105)-a(123)-a(129)+a(137)-a(144)-a(148)
      a_var(56) = a(5)+a(20)-a(21)+a(22)+a(25)-a(29)+a(30)-2*a(31)-2   &
                 *a(32)-a(33)-a(34)-a(35)+a(36)-a(41)+a(44)+a(45)   &
                 +a(48)+2*a(49)+a(51)+a(52)+a(53)+a(54)+a(57)+a(58)   &
                 +a(59)+a(60)-a(61)-a(62)+0.32*a(63)+0.6*a(64)+a(65)   &
                 +a(66)-a(73)+a(78)+0.22*a(81)+a(82)+0.26*a(83)+0.22   &
                 *a(84)+a(85)+a(86)+0.2*a(89)+0.55*a(90)+0.95*a(91)   &
                 +0.6*a(92)+2*a(95)+a(96)+0.76*a(97)+0.9*a(98)+0.9   &
                 *a(101)+0.76*a(102)+0.5*a(104)+0.9*a(106)+0.5*a(108)   &
                 -a(110)-a(111)-a(112)-a(113)+0.54*a(114)+0.965*a(122)   &
                 +a(124)+0.27*a(127)+a(130)-a(131)-a(139)+a(140)   &
                 +a(145)-a(149)+a(150)
      a_var(57) = -a(2)+a(6)+a(16)+a(19)-a(25)+a(27)-a(37)-a(38)-a(39)   &
                 -2*a(40)-a(41)+a(43)-a(52)-a(59)-a(60)-a(68)-a(72)   &
                 -a(80)-a(87)-a(88)-a(93)-a(106)-a(107)-a(108)-a(109)   &
                 -a(120)-a(132)
      a_var(58) = a(46)+0.7*a(55)-a(57)-a(59)-a(61)-a(63)+a(66)+a(71)   &
                 +a(72)+a(74)+a(76)+0.07*a(83)+0.1*a(84)+a(121)+0.035   &
                 *a(122)-a(124)+0.73*a(127)-a(130)-a(133)+a(136)   &
                 -a(140)-a(145)+a(146)
      a_var(59) = a(3)+a(4)+2*a(9)+2*a(12)-a(20)+a(21)-a(22)-a(23)   &
                 -a(24)-a(25)-a(26)-a(27)-a(28)-a(29)-a(30)+a(33)+0.7   &
                 *a(41)-a(44)-a(45)-a(46)-a(47)-a(48)-a(51)+a(53)   &
                 +a(54)-0.7*a(55)-0.5*a(56)-a(65)-a(67)-a(75)-a(77)   &
                 -a(79)+0.12*a(81)-a(82)+0.33*a(83)+0.6*a(84)-a(85)   &
                 -a(86)-a(89)-a(90)-a(92)-a(95)+0.08*a(97)+a(98)-0.77   &
                 *a(99)-a(100)-a(119)-a(122)-a(127)-a(128)-a(134)   &
                 +a(139)-a(141)
      return
      end subroutine cbmz_v02r05_dydt                                      


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r05_jacob( nvardum, tdum, v, jvs, f, rconst )
!
!   computes jacobian for mechanism-version-regime = cbmz_v02r05
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     nvardum = number of variable species [input]
      integer nvardum
!     tdum = time [input]
      real tdum
!     v = concentrations of variable species [input]
      real v(nvar_r05_kpp)
!     jvs = non-zero jacobian elements [output]
      real jvs(lu_nonzero_v_r05_kpp)
!     f = concentrations of fixed species [input]
      real f(nfixed_kppmax)
!     rconst = reaction rate constants [input]
      real rconst(nreact_r05_kpp)

!   local variables
!     b(i,j) = d[reaction_rate(i)] / d[species_conc(j)]
      real b(nreact_r05_kpp,nvar_r05_kpp)

! computation of b(i,j) = da(i)/dv(j)
      b(1,53) = rconst(1)
      b(2,57) = rconst(2)
      b(3,24) = rconst(3)
      b(4,31) = rconst(4)
      b(5,18) = rconst(5)
      b(6,14) = rconst(6)
      b(7,51) = rconst(7)
      b(8,51) = rconst(8)
      b(9,13) = rconst(9)
      b(10,6) = rconst(10)*f(4)
      b(11,6) = rconst(11)*f(5)
      b(12,6) = rconst(12)*f(2)
      b(13,43) = rconst(13)*f(4)
      b(14,43) = rconst(14)*v(51)
      b(14,51) = rconst(14)*v(43)
      b(15,43) = rconst(15)*v(53)
      b(15,53) = rconst(15)*v(43)
      b(16,43) = rconst(16)*v(53)
      b(16,53) = rconst(16)*v(43)
      b(17,43) = rconst(17)*v(55)
      b(17,55) = rconst(17)*v(43)
      b(18,51) = rconst(18)*v(55)
      b(18,55) = rconst(18)*v(51)
      b(19,51) = rconst(19)*v(53)
      b(19,53) = rconst(19)*v(51)
      b(20,51) = rconst(20)*v(59)
      b(20,59) = rconst(20)*v(51)
      b(21,51) = rconst(21)*v(56)
      b(21,56) = rconst(21)*v(51)
      b(22,59) = rconst(22)*f(3)
      b(23,55) = rconst(23)*v(59)
      b(23,59) = rconst(23)*v(55)
      b(24,53) = rconst(24)*v(59)
      b(24,59) = rconst(24)*v(53)
      b(25,57) = rconst(25)*v(59)
      b(25,59) = rconst(25)*v(57)
      b(26,24) = rconst(26)*v(59)
      b(26,59) = rconst(26)*v(24)
      b(27,31) = rconst(27)*v(59)
      b(27,59) = rconst(27)*v(31)
      b(28,18) = rconst(28)*v(59)
      b(28,59) = rconst(28)*v(18)
      b(29,56) = rconst(29)*v(59)
      b(29,59) = rconst(29)*v(56)
      b(30,13) = rconst(30)*v(59)
      b(30,59) = rconst(30)*v(13)
      b(31,56) = rconst(31)*2*v(56)
      b(32,56) = rconst(32)*2*v(56)*f(2)
      b(33,55) = rconst(33)*v(56)
      b(33,56) = rconst(33)*v(55)
      b(34,53) = rconst(34)*v(56)
      b(34,56) = rconst(34)*v(53)
      b(35,53) = rconst(35)*v(56)
      b(35,56) = rconst(35)*v(53)
      b(36,18) = rconst(36)
      b(37,55) = rconst(37)*v(57)
      b(37,57) = rconst(37)*v(55)
      b(38,53) = rconst(38)*v(57)
      b(38,57) = rconst(38)*v(53)
      b(39,53) = rconst(39)*v(57)
      b(39,57) = rconst(39)*v(53)
      b(40,57) = rconst(40)*2*v(57)
      b(41,56) = rconst(41)*v(57)
      b(41,57) = rconst(41)*v(56)
      b(42,14) = rconst(42)*f(2)
      b(43,14) = rconst(43)
      b(44,28) = rconst(44)*v(59)
      b(44,59) = rconst(44)*v(28)
      b(45,8) = rconst(45)*v(59)
      b(45,59) = rconst(45)*v(8)
      b(46,59) = rconst(46)*f(1)
      b(47,9) = rconst(47)*v(59)
      b(47,59) = rconst(47)*v(9)
      b(48,23) = rconst(48)*v(59)
      b(48,59) = rconst(48)*v(23)
      b(49,47) = rconst(49)
      b(50,47) = rconst(50)
      b(51,47) = rconst(51)*v(59)
      b(51,59) = rconst(51)*v(47)
      b(52,47) = rconst(52)*v(57)
      b(52,57) = rconst(52)*v(47)
      b(53,25) = rconst(53)
      b(54,21) = rconst(54)
      b(55,25) = rconst(55)*v(59)
      b(55,59) = rconst(55)*v(25)
      b(56,21) = rconst(56)*v(59)
      b(56,59) = rconst(56)*v(21)
      b(57,55) = rconst(57)*v(58)
      b(57,58) = rconst(57)*v(55)
      b(58,41) = rconst(58)*v(55)
      b(58,55) = rconst(58)*v(41)
      b(59,57) = rconst(59)*v(58)
      b(59,58) = rconst(59)*v(57)
      b(60,41) = rconst(60)*v(57)
      b(60,57) = rconst(60)*v(41)
      b(61,56) = rconst(61)*v(58)
      b(61,58) = rconst(61)*v(56)
      b(62,41) = rconst(62)*v(56)
      b(62,56) = rconst(62)*v(41)
      b(63,58) = rconst(63)
      b(64,41) = rconst(64)
      b(65,7) = rconst(65)*v(59)
      b(65,59) = rconst(65)*v(7)
      b(66,45) = rconst(66)
      b(67,45) = rconst(67)*v(59)
      b(67,59) = rconst(67)*v(45)
      b(68,45) = rconst(68)*v(57)
      b(68,57) = rconst(68)*v(45)
      b(69,46) = rconst(69)*v(53)
      b(69,53) = rconst(69)*v(46)
      b(70,10) = rconst(70)
      b(71,46) = rconst(71)*v(55)
      b(71,55) = rconst(71)*v(46)
      b(72,46) = rconst(72)*v(57)
      b(72,57) = rconst(72)*v(46)
      b(73,46) = rconst(73)*v(56)
      b(73,56) = rconst(73)*v(46)
      b(74,46) = rconst(74)
      b(75,16) = rconst(75)*v(59)
      b(75,59) = rconst(75)*v(16)
      b(76,35) = rconst(76)
      b(77,35) = rconst(77)*v(59)
      b(77,59) = rconst(77)*v(35)
      b(78,40) = rconst(78)
      b(79,40) = rconst(79)*v(59)
      b(79,59) = rconst(79)*v(40)
      b(80,40) = rconst(80)*v(57)
      b(80,57) = rconst(80)*v(40)
      b(81,22) = rconst(81)*v(51)
      b(81,51) = rconst(81)*v(22)
      b(82,22) = rconst(82)*v(59)
      b(82,59) = rconst(82)*v(22)
      b(83,36) = rconst(83)*v(51)
      b(83,51) = rconst(83)*v(36)
      b(84,39) = rconst(84)*v(51)
      b(84,51) = rconst(84)*v(39)
      b(85,36) = rconst(85)*v(59)
      b(85,59) = rconst(85)*v(36)
      b(86,39) = rconst(86)*v(59)
      b(86,59) = rconst(86)*v(39)
      b(87,36) = rconst(87)*v(57)
      b(87,57) = rconst(87)*v(36)
      b(88,39) = rconst(88)*v(57)
      b(88,57) = rconst(88)*v(39)
      b(89,12) = rconst(89)*v(59)
      b(89,59) = rconst(89)*v(12)
      b(90,15) = rconst(90)*v(59)
      b(90,59) = rconst(90)*v(15)
      b(91,19) = rconst(91)*v(55)
      b(91,55) = rconst(91)*v(19)
      b(92,26) = rconst(92)*v(59)
      b(92,59) = rconst(92)*v(26)
      b(93,26) = rconst(93)*v(57)
      b(93,57) = rconst(93)*v(26)
      b(94,17) = rconst(94)*v(53)
      b(94,53) = rconst(94)*v(17)
      b(95,32) = rconst(95)*v(59)
      b(95,59) = rconst(95)*v(32)
      b(96,32) = rconst(96)
      b(97,32) = rconst(97)*v(51)
      b(97,51) = rconst(97)*v(32)
      b(98,33) = rconst(98)
      b(99,33) = rconst(99)*v(59)
      b(99,59) = rconst(99)*v(33)
      b(100,50) = rconst(100)*v(59)
      b(100,59) = rconst(100)*v(50)
      b(101,50) = rconst(101)
      b(102,49) = rconst(102)*v(55)
      b(102,55) = rconst(102)*v(49)
      b(103,48) = rconst(103)*v(55)
      b(103,55) = rconst(103)*v(48)
      b(104,44) = rconst(104)*v(55)
      b(104,55) = rconst(104)*v(44)
      b(105,37) = rconst(105)*v(55)
      b(105,55) = rconst(105)*v(37)
      b(106,49) = rconst(106)*v(57)
      b(106,57) = rconst(106)*v(49)
      b(107,48) = rconst(107)*v(57)
      b(107,57) = rconst(107)*v(48)
      b(108,44) = rconst(108)*v(57)
      b(108,57) = rconst(108)*v(44)
      b(109,37) = rconst(109)*v(57)
      b(109,57) = rconst(109)*v(37)
      b(110,49) = rconst(110)*v(56)
      b(110,56) = rconst(110)*v(49)
      b(111,48) = rconst(111)*v(56)
      b(111,56) = rconst(111)*v(48)
      b(112,44) = rconst(112)*v(56)
      b(112,56) = rconst(112)*v(44)
      b(113,37) = rconst(113)*v(56)
      b(113,56) = rconst(113)*v(37)
      b(114,49) = rconst(114)
      b(115,48) = rconst(115)
      b(116,44) = rconst(116)
      b(117,37) = rconst(117)
      b(118,16) = rconst(118)*v(27)
      b(118,27) = rconst(118)*v(16)
      b(119,34) = rconst(119)*v(59)
      b(119,59) = rconst(119)*v(34)
      b(120,34) = rconst(120)*v(57)
      b(120,57) = rconst(120)*v(34)
      b(121,34) = rconst(121)*v(43)
      b(121,43) = rconst(121)*v(34)
      b(122,34) = rconst(122)*v(59)
      b(122,59) = rconst(122)*v(34)
      b(123,38) = rconst(123)*v(55)
      b(123,55) = rconst(123)*v(38)
      b(124,38) = rconst(124)*v(58)
      b(124,58) = rconst(124)*v(38)
      b(125,38) = rconst(125)*v(54)
      b(125,54) = rconst(125)*v(38)
      b(126,38) = rconst(126)*2*v(38)
      b(127,20) = rconst(127)*v(59)
      b(127,59) = rconst(127)*v(20)
      b(128,11) = rconst(128)*v(59)
      b(128,59) = rconst(128)*v(11)
      b(129,30) = rconst(129)*v(55)
      b(129,55) = rconst(129)*v(30)
      b(130,30) = rconst(130)*v(58)
      b(130,58) = rconst(130)*v(30)
      b(131,42) = rconst(131)*v(56)
      b(131,56) = rconst(131)*v(42)
      b(132,42) = rconst(132)*v(57)
      b(132,57) = rconst(132)*v(42)
      b(133,42) = rconst(133)*v(58)
      b(133,58) = rconst(133)*v(42)
      b(134,42) = rconst(134)*v(59)
      b(134,59) = rconst(134)*v(42)
      b(135,42) = rconst(135)*v(52)
      b(135,52) = rconst(135)*v(42)
      b(136,54) = rconst(136)
      b(137,53) = rconst(137)*v(54)
      b(137,54) = rconst(137)*v(53)
      b(138,51) = rconst(138)*v(54)
      b(138,54) = rconst(138)*v(51)
      b(139,54) = rconst(139)*v(56)
      b(139,56) = rconst(139)*v(54)
      b(140,54) = rconst(140)*v(58)
      b(140,58) = rconst(140)*v(54)
      b(141,54) = rconst(141)*v(59)
      b(141,59) = rconst(141)*v(54)
      b(142,54) = rconst(142)*f(4)
      b(143,29) = rconst(143)
      b(144,29) = rconst(144)*v(55)
      b(144,55) = rconst(144)*v(29)
      b(145,29) = rconst(145)*v(58)
      b(145,58) = rconst(145)*v(29)
      b(146,52) = rconst(146)
      b(147,52) = rconst(147)*v(53)
      b(147,53) = rconst(147)*v(52)
      b(148,52) = rconst(148)*v(55)
      b(148,55) = rconst(148)*v(52)
      b(149,52) = rconst(149)*v(56)
      b(149,56) = rconst(149)*v(52)
      b(150,47) = rconst(150)*v(52)
      b(150,52) = rconst(150)*v(47)

! construct the jacobian terms from b's
      jvs(1) = 0
      jvs(2) = b(45,8)
      jvs(3) = b(146,52)
      jvs(4) = b(45,59)
      jvs(5) = 0
      jvs(6) = 0.52*b(81,22)
      jvs(7) = 0.22*b(83,36)
      jvs(8) = 0.52*b(81,51)+0.22*b(83,51)
      jvs(9) = 0
      jvs(10) = 0.09*b(83,36)
      jvs(11) = 0.16*b(84,39)
      jvs(12) = 0.4*b(73,46)
      jvs(13) = 0.09*b(83,51)+0.16*b(84,51)
      jvs(14) = 0.4*b(73,56)
      jvs(15) = 0
      jvs(16) = b(135,42)
      jvs(17) = b(150,47)
      jvs(18) = b(135,52)+b(147,52)+b(148,52)+b(149,52)+b(150,52)
      jvs(19) = b(147,53)
      jvs(20) = b(141,54)
      jvs(21) = b(148,55)
      jvs(22) = b(149,56)
      jvs(23) = b(141,59)
      jvs(24) = 0
      jvs(25) = 0.15*b(126,38)
      jvs(26) = -b(10,6)-b(11,6)-b(12,6)
      jvs(27) = b(8,51)
      jvs(28) = -b(65,7)
      jvs(29) = -b(65,59)
      jvs(30) = -b(45,8)
      jvs(31) = b(136,54)
      jvs(32) = -b(45,59)
      jvs(33) = -b(47,9)
      jvs(34) = 0.2*b(64,41)
      jvs(35) = -b(47,59)
      jvs(36) = -b(70,10)
      jvs(37) = b(69,46)
      jvs(38) = b(69,53)
      jvs(39) = -b(128,11)
      jvs(40) = 0.27*b(127,20)
      jvs(41) = 0.27*b(127,59)-b(128,59)
      jvs(42) = -b(89,12)
      jvs(43) = -b(89,59)
      jvs(44) = -b(9,13)-b(30,13)
      jvs(45) = b(131,42)
      jvs(46) = b(31,56)+b(32,56)+b(131,56)
      jvs(47) = -b(30,59)
      jvs(48) = -b(6,14)-b(42,14)-b(43,14)
      jvs(49) = b(39,53)
      jvs(50) = b(39,57)
      jvs(51) = -b(90,15)
      jvs(52) = -b(90,59)
      jvs(53) = 1.1*b(90,15)
      jvs(54) = -b(75,16)-b(118,16)
      jvs(55) = -b(118,27)
      jvs(56) = -b(75,59)+1.1*b(90,59)
      jvs(57) = -b(94,17)
      jvs(58) = 0.4*b(92,26)+b(93,26)
      jvs(59) = -b(94,53)
      jvs(60) = b(93,57)
      jvs(61) = 0.4*b(92,59)
      jvs(62) = -b(5,18)-b(28,18)-b(36,18)
      jvs(63) = b(34,53)
      jvs(64) = b(34,56)
      jvs(65) = -b(28,59)
      jvs(66) = 0.8*b(89,12)
      jvs(67) = 0.45*b(90,15)
      jvs(68) = -b(91,19)
      jvs(69) = -b(91,55)
      jvs(70) = 0.8*b(89,59)+0.45*b(90,59)
      jvs(71) = -b(127,20)
      jvs(72) = 0.965*b(122,34)
      jvs(73) = 0.965*b(122,59)-b(127,59)
      jvs(74) = -b(54,21)-b(56,21)
      jvs(75) = b(62,41)
      jvs(76) = b(62,56)
      jvs(77) = -b(56,59)
      jvs(78) = -b(81,22)-b(82,22)
      jvs(79) = -b(81,51)
      jvs(80) = -b(82,59)
      jvs(81) = -b(48,23)
      jvs(82) = 0.03*b(83,36)
      jvs(83) = 0.04*b(84,39)
      jvs(84) = 0.03*b(83,51)+0.04*b(84,51)
      jvs(85) = 0.34*b(63,58)
      jvs(86) = -b(48,59)
      jvs(87) = -b(3,24)-b(26,24)
      jvs(88) = b(148,52)
      jvs(89) = b(35,53)
      jvs(90) = b(23,55)+b(148,55)
      jvs(91) = b(35,56)
      jvs(92) = b(23,59)-b(26,59)
      jvs(93) = -b(53,25)-b(55,25)
      jvs(94) = b(133,42)
      jvs(95) = b(61,56)
      jvs(96) = b(61,58)+b(133,58)
      jvs(97) = -b(55,59)
      jvs(98) = 0.12*b(89,12)
      jvs(99) = 0.05*b(90,15)
      jvs(100) = -b(92,26)-b(93,26)
      jvs(101) = -b(93,57)
      jvs(102) = 0.12*b(89,59)+0.05*b(90,59)-b(92,59)
      jvs(103) = -b(118,16)
      jvs(104) = -b(118,27)
      jvs(105) = 1.98*b(98,33)+0.42*b(99,33)
      jvs(106) = 1.06*b(83,36)+b(85,36)
      jvs(107) = 2.26*b(84,39)+2.23*b(86,39)
      jvs(108) = b(104,44)+b(108,44)+b(116,44)
      jvs(109) = 1.68*b(102,49)+1.98*b(106,49)+1.25*b(114,49)
      jvs(110) = 1.98*b(101,50)
      jvs(111) = 1.06*b(83,51)+2.26*b(84,51)
      jvs(112) = 1.68*b(102,55)+b(104,55)
      jvs(113) = 1.98*b(106,57)+b(108,57)
      jvs(114) = b(85,59)+2.23*b(86,59)+0.42*b(99,59)
      jvs(115) = 0.24*b(81,22)
      jvs(116) = -b(44,28)
      jvs(117) = 2*b(95,32)+b(96,32)+0.69*b(97,32)
      jvs(118) = 0.31*b(83,36)
      jvs(119) = 0.3*b(84,39)
      jvs(120) = b(78,40)+b(80,40)
      jvs(121) = b(66,45)
      jvs(122) = b(49,47)+b(50,47)+b(51,47)+b(52,47)+b(150,47)
      jvs(123) = 0.24*b(81,51)+0.31*b(83,51)+0.3*b(84,51)+0.69   &
                *b(97,51)
      jvs(124) = b(150,52)
      jvs(125) = b(52,57)+b(80,57)
      jvs(126) = -b(44,59)+b(51,59)+2*b(95,59)
      jvs(127) = -b(143,29)-b(144,29)-b(145,29)
      jvs(128) = b(142,54)
      jvs(129) = -b(144,55)
      jvs(130) = -b(145,58)
      jvs(131) = b(128,11)
      jvs(132) = 0
      jvs(133) = -b(129,30)-b(130,30)
      jvs(134) = 0
      jvs(135) = -b(129,55)
      jvs(136) = -b(130,58)
      jvs(137) = b(128,59)
      jvs(138) = 2*b(42,14)
      jvs(139) = b(93,26)
      jvs(140) = -b(4,31)-b(27,31)
      jvs(141) = b(120,34)
      jvs(142) = b(80,40)
      jvs(143) = b(132,42)
      jvs(144) = b(68,45)
      jvs(145) = b(52,47)
      jvs(146) = b(147,52)
      jvs(147) = b(24,53)+b(147,53)
      jvs(148) = 0.3*b(41,56)
      jvs(149) = 0.3*b(41,57)+b(52,57)+b(68,57)+b(80,57)+b(93,57)   &
                +b(120,57)+b(132,57)
      jvs(150) = b(24,59)-b(27,59)
      jvs(151) = 0.95*b(91,19)
      jvs(152) = 0.3*b(92,26)
      jvs(153) = -b(95,32)-b(96,32)-b(97,32)
      jvs(154) = -b(97,51)
      jvs(155) = 0.95*b(91,55)
      jvs(156) = 0
      jvs(157) = 0.3*b(92,59)-b(95,59)
      jvs(158) = -b(98,33)-b(99,33)
      jvs(159) = b(111,48)
      jvs(160) = b(110,49)
      jvs(161) = b(110,56)+b(111,56)
      jvs(162) = -b(99,59)
      jvs(163) = -b(119,34)-b(120,34)-b(121,34)-b(122,34)
      jvs(164) = -b(121,43)
      jvs(165) = -b(120,57)
      jvs(166) = -b(119,59)-b(122,59)
      jvs(167) = 0.74*b(98,33)
      jvs(168) = -b(76,35)-b(77,35)
      jvs(169) = 0.07*b(84,39)+0.23*b(86,39)
      jvs(170) = 0.15*b(115,48)
      jvs(171) = 0.62*b(102,49)+0.74*b(106,49)+0.57*b(114,49)
      jvs(172) = 0.74*b(101,50)
      jvs(173) = 0.07*b(84,51)
      jvs(174) = 0.62*b(102,55)
      jvs(175) = 0
      jvs(176) = 0.74*b(106,57)
      jvs(177) = -b(77,59)+0.23*b(86,59)
      jvs(178) = -b(83,36)-b(85,36)-b(87,36)
      jvs(179) = -b(83,51)
      jvs(180) = -b(87,57)
      jvs(181) = -b(85,59)
      jvs(182) = 0.08*b(89,12)
      jvs(183) = 0.5*b(90,15)
      jvs(184) = b(82,22)
      jvs(185) = 0.6*b(92,26)
      jvs(186) = b(95,32)+0.03*b(97,32)
      jvs(187) = 0.4*b(98,33)
      jvs(188) = b(85,36)
      jvs(189) = -b(105,37)-b(109,37)-b(113,37)-b(117,37)
      jvs(190) = b(86,39)
      jvs(191) = b(79,40)
      jvs(192) = 0
      jvs(193) = 0.34*b(102,49)+0.4*b(106,49)+0.24*b(114,49)
      jvs(194) = 0.4*b(101,50)
      jvs(195) = 0.03*b(97,51)
      jvs(196) = 0.34*b(102,55)-b(105,55)
      jvs(197) = -b(113,56)
      jvs(198) = 0.4*b(106,57)-b(109,57)
      jvs(199) = b(79,59)+b(82,59)+b(85,59)+b(86,59)+0.08*b(89,59)+0.5   &
                *b(90,59)+0.6*b(92,59)+b(95,59)
      jvs(200) = b(119,34)+b(120,34)
      jvs(201) = -b(123,38)-b(124,38)-b(125,38)-2*b(126,38)
      jvs(202) = 0
      jvs(203) = -b(125,54)
      jvs(204) = -b(123,55)
      jvs(205) = b(120,57)
      jvs(206) = -b(124,58)
      jvs(207) = b(119,59)
      jvs(208) = -b(84,39)-b(86,39)-b(88,39)
      jvs(209) = -b(84,51)
      jvs(210) = -b(88,57)
      jvs(211) = -b(86,59)
      jvs(212) = 0.8*b(90,15)
      jvs(213) = 0.2*b(97,32)
      jvs(214) = 0.19*b(99,33)
      jvs(215) = 0.04*b(83,36)
      jvs(216) = 0.07*b(84,39)
      jvs(217) = -b(78,40)-b(79,40)-b(80,40)
      jvs(218) = 0.15*b(115,48)
      jvs(219) = 0
      jvs(220) = 0.04*b(83,51)+0.07*b(84,51)+0.2*b(97,51)
      jvs(221) = 0
      jvs(222) = 0
      jvs(223) = -b(80,57)
      jvs(224) = -b(79,59)+0.8*b(90,59)+0.19*b(99,59)
      jvs(225) = b(47,9)
      jvs(226) = 0.5*b(56,21)
      jvs(227) = 0.1*b(98,33)
      jvs(228) = 0.06*b(83,36)
      jvs(229) = 0.05*b(84,39)
      jvs(230) = -b(58,41)-b(60,41)-b(62,41)-b(64,41)
      jvs(231) = 0
      jvs(232) = 0.08*b(102,49)+0.1*b(106,49)+0.06*b(114,49)
      jvs(233) = 0.1*b(101,50)
      jvs(234) = 0.06*b(83,51)+0.05*b(84,51)
      jvs(235) = -b(58,55)+0.08*b(102,55)
      jvs(236) = -b(62,56)
      jvs(237) = -b(60,57)+0.1*b(106,57)
      jvs(238) = b(47,59)+0.5*b(56,59)
      jvs(239) = 0.73*b(127,20)
      jvs(240) = 0
      jvs(241) = -b(131,42)-b(132,42)-b(133,42)-b(134,42)-b(135,42)
      jvs(242) = 0
      jvs(243) = -b(135,52)
      jvs(244) = -b(131,56)
      jvs(245) = -b(132,57)
      jvs(246) = -b(133,58)
      jvs(247) = 0.73*b(127,59)-b(134,59)
      jvs(248) = b(10,6)+b(11,6)
      jvs(249) = -b(121,34)
      jvs(250) = -b(13,43)-b(14,43)-b(15,43)-b(16,43)-b(17,43)   &
                -b(121,43)
      jvs(251) = b(7,51)-b(14,51)
      jvs(252) = b(1,53)-b(15,53)-b(16,53)
      jvs(253) = -b(17,55)
      jvs(254) = 0.89*b(2,57)
      jvs(255) = 0
      jvs(256) = b(87,36)
      jvs(257) = b(88,39)
      jvs(258) = -b(104,44)-b(108,44)-b(112,44)-b(116,44)
      jvs(259) = b(100,50)
      jvs(260) = 0
      jvs(261) = -b(104,55)
      jvs(262) = -b(112,56)
      jvs(263) = b(87,57)+b(88,57)-b(108,57)
      jvs(264) = b(100,59)
      jvs(265) = b(65,7)
      jvs(266) = b(54,21)+0.5*b(56,21)
      jvs(267) = 0.22*b(82,22)
      jvs(268) = 0.03*b(97,32)
      jvs(269) = 0.3*b(98,33)+0.04*b(99,33)
      jvs(270) = 0.47*b(83,36)+b(85,36)
      jvs(271) = 1.03*b(84,39)+1.77*b(86,39)
      jvs(272) = b(58,41)+b(60,41)+0.8*b(64,41)
      jvs(273) = 0.5*b(104,44)+0.5*b(108,44)+0.5*b(116,44)
      jvs(274) = -b(66,45)-b(67,45)-b(68,45)
      jvs(275) = 0
      jvs(276) = 0.25*b(102,49)+0.3*b(106,49)+0.21*b(114,49)
      jvs(277) = 0.3*b(101,50)
      jvs(278) = 0.47*b(83,51)+1.03*b(84,51)+0.03*b(97,51)
      jvs(279) = b(58,55)+0.25*b(102,55)+0.5*b(104,55)
      jvs(280) = 0
      jvs(281) = b(60,57)-b(68,57)+0.3*b(106,57)+0.5*b(108,57)
      jvs(282) = 0.5*b(56,59)+b(65,59)-b(67,59)+0.22*b(82,59)+b(85,59)   &
                +1.77*b(86,59)+0.04*b(99,59)
      jvs(283) = b(70,10)
      jvs(284) = b(95,32)+b(96,32)+0.62*b(97,32)
      jvs(285) = b(76,35)
      jvs(286) = 0.13*b(83,36)
      jvs(287) = 0.19*b(84,39)
      jvs(288) = b(78,40)+b(79,40)+b(80,40)
      jvs(289) = b(67,45)+b(68,45)
      jvs(290) = -b(69,46)-b(71,46)-b(72,46)-b(73,46)-b(74,46)
      jvs(291) = b(103,48)+b(107,48)+0.7*b(115,48)
      jvs(292) = 0
      jvs(293) = 0
      jvs(294) = 0.13*b(83,51)+0.19*b(84,51)+0.62*b(97,51)
      jvs(295) = -b(69,53)
      jvs(296) = -b(71,55)+b(103,55)
      jvs(297) = -b(73,56)
      jvs(298) = b(68,57)-b(72,57)+b(80,57)+b(107,57)
      jvs(299) = b(67,59)+b(79,59)+b(95,59)
      jvs(300) = b(81,22)+1.56*b(82,22)
      jvs(301) = b(48,23)
      jvs(302) = b(53,25)+0.3*b(55,25)
      jvs(303) = b(145,29)
      jvs(304) = b(129,30)+2*b(130,30)
      jvs(305) = b(95,32)+0.7*b(97,32)
      jvs(306) = 0
      jvs(307) = 0.57*b(83,36)+b(85,36)
      jvs(308) = b(123,38)+2*b(124,38)+b(125,38)
      jvs(309) = 0
      jvs(310) = 0
      jvs(311) = 0
      jvs(312) = 0.5*b(104,44)+0.5*b(108,44)+0.5*b(116,44)
      jvs(313) = -b(49,47)-b(50,47)-b(51,47)-b(52,47)-b(150,47)
      jvs(314) = b(103,48)+b(107,48)+0.7*b(115,48)
      jvs(315) = 0
      jvs(316) = b(81,51)+0.57*b(83,51)+0.7*b(97,51)
      jvs(317) = -b(150,52)
      jvs(318) = 0
      jvs(319) = b(125,54)+b(140,54)
      jvs(320) = b(57,55)+b(103,55)+0.5*b(104,55)+b(123,55)+b(129,55)
      jvs(321) = 0
      jvs(322) = -b(52,57)+b(59,57)+b(107,57)+0.5*b(108,57)
      jvs(323) = b(57,58)+b(59,58)+0.66*b(63,58)+2*b(124,58)+2   &
                *b(130,58)+b(140,58)+b(145,58)
      jvs(324) = b(48,59)-b(51,59)+0.3*b(55,59)+1.56*b(82,59)+b(85,59)   &
                +b(95,59)
      jvs(325) = b(77,35)
      jvs(326) = 0.11*b(84,39)
      jvs(327) = -b(103,48)-b(107,48)-b(111,48)-b(115,48)
      jvs(328) = 0
      jvs(329) = 0
      jvs(330) = 0.11*b(84,51)
      jvs(331) = -b(103,55)
      jvs(332) = -b(111,56)
      jvs(333) = -b(107,57)
      jvs(334) = b(77,59)
      jvs(335) = b(75,16)
      jvs(336) = 0
      jvs(337) = 0.77*b(99,33)
      jvs(338) = 0.03*b(83,36)
      jvs(339) = 0.09*b(84,39)
      jvs(340) = 0
      jvs(341) = 0
      jvs(342) = -b(102,49)-b(106,49)-b(110,49)-b(114,49)
      jvs(343) = 0
      jvs(344) = 0.03*b(83,51)+0.09*b(84,51)
      jvs(345) = -b(102,55)
      jvs(346) = -b(110,56)
      jvs(347) = -b(106,57)
      jvs(348) = b(75,59)+0.77*b(99,59)
      jvs(349) = b(94,17)
      jvs(350) = 0.05*b(91,19)
      jvs(351) = 0
      jvs(352) = 0.5*b(104,44)+0.5*b(108,44)+b(112,44)+0.5*b(116,44)
      jvs(353) = 0.16*b(102,49)
      jvs(354) = -b(100,50)-b(101,50)
      jvs(355) = 0
      jvs(356) = b(94,53)
      jvs(357) = 0.05*b(91,55)+0.16*b(102,55)+0.5*b(104,55)
      jvs(358) = b(112,56)
      jvs(359) = 0.5*b(108,57)
      jvs(360) = -b(100,59)
      jvs(361) = -b(81,22)
      jvs(362) = -b(97,32)
      jvs(363) = -b(83,36)
      jvs(364) = -b(84,39)
      jvs(365) = b(13,43)-b(14,43)
      jvs(366) = 0.4*b(73,46)
      jvs(367) = 0
      jvs(368) = 0
      jvs(369) = 0
      jvs(370) = -b(7,51)-b(8,51)-b(14,51)-b(18,51)-b(19,51)-b(20,51)   &
                -b(21,51)-b(81,51)-b(83,51)-b(84,51)-b(97,51)   &
                -b(138,51)
      jvs(371) = -b(19,53)
      jvs(372) = -b(138,54)
      jvs(373) = -b(18,55)
      jvs(374) = -b(21,56)+0.4*b(73,56)
      jvs(375) = 0
      jvs(376) = -b(20,59)
      jvs(377) = b(144,29)+b(145,29)
      jvs(378) = b(125,38)
      jvs(379) = -b(135,42)
      jvs(380) = 0
      jvs(381) = -b(150,47)
      jvs(382) = 0
      jvs(383) = 0
      jvs(384) = 0
      jvs(385) = b(138,51)
      jvs(386) = -b(135,52)-b(146,52)-b(147,52)-b(148,52)-b(149,52)   &
                -b(150,52)
      jvs(387) = b(137,53)-b(147,53)
      jvs(388) = b(125,54)+b(137,54)+b(138,54)+b(139,54)+b(140,54)
      jvs(389) = b(144,55)-b(148,55)
      jvs(390) = b(139,56)-b(149,56)
      jvs(391) = 0
      jvs(392) = b(140,58)+b(145,58)
      jvs(393) = 0
      jvs(394) = b(70,10)
      jvs(395) = b(6,14)+b(43,14)
      jvs(396) = -b(94,17)
      jvs(397) = b(5,18)+b(28,18)+b(36,18)
      jvs(398) = 0.95*b(91,19)
      jvs(399) = b(26,24)
      jvs(400) = 0
      jvs(401) = b(144,29)
      jvs(402) = b(129,30)
      jvs(403) = b(4,31)
      jvs(404) = 0
      jvs(405) = b(105,37)+b(109,37)
      jvs(406) = b(123,38)
      jvs(407) = 0
      jvs(408) = 0
      jvs(409) = b(58,41)+b(60,41)
      jvs(410) = 0
      jvs(411) = -b(15,43)-b(16,43)+b(17,43)
      jvs(412) = 1.5*b(104,44)+1.5*b(108,44)+0.5*b(116,44)
      jvs(413) = 0
      jvs(414) = -b(69,46)+b(71,46)+b(72,46)
      jvs(415) = 0
      jvs(416) = b(103,48)+b(107,48)
      jvs(417) = 0.84*b(102,49)+b(106,49)
      jvs(418) = b(101,50)
      jvs(419) = b(18,51)-b(19,51)
      jvs(420) = -b(147,52)
      jvs(421) = -b(1,53)-b(15,53)-b(16,53)-b(19,53)-b(24,53)-b(34,53)   &
                -b(35,53)-b(39,53)-b(69,53)-b(94,53)-b(137,53)   &
                -b(147,53)
      jvs(422) = -b(137,54)
      jvs(423) = b(17,55)+b(18,55)+b(33,55)+2*b(37,55)+b(57,55)   &
                +b(58,55)+b(71,55)+0.95*b(91,55)+0.84*b(102,55)   &
                +b(103,55)+1.5*b(104,55)+b(105,55)+b(123,55)+b(129,55)   &
                +b(144,55)
      jvs(424) = b(33,56)-b(34,56)-b(35,56)+0.7*b(41,56)
      jvs(425) = b(57,58)+b(59,58)
      jvs(426) = -b(24,59)+b(25,59)+b(26,59)+b(28,59)
      jvs(427) = b(143,29)
      jvs(428) = b(129,30)+b(130,30)
      jvs(429) = b(121,34)+0.035*b(122,34)
      jvs(430) = b(123,38)+b(124,38)+1.85*b(126,38)
      jvs(431) = b(131,42)+b(132,42)+b(133,42)+b(134,42)+b(135,42)
      jvs(432) = b(121,43)
      jvs(433) = -b(138,51)
      jvs(434) = b(135,52)
      jvs(435) = -b(137,53)
      jvs(436) = -b(136,54)-b(137,54)-b(138,54)-b(139,54)-b(140,54)   &
                -b(141,54)-b(142,54)
      jvs(437) = b(123,55)+b(129,55)
      jvs(438) = b(131,56)-b(139,56)
      jvs(439) = b(132,57)
      jvs(440) = b(124,58)+b(130,58)+b(133,58)-b(140,58)
      jvs(441) = 0.035*b(122,59)+b(134,59)-b(141,59)
      jvs(442) = -b(91,19)
      jvs(443) = b(3,24)
      jvs(444) = -b(144,29)
      jvs(445) = -b(129,30)
      jvs(446) = 0
      jvs(447) = -b(105,37)
      jvs(448) = -b(123,38)
      jvs(449) = 0
      jvs(450) = 0
      jvs(451) = -b(58,41)
      jvs(452) = b(15,43)-b(17,43)
      jvs(453) = -b(104,44)
      jvs(454) = -b(71,46)
      jvs(455) = -b(103,48)
      jvs(456) = -b(102,49)
      jvs(457) = 0
      jvs(458) = -b(18,51)
      jvs(459) = -b(148,52)
      jvs(460) = b(1,53)+b(15,53)+b(38,53)+b(137,53)
      jvs(461) = b(137,54)
      jvs(462) = -b(17,55)-b(18,55)-b(23,55)-b(33,55)-b(37,55)   &
                -b(57,55)-b(58,55)-b(71,55)-b(91,55)-b(102,55)   &
                -b(103,55)-b(104,55)-b(105,55)-b(123,55)-b(129,55)   &
                -b(144,55)-b(148,55)
      jvs(463) = -b(33,56)
      jvs(464) = 0.11*b(2,57)-b(37,57)+b(38,57)
      jvs(465) = -b(57,58)
      jvs(466) = -b(23,59)
      jvs(467) = b(65,7)
      jvs(468) = b(45,8)
      jvs(469) = 0.2*b(89,12)
      jvs(470) = b(30,13)
      jvs(471) = 0.55*b(90,15)
      jvs(472) = b(5,18)+b(36,18)
      jvs(473) = 0.95*b(91,19)
      jvs(474) = 0.27*b(127,20)
      jvs(475) = b(54,21)
      jvs(476) = 0.22*b(81,22)+b(82,22)
      jvs(477) = b(48,23)
      jvs(478) = b(53,25)
      jvs(479) = 0.6*b(92,26)
      jvs(480) = b(44,28)
      jvs(481) = b(145,29)
      jvs(482) = b(130,30)
      jvs(483) = 2*b(95,32)+b(96,32)+0.76*b(97,32)
      jvs(484) = 0.9*b(98,33)
      jvs(485) = 0.965*b(122,34)
      jvs(486) = 0.26*b(83,36)+b(85,36)
      jvs(487) = -b(113,37)
      jvs(488) = b(124,38)
      jvs(489) = 0.22*b(84,39)+b(86,39)
      jvs(490) = b(78,40)
      jvs(491) = b(58,41)+b(60,41)-b(62,41)+0.6*b(64,41)
      jvs(492) = -b(131,42)
      jvs(493) = 0
      jvs(494) = 0.5*b(104,44)+0.5*b(108,44)-b(112,44)
      jvs(495) = b(66,45)
      jvs(496) = -b(73,46)
      jvs(497) = 2*b(49,47)+b(51,47)+b(52,47)+b(150,47)
      jvs(498) = -b(111,48)
      jvs(499) = 0.76*b(102,49)+0.9*b(106,49)-b(110,49)+0.54*b(114,49)
      jvs(500) = 0.9*b(101,50)
      jvs(501) = b(20,51)-b(21,51)+0.22*b(81,51)+0.26*b(83,51)+0.22   &
                *b(84,51)+0.76*b(97,51)
      jvs(502) = -b(149,52)+b(150,52)
      jvs(503) = -b(34,53)-b(35,53)
      jvs(504) = -b(139,54)+b(140,54)
      jvs(505) = -b(33,55)+b(57,55)+b(58,55)+0.95*b(91,55)+0.76   &
                *b(102,55)+0.5*b(104,55)
      jvs(506) = -b(21,56)-b(29,56)-2*b(31,56)-2*b(32,56)-b(33,56)   &
                -b(34,56)-b(35,56)-b(41,56)-b(61,56)-b(62,56)-b(73,56)   &
                -b(110,56)-b(111,56)-b(112,56)-b(113,56)-b(131,56)   &
                -b(139,56)-b(149,56)
      jvs(507) = b(25,57)-b(41,57)+b(52,57)+b(59,57)+b(60,57)+0.9   &
                *b(106,57)+0.5*b(108,57)
      jvs(508) = b(57,58)+b(59,58)-b(61,58)+0.32*b(63,58)+b(124,58)   &
                +b(130,58)+b(140,58)+b(145,58)
      jvs(509) = b(20,59)+b(22,59)+b(25,59)-b(29,59)+b(30,59)+b(44,59)   &
                +b(45,59)+b(48,59)+b(51,59)+b(65,59)+b(82,59)+b(85,59)   &
                +b(86,59)+0.2*b(89,59)+0.55*b(90,59)+0.6*b(92,59)+2   &
                *b(95,59)+0.965*b(122,59)+0.27*b(127,59)
      jvs(510) = b(6,14)+b(43,14)
      jvs(511) = -b(93,26)
      jvs(512) = b(27,31)
      jvs(513) = -b(120,34)
      jvs(514) = -b(87,36)
      jvs(515) = -b(109,37)
      jvs(516) = -b(88,39)
      jvs(517) = -b(80,40)
      jvs(518) = -b(60,41)
      jvs(519) = -b(132,42)
      jvs(520) = b(16,43)
      jvs(521) = -b(108,44)
      jvs(522) = -b(68,45)
      jvs(523) = -b(72,46)
      jvs(524) = -b(52,47)
      jvs(525) = -b(107,48)
      jvs(526) = -b(106,49)
      jvs(527) = 0
      jvs(528) = b(19,51)
      jvs(529) = 0
      jvs(530) = b(16,53)+b(19,53)-b(38,53)-b(39,53)
      jvs(531) = 0
      jvs(532) = -b(37,55)
      jvs(533) = -b(41,56)
      jvs(534) = -b(2,57)-b(25,57)-b(37,57)-b(38,57)-b(39,57)-2   &
                *b(40,57)-b(41,57)-b(52,57)-b(59,57)-b(60,57)-b(68,57)   &
                -b(72,57)-b(80,57)-b(87,57)-b(88,57)-b(93,57)   &
                -b(106,57)-b(107,57)-b(108,57)-b(109,57)-b(120,57)   &
                -b(132,57)
      jvs(535) = -b(59,58)
      jvs(536) = -b(25,59)+b(27,59)
      jvs(537) = 0.73*b(127,20)
      jvs(538) = 0.7*b(55,25)
      jvs(539) = -b(145,29)
      jvs(540) = -b(130,30)
      jvs(541) = b(121,34)+0.035*b(122,34)
      jvs(542) = b(76,35)
      jvs(543) = 0.07*b(83,36)
      jvs(544) = -b(124,38)
      jvs(545) = 0.1*b(84,39)
      jvs(546) = -b(133,42)
      jvs(547) = b(121,43)
      jvs(548) = b(66,45)
      jvs(549) = b(71,46)+b(72,46)+b(74,46)
      jvs(550) = 0
      jvs(551) = 0
      jvs(552) = 0
      jvs(553) = 0.07*b(83,51)+0.1*b(84,51)
      jvs(554) = b(146,52)
      jvs(555) = 0
      jvs(556) = b(136,54)-b(140,54)
      jvs(557) = -b(57,55)+b(71,55)
      jvs(558) = -b(61,56)
      jvs(559) = -b(59,57)+b(72,57)
      jvs(560) = -b(57,58)-b(59,58)-b(61,58)-b(63,58)-b(124,58)   &
                -b(130,58)-b(133,58)-b(140,58)-b(145,58)
      jvs(561) = b(46,59)+0.7*b(55,59)+0.035*b(122,59)+0.73*b(127,59)
      jvs(562) = 2*b(12,6)
      jvs(563) = -b(65,7)
      jvs(564) = -b(45,8)
      jvs(565) = -b(47,9)
      jvs(566) = -b(128,11)
      jvs(567) = -b(89,12)
      jvs(568) = 2*b(9,13)-b(30,13)
      jvs(569) = -b(90,15)
      jvs(570) = -b(75,16)
      jvs(571) = -b(28,18)
      jvs(572) = -b(127,20)
      jvs(573) = b(54,21)-0.5*b(56,21)
      jvs(574) = 0.12*b(81,22)-b(82,22)
      jvs(575) = -b(48,23)
      jvs(576) = b(3,24)-b(26,24)
      jvs(577) = b(53,25)-0.7*b(55,25)
      jvs(578) = -b(92,26)
      jvs(579) = 0
      jvs(580) = -b(44,28)
      jvs(581) = b(4,31)-b(27,31)
      jvs(582) = -b(95,32)+0.08*b(97,32)
      jvs(583) = b(98,33)-0.77*b(99,33)
      jvs(584) = -b(119,34)-b(122,34)
      jvs(585) = -b(77,35)
      jvs(586) = 0.33*b(83,36)-b(85,36)
      jvs(587) = 0.6*b(84,39)-b(86,39)
      jvs(588) = -b(79,40)
      jvs(589) = 0
      jvs(590) = -b(134,42)
      jvs(591) = 0
      jvs(592) = 0
      jvs(593) = -b(67,45)
      jvs(594) = -b(51,47)
      jvs(595) = 0
      jvs(596) = 0
      jvs(597) = -b(100,50)
      jvs(598) = -b(20,51)+b(21,51)+0.12*b(81,51)+0.33*b(83,51)+0.6   &
                *b(84,51)+0.08*b(97,51)
      jvs(599) = 0
      jvs(600) = -b(24,53)
      jvs(601) = b(139,54)-b(141,54)
      jvs(602) = -b(23,55)+b(33,55)
      jvs(603) = b(21,56)-b(29,56)+b(33,56)+0.7*b(41,56)+b(139,56)
      jvs(604) = -b(25,57)+0.7*b(41,57)
      jvs(605) = 0
      jvs(606) = -b(20,59)-b(22,59)-b(23,59)-b(24,59)-b(25,59)   &
                -b(26,59)-b(27,59)-b(28,59)-b(29,59)-b(30,59)-b(44,59)   &
                -b(45,59)-b(46,59)-b(47,59)-b(48,59)-b(51,59)-0.7   &
                *b(55,59)-0.5*b(56,59)-b(65,59)-b(67,59)-b(75,59)   &
                -b(77,59)-b(79,59)-b(82,59)-b(85,59)-b(86,59)-b(89,59)   &
                -b(90,59)-b(92,59)-b(95,59)-0.77*b(99,59)-b(100,59)   &
                -b(119,59)-b(122,59)-b(127,59)-b(128,59)-b(134,59)   &
                -b(141,59)
      return
      end subroutine cbmz_v02r05_jacob                                    


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r05_decomp( n, v, ier,   &
          lu_crow_v, lu_diag_v, lu_icol_v )
!
!   computes l-u-decomposition of sparse jacobian
!   for mechanism-version-regime = cbmz_v02r05
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     n = number of variable species [input]
      integer n
!     ier = status flag [output]
!         0 = success    other = failure [output]
      integer ier

!     v = elements of the sparse jacobian [input]
      real v(lu_nonzero_v_r05_kpp)

      integer lu_crow_v(nvar_r05_kpp + 1)
      integer lu_diag_v(nvar_r05_kpp + 1)
      integer lu_icol_v(lu_nonzero_v_r05_kpp)

!   local variables
      integer k, kk, j, jj
      real a, w(nvar_r05_kpp + 1)


      ier = 0
      do k=1,n
        if ( v( lu_diag_v(k) ) .eq. 0. ) then
            ier = k
            return
        end if
        do kk = lu_crow_v(k), lu_crow_v(k+1)-1
              w( lu_icol_v(kk) ) = v(kk)
        end do
        do kk = lu_crow_v(k), lu_diag_v(k)-1
            j = lu_icol_v(kk)
            a = -w(j) / v( lu_diag_v(j) )
            w(j) = -a
            do jj = lu_diag_v(j)+1, lu_crow_v(j+1)-1
               w( lu_icol_v(jj) ) = w( lu_icol_v(jj) ) + a*v(jj)
            end do
         end do
         do kk = lu_crow_v(k), lu_crow_v(k+1)-1
            v(kk) = w( lu_icol_v(kk) )
         end do
      end do
      return
      end subroutine cbmz_v02r05_decomp            


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r05_solve( jvs, x )
!
!   does back-solve for mechanism-version-regime = cbmz_v02r05
!
      implicit none

!   subr parameters
!     jvs = the non-zero elements of the l-u-decomposition
!           of the augmented jacobian [input]
      real jvs(*)
!     x = the right-hand side of the linear equation set being solved [on input]
!     x = concentrations of variable species [on output]
      real x(*)


      x(16) = x(16)-jvs(53)*x(15)
      x(19) = x(19)-jvs(66)*x(12)-jvs(67)*x(15)
      x(26) = x(26)-jvs(98)*x(12)-jvs(99)*x(15)
      x(27) = x(27)-jvs(103)*x(16)
      x(28) = x(28)-jvs(115)*x(22)
      x(30) = x(30)-jvs(131)*x(11)-jvs(132)*x(20)
      x(31) = x(31)-jvs(138)*x(14)-jvs(139)*x(26)
      x(32) = x(32)-jvs(151)*x(19)-jvs(152)*x(26)
      x(35) = x(35)-jvs(167)*x(33)
      x(37) = x(37)-jvs(182)*x(12)-jvs(183)*x(15)-jvs(184)*x(22)   &
             -jvs(185)*x(26)-jvs(186)*x(32)-jvs(187)*x(33)-jvs(188)   &
             *x(36)
      x(38) = x(38)-jvs(200)*x(34)
      x(40) = x(40)-jvs(212)*x(15)-jvs(213)*x(32)-jvs(214)*x(33)   &
             -jvs(215)*x(36)-jvs(216)*x(39)
      x(41) = x(41)-jvs(225)*x(9)-jvs(226)*x(21)-jvs(227)*x(33)   &
             -jvs(228)*x(36)-jvs(229)*x(39)
      x(42) = x(42)-jvs(239)*x(20)-jvs(240)*x(34)
      x(43) = x(43)-jvs(248)*x(6)-jvs(249)*x(34)
      x(44) = x(44)-jvs(256)*x(36)-jvs(257)*x(39)
      x(45) = x(45)-jvs(265)*x(7)-jvs(266)*x(21)-jvs(267)*x(22)   &
             -jvs(268)*x(32)-jvs(269)*x(33)-jvs(270)*x(36)-jvs(271)   &
             *x(39)-jvs(272)*x(41)-jvs(273)*x(44)
      x(46) = x(46)-jvs(283)*x(10)-jvs(284)*x(32)-jvs(285)*x(35)   &
             -jvs(286)*x(36)-jvs(287)*x(39)-jvs(288)*x(40)-jvs(289)   &
             *x(45)
      x(47) = x(47)-jvs(300)*x(22)-jvs(301)*x(23)-jvs(302)*x(25)   &
             -jvs(303)*x(29)-jvs(304)*x(30)-jvs(305)*x(32)-jvs(306)   &
             *x(34)-jvs(307)*x(36)-jvs(308)*x(38)-jvs(309)*x(39)   &
             -jvs(310)*x(42)-jvs(311)*x(43)-jvs(312)*x(44)
      x(48) = x(48)-jvs(325)*x(35)-jvs(326)*x(39)
      x(49) = x(49)-jvs(335)*x(16)-jvs(336)*x(27)-jvs(337)*x(33)   &
             -jvs(338)*x(36)-jvs(339)*x(39)-jvs(340)*x(44)-jvs(341)   &
             *x(48)
      x(50) = x(50)-jvs(349)*x(17)-jvs(350)*x(19)-jvs(351)*x(26)   &
             -jvs(352)*x(44)-jvs(353)*x(49)
      x(51) = x(51)-jvs(361)*x(22)-jvs(362)*x(32)-jvs(363)*x(36)   &
             -jvs(364)*x(39)-jvs(365)*x(43)-jvs(366)*x(46)-jvs(367)   &
             *x(48)-jvs(368)*x(49)-jvs(369)*x(50)
      x(52) = x(52)-jvs(377)*x(29)-jvs(378)*x(38)-jvs(379)*x(42)   &
             -jvs(380)*x(43)-jvs(381)*x(47)-jvs(382)*x(48)-jvs(383)   &
             *x(49)-jvs(384)*x(50)-jvs(385)*x(51)
      x(53) = x(53)-jvs(394)*x(10)-jvs(395)*x(14)-jvs(396)*x(17)   &
             -jvs(397)*x(18)-jvs(398)*x(19)-jvs(399)*x(24)-jvs(400)   &
             *x(26)-jvs(401)*x(29)-jvs(402)*x(30)-jvs(403)*x(31)   &
             -jvs(404)*x(34)-jvs(405)*x(37)-jvs(406)*x(38)-jvs(407)   &
             *x(39)-jvs(408)*x(40)-jvs(409)*x(41)-jvs(410)*x(42)   &
             -jvs(411)*x(43)-jvs(412)*x(44)-jvs(413)*x(45)-jvs(414)   &
             *x(46)-jvs(415)*x(47)-jvs(416)*x(48)-jvs(417)*x(49)   &
             -jvs(418)*x(50)-jvs(419)*x(51)-jvs(420)*x(52)
      x(54) = x(54)-jvs(427)*x(29)-jvs(428)*x(30)-jvs(429)*x(34)   &
             -jvs(430)*x(38)-jvs(431)*x(42)-jvs(432)*x(43)-jvs(433)   &
             *x(51)-jvs(434)*x(52)-jvs(435)*x(53)
      x(55) = x(55)-jvs(442)*x(19)-jvs(443)*x(24)-jvs(444)*x(29)   &
             -jvs(445)*x(30)-jvs(446)*x(34)-jvs(447)*x(37)-jvs(448)   &
             *x(38)-jvs(449)*x(39)-jvs(450)*x(40)-jvs(451)*x(41)   &
             -jvs(452)*x(43)-jvs(453)*x(44)-jvs(454)*x(46)-jvs(455)   &
             *x(48)-jvs(456)*x(49)-jvs(457)*x(50)-jvs(458)*x(51)   &
             -jvs(459)*x(52)-jvs(460)*x(53)-jvs(461)*x(54)
      x(56) = x(56)-jvs(467)*x(7)-jvs(468)*x(8)-jvs(469)*x(12)   &
             -jvs(470)*x(13)-jvs(471)*x(15)-jvs(472)*x(18)-jvs(473)   &
             *x(19)-jvs(474)*x(20)-jvs(475)*x(21)-jvs(476)*x(22)   &
             -jvs(477)*x(23)-jvs(478)*x(25)-jvs(479)*x(26)-jvs(480)   &
             *x(28)-jvs(481)*x(29)-jvs(482)*x(30)-jvs(483)*x(32)   &
             -jvs(484)*x(33)-jvs(485)*x(34)-jvs(486)*x(36)-jvs(487)   &
             *x(37)-jvs(488)*x(38)-jvs(489)*x(39)-jvs(490)*x(40)   &
             -jvs(491)*x(41)-jvs(492)*x(42)-jvs(493)*x(43)-jvs(494)   &
             *x(44)-jvs(495)*x(45)-jvs(496)*x(46)-jvs(497)*x(47)   &
             -jvs(498)*x(48)-jvs(499)*x(49)-jvs(500)*x(50)-jvs(501)   &
             *x(51)-jvs(502)*x(52)-jvs(503)*x(53)-jvs(504)*x(54)   &
             -jvs(505)*x(55)
      x(57) = x(57)-jvs(510)*x(14)-jvs(511)*x(26)-jvs(512)*x(31)   &
             -jvs(513)*x(34)-jvs(514)*x(36)-jvs(515)*x(37)-jvs(516)   &
             *x(39)-jvs(517)*x(40)-jvs(518)*x(41)-jvs(519)*x(42)   &
             -jvs(520)*x(43)-jvs(521)*x(44)-jvs(522)*x(45)-jvs(523)   &
             *x(46)-jvs(524)*x(47)-jvs(525)*x(48)-jvs(526)*x(49)   &
             -jvs(527)*x(50)-jvs(528)*x(51)-jvs(529)*x(52)-jvs(530)   &
             *x(53)-jvs(531)*x(54)-jvs(532)*x(55)-jvs(533)*x(56)
      x(58) = x(58)-jvs(537)*x(20)-jvs(538)*x(25)-jvs(539)*x(29)   &
             -jvs(540)*x(30)-jvs(541)*x(34)-jvs(542)*x(35)-jvs(543)   &
             *x(36)-jvs(544)*x(38)-jvs(545)*x(39)-jvs(546)*x(42)   &
             -jvs(547)*x(43)-jvs(548)*x(45)-jvs(549)*x(46)-jvs(550)   &
             *x(48)-jvs(551)*x(49)-jvs(552)*x(50)-jvs(553)*x(51)   &
             -jvs(554)*x(52)-jvs(555)*x(53)-jvs(556)*x(54)-jvs(557)   &
             *x(55)-jvs(558)*x(56)-jvs(559)*x(57)
      x(59) = x(59)-jvs(562)*x(6)-jvs(563)*x(7)-jvs(564)*x(8)-jvs(565)   &
             *x(9)-jvs(566)*x(11)-jvs(567)*x(12)-jvs(568)*x(13)   &
             -jvs(569)*x(15)-jvs(570)*x(16)-jvs(571)*x(18)-jvs(572)   &
             *x(20)-jvs(573)*x(21)-jvs(574)*x(22)-jvs(575)*x(23)   &
             -jvs(576)*x(24)-jvs(577)*x(25)-jvs(578)*x(26)-jvs(579)   &
             *x(27)-jvs(580)*x(28)-jvs(581)*x(31)-jvs(582)*x(32)   &
             -jvs(583)*x(33)-jvs(584)*x(34)-jvs(585)*x(35)-jvs(586)   &
             *x(36)-jvs(587)*x(39)-jvs(588)*x(40)-jvs(589)*x(41)   &
             -jvs(590)*x(42)-jvs(591)*x(43)-jvs(592)*x(44)-jvs(593)   &
             *x(45)-jvs(594)*x(47)-jvs(595)*x(48)-jvs(596)*x(49)   &
             -jvs(597)*x(50)-jvs(598)*x(51)-jvs(599)*x(52)-jvs(600)   &
             *x(53)-jvs(601)*x(54)-jvs(602)*x(55)-jvs(603)*x(56)   &
             -jvs(604)*x(57)-jvs(605)*x(58)
      x(59) = x(59)/jvs(606)
      x(58) = (x(58)-jvs(561)*x(59))/(jvs(560))
      x(57) = (x(57)-jvs(535)*x(58)-jvs(536)*x(59))/(jvs(534))
      x(56) = (x(56)-jvs(507)*x(57)-jvs(508)*x(58)-jvs(509)*x(59))/   &
             (jvs(506))
      x(55) = (x(55)-jvs(463)*x(56)-jvs(464)*x(57)-jvs(465)*x(58)   &
             -jvs(466)*x(59))/(jvs(462))
      x(54) = (x(54)-jvs(437)*x(55)-jvs(438)*x(56)-jvs(439)*x(57)   &
             -jvs(440)*x(58)-jvs(441)*x(59))/(jvs(436))
      x(53) = (x(53)-jvs(422)*x(54)-jvs(423)*x(55)-jvs(424)*x(56)   &
             -jvs(425)*x(58)-jvs(426)*x(59))/(jvs(421))
      x(52) = (x(52)-jvs(387)*x(53)-jvs(388)*x(54)-jvs(389)*x(55)   &
             -jvs(390)*x(56)-jvs(391)*x(57)-jvs(392)*x(58)-jvs(393)   &
             *x(59))/(jvs(386))
      x(51) = (x(51)-jvs(371)*x(53)-jvs(372)*x(54)-jvs(373)*x(55)   &
             -jvs(374)*x(56)-jvs(375)*x(57)-jvs(376)*x(59))/(jvs(370))
      x(50) = (x(50)-jvs(355)*x(51)-jvs(356)*x(53)-jvs(357)*x(55)   &
             -jvs(358)*x(56)-jvs(359)*x(57)-jvs(360)*x(59))/(jvs(354))
      x(49) = (x(49)-jvs(343)*x(50)-jvs(344)*x(51)-jvs(345)*x(55)   &
             -jvs(346)*x(56)-jvs(347)*x(57)-jvs(348)*x(59))/(jvs(342))
      x(48) = (x(48)-jvs(328)*x(49)-jvs(329)*x(50)-jvs(330)*x(51)   &
             -jvs(331)*x(55)-jvs(332)*x(56)-jvs(333)*x(57)-jvs(334)   &
             *x(59))/(jvs(327))
      x(47) = (x(47)-jvs(314)*x(48)-jvs(315)*x(50)-jvs(316)*x(51)   &
             -jvs(317)*x(52)-jvs(318)*x(53)-jvs(319)*x(54)-jvs(320)   &
             *x(55)-jvs(321)*x(56)-jvs(322)*x(57)-jvs(323)*x(58)   &
             -jvs(324)*x(59))/(jvs(313))
      x(46) = (x(46)-jvs(291)*x(48)-jvs(292)*x(49)-jvs(293)*x(50)   &
             -jvs(294)*x(51)-jvs(295)*x(53)-jvs(296)*x(55)-jvs(297)   &
             *x(56)-jvs(298)*x(57)-jvs(299)*x(59))/(jvs(290))
      x(45) = (x(45)-jvs(275)*x(48)-jvs(276)*x(49)-jvs(277)*x(50)   &
             -jvs(278)*x(51)-jvs(279)*x(55)-jvs(280)*x(56)-jvs(281)   &
             *x(57)-jvs(282)*x(59))/(jvs(274))
      x(44) = (x(44)-jvs(259)*x(50)-jvs(260)*x(51)-jvs(261)*x(55)   &
             -jvs(262)*x(56)-jvs(263)*x(57)-jvs(264)*x(59))/(jvs(258))
      x(43) = (x(43)-jvs(251)*x(51)-jvs(252)*x(53)-jvs(253)*x(55)   &
             -jvs(254)*x(57)-jvs(255)*x(59))/(jvs(250))
      x(42) = (x(42)-jvs(242)*x(43)-jvs(243)*x(52)-jvs(244)*x(56)   &
             -jvs(245)*x(57)-jvs(246)*x(58)-jvs(247)*x(59))/(jvs(241))
      x(41) = (x(41)-jvs(231)*x(48)-jvs(232)*x(49)-jvs(233)*x(50)   &
             -jvs(234)*x(51)-jvs(235)*x(55)-jvs(236)*x(56)-jvs(237)   &
             *x(57)-jvs(238)*x(59))/(jvs(230))
      x(40) = (x(40)-jvs(218)*x(48)-jvs(219)*x(49)-jvs(220)*x(51)   &
             -jvs(221)*x(55)-jvs(222)*x(56)-jvs(223)*x(57)-jvs(224)   &
             *x(59))/(jvs(217))
      x(39) = (x(39)-jvs(209)*x(51)-jvs(210)*x(57)-jvs(211)*x(59))/   &
             (jvs(208))
      x(38) = (x(38)-jvs(202)*x(43)-jvs(203)*x(54)-jvs(204)*x(55)   &
             -jvs(205)*x(57)-jvs(206)*x(58)-jvs(207)*x(59))/(jvs(201))
      x(37) = (x(37)-jvs(190)*x(39)-jvs(191)*x(40)-jvs(192)*x(48)   &
             -jvs(193)*x(49)-jvs(194)*x(50)-jvs(195)*x(51)-jvs(196)   &
             *x(55)-jvs(197)*x(56)-jvs(198)*x(57)-jvs(199)*x(59))/   &
             (jvs(189))
      x(36) = (x(36)-jvs(179)*x(51)-jvs(180)*x(57)-jvs(181)*x(59))/   &
             (jvs(178))
      x(35) = (x(35)-jvs(169)*x(39)-jvs(170)*x(48)-jvs(171)*x(49)   &
             -jvs(172)*x(50)-jvs(173)*x(51)-jvs(174)*x(55)-jvs(175)   &
             *x(56)-jvs(176)*x(57)-jvs(177)*x(59))/(jvs(168))
      x(34) = (x(34)-jvs(164)*x(43)-jvs(165)*x(57)-jvs(166)*x(59))/   &
             (jvs(163))
      x(33) = (x(33)-jvs(159)*x(48)-jvs(160)*x(49)-jvs(161)*x(56)   &
             -jvs(162)*x(59))/(jvs(158))
      x(32) = (x(32)-jvs(154)*x(51)-jvs(155)*x(55)-jvs(156)*x(57)   &
             -jvs(157)*x(59))/(jvs(153))
      x(31) = (x(31)-jvs(141)*x(34)-jvs(142)*x(40)-jvs(143)*x(42)   &
             -jvs(144)*x(45)-jvs(145)*x(47)-jvs(146)*x(52)-jvs(147)   &
             *x(53)-jvs(148)*x(56)-jvs(149)*x(57)-jvs(150)*x(59))/   &
             (jvs(140))
      x(30) = (x(30)-jvs(134)*x(34)-jvs(135)*x(55)-jvs(136)*x(58)   &
             -jvs(137)*x(59))/(jvs(133))
      x(29) = (x(29)-jvs(128)*x(54)-jvs(129)*x(55)-jvs(130)*x(58))/   &
             (jvs(127))
      x(28) = (x(28)-jvs(117)*x(32)-jvs(118)*x(36)-jvs(119)*x(39)   &
             -jvs(120)*x(40)-jvs(121)*x(45)-jvs(122)*x(47)-jvs(123)   &
             *x(51)-jvs(124)*x(52)-jvs(125)*x(57)-jvs(126)*x(59))/   &
             (jvs(116))
      x(27) = (x(27)-jvs(105)*x(33)-jvs(106)*x(36)-jvs(107)*x(39)   &
             -jvs(108)*x(44)-jvs(109)*x(49)-jvs(110)*x(50)-jvs(111)   &
             *x(51)-jvs(112)*x(55)-jvs(113)*x(57)-jvs(114)*x(59))/   &
             (jvs(104))
      x(26) = (x(26)-jvs(101)*x(57)-jvs(102)*x(59))/(jvs(100))
      x(25) = (x(25)-jvs(94)*x(42)-jvs(95)*x(56)-jvs(96)*x(58)-jvs(97)   &
             *x(59))/(jvs(93))
      x(24) = (x(24)-jvs(88)*x(52)-jvs(89)*x(53)-jvs(90)*x(55)-jvs(91)   &
             *x(56)-jvs(92)*x(59))/(jvs(87))
      x(23) = (x(23)-jvs(82)*x(36)-jvs(83)*x(39)-jvs(84)*x(51)-jvs(85)   &
             *x(58)-jvs(86)*x(59))/(jvs(81))
      x(22) = (x(22)-jvs(79)*x(51)-jvs(80)*x(59))/(jvs(78))
      x(21) = (x(21)-jvs(75)*x(41)-jvs(76)*x(56)-jvs(77)*x(59))/   &
             (jvs(74))
      x(20) = (x(20)-jvs(72)*x(34)-jvs(73)*x(59))/(jvs(71))
      x(19) = (x(19)-jvs(69)*x(55)-jvs(70)*x(59))/(jvs(68))
      x(18) = (x(18)-jvs(63)*x(53)-jvs(64)*x(56)-jvs(65)*x(59))/   &
             (jvs(62))
      x(17) = (x(17)-jvs(58)*x(26)-jvs(59)*x(53)-jvs(60)*x(57)-jvs(61)   &
             *x(59))/(jvs(57))
      x(16) = (x(16)-jvs(55)*x(27)-jvs(56)*x(59))/(jvs(54))
      x(15) = (x(15)-jvs(52)*x(59))/(jvs(51))
      x(14) = (x(14)-jvs(49)*x(53)-jvs(50)*x(57))/(jvs(48))
      x(13) = (x(13)-jvs(45)*x(42)-jvs(46)*x(56)-jvs(47)*x(59))/   &
             (jvs(44))
      x(12) = (x(12)-jvs(43)*x(59))/(jvs(42))
      x(11) = (x(11)-jvs(40)*x(20)-jvs(41)*x(59))/(jvs(39))
      x(10) = (x(10)-jvs(37)*x(46)-jvs(38)*x(53))/(jvs(36))
      x(9) = (x(9)-jvs(34)*x(41)-jvs(35)*x(59))/(jvs(33))
      x(8) = (x(8)-jvs(31)*x(54)-jvs(32)*x(59))/(jvs(30))
      x(7) = (x(7)-jvs(29)*x(59))/(jvs(28))
      x(6) = (x(6)-jvs(27)*x(51))/(jvs(26))
      x(5) = (x(5)-jvs(25)*x(38))/(jvs(24))
      x(4) = (x(4)-jvs(16)*x(42)-jvs(17)*x(47)-jvs(18)*x(52)-jvs(19)   &
            *x(53)-jvs(20)*x(54)-jvs(21)*x(55)-jvs(22)*x(56)-jvs(23)   &
            *x(59))/(jvs(15))
      x(3) = (x(3)-jvs(10)*x(36)-jvs(11)*x(39)-jvs(12)*x(46)-jvs(13)   &
            *x(51)-jvs(14)*x(56))/(jvs(9))
      x(2) = (x(2)-jvs(6)*x(22)-jvs(7)*x(36)-jvs(8)*x(51))/(jvs(5))
      x(1) = (x(1)-jvs(2)*x(8)-jvs(3)*x(52)-jvs(4)*x(59))/(jvs(1))
      return
      end subroutine cbmz_v02r05_solve          


!   cbmz_v02r06_torodas.f - created on 18-nov-2003 from previous
!        cbmz_v02r06_torodas.f        cbmz_v02r06_mapconcs.f
!        cbmz_v02r06_maprates.f       cbmz_v02r06_dydt.f
!        cbmz_v02r06_jacob.f          cbmz_v02r06_decomp.f
!        cbmz_v02r06_solve.f
!   so now everything is in a single file
!-----------------------------------------------------------------------

      subroutine cbmz_v02r06_torodas(   &
          ngas, taa, tzz,   &
          stot, atol, rtol, yposlimit, yneglimit,   &
          sfixedkpp, rconstkpp,   &
          hmin, hstart,   &
          info_rodas, iok, lunerr, idydt_sngldble )
!
!   interfaces to rodas3 solver formechanism-version-regime =cbmz_v02r06
!
!   *** do not include any pegasus common blocks ***
!
      use module_data_cbmz
      use module_cbmz_rodas3_solver, only:  rodas3_ff_x2
      implicit none

!   subr parameters
      integer ngas, iok, lunerr, idydt_sngldble
      integer info_rodas(6)
      real taa, tzz, hmin, hstart
      real stot(ngas), atol(ngas), rtol(ngas)
      real yposlimit(ngas), yneglimit(ngas)
      real sfixedkpp(nfixed_kppmax), rconstkpp(nreact_kppmax)

!   local variables

!     external cbmz_v02r06_dydt
!     external cbmz_v02r06_jacob
!     external cbmz_v02r06_decomp
!     external cbmz_v02r06_solve

      integer i

      real hmax

      integer lu_crow_v(nvar_r06_kpp + 1)
      save    lu_crow_v
      integer lu_diag_v(nvar_r06_kpp + 1)
      save    lu_diag_v
      integer lu_icol_v(lu_nonzero_v_r06_kpp)
      save    lu_icol_v

      data( lu_icol_v(i), i = 1, 252 ) /   &
        1,  8, 54, 61,  2, 22, 37, 38, 51, 58,  3, 37,   &
       42, 52, 58, 63,  4, 45, 49, 50, 54, 56, 60, 61,   &
       63,  5, 39,  6, 58,  7, 61,  8, 50, 61,  9, 44,   &
       61, 10, 52, 56, 11, 19, 61, 12, 61, 13, 45, 61,   &
       63, 14, 56, 62, 15, 61, 16, 26, 56, 61, 62, 17,   &
       56, 61, 63, 12, 15, 18, 60, 61, 19, 34, 61, 20,   &
       30, 37, 42, 47, 53, 57, 58, 59, 60, 61, 62, 21,   &
       44, 61, 63, 22, 58, 61, 23, 37, 42, 55, 58, 61,   &
       24, 54, 56, 60, 61, 63, 25, 45, 55, 61, 63, 12,   &
       15, 26, 61, 62, 27, 50, 55, 60, 11, 19, 28, 34,   &
       55, 60, 61, 29, 38, 60, 62, 63, 15, 20, 29, 30,   &
       35, 37, 38, 42, 47, 51, 53, 57, 58, 59, 60, 61,   &
       62, 63, 22, 31, 32, 36, 37, 38, 42, 43, 48, 49,   &
       51, 54, 58, 60, 61, 62, 18, 26, 32, 58, 60, 61,   &
       62, 14, 26, 33, 34, 43, 45, 48, 49, 51, 54, 56,   &
       61, 62, 63, 34, 46, 61, 62, 35, 38, 60, 61, 63,   &
       36, 51, 60, 61, 63, 37, 58, 61, 62, 38, 58, 61,   &
       62, 34, 39, 46, 50, 55, 60, 61, 62, 12, 15, 22,   &
       26, 32, 37, 38, 40, 42, 43, 51, 53, 57, 58, 59,   &
       60, 61, 62, 63, 36, 41, 42, 51, 53, 57, 58, 59,   &
       60, 61, 62, 63, 64, 42, 58, 61, 62, 15, 32, 36 /

      data( lu_icol_v(i), i = 253, 504 ) /   &
       37, 42, 43, 51, 53, 58, 60, 61, 62, 63, 64,  9,   &
       21, 37, 42, 44, 53, 57, 58, 59, 60, 61, 62, 63,   &
       19, 34, 45, 46, 54, 55, 61, 62, 63,  6, 34, 46,   &
       56, 58, 60, 61, 62, 37, 42, 47, 57, 58, 60, 61,   &
       62, 63,  7, 21, 22, 29, 32, 36, 37, 38, 42, 44,   &
       47, 48, 51, 53, 57, 58, 59, 60, 61, 62, 63, 22,   &
       23, 25, 27, 28, 32, 34, 35, 36, 37, 38, 39, 42,   &
       45, 46, 47, 49, 50, 51, 54, 55, 56, 57, 58, 60,   &
       61, 62, 63, 64, 27, 28, 34, 39, 45, 46, 50, 54,   &
       55, 56, 58, 60, 61, 62, 63, 29, 35, 38, 51, 58,   &
       60, 61, 62, 63, 10, 32, 37, 38, 41, 42, 43, 48,   &
       51, 52, 53, 56, 57, 58, 59, 60, 61, 62, 63, 64,   &
       35, 36, 38, 51, 53, 58, 59, 60, 61, 62, 63, 64,   &
       27, 39, 45, 46, 49, 50, 51, 54, 55, 56, 57, 58,   &
       60, 61, 62, 63, 64, 19, 25, 27, 28, 34, 37, 39,   &
       41, 42, 45, 46, 48, 50, 51, 52, 53, 54, 55, 56,   &
       57, 58, 59, 60, 61, 62, 63, 64, 10, 14, 16, 17,   &
       18, 24, 26, 27, 28, 29, 33, 34, 35, 36, 38, 39,   &
       40, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,   &
       53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64,   &
       16, 18, 26, 29, 35, 38, 47, 51, 56, 57, 58, 59 /

      data( lu_icol_v(i), i = 505, 715 ) /   &
       60, 61, 62, 63, 64, 22, 32, 37, 38, 42, 46, 50,   &
       51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62,   &
       63, 64, 30, 35, 37, 38, 42, 47, 51, 53, 57, 58,   &
       59, 60, 61, 62, 63, 64, 18, 24, 27, 28, 29, 34,   &
       35, 36, 38, 39, 40, 42, 43, 44, 46, 47, 50, 51,   &
       52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,   &
       64,  6,  7,  8,  9, 11, 12, 13, 15, 17, 19, 21,   &
       22, 23, 24, 25, 26, 30, 31, 32, 33, 34, 35, 36,   &
       37, 38, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,   &
       51, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,   &
       64, 14, 26, 33, 34, 37, 38, 40, 42, 43, 44, 45,   &
       46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57,   &
       58, 59, 60, 61, 62, 63, 64,  7,  8, 12, 13, 15,   &
       17, 18, 19, 21, 22, 23, 25, 26, 27, 28, 29, 31,   &
       32, 34, 35, 36, 37, 38, 39, 40, 42, 43, 44, 45,   &
       46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57,   &
       58, 59, 60, 61, 62, 63, 64, 41, 42, 51, 53, 57,   &
       58, 59, 60, 61, 62, 63, 64 /

      data lu_crow_v /   &
        1,  5, 11, 17, 26, 28, 30, 32, 35, 38, 41, 44,   &
       46, 50, 53, 55, 60, 64, 69, 72, 84, 88, 91, 97,   &
      103,108,113,117,124,129,147,163,170,184,188,193,   &
      198,202,206,214,233,246,250,264,277,286,294,303,   &
      324,353,368,377,397,409,426,453,493,510,531,547,   &
      578,626,656,704,716 /

      data lu_diag_v /   &
        1,  5, 11, 17, 26, 28, 30, 32, 35, 38, 41, 44,   &
       46, 50, 53, 55, 60, 66, 69, 72, 84, 88, 91, 97,   &
      103,110,113,119,124,132,148,165,172,184,188,193,   &
      198,202,207,221,234,246,255,268,279,288,296,314,   &
      340,359,371,386,401,416,443,484,502,524,541,573,   &
      622,653,702,715,716 /



      info_rodas(1) = 1
      do i = 2, 6
          info_rodas(i) = 0
      end do
      hmax = tzz - taa

!   do not integrate if hmax is less/equal to hmin
!   because hmin is generally 0.1 s or less
      if (hmax .le. 1.001*hmin) then
          iok = 11
          return
      end if

      call rodas3_ff_x2(   &
           nvar_r06_kpp, taa, tzz, hmin, hmax, hstart,   &
           stot, atol, rtol, yposlimit, yneglimit,   &
           sfixedkpp, rconstkpp,   &
           lu_nonzero_v_r06_kpp, lu_crow_v, lu_diag_v, lu_icol_v,   &
           info_rodas, iok, lunerr,   &
           cbmz_v02r06_dydt,   &
           cbmz_v02r06_jacob,   &
           cbmz_v02r06_decomp,   &
           cbmz_v02r06_solve )

      return
      end subroutine cbmz_v02r06_torodas 


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r06_mapconcs( imap, nyy, yy, yyfixed, cbox )
!
!   maps species concentrations (gaschemistry cbox array <--> kpp yy array)
!   for mechanism-version-regime = cbmz_v02r06
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     imap = mapping direction flag [input]
!         0 = map cbox --> yy and yyfixed
!         1 = map yy --> cbox
      integer imap
!     nyy = number of kpp "variable" species [output]
      integer nyy
!     yy = kpp species concentrations array [input/output]
      real yy(nvar_r06_kpp)
!     yyfixed = kpp species concentrations array [input/output]
      real yyfixed(nfixed_kppmax)
!     cbox = main gaschemistry species conc array [input/output]
      real cbox(ngas_z)

!   local variables
      integer ih2so4_kpp
      parameter ( ih2so4_kpp = 1 )
      integer ihcooh_kpp
      parameter ( ihcooh_kpp = 2 )
      integer ircooh_kpp
      parameter ( ircooh_kpp = 3 )
      integer imsa_kpp
      parameter ( imsa_kpp = 4 )
      integer imtf_kpp
      parameter ( imtf_kpp = 5 )
      integer io1d_kpp
      parameter ( io1d_kpp = 6 )
      integer ic2h5oh_kpp
      parameter ( ic2h5oh_kpp = 7 )
      integer iso2_kpp
      parameter ( iso2_kpp = 8 )
      integer ic2h6_kpp
      parameter ( ic2h6_kpp = 9 )
      integer ipan_kpp
      parameter ( ipan_kpp = 10 )
      integer idmso2_kpp
      parameter ( idmso2_kpp = 11 )
      integer itol_kpp
      parameter ( itol_kpp = 12 )
      integer ih2o2_kpp
      parameter ( ih2o2_kpp = 13 )
      integer in2o5_kpp
      parameter ( in2o5_kpp = 14 )
      integer ixyl_kpp
      parameter ( ixyl_kpp = 15 )
      integer icro_kpp
      parameter ( icro_kpp = 16 )
      integer ihno4_kpp
      parameter ( ihno4_kpp = 17 )
      integer ito2_kpp
      parameter ( ito2_kpp = 18 )
      integer idmso_kpp
      parameter ( idmso_kpp = 19 )
      integer ixpar_kpp
      parameter ( ixpar_kpp = 20 )
      integer iethooh_kpp
      parameter ( iethooh_kpp = 21 )
      integer ieth_kpp
      parameter ( ieth_kpp = 22 )
      integer ich3oh_kpp
      parameter ( ich3oh_kpp = 23 )
      integer ihono_kpp
      parameter ( ihono_kpp = 24 )
      integer ich3ooh_kpp
      parameter ( ich3ooh_kpp = 25 )
      integer icres_kpp
      parameter ( icres_kpp = 26 )
      integer ich3so2oo_kpp
      parameter ( ich3so2oo_kpp = 27 )
      integer ich3so2ch2oo_kpp
      parameter ( ich3so2ch2oo_kpp = 28 )
      integer iisopn_kpp
      parameter ( iisopn_kpp = 29 )
      integer ipar_kpp
      parameter ( ipar_kpp = 30 )
      integer ico_kpp
      parameter ( ico_kpp = 31 )
      integer iopen_kpp
      parameter ( iopen_kpp = 32 )
      integer ihno3_kpp
      parameter ( ihno3_kpp = 33 )
      integer idms_kpp
      parameter ( idms_kpp = 34 )
      integer iisopp_kpp
      parameter ( iisopp_kpp = 35 )
      integer iisopo2_kpp
      parameter ( iisopo2_kpp = 36 )
      integer iolet_kpp
      parameter ( iolet_kpp = 37 )
      integer iisop_kpp
      parameter ( iisop_kpp = 38 )
      integer ich3sch2oo_kpp
      parameter ( ich3sch2oo_kpp = 39 )
      integer ixo2_kpp
      parameter ( ixo2_kpp = 40 )
      integer iaone_kpp
      parameter ( iaone_kpp = 41 )
      integer iolei_kpp
      parameter ( iolei_kpp = 42 )
      integer imgly_kpp
      parameter ( imgly_kpp = 43 )
      integer iethp_kpp
      parameter ( iethp_kpp = 44 )
      integer ich3so2h_kpp
      parameter ( ich3so2h_kpp = 45 )
      integer io3p_kpp
      parameter ( io3p_kpp = 46 )
      integer inap_kpp
      parameter ( inap_kpp = 47 )
      integer iald2_kpp
      parameter ( iald2_kpp = 48 )
      integer ihcho_kpp
      parameter ( ihcho_kpp = 49 )
      integer ich3so2_kpp
      parameter ( ich3so2_kpp = 50 )
      integer iisoprd_kpp
      parameter ( iisoprd_kpp = 51 )
      integer ic2o3_kpp
      parameter ( ic2o3_kpp = 52 )
      integer irooh_kpp
      parameter ( irooh_kpp = 53 )
      integer ich3so3_kpp
      parameter ( ich3so3_kpp = 54 )
      integer ich3o2_kpp
      parameter ( ich3o2_kpp = 55 )
      integer ino2_kpp
      parameter ( ino2_kpp = 56 )
      integer ionit_kpp
      parameter ( ionit_kpp = 57 )
      integer io3_kpp
      parameter ( io3_kpp = 58 )
      integer iro2_kpp
      parameter ( iro2_kpp = 59 )
      integer ino_kpp
      parameter ( ino_kpp = 60 )
      integer ioh_kpp
      parameter ( ioh_kpp = 61 )
      integer ino3_kpp
      parameter ( ino3_kpp = 62 )
      integer iho2_kpp
      parameter ( iho2_kpp = 63 )
      integer iano2_kpp
      parameter ( iano2_kpp = 64 )

! indexes declaration for fixed species
      integer ich4_kpp
      parameter ( ich4_kpp = 1 )
      integer ih2o_kpp
      parameter ( ih2o_kpp = 2 )
      integer ih2_kpp
      parameter ( ih2_kpp = 3 )
      integer io2_kpp
      parameter ( io2_kpp = 4 )
      integer in2_kpp
      parameter ( in2_kpp = 5 )


      nyy = nvar_r06_kpp

      if (imap .le. 0) goto 1000
      if (imap .ge. 1) goto 2000


!
!   map cbox values into yyvarkpp and yyfixkpp
!
1000  continue
      yy(ih2so4_kpp)	= cbox(ih2so4_z)
      yy(ihcooh_kpp)	= cbox(ihcooh_z)
      yy(ircooh_kpp)	= cbox(ircooh_z)
      yy(imsa_kpp)	= cbox(imsa_z)
      yy(imtf_kpp)	= cbox(imtf_z)
      yy(io1d_kpp)	= cbox(io1d_z)
      yy(ic2h5oh_kpp)	= cbox(ic2h5oh_z)
      yy(iso2_kpp)	= cbox(iso2_z)
      yy(ic2h6_kpp)	= cbox(ic2h6_z)
      yy(ipan_kpp)	= cbox(ipan_z)
      yy(idmso2_kpp)	= cbox(idmso2_z)
      yy(itol_kpp)	= cbox(itol_z)
      yy(ih2o2_kpp)	= cbox(ih2o2_z)
      yy(in2o5_kpp)	= cbox(in2o5_z)
      yy(ixyl_kpp)	= cbox(ixyl_z)
      yy(icro_kpp)	= cbox(icro_z)
      yy(ihno4_kpp)	= cbox(ihno4_z)
      yy(ito2_kpp)	= cbox(ito2_z)
      yy(idmso_kpp)	= cbox(idmso_z)
      yy(ixpar_kpp)	= cbox(ixpar_z)
      yy(iethooh_kpp)	= cbox(iethooh_z)
      yy(ieth_kpp)	= cbox(ieth_z)
      yy(ich3oh_kpp)	= cbox(ich3oh_z)
      yy(ihono_kpp)	= cbox(ihono_z)
      yy(ich3ooh_kpp)	= cbox(ich3ooh_z)
      yy(icres_kpp)	= cbox(icres_z)
      yy(ich3so2oo_kpp)	= cbox(ich3so2oo_z)
      yy(ich3so2ch2oo_kpp)	= cbox(ich3so2ch2oo_z)
      yy(iisopn_kpp)	= cbox(iisopn_z)
      yy(ipar_kpp)	= cbox(ipar_z)
      yy(ico_kpp)	= cbox(ico_z)
      yy(iopen_kpp)	= cbox(iopen_z)
      yy(ihno3_kpp)	= cbox(ihno3_z)
      yy(idms_kpp)	= cbox(idms_z)
      yy(iisopp_kpp)	= cbox(iisopp_z)
      yy(iisopo2_kpp)	= cbox(iisopo2_z)
      yy(iolet_kpp)	= cbox(iolet_z)
      yy(iisop_kpp)	= cbox(iisop_z)
      yy(ich3sch2oo_kpp)	= cbox(ich3sch2oo_z)
      yy(ixo2_kpp)	= cbox(ixo2_z)
      yy(iaone_kpp)	= cbox(iaone_z)
      yy(iolei_kpp)	= cbox(iolei_z)
      yy(imgly_kpp)	= cbox(imgly_z)
      yy(iethp_kpp)	= cbox(iethp_z)
      yy(ich3so2h_kpp)	= cbox(ich3so2h_z)
      yy(io3p_kpp)	= cbox(io3p_z)
      yy(inap_kpp)	= cbox(inap_z)
      yy(iald2_kpp)	= cbox(iald2_z)
      yy(ihcho_kpp)	= cbox(ihcho_z)
      yy(ich3so2_kpp)	= cbox(ich3so2_z)
      yy(iisoprd_kpp)	= cbox(iisoprd_z)
      yy(ic2o3_kpp)	= cbox(ic2o3_z)
      yy(irooh_kpp)	= cbox(irooh_z)
      yy(ich3so3_kpp)	= cbox(ich3so3_z)
      yy(ich3o2_kpp)	= cbox(ich3o2_z)
      yy(ino2_kpp)	= cbox(ino2_z)
      yy(ionit_kpp)	= cbox(ionit_z)
      yy(io3_kpp)	= cbox(io3_z)
      yy(iro2_kpp)	= cbox(iro2_z)
      yy(ino_kpp)	= cbox(ino_z)
      yy(ioh_kpp)	= cbox(ioh_z)
      yy(ino3_kpp)	= cbox(ino3_z)
      yy(iho2_kpp)	= cbox(iho2_z)
      yy(iano2_kpp)	= cbox(iano2_z)

      yyfixed(ich4_kpp)	= cbox(ich4_z)
      yyfixed(ih2o_kpp)	= cbox(ih2o_z)
      yyfixed(ih2_kpp)	= cbox(ih2_z)
      yyfixed(io2_kpp)	= cbox(io2_z)
      yyfixed(in2_kpp)	= cbox(in2_z)

!
!   map yyvarkpp values into cbox
!
2000  continue
      cbox(ih2so4_z)	= yy(ih2so4_kpp)
      cbox(ihcooh_z)	= yy(ihcooh_kpp)
      cbox(ircooh_z)	= yy(ircooh_kpp)
      cbox(imsa_z)	= yy(imsa_kpp)
      cbox(imtf_z)	= yy(imtf_kpp)
      cbox(io1d_z)	= yy(io1d_kpp)
      cbox(ic2h5oh_z)	= yy(ic2h5oh_kpp)
      cbox(iso2_z)	= yy(iso2_kpp)
      cbox(ic2h6_z)	= yy(ic2h6_kpp)
      cbox(ipan_z)	= yy(ipan_kpp)
      cbox(idmso2_z)	= yy(idmso2_kpp)
      cbox(itol_z)	= yy(itol_kpp)
      cbox(ih2o2_z)	= yy(ih2o2_kpp)
      cbox(in2o5_z)	= yy(in2o5_kpp)
      cbox(ixyl_z)	= yy(ixyl_kpp)
      cbox(icro_z)	= yy(icro_kpp)
      cbox(ihno4_z)	= yy(ihno4_kpp)
      cbox(ito2_z)	= yy(ito2_kpp)
      cbox(idmso_z)	= yy(idmso_kpp)
      cbox(ixpar_z)	= yy(ixpar_kpp)
      cbox(iethooh_z)	= yy(iethooh_kpp)
      cbox(ieth_z)	= yy(ieth_kpp)
      cbox(ich3oh_z)	= yy(ich3oh_kpp)
      cbox(ihono_z)	= yy(ihono_kpp)
      cbox(ich3ooh_z)	= yy(ich3ooh_kpp)
      cbox(icres_z)	= yy(icres_kpp)
      cbox(ich3so2oo_z)	= yy(ich3so2oo_kpp)
      cbox(ich3so2ch2oo_z)	= yy(ich3so2ch2oo_kpp)
      cbox(iisopn_z)	= yy(iisopn_kpp)
      cbox(ipar_z)	= yy(ipar_kpp)
      cbox(ico_z)	= yy(ico_kpp)
      cbox(iopen_z)	= yy(iopen_kpp)
      cbox(ihno3_z)	= yy(ihno3_kpp)
      cbox(idms_z)	= yy(idms_kpp)
      cbox(iisopp_z)	= yy(iisopp_kpp)
      cbox(iisopo2_z)	= yy(iisopo2_kpp)
      cbox(iolet_z)	= yy(iolet_kpp)
      cbox(iisop_z)	= yy(iisop_kpp)
      cbox(ich3sch2oo_z)	= yy(ich3sch2oo_kpp)
      cbox(ixo2_z)	= yy(ixo2_kpp)
      cbox(iaone_z)	= yy(iaone_kpp)
      cbox(iolei_z)	= yy(iolei_kpp)
      cbox(imgly_z)	= yy(imgly_kpp)
      cbox(iethp_z)	= yy(iethp_kpp)
      cbox(ich3so2h_z)	= yy(ich3so2h_kpp)
      cbox(io3p_z)	= yy(io3p_kpp)
      cbox(inap_z)	= yy(inap_kpp)
      cbox(iald2_z)	= yy(iald2_kpp)
      cbox(ihcho_z)	= yy(ihcho_kpp)
      cbox(ich3so2_z)	= yy(ich3so2_kpp)
      cbox(iisoprd_z)	= yy(iisoprd_kpp)
      cbox(ic2o3_z)	= yy(ic2o3_kpp)
      cbox(irooh_z)	= yy(irooh_kpp)
      cbox(ich3so3_z)	= yy(ich3so3_kpp)
      cbox(ich3o2_z)	= yy(ich3o2_kpp)
      cbox(ino2_z)	= yy(ino2_kpp)
      cbox(ionit_z)	= yy(ionit_kpp)
      cbox(io3_z)	= yy(io3_kpp)
      cbox(iro2_z)	= yy(iro2_kpp)
      cbox(ino_z)	= yy(ino_kpp)
      cbox(ioh_z)	= yy(ioh_kpp)
      cbox(ino3_z)	= yy(ino3_kpp)
      cbox(iho2_z)	= yy(iho2_kpp)
      cbox(iano2_z)	= yy(iano2_kpp)

      return
      end subroutine cbmz_v02r06_mapconcs                                


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r06_maprates(   &
          rk_m1,   &
          rk_m2,   &
          rk_m3,   &
          rk_m4,   &
          rconst )
!
!   maps reaction rate constants (host code array --> kpp rconst array)
!   for mechanism-version-regime = cbmz_v02r06
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     all are host-code reaction-rate-constant arrays [input]
      real rk_m1(*)
      real rk_m2(*)
      real rk_m3(*)
      real rk_m4(*)
      real rconst(nreact_kppmax)

!   local variables
      integer i

      do i = 1, nreact_kppmax
          rconst(i) = 0.
      end do


      rconst(1) = (rk_m1(1))
      rconst(2) = (rk_m1(2))
      rconst(3) = (rk_m1(3))
      rconst(4) = (rk_m1(4))
      rconst(5) = (rk_m1(5))
      rconst(6) = (rk_m1(6))
      rconst(7) = (rk_m1(7))
      rconst(8) = (rk_m1(8))
      rconst(9) = (rk_m1(9))
      rconst(10) = (rk_m1(10))
      rconst(11) = (rk_m1(11))
      rconst(12) = (rk_m1(12))
      rconst(13) = (rk_m1(13))
      rconst(14) = (rk_m1(14))
      rconst(15) = (rk_m1(15))
      rconst(16) = (rk_m1(16))
      rconst(17) = (rk_m1(17))
      rconst(18) = (rk_m1(18))
      rconst(19) = (rk_m1(19))
      rconst(20) = (rk_m1(20))
      rconst(21) = (rk_m1(21))
      rconst(22) = (rk_m1(22))
      rconst(23) = (rk_m1(23))
      rconst(24) = (rk_m1(24))
      rconst(25) = (rk_m1(25))
      rconst(26) = (rk_m1(26))
      rconst(27) = (rk_m1(27))
      rconst(28) = (rk_m1(28))
      rconst(29) = (rk_m1(29))
      rconst(30) = (rk_m1(30))
      rconst(31) = (rk_m1(31))
      rconst(32) = (rk_m1(32))
      rconst(33) = (rk_m1(33))
      rconst(34) = (rk_m1(34))
      rconst(35) = (rk_m1(35))
      rconst(36) = (rk_m1(36))
      rconst(37) = (rk_m1(37))
      rconst(38) = (rk_m1(38))
      rconst(39) = (rk_m1(39))
      rconst(40) = (rk_m1(40))
      rconst(41) = (rk_m1(41))
      rconst(42) = (rk_m1(42))
      rconst(43) = (rk_m1(43))
      rconst(44) = (rk_m1(44))
      rconst(45) = (rk_m1(45))
      rconst(46) = (rk_m1(46))
      rconst(47) = (rk_m1(47))
      rconst(48) = (rk_m1(48))
      rconst(49) = (rk_m1(49))
      rconst(50) = (rk_m1(50))
      rconst(51) = (rk_m1(51))
      rconst(52) = (rk_m1(52))
      rconst(53) = (rk_m1(53))
      rconst(54) = (rk_m1(54))
      rconst(55) = (rk_m1(55))
      rconst(56) = (rk_m1(56))
      rconst(57) = (rk_m1(57))
      rconst(58) = (rk_m1(58))
      rconst(59) = (rk_m1(59))
      rconst(60) = (rk_m1(60))
      rconst(61) = (rk_m1(61))
      rconst(62) = (rk_m1(62))
      rconst(63) = (rk_m1(63))
      rconst(64) = (rk_m1(64))
      rconst(65) = (rk_m1(65))
      rconst(66) = (rk_m2(2))
      rconst(67) = (rk_m2(3))
      rconst(68) = (rk_m2(4))
      rconst(69) = (rk_m2(31))
      rconst(70) = (rk_m2(32))
      rconst(71) = (rk_m2(34))
      rconst(72) = (rk_m2(39))
      rconst(73) = (rk_m2(44))
      rconst(74) = (rk_m2(49))
      rconst(75) = (rk_m2(1))
      rconst(76) = (rk_m2(5))
      rconst(77) = (rk_m2(6))
      rconst(78) = (rk_m2(7))
      rconst(79) = (rk_m2(8))
      rconst(80) = (rk_m2(9))
      rconst(81) = (rk_m2(10))
      rconst(82) = (rk_m2(11))
      rconst(83) = (rk_m2(12))
      rconst(84) = (rk_m2(13))
      rconst(85) = (rk_m2(14))
      rconst(86) = (rk_m2(15))
      rconst(87) = (rk_m2(16))
      rconst(88) = (rk_m2(17))
      rconst(89) = (rk_m2(18))
      rconst(90) = (rk_m2(19))
      rconst(91) = (rk_m2(20))
      rconst(92) = (rk_m2(21))
      rconst(93) = (rk_m2(22))
      rconst(94) = (rk_m2(23))
      rconst(95) = (rk_m2(24))
      rconst(96) = (rk_m2(25))
      rconst(97) = (rk_m2(26))
      rconst(98) = (rk_m2(27))
      rconst(99) = (rk_m2(28))
      rconst(100) = (rk_m2(29))
      rconst(101) = (rk_m2(30))
      rconst(102) = (rk_m2(33))
      rconst(103) = (rk_m2(35))
      rconst(104) = (rk_m2(36))
      rconst(105) = (rk_m2(37))
      rconst(106) = (rk_m2(38))
      rconst(107) = (rk_m2(40))
      rconst(108) = (rk_m2(41))
      rconst(109) = (rk_m2(42))
      rconst(110) = (rk_m2(43))
      rconst(111) = (rk_m2(45))
      rconst(112) = (rk_m2(46))
      rconst(113) = (rk_m2(47))
      rconst(114) = (rk_m2(48))
      rconst(115) = (rk_m2(50))
      rconst(116) = (rk_m2(51))
      rconst(117) = (rk_m2(52))
      rconst(118) = (rk_m2(53))
      rconst(119) = (rk_m3(1))
      rconst(120) = (rk_m3(2))
      rconst(121) = (rk_m3(3))
      rconst(122) = (rk_m3(4))
      rconst(123) = (rk_m3(5))
      rconst(124) = (rk_m3(6))
      rconst(125) = (rk_m3(7))
      rconst(126) = (rk_m3(8))
      rconst(127) = (rk_m3(9))
      rconst(128) = (rk_m3(10))
      rconst(129) = (rk_m3(11))
      rconst(130) = (rk_m3(12))
      rconst(131) = (rk_m3(13))
      rconst(132) = (rk_m3(14))
      rconst(133) = (rk_m3(15))
      rconst(134) = (rk_m3(16))
      rconst(135) = (rk_m4(1))
      rconst(136) = (rk_m4(2))
      rconst(137) = (rk_m4(3))
      rconst(138) = (rk_m4(4))
      rconst(139) = (rk_m4(5))
      rconst(140) = (rk_m4(6))
      rconst(141) = (rk_m4(7))
      rconst(142) = (rk_m4(8))
      rconst(143) = (rk_m4(9))
      rconst(144) = (rk_m4(10))
      rconst(145) = (rk_m4(11))
      rconst(146) = (rk_m4(12))
      rconst(147) = (rk_m4(13))
      rconst(148) = (rk_m4(14))
      rconst(149) = (rk_m4(15))
      rconst(150) = (rk_m4(16))
      rconst(151) = (rk_m4(17))
      rconst(152) = (rk_m4(18))
      rconst(153) = (rk_m4(19))
      rconst(154) = (rk_m4(20))
      rconst(155) = (rk_m4(21))
      rconst(156) = (rk_m4(22))
      rconst(157) = (rk_m4(23))
      rconst(158) = (rk_m4(24))
      rconst(159) = (rk_m4(25))
      rconst(160) = (rk_m4(26))
      rconst(161) = (rk_m4(27))
      rconst(162) = (rk_m4(28))
      rconst(163) = (rk_m4(29))
      rconst(164) = (rk_m4(30))
      rconst(165) = (rk_m4(31))
      rconst(166) = (rk_m4(32))
      return
      end subroutine cbmz_v02r06_maprates 


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r06_dydt( nvardum, tdum, v, a_var, f, rconst )
!
!   computes rates of change for mechanism-version-regime = cbmz_v02r06
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     nvardum = number of variable species [input]
      integer nvardum
!     tdum = time [input]
      real tdum
!     v = concentrations of variable species [input]
      real v(nvar_r06_kpp)
!     a_var = dydt for each species [output]
      real a_var(nvar_r06_kpp)
!     f = concentrations of fixed species [input]
      real f(nfixed_kppmax)
!     rconst = reaction rate constants [input]
      real rconst(nreact_r06_kpp)

!   local variables
!     a = rate for each reaction
      real a(nreact_r06_kpp)

! computation of equation rates
      a(1) = rconst(1)*v(56)
      a(2) = rconst(2)*v(62)
      a(3) = rconst(3)*v(24)
      a(4) = rconst(4)*v(33)
      a(5) = rconst(5)*v(17)
      a(6) = rconst(6)*v(14)
      a(7) = rconst(7)*v(58)
      a(8) = rconst(8)*v(58)
      a(9) = rconst(9)*v(13)
      a(10) = rconst(10)*v(6)*f(4)
      a(11) = rconst(11)*v(6)*f(5)
      a(12) = rconst(12)*v(6)*f(2)
      a(13) = rconst(13)*v(46)*f(4)
      a(14) = rconst(14)*v(46)*v(58)
      a(15) = rconst(15)*v(46)*v(56)
      a(16) = rconst(16)*v(46)*v(56)
      a(17) = rconst(17)*v(46)*v(60)
      a(18) = rconst(18)*v(58)*v(60)
      a(19) = rconst(19)*v(56)*v(58)
      a(20) = rconst(20)*v(58)*v(61)
      a(21) = rconst(21)*v(58)*v(63)
      a(22) = rconst(22)*v(61)*f(3)
      a(23) = rconst(23)*v(60)*v(61)
      a(24) = rconst(24)*v(56)*v(61)
      a(25) = rconst(25)*v(61)*v(62)
      a(26) = rconst(26)*v(24)*v(61)
      a(27) = rconst(27)*v(33)*v(61)
      a(28) = rconst(28)*v(17)*v(61)
      a(29) = rconst(29)*v(61)*v(63)
      a(30) = rconst(30)*v(13)*v(61)
      a(31) = rconst(31)*v(63)*v(63)
      a(32) = rconst(32)*v(63)*v(63)*f(2)
      a(33) = rconst(33)*v(60)*v(63)
      a(34) = rconst(34)*v(56)*v(63)
      a(35) = rconst(35)*v(56)*v(63)
      a(36) = rconst(36)*v(17)
      a(37) = rconst(37)*v(60)*v(62)
      a(38) = rconst(38)*v(56)*v(62)
      a(39) = rconst(39)*v(56)*v(62)
      a(40) = rconst(40)*v(62)*v(62)
      a(41) = rconst(41)*v(62)*v(63)
      a(42) = rconst(42)*v(14)*f(2)
      a(43) = rconst(43)*v(14)
      a(44) = rconst(44)*v(31)*v(61)
      a(45) = rconst(45)*v(8)*v(61)
      a(46) = rconst(46)*v(61)*f(1)
      a(47) = rconst(47)*v(9)*v(61)
      a(48) = rconst(48)*v(23)*v(61)
      a(49) = rconst(49)*v(49)
      a(50) = rconst(50)*v(49)
      a(51) = rconst(51)*v(49)*v(61)
      a(52) = rconst(52)*v(49)*v(62)
      a(53) = rconst(53)*v(25)
      a(54) = rconst(54)*v(21)
      a(55) = rconst(55)*v(25)*v(61)
      a(56) = rconst(56)*v(21)*v(61)
      a(57) = rconst(57)*v(55)*v(60)
      a(58) = rconst(58)*v(44)*v(60)
      a(59) = rconst(59)*v(55)*v(62)
      a(60) = rconst(60)*v(44)*v(62)
      a(61) = rconst(61)*v(55)*v(63)
      a(62) = rconst(62)*v(44)*v(63)
      a(63) = rconst(63)*v(55)
      a(64) = rconst(64)*v(44)
      a(65) = rconst(65)*v(7)*v(61)
      a(66) = rconst(66)*v(48)
      a(67) = rconst(67)*v(48)*v(61)
      a(68) = rconst(68)*v(48)*v(62)
      a(69) = rconst(69)*v(52)*v(56)
      a(70) = rconst(70)*v(10)
      a(71) = rconst(71)*v(52)*v(60)
      a(72) = rconst(72)*v(52)*v(62)
      a(73) = rconst(73)*v(52)*v(63)
      a(74) = rconst(74)*v(52)
      a(75) = rconst(75)*v(30)*v(61)
      a(76) = rconst(76)*v(41)
      a(77) = rconst(77)*v(41)*v(61)
      a(78) = rconst(78)*v(43)
      a(79) = rconst(79)*v(43)*v(61)
      a(80) = rconst(80)*v(43)*v(62)
      a(81) = rconst(81)*v(22)*v(58)
      a(82) = rconst(82)*v(22)*v(61)
      a(83) = rconst(83)*v(37)*v(58)
      a(84) = rconst(84)*v(42)*v(58)
      a(85) = rconst(85)*v(37)*v(61)
      a(86) = rconst(86)*v(42)*v(61)
      a(87) = rconst(87)*v(37)*v(62)
      a(88) = rconst(88)*v(42)*v(62)
      a(89) = rconst(89)*v(12)*v(61)
      a(90) = rconst(90)*v(15)*v(61)
      a(91) = rconst(91)*v(18)*v(60)
      a(92) = rconst(92)*v(26)*v(61)
      a(93) = rconst(93)*v(26)*v(62)
      a(94) = rconst(94)*v(16)*v(56)
      a(95) = rconst(95)*v(32)*v(61)
      a(96) = rconst(96)*v(32)
      a(97) = rconst(97)*v(32)*v(58)
      a(98) = rconst(98)*v(53)
      a(99) = rconst(99)*v(53)*v(61)
      a(100) = rconst(100)*v(57)*v(61)
      a(101) = rconst(101)*v(57)
      a(102) = rconst(102)*v(59)*v(60)
      a(103) = rconst(103)*v(60)*v(64)
      a(104) = rconst(104)*v(47)*v(60)
      a(105) = rconst(105)*v(40)*v(60)
      a(106) = rconst(106)*v(59)*v(62)
      a(107) = rconst(107)*v(62)*v(64)
      a(108) = rconst(108)*v(47)*v(62)
      a(109) = rconst(109)*v(40)*v(62)
      a(110) = rconst(110)*v(59)*v(63)
      a(111) = rconst(111)*v(63)*v(64)
      a(112) = rconst(112)*v(47)*v(63)
      a(113) = rconst(113)*v(40)*v(63)
      a(114) = rconst(114)*v(59)
      a(115) = rconst(115)*v(64)
      a(116) = rconst(116)*v(47)
      a(117) = rconst(117)*v(40)
      a(118) = rconst(118)*v(20)*v(30)
      a(119) = rconst(119)*v(38)*v(61)
      a(120) = rconst(120)*v(38)*v(58)
      a(121) = rconst(121)*v(38)*v(62)
      a(122) = rconst(122)*v(51)
      a(123) = rconst(123)*v(51)*v(61)
      a(124) = rconst(124)*v(51)*v(58)
      a(125) = rconst(125)*v(51)*v(62)
      a(126) = rconst(126)*v(35)*v(60)
      a(127) = rconst(127)*v(29)*v(60)
      a(128) = rconst(128)*v(36)*v(60)
      a(129) = rconst(129)*v(35)*v(63)
      a(130) = rconst(130)*v(29)*v(63)
      a(131) = rconst(131)*v(36)*v(63)
      a(132) = rconst(132)*v(35)
      a(133) = rconst(133)*v(29)
      a(134) = rconst(134)*v(36)
      a(135) = rconst(135)*v(34)*v(61)
      a(136) = rconst(136)*v(34)*v(62)
      a(137) = rconst(137)*v(34)*v(46)
      a(138) = rconst(138)*v(34)*v(61)
      a(139) = rconst(139)*v(39)*v(60)
      a(140) = rconst(140)*v(39)*v(55)
      a(141) = rconst(141)*v(39)*v(50)
      a(142) = rconst(142)*v(39)*v(39)
      a(143) = rconst(143)*v(19)*v(61)
      a(144) = rconst(144)*v(11)*v(61)
      a(145) = rconst(145)*v(28)*v(60)
      a(146) = rconst(146)*v(28)*v(55)
      a(147) = rconst(147)*v(45)*v(63)
      a(148) = rconst(148)*v(45)*v(62)
      a(149) = rconst(149)*v(45)*v(55)
      a(150) = rconst(150)*v(45)*v(61)
      a(151) = rconst(151)*v(45)*v(54)
      a(152) = rconst(152)*v(50)
      a(153) = rconst(153)*v(50)*v(56)
      a(154) = rconst(154)*v(50)*v(58)
      a(155) = rconst(155)*v(50)*v(63)
      a(156) = rconst(156)*v(50)*v(55)
      a(157) = rconst(157)*v(50)*v(61)
      a(158) = rconst(158)*v(50)*f(4)
      a(159) = rconst(159)*v(27)
      a(160) = rconst(160)*v(27)*v(60)
      a(161) = rconst(161)*v(27)*v(55)
      a(162) = rconst(162)*v(54)
      a(163) = rconst(163)*v(54)*v(56)
      a(164) = rconst(164)*v(54)*v(60)
      a(165) = rconst(165)*v(54)*v(63)
      a(166) = rconst(166)*v(49)*v(54)

! aggregate function
      a_var(1) = a(45)+a(162)
      a_var(2) = 0.52*a(81)+0.22*a(83)+0.39*a(120)+0.46*a(124)
      a_var(3) = 0.4*a(73)+0.09*a(83)+0.16*a(84)
      a_var(4) = a(151)+a(157)+a(163)+a(164)+a(165)+a(166)
      a_var(5) = 0.15*a(142)
      a_var(6) = a(8)-a(10)-a(11)-a(12)
      a_var(7) = -a(65)
      a_var(8) = -a(45)+a(152)
      a_var(9) = -a(47)+0.2*a(64)
      a_var(10) = a(69)-a(70)
      a_var(11) = 0.27*a(143)-a(144)
      a_var(12) = -a(89)
      a_var(13) = -a(9)-a(30)+a(31)+a(32)+a(147)
      a_var(14) = -a(6)+a(39)-a(42)-a(43)
      a_var(15) = -a(90)
      a_var(16) = 0.4*a(92)+a(93)-a(94)
      a_var(17) = -a(5)-a(28)+a(34)-a(36)
      a_var(18) = 0.8*a(89)+0.45*a(90)-a(91)
      a_var(19) = 0.965*a(138)-a(143)
      a_var(20) = 1.06*a(83)+2.26*a(84)+a(85)+2.23*a(86)+1.98*a(98)   &
                 +0.42*a(99)+1.98*a(101)+1.68*a(102)+a(104)+1.98   &
                 *a(106)+a(108)+1.25*a(114)+a(116)-a(118)
      a_var(21) = -a(54)-a(56)+a(62)
      a_var(22) = -a(81)-a(82)
      a_var(23) = -a(48)+0.34*a(63)+0.03*a(83)+0.04*a(84)
      a_var(24) = -a(3)+a(23)-a(26)+a(35)+a(164)
      a_var(25) = -a(53)-a(55)+a(61)+a(149)
      a_var(26) = 0.12*a(89)+0.05*a(90)-a(92)-a(93)
      a_var(27) = a(158)-a(159)-a(160)-a(161)
      a_var(28) = a(144)-a(145)-a(146)
      a_var(29) = a(121)-a(127)-a(130)-a(133)
      a_var(30) = -a(75)+1.1*a(90)-a(118)+1.86*a(125)+0.18*a(126)+1.6   &
                 *a(127)+2*a(130)+2*a(133)
      a_var(31) = -a(44)+a(49)+a(50)+a(51)+a(52)+a(66)+a(78)+a(80)   &
                 +0.24*a(81)+0.31*a(83)+0.3*a(84)+2*a(95)+a(96)+0.69   &
                 *a(97)+0.07*a(120)+0.33*a(122)+0.16*a(124)+0.64   &
                 *a(125)+0.59*a(128)+a(166)
      a_var(32) = 0.95*a(91)+0.3*a(92)-a(95)-a(96)-a(97)
      a_var(33) = -a(4)+a(24)-a(27)+0.3*a(41)+2*a(42)+a(52)+a(68)   &
                 +a(80)+a(93)+0.07*a(125)+a(136)+a(148)+a(163)
      a_var(34) = -a(135)-a(136)-a(137)-a(138)
      a_var(35) = a(119)-a(126)-a(129)-a(132)
      a_var(36) = 0.5*a(123)-a(128)-a(131)-a(134)
      a_var(37) = -a(83)-a(85)-a(87)
      a_var(38) = -a(119)-a(120)-a(121)
      a_var(39) = a(135)+a(136)-a(139)-a(140)-a(141)-2*a(142)
      a_var(40) = a(79)+a(82)+a(85)+a(86)+0.08*a(89)+0.5*a(90)+0.6   &
                 *a(92)+a(95)+0.03*a(97)+0.4*a(98)+0.4*a(101)+0.34   &
                 *a(102)-a(105)+0.4*a(106)-a(109)-a(113)+0.24*a(114)   &
                 -a(117)+0.08*a(119)+0.2*a(120)+0.2*a(123)+0.07*a(124)   &
                 +0.93*a(125)
      a_var(41) = -a(76)-a(77)+0.07*a(84)+0.23*a(86)+0.74*a(98)+0.74   &
                 *a(101)+0.62*a(102)+0.74*a(106)+0.57*a(114)+0.15   &
                 *a(115)+0.03*a(122)+0.09*a(124)+0.63*a(128)+0.5   &
                 *a(134)
      a_var(42) = -a(84)-a(86)-a(88)
      a_var(43) = -a(78)-a(79)-a(80)+0.04*a(83)+0.07*a(84)+0.8*a(90)   &
                 +0.2*a(97)+0.19*a(99)+0.15*a(115)+0.85*a(124)+0.34   &
                 *a(128)
      a_var(44) = a(47)+0.5*a(56)-a(58)-a(60)-a(62)-a(64)+0.06*a(83)   &
                 +0.05*a(84)+0.1*a(98)+0.1*a(101)+0.08*a(102)+0.1   &
                 *a(106)+0.06*a(114)
      a_var(45) = 0.73*a(143)-a(147)-a(148)-a(149)-a(150)-a(151)
      a_var(46) = a(1)+0.89*a(2)+a(7)+a(10)+a(11)-a(13)-a(14)-a(15)   &
                 -a(16)-a(17)-a(137)
      a_var(47) = a(87)+a(88)+a(100)-a(104)-a(108)-a(112)-a(116)
      a_var(48) = a(54)+0.5*a(56)+a(58)+a(60)+0.8*a(64)+a(65)-a(66)   &
                 -a(67)-a(68)+0.22*a(82)+0.47*a(83)+1.03*a(84)+a(85)   &
                 +1.77*a(86)+0.03*a(97)+0.3*a(98)+0.04*a(99)+0.3   &
                 *a(101)+0.25*a(102)+0.5*a(104)+0.3*a(106)+0.5*a(108)   &
                 +0.21*a(114)+0.5*a(116)+0.15*a(120)+0.07*a(122)+0.02   &
                 *a(124)+0.28*a(125)+0.8*a(127)+0.55*a(128)+a(133)+0.5   &
                 *a(134)
      a_var(49) = a(48)-a(49)-a(50)-a(51)-a(52)+a(53)+0.3*a(55)+a(57)   &
                 +a(59)+0.66*a(63)+a(81)+1.56*a(82)+0.57*a(83)+a(85)   &
                 +a(95)+0.7*a(97)+a(103)+0.5*a(104)+a(107)+0.5*a(108)   &
                 +0.7*a(115)+0.5*a(116)+0.6*a(120)+0.2*a(122)+0.15   &
                 *a(124)+0.28*a(125)+0.63*a(126)+0.25*a(128)+a(139)+2   &
                 *a(140)+a(141)+a(145)+2*a(146)+a(156)+a(161)-a(166)
      a_var(50) = a(137)+0.035*a(138)+a(139)+a(140)+1.85*a(142)+a(145)   &
                 +a(146)+a(147)+a(148)+a(149)+a(150)+a(151)-a(152)   &
                 -a(153)-a(154)-a(155)-a(156)-a(157)-a(158)+a(159)
      a_var(51) = 0.65*a(120)-a(122)-a(123)-a(124)-a(125)+0.91*a(126)   &
                 +0.2*a(127)+a(132)
      a_var(52) = a(67)+a(68)-a(69)+a(70)-a(71)-a(72)-a(73)-a(74)   &
                 +a(76)+a(78)+a(79)+a(80)+0.13*a(83)+0.19*a(84)+a(95)   &
                 +a(96)+0.62*a(97)+a(103)+a(107)+0.7*a(115)+0.2*a(120)   &
                 +0.97*a(122)+0.5*a(123)+0.11*a(124)+0.07*a(125)
      a_var(53) = -a(98)-a(99)+a(110)+a(111)+a(129)+a(131)
      a_var(54) = a(141)-a(151)+a(153)+a(154)+a(155)+a(156)+a(160)   &
                 +a(161)-a(162)-a(163)-a(164)-a(165)-a(166)
      a_var(55) = a(46)+0.7*a(55)-a(57)-a(59)-a(61)-a(63)+a(66)+a(71)   &
                 +a(72)+a(74)+a(76)+0.07*a(83)+0.1*a(84)+0.7*a(122)   &
                 +0.05*a(124)+a(137)+0.035*a(138)-a(140)+0.73*a(143)   &
                 -a(146)-a(149)+a(152)-a(156)-a(161)+a(162)
      a_var(56) = -a(1)+0.89*a(2)+a(4)+a(5)+a(6)-a(15)-a(16)+a(17)   &
                 +a(18)-a(19)-a(24)+a(25)+a(26)+a(28)+a(33)-a(34)   &
                 -a(35)+a(36)+2*a(37)-a(39)+2*a(40)+0.7*a(41)+a(43)   &
                 +a(57)+a(58)+a(59)+a(60)-a(69)+a(70)+a(71)+a(72)+0.95   &
                 *a(91)-a(94)+a(101)+0.84*a(102)+a(103)+1.5*a(104)   &
                 +a(105)+a(106)+a(107)+1.5*a(108)+a(109)+0.5*a(116)   &
                 +0.91*a(126)+1.2*a(127)+a(128)+a(139)+a(145)-a(153)   &
                 +a(160)-a(163)
      a_var(57) = 0.05*a(91)+a(94)-a(100)-a(101)+0.16*a(102)+0.5   &
                 *a(104)+0.5*a(108)+a(112)+0.5*a(116)+0.93*a(125)+0.09   &
                 *a(126)+0.8*a(127)+a(130)+a(133)
      a_var(58) = -a(7)-a(8)+a(13)-a(14)-a(18)-a(19)-a(20)-a(21)+0.4   &
                 *a(73)-a(81)-a(83)-a(84)-a(97)-a(120)-a(124)-a(154)
      a_var(59) = a(75)+0.03*a(83)+0.09*a(84)+0.77*a(99)-a(102)-a(106)   &
                 -a(110)-a(114)
      a_var(60) = a(1)+0.11*a(2)+a(3)+a(15)-a(17)-a(18)-a(23)-a(33)   &
                 -a(37)+a(38)-a(57)-a(58)-a(71)-a(91)-a(102)-a(103)   &
                 -a(104)-a(105)-a(126)-a(127)-a(128)-a(139)-a(145)   &
                 +a(153)-a(160)-a(164)
      a_var(61) = a(3)+a(4)+2*a(9)+2*a(12)-a(20)+a(21)-a(22)-a(23)   &
                 -a(24)-a(25)-a(26)-a(27)-a(28)-a(29)-a(30)+a(33)+0.7   &
                 *a(41)-a(44)-a(45)-a(46)-a(47)-a(48)-a(51)+a(53)   &
                 +a(54)-0.7*a(55)-0.5*a(56)-a(65)-a(67)-a(75)-a(77)   &
                 -a(79)+0.12*a(81)-a(82)+0.33*a(83)+0.6*a(84)-a(85)   &
                 -a(86)-a(89)-a(90)-a(92)-a(95)+0.08*a(97)+a(98)-0.77   &
                 *a(99)-a(100)-a(119)+0.27*a(120)-a(123)+0.27*a(124)   &
                 -a(135)-a(138)-a(143)-a(144)-a(150)+a(155)-a(157)
      a_var(62) = -a(2)+a(6)+a(16)+a(19)-a(25)+a(27)-a(37)-a(38)-a(39)   &
                 -2*a(40)-a(41)+a(43)-a(52)-a(59)-a(60)-a(68)-a(72)   &
                 -a(80)-a(87)-a(88)-a(93)-a(106)-a(107)-a(108)-a(109)   &
                 -a(121)-a(125)-a(136)-a(148)
      a_var(63) = a(5)+a(20)-a(21)+a(22)+a(25)-a(29)+a(30)-2*a(31)-2   &
                 *a(32)-a(33)-a(34)-a(35)+a(36)-a(41)+a(44)+a(45)   &
                 +a(48)+2*a(49)+a(51)+a(52)+a(53)+a(54)+a(57)+a(58)   &
                 +a(59)+a(60)-a(61)-a(62)+0.32*a(63)+0.6*a(64)+a(65)   &
                 +a(66)-a(73)+a(78)+0.22*a(81)+a(82)+0.26*a(83)+0.22   &
                 *a(84)+a(85)+a(86)+0.2*a(89)+0.55*a(90)+0.95*a(91)   &
                 +0.6*a(92)+2*a(95)+a(96)+0.76*a(97)+0.9*a(98)+0.9   &
                 *a(101)+0.76*a(102)+0.5*a(104)+0.9*a(106)+0.5*a(108)   &
                 -a(110)-a(111)-a(112)-a(113)+0.54*a(114)+0.07*a(120)   &
                 +0.33*a(122)+0.1*a(124)+0.93*a(125)+0.91*a(126)+0.8   &
                 *a(127)+a(128)-a(129)-a(130)-a(131)+0.965*a(138)   &
                 +a(140)+0.27*a(143)+a(146)-a(147)-a(155)+a(156)   &
                 +a(161)-a(165)+a(166)
      a_var(64) = a(77)+0.11*a(84)-a(103)-a(107)-a(111)-a(115)
      return
      end subroutine cbmz_v02r06_dydt                                      


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r06_jacob( nvardum, tdum, v, jvs, f, rconst )
!
!   computes jacobian for mechanism-version-regime = cbmz_v02r06
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     nvardum = number of variable species [input]
      integer nvardum
!     tdum = time [input]
      real tdum
!     v = concentrations of variable species [input]
      real v(nvar_r06_kpp)
!     jvs = non-zero jacobian elements [output]
      real jvs(lu_nonzero_v_r06_kpp)
!     f = concentrations of fixed species [input]
      real f(nfixed_kppmax)
!     rconst = reaction rate constants [input]
      real rconst(nreact_r06_kpp)

!   local variables
!     b(i,j) = d[reaction_rate(i)] / d[species_conc(j)]
      real b(nreact_r06_kpp,nvar_r06_kpp)

! computation of b(i,j) = da(i)/dv(j)
      b(1,56) = rconst(1)
      b(2,62) = rconst(2)
      b(3,24) = rconst(3)
      b(4,33) = rconst(4)
      b(5,17) = rconst(5)
      b(6,14) = rconst(6)
      b(7,58) = rconst(7)
      b(8,58) = rconst(8)
      b(9,13) = rconst(9)
      b(10,6) = rconst(10)*f(4)
      b(11,6) = rconst(11)*f(5)
      b(12,6) = rconst(12)*f(2)
      b(13,46) = rconst(13)*f(4)
      b(14,46) = rconst(14)*v(58)
      b(14,58) = rconst(14)*v(46)
      b(15,46) = rconst(15)*v(56)
      b(15,56) = rconst(15)*v(46)
      b(16,46) = rconst(16)*v(56)
      b(16,56) = rconst(16)*v(46)
      b(17,46) = rconst(17)*v(60)
      b(17,60) = rconst(17)*v(46)
      b(18,58) = rconst(18)*v(60)
      b(18,60) = rconst(18)*v(58)
      b(19,56) = rconst(19)*v(58)
      b(19,58) = rconst(19)*v(56)
      b(20,58) = rconst(20)*v(61)
      b(20,61) = rconst(20)*v(58)
      b(21,58) = rconst(21)*v(63)
      b(21,63) = rconst(21)*v(58)
      b(22,61) = rconst(22)*f(3)
      b(23,60) = rconst(23)*v(61)
      b(23,61) = rconst(23)*v(60)
      b(24,56) = rconst(24)*v(61)
      b(24,61) = rconst(24)*v(56)
      b(25,61) = rconst(25)*v(62)
      b(25,62) = rconst(25)*v(61)
      b(26,24) = rconst(26)*v(61)
      b(26,61) = rconst(26)*v(24)
      b(27,33) = rconst(27)*v(61)
      b(27,61) = rconst(27)*v(33)
      b(28,17) = rconst(28)*v(61)
      b(28,61) = rconst(28)*v(17)
      b(29,61) = rconst(29)*v(63)
      b(29,63) = rconst(29)*v(61)
      b(30,13) = rconst(30)*v(61)
      b(30,61) = rconst(30)*v(13)
      b(31,63) = rconst(31)*2*v(63)
      b(32,63) = rconst(32)*2*v(63)*f(2)
      b(33,60) = rconst(33)*v(63)
      b(33,63) = rconst(33)*v(60)
      b(34,56) = rconst(34)*v(63)
      b(34,63) = rconst(34)*v(56)
      b(35,56) = rconst(35)*v(63)
      b(35,63) = rconst(35)*v(56)
      b(36,17) = rconst(36)
      b(37,60) = rconst(37)*v(62)
      b(37,62) = rconst(37)*v(60)
      b(38,56) = rconst(38)*v(62)
      b(38,62) = rconst(38)*v(56)
      b(39,56) = rconst(39)*v(62)
      b(39,62) = rconst(39)*v(56)
      b(40,62) = rconst(40)*2*v(62)
      b(41,62) = rconst(41)*v(63)
      b(41,63) = rconst(41)*v(62)
      b(42,14) = rconst(42)*f(2)
      b(43,14) = rconst(43)
      b(44,31) = rconst(44)*v(61)
      b(44,61) = rconst(44)*v(31)
      b(45,8) = rconst(45)*v(61)
      b(45,61) = rconst(45)*v(8)
      b(46,61) = rconst(46)*f(1)
      b(47,9) = rconst(47)*v(61)
      b(47,61) = rconst(47)*v(9)
      b(48,23) = rconst(48)*v(61)
      b(48,61) = rconst(48)*v(23)
      b(49,49) = rconst(49)
      b(50,49) = rconst(50)
      b(51,49) = rconst(51)*v(61)
      b(51,61) = rconst(51)*v(49)
      b(52,49) = rconst(52)*v(62)
      b(52,62) = rconst(52)*v(49)
      b(53,25) = rconst(53)
      b(54,21) = rconst(54)
      b(55,25) = rconst(55)*v(61)
      b(55,61) = rconst(55)*v(25)
      b(56,21) = rconst(56)*v(61)
      b(56,61) = rconst(56)*v(21)
      b(57,55) = rconst(57)*v(60)
      b(57,60) = rconst(57)*v(55)
      b(58,44) = rconst(58)*v(60)
      b(58,60) = rconst(58)*v(44)
      b(59,55) = rconst(59)*v(62)
      b(59,62) = rconst(59)*v(55)
      b(60,44) = rconst(60)*v(62)
      b(60,62) = rconst(60)*v(44)
      b(61,55) = rconst(61)*v(63)
      b(61,63) = rconst(61)*v(55)
      b(62,44) = rconst(62)*v(63)
      b(62,63) = rconst(62)*v(44)
      b(63,55) = rconst(63)
      b(64,44) = rconst(64)
      b(65,7) = rconst(65)*v(61)
      b(65,61) = rconst(65)*v(7)
      b(66,48) = rconst(66)
      b(67,48) = rconst(67)*v(61)
      b(67,61) = rconst(67)*v(48)
      b(68,48) = rconst(68)*v(62)
      b(68,62) = rconst(68)*v(48)
      b(69,52) = rconst(69)*v(56)
      b(69,56) = rconst(69)*v(52)
      b(70,10) = rconst(70)
      b(71,52) = rconst(71)*v(60)
      b(71,60) = rconst(71)*v(52)
      b(72,52) = rconst(72)*v(62)
      b(72,62) = rconst(72)*v(52)
      b(73,52) = rconst(73)*v(63)
      b(73,63) = rconst(73)*v(52)
      b(74,52) = rconst(74)
      b(75,30) = rconst(75)*v(61)
      b(75,61) = rconst(75)*v(30)
      b(76,41) = rconst(76)
      b(77,41) = rconst(77)*v(61)
      b(77,61) = rconst(77)*v(41)
      b(78,43) = rconst(78)
      b(79,43) = rconst(79)*v(61)
      b(79,61) = rconst(79)*v(43)
      b(80,43) = rconst(80)*v(62)
      b(80,62) = rconst(80)*v(43)
      b(81,22) = rconst(81)*v(58)
      b(81,58) = rconst(81)*v(22)
      b(82,22) = rconst(82)*v(61)
      b(82,61) = rconst(82)*v(22)
      b(83,37) = rconst(83)*v(58)
      b(83,58) = rconst(83)*v(37)
      b(84,42) = rconst(84)*v(58)
      b(84,58) = rconst(84)*v(42)
      b(85,37) = rconst(85)*v(61)
      b(85,61) = rconst(85)*v(37)
      b(86,42) = rconst(86)*v(61)
      b(86,61) = rconst(86)*v(42)
      b(87,37) = rconst(87)*v(62)
      b(87,62) = rconst(87)*v(37)
      b(88,42) = rconst(88)*v(62)
      b(88,62) = rconst(88)*v(42)
      b(89,12) = rconst(89)*v(61)
      b(89,61) = rconst(89)*v(12)
      b(90,15) = rconst(90)*v(61)
      b(90,61) = rconst(90)*v(15)
      b(91,18) = rconst(91)*v(60)
      b(91,60) = rconst(91)*v(18)
      b(92,26) = rconst(92)*v(61)
      b(92,61) = rconst(92)*v(26)
      b(93,26) = rconst(93)*v(62)
      b(93,62) = rconst(93)*v(26)
      b(94,16) = rconst(94)*v(56)
      b(94,56) = rconst(94)*v(16)
      b(95,32) = rconst(95)*v(61)
      b(95,61) = rconst(95)*v(32)
      b(96,32) = rconst(96)
      b(97,32) = rconst(97)*v(58)
      b(97,58) = rconst(97)*v(32)
      b(98,53) = rconst(98)
      b(99,53) = rconst(99)*v(61)
      b(99,61) = rconst(99)*v(53)
      b(100,57) = rconst(100)*v(61)
      b(100,61) = rconst(100)*v(57)
      b(101,57) = rconst(101)
      b(102,59) = rconst(102)*v(60)
      b(102,60) = rconst(102)*v(59)
      b(103,60) = rconst(103)*v(64)
      b(103,64) = rconst(103)*v(60)
      b(104,47) = rconst(104)*v(60)
      b(104,60) = rconst(104)*v(47)
      b(105,40) = rconst(105)*v(60)
      b(105,60) = rconst(105)*v(40)
      b(106,59) = rconst(106)*v(62)
      b(106,62) = rconst(106)*v(59)
      b(107,62) = rconst(107)*v(64)
      b(107,64) = rconst(107)*v(62)
      b(108,47) = rconst(108)*v(62)
      b(108,62) = rconst(108)*v(47)
      b(109,40) = rconst(109)*v(62)
      b(109,62) = rconst(109)*v(40)
      b(110,59) = rconst(110)*v(63)
      b(110,63) = rconst(110)*v(59)
      b(111,63) = rconst(111)*v(64)
      b(111,64) = rconst(111)*v(63)
      b(112,47) = rconst(112)*v(63)
      b(112,63) = rconst(112)*v(47)
      b(113,40) = rconst(113)*v(63)
      b(113,63) = rconst(113)*v(40)
      b(114,59) = rconst(114)
      b(115,64) = rconst(115)
      b(116,47) = rconst(116)
      b(117,40) = rconst(117)
      b(118,20) = rconst(118)*v(30)
      b(118,30) = rconst(118)*v(20)
      b(119,38) = rconst(119)*v(61)
      b(119,61) = rconst(119)*v(38)
      b(120,38) = rconst(120)*v(58)
      b(120,58) = rconst(120)*v(38)
      b(121,38) = rconst(121)*v(62)
      b(121,62) = rconst(121)*v(38)
      b(122,51) = rconst(122)
      b(123,51) = rconst(123)*v(61)
      b(123,61) = rconst(123)*v(51)
      b(124,51) = rconst(124)*v(58)
      b(124,58) = rconst(124)*v(51)
      b(125,51) = rconst(125)*v(62)
      b(125,62) = rconst(125)*v(51)
      b(126,35) = rconst(126)*v(60)
      b(126,60) = rconst(126)*v(35)
      b(127,29) = rconst(127)*v(60)
      b(127,60) = rconst(127)*v(29)
      b(128,36) = rconst(128)*v(60)
      b(128,60) = rconst(128)*v(36)
      b(129,35) = rconst(129)*v(63)
      b(129,63) = rconst(129)*v(35)
      b(130,29) = rconst(130)*v(63)
      b(130,63) = rconst(130)*v(29)
      b(131,36) = rconst(131)*v(63)
      b(131,63) = rconst(131)*v(36)
      b(132,35) = rconst(132)
      b(133,29) = rconst(133)
      b(134,36) = rconst(134)
      b(135,34) = rconst(135)*v(61)
      b(135,61) = rconst(135)*v(34)
      b(136,34) = rconst(136)*v(62)
      b(136,62) = rconst(136)*v(34)
      b(137,34) = rconst(137)*v(46)
      b(137,46) = rconst(137)*v(34)
      b(138,34) = rconst(138)*v(61)
      b(138,61) = rconst(138)*v(34)
      b(139,39) = rconst(139)*v(60)
      b(139,60) = rconst(139)*v(39)
      b(140,39) = rconst(140)*v(55)
      b(140,55) = rconst(140)*v(39)
      b(141,39) = rconst(141)*v(50)
      b(141,50) = rconst(141)*v(39)
      b(142,39) = rconst(142)*2*v(39)
      b(143,19) = rconst(143)*v(61)
      b(143,61) = rconst(143)*v(19)
      b(144,11) = rconst(144)*v(61)
      b(144,61) = rconst(144)*v(11)
      b(145,28) = rconst(145)*v(60)
      b(145,60) = rconst(145)*v(28)
      b(146,28) = rconst(146)*v(55)
      b(146,55) = rconst(146)*v(28)
      b(147,45) = rconst(147)*v(63)
      b(147,63) = rconst(147)*v(45)
      b(148,45) = rconst(148)*v(62)
      b(148,62) = rconst(148)*v(45)
      b(149,45) = rconst(149)*v(55)
      b(149,55) = rconst(149)*v(45)
      b(150,45) = rconst(150)*v(61)
      b(150,61) = rconst(150)*v(45)
      b(151,45) = rconst(151)*v(54)
      b(151,54) = rconst(151)*v(45)
      b(152,50) = rconst(152)
      b(153,50) = rconst(153)*v(56)
      b(153,56) = rconst(153)*v(50)
      b(154,50) = rconst(154)*v(58)
      b(154,58) = rconst(154)*v(50)
      b(155,50) = rconst(155)*v(63)
      b(155,63) = rconst(155)*v(50)
      b(156,50) = rconst(156)*v(55)
      b(156,55) = rconst(156)*v(50)
      b(157,50) = rconst(157)*v(61)
      b(157,61) = rconst(157)*v(50)
      b(158,50) = rconst(158)*f(4)
      b(159,27) = rconst(159)
      b(160,27) = rconst(160)*v(60)
      b(160,60) = rconst(160)*v(27)
      b(161,27) = rconst(161)*v(55)
      b(161,55) = rconst(161)*v(27)
      b(162,54) = rconst(162)
      b(163,54) = rconst(163)*v(56)
      b(163,56) = rconst(163)*v(54)
      b(164,54) = rconst(164)*v(60)
      b(164,60) = rconst(164)*v(54)
      b(165,54) = rconst(165)*v(63)
      b(165,63) = rconst(165)*v(54)
      b(166,49) = rconst(166)*v(54)
      b(166,54) = rconst(166)*v(49)

! construct the jacobian terms from b's
      jvs(1) = 0
      jvs(2) = b(45,8)
      jvs(3) = b(162,54)
      jvs(4) = b(45,61)
      jvs(5) = 0
      jvs(6) = 0.52*b(81,22)
      jvs(7) = 0.22*b(83,37)
      jvs(8) = 0.39*b(120,38)
      jvs(9) = 0.46*b(124,51)
      jvs(10) = 0.52*b(81,58)+0.22*b(83,58)+0.39*b(120,58)+0.46   &
               *b(124,58)
      jvs(11) = 0
      jvs(12) = 0.09*b(83,37)
      jvs(13) = 0.16*b(84,42)
      jvs(14) = 0.4*b(73,52)
      jvs(15) = 0.09*b(83,58)+0.16*b(84,58)
      jvs(16) = 0.4*b(73,63)
      jvs(17) = 0
      jvs(18) = b(151,45)
      jvs(19) = b(166,49)
      jvs(20) = b(157,50)
      jvs(21) = b(151,54)+b(163,54)+b(164,54)+b(165,54)+b(166,54)
      jvs(22) = b(163,56)
      jvs(23) = b(164,60)
      jvs(24) = b(157,61)
      jvs(25) = b(165,63)
      jvs(26) = 0
      jvs(27) = 0.15*b(142,39)
      jvs(28) = -b(10,6)-b(11,6)-b(12,6)
      jvs(29) = b(8,58)
      jvs(30) = -b(65,7)
      jvs(31) = -b(65,61)
      jvs(32) = -b(45,8)
      jvs(33) = b(152,50)
      jvs(34) = -b(45,61)
      jvs(35) = -b(47,9)
      jvs(36) = 0.2*b(64,44)
      jvs(37) = -b(47,61)
      jvs(38) = -b(70,10)
      jvs(39) = b(69,52)
      jvs(40) = b(69,56)
      jvs(41) = -b(144,11)
      jvs(42) = 0.27*b(143,19)
      jvs(43) = 0.27*b(143,61)-b(144,61)
      jvs(44) = -b(89,12)
      jvs(45) = -b(89,61)
      jvs(46) = -b(9,13)-b(30,13)
      jvs(47) = b(147,45)
      jvs(48) = -b(30,61)
      jvs(49) = b(31,63)+b(32,63)+b(147,63)
      jvs(50) = -b(6,14)-b(42,14)-b(43,14)
      jvs(51) = b(39,56)
      jvs(52) = b(39,62)
      jvs(53) = -b(90,15)
      jvs(54) = -b(90,61)
      jvs(55) = -b(94,16)
      jvs(56) = 0.4*b(92,26)+b(93,26)
      jvs(57) = -b(94,56)
      jvs(58) = 0.4*b(92,61)
      jvs(59) = b(93,62)
      jvs(60) = -b(5,17)-b(28,17)-b(36,17)
      jvs(61) = b(34,56)
      jvs(62) = -b(28,61)
      jvs(63) = b(34,63)
      jvs(64) = 0.8*b(89,12)
      jvs(65) = 0.45*b(90,15)
      jvs(66) = -b(91,18)
      jvs(67) = -b(91,60)
      jvs(68) = 0.8*b(89,61)+0.45*b(90,61)
      jvs(69) = -b(143,19)
      jvs(70) = 0.965*b(138,34)
      jvs(71) = 0.965*b(138,61)-b(143,61)
      jvs(72) = -b(118,20)
      jvs(73) = -b(118,30)
      jvs(74) = 1.06*b(83,37)+b(85,37)
      jvs(75) = 2.26*b(84,42)+2.23*b(86,42)
      jvs(76) = b(104,47)+b(108,47)+b(116,47)
      jvs(77) = 1.98*b(98,53)+0.42*b(99,53)
      jvs(78) = 1.98*b(101,57)
      jvs(79) = 1.06*b(83,58)+2.26*b(84,58)
      jvs(80) = 1.68*b(102,59)+1.98*b(106,59)+1.25*b(114,59)
      jvs(81) = 1.68*b(102,60)+b(104,60)
      jvs(82) = b(85,61)+2.23*b(86,61)+0.42*b(99,61)
      jvs(83) = 1.98*b(106,62)+b(108,62)
      jvs(84) = -b(54,21)-b(56,21)
      jvs(85) = b(62,44)
      jvs(86) = -b(56,61)
      jvs(87) = b(62,63)
      jvs(88) = -b(81,22)-b(82,22)
      jvs(89) = -b(81,58)
      jvs(90) = -b(82,61)
      jvs(91) = -b(48,23)
      jvs(92) = 0.03*b(83,37)
      jvs(93) = 0.04*b(84,42)
      jvs(94) = 0.34*b(63,55)
      jvs(95) = 0.03*b(83,58)+0.04*b(84,58)
      jvs(96) = -b(48,61)
      jvs(97) = -b(3,24)-b(26,24)
      jvs(98) = b(164,54)
      jvs(99) = b(35,56)
      jvs(100) = b(23,60)+b(164,60)
      jvs(101) = b(23,61)-b(26,61)
      jvs(102) = b(35,63)
      jvs(103) = -b(53,25)-b(55,25)
      jvs(104) = b(149,45)
      jvs(105) = b(61,55)+b(149,55)
      jvs(106) = -b(55,61)
      jvs(107) = b(61,63)
      jvs(108) = 0.12*b(89,12)
      jvs(109) = 0.05*b(90,15)
      jvs(110) = -b(92,26)-b(93,26)
      jvs(111) = 0.12*b(89,61)+0.05*b(90,61)-b(92,61)
      jvs(112) = -b(93,62)
      jvs(113) = -b(159,27)-b(160,27)-b(161,27)
      jvs(114) = b(158,50)
      jvs(115) = -b(161,55)
      jvs(116) = -b(160,60)
      jvs(117) = b(144,11)
      jvs(118) = 0
      jvs(119) = -b(145,28)-b(146,28)
      jvs(120) = 0
      jvs(121) = -b(146,55)
      jvs(122) = -b(145,60)
      jvs(123) = b(144,61)
      jvs(124) = -b(127,29)-b(130,29)-b(133,29)
      jvs(125) = b(121,38)
      jvs(126) = -b(127,60)
      jvs(127) = b(121,62)
      jvs(128) = -b(130,63)
      jvs(129) = 1.1*b(90,15)
      jvs(130) = -b(118,20)
      jvs(131) = 1.6*b(127,29)+2*b(130,29)+2*b(133,29)
      jvs(132) = -b(75,30)-b(118,30)
      jvs(133) = 0.18*b(126,35)
      jvs(134) = 0
      jvs(135) = 0
      jvs(136) = 0
      jvs(137) = 0
      jvs(138) = 1.86*b(125,51)
      jvs(139) = 0
      jvs(140) = 0
      jvs(141) = 0
      jvs(142) = 0
      jvs(143) = 0.18*b(126,60)+1.6*b(127,60)
      jvs(144) = -b(75,61)+1.1*b(90,61)
      jvs(145) = 1.86*b(125,62)
      jvs(146) = 2*b(130,63)
      jvs(147) = 0.24*b(81,22)
      jvs(148) = -b(44,31)
      jvs(149) = 2*b(95,32)+b(96,32)+0.69*b(97,32)
      jvs(150) = 0.59*b(128,36)
      jvs(151) = 0.31*b(83,37)
      jvs(152) = 0.07*b(120,38)
      jvs(153) = 0.3*b(84,42)
      jvs(154) = b(78,43)+b(80,43)
      jvs(155) = b(66,48)
      jvs(156) = b(49,49)+b(50,49)+b(51,49)+b(52,49)+b(166,49)
      jvs(157) = 0.33*b(122,51)+0.16*b(124,51)+0.64*b(125,51)
      jvs(158) = b(166,54)
      jvs(159) = 0.24*b(81,58)+0.31*b(83,58)+0.3*b(84,58)+0.69   &
                *b(97,58)+0.07*b(120,58)+0.16*b(124,58)
      jvs(160) = 0.59*b(128,60)
      jvs(161) = -b(44,61)+b(51,61)+2*b(95,61)
      jvs(162) = b(52,62)+b(80,62)+0.64*b(125,62)
      jvs(163) = 0.95*b(91,18)
      jvs(164) = 0.3*b(92,26)
      jvs(165) = -b(95,32)-b(96,32)-b(97,32)
      jvs(166) = -b(97,58)
      jvs(167) = 0.95*b(91,60)
      jvs(168) = 0.3*b(92,61)-b(95,61)
      jvs(169) = 0
      jvs(170) = 2*b(42,14)
      jvs(171) = b(93,26)
      jvs(172) = -b(4,33)-b(27,33)
      jvs(173) = b(136,34)
      jvs(174) = b(80,43)
      jvs(175) = b(148,45)
      jvs(176) = b(68,48)
      jvs(177) = b(52,49)
      jvs(178) = 0.07*b(125,51)
      jvs(179) = b(163,54)
      jvs(180) = b(24,56)+b(163,56)
      jvs(181) = b(24,61)-b(27,61)
      jvs(182) = 0.3*b(41,62)+b(52,62)+b(68,62)+b(80,62)+b(93,62)+0.07   &
                *b(125,62)+b(136,62)+b(148,62)
      jvs(183) = 0.3*b(41,63)
      jvs(184) = -b(135,34)-b(136,34)-b(137,34)-b(138,34)
      jvs(185) = -b(137,46)
      jvs(186) = -b(135,61)-b(138,61)
      jvs(187) = -b(136,62)
      jvs(188) = -b(126,35)-b(129,35)-b(132,35)
      jvs(189) = b(119,38)
      jvs(190) = -b(126,60)
      jvs(191) = b(119,61)
      jvs(192) = -b(129,63)
      jvs(193) = -b(128,36)-b(131,36)-b(134,36)
      jvs(194) = 0.5*b(123,51)
      jvs(195) = -b(128,60)
      jvs(196) = 0.5*b(123,61)
      jvs(197) = -b(131,63)
      jvs(198) = -b(83,37)-b(85,37)-b(87,37)
      jvs(199) = -b(83,58)
      jvs(200) = -b(85,61)
      jvs(201) = -b(87,62)
      jvs(202) = -b(119,38)-b(120,38)-b(121,38)
      jvs(203) = -b(120,58)
      jvs(204) = -b(119,61)
      jvs(205) = -b(121,62)
      jvs(206) = b(135,34)+b(136,34)
      jvs(207) = -b(139,39)-b(140,39)-b(141,39)-2*b(142,39)
      jvs(208) = 0
      jvs(209) = -b(141,50)
      jvs(210) = -b(140,55)
      jvs(211) = -b(139,60)
      jvs(212) = b(135,61)
      jvs(213) = b(136,62)
      jvs(214) = 0.08*b(89,12)
      jvs(215) = 0.5*b(90,15)
      jvs(216) = b(82,22)
      jvs(217) = 0.6*b(92,26)
      jvs(218) = b(95,32)+0.03*b(97,32)
      jvs(219) = b(85,37)
      jvs(220) = 0.08*b(119,38)+0.2*b(120,38)
      jvs(221) = -b(105,40)-b(109,40)-b(113,40)-b(117,40)
      jvs(222) = b(86,42)
      jvs(223) = b(79,43)
      jvs(224) = 0.2*b(123,51)+0.07*b(124,51)+0.93*b(125,51)
      jvs(225) = 0.4*b(98,53)
      jvs(226) = 0.4*b(101,57)
      jvs(227) = 0.03*b(97,58)+0.2*b(120,58)+0.07*b(124,58)
      jvs(228) = 0.34*b(102,59)+0.4*b(106,59)+0.24*b(114,59)
      jvs(229) = 0.34*b(102,60)-b(105,60)
      jvs(230) = b(79,61)+b(82,61)+b(85,61)+b(86,61)+0.08*b(89,61)+0.5   &
                *b(90,61)+0.6*b(92,61)+b(95,61)+0.08*b(119,61)+0.2   &
                *b(123,61)
      jvs(231) = 0.4*b(106,62)-b(109,62)+0.93*b(125,62)
      jvs(232) = -b(113,63)
      jvs(233) = 0.63*b(128,36)+0.5*b(134,36)
      jvs(234) = -b(76,41)-b(77,41)
      jvs(235) = 0.07*b(84,42)+0.23*b(86,42)
      jvs(236) = 0.03*b(122,51)+0.09*b(124,51)
      jvs(237) = 0.74*b(98,53)
      jvs(238) = 0.74*b(101,57)
      jvs(239) = 0.07*b(84,58)+0.09*b(124,58)
      jvs(240) = 0.62*b(102,59)+0.74*b(106,59)+0.57*b(114,59)
      jvs(241) = 0.62*b(102,60)+0.63*b(128,60)
      jvs(242) = -b(77,61)+0.23*b(86,61)
      jvs(243) = 0.74*b(106,62)
      jvs(244) = 0
      jvs(245) = 0.15*b(115,64)
      jvs(246) = -b(84,42)-b(86,42)-b(88,42)
      jvs(247) = -b(84,58)
      jvs(248) = -b(86,61)
      jvs(249) = -b(88,62)
      jvs(250) = 0.8*b(90,15)
      jvs(251) = 0.2*b(97,32)
      jvs(252) = 0.34*b(128,36)
      jvs(253) = 0.04*b(83,37)
      jvs(254) = 0.07*b(84,42)
      jvs(255) = -b(78,43)-b(79,43)-b(80,43)
      jvs(256) = 0.85*b(124,51)
      jvs(257) = 0.19*b(99,53)
      jvs(258) = 0.04*b(83,58)+0.07*b(84,58)+0.2*b(97,58)+0.85   &
                *b(124,58)
      jvs(259) = 0.34*b(128,60)
      jvs(260) = -b(79,61)+0.8*b(90,61)+0.19*b(99,61)
      jvs(261) = -b(80,62)
      jvs(262) = 0
      jvs(263) = 0.15*b(115,64)
      jvs(264) = b(47,9)
      jvs(265) = 0.5*b(56,21)
      jvs(266) = 0.06*b(83,37)
      jvs(267) = 0.05*b(84,42)
      jvs(268) = -b(58,44)-b(60,44)-b(62,44)-b(64,44)
      jvs(269) = 0.1*b(98,53)
      jvs(270) = 0.1*b(101,57)
      jvs(271) = 0.06*b(83,58)+0.05*b(84,58)
      jvs(272) = 0.08*b(102,59)+0.1*b(106,59)+0.06*b(114,59)
      jvs(273) = -b(58,60)+0.08*b(102,60)
      jvs(274) = b(47,61)+0.5*b(56,61)
      jvs(275) = -b(60,62)+0.1*b(106,62)
      jvs(276) = -b(62,63)
      jvs(277) = 0.73*b(143,19)
      jvs(278) = 0
      jvs(279) = -b(147,45)-b(148,45)-b(149,45)-b(150,45)-b(151,45)
      jvs(280) = 0
      jvs(281) = -b(151,54)
      jvs(282) = -b(149,55)
      jvs(283) = 0.73*b(143,61)-b(150,61)
      jvs(284) = -b(148,62)
      jvs(285) = -b(147,63)
      jvs(286) = b(10,6)+b(11,6)
      jvs(287) = -b(137,34)
      jvs(288) = -b(13,46)-b(14,46)-b(15,46)-b(16,46)-b(17,46)   &
                -b(137,46)
      jvs(289) = b(1,56)-b(15,56)-b(16,56)
      jvs(290) = b(7,58)-b(14,58)
      jvs(291) = -b(17,60)
      jvs(292) = 0
      jvs(293) = 0.89*b(2,62)
      jvs(294) = b(87,37)
      jvs(295) = b(88,42)
      jvs(296) = -b(104,47)-b(108,47)-b(112,47)-b(116,47)
      jvs(297) = b(100,57)
      jvs(298) = 0
      jvs(299) = -b(104,60)
      jvs(300) = b(100,61)
      jvs(301) = b(87,62)+b(88,62)-b(108,62)
      jvs(302) = -b(112,63)
      jvs(303) = b(65,7)
      jvs(304) = b(54,21)+0.5*b(56,21)
      jvs(305) = 0.22*b(82,22)
      jvs(306) = 0.8*b(127,29)+b(133,29)
      jvs(307) = 0.03*b(97,32)
      jvs(308) = 0.55*b(128,36)+0.5*b(134,36)
      jvs(309) = 0.47*b(83,37)+b(85,37)
      jvs(310) = 0.15*b(120,38)
      jvs(311) = 1.03*b(84,42)+1.77*b(86,42)
      jvs(312) = b(58,44)+b(60,44)+0.8*b(64,44)
      jvs(313) = 0.5*b(104,47)+0.5*b(108,47)+0.5*b(116,47)
      jvs(314) = -b(66,48)-b(67,48)-b(68,48)
      jvs(315) = 0.07*b(122,51)+0.02*b(124,51)+0.28*b(125,51)
      jvs(316) = 0.3*b(98,53)+0.04*b(99,53)
      jvs(317) = 0.3*b(101,57)
      jvs(318) = 0.47*b(83,58)+1.03*b(84,58)+0.03*b(97,58)+0.15   &
                *b(120,58)+0.02*b(124,58)
      jvs(319) = 0.25*b(102,59)+0.3*b(106,59)+0.21*b(114,59)
      jvs(320) = b(58,60)+0.25*b(102,60)+0.5*b(104,60)+0.8*b(127,60)   &
                +0.55*b(128,60)
      jvs(321) = 0.5*b(56,61)+b(65,61)-b(67,61)+0.22*b(82,61)+b(85,61)   &
                +1.77*b(86,61)+0.04*b(99,61)
      jvs(322) = b(60,62)-b(68,62)+0.3*b(106,62)+0.5*b(108,62)+0.28   &
                *b(125,62)
      jvs(323) = 0
      jvs(324) = b(81,22)+1.56*b(82,22)
      jvs(325) = b(48,23)
      jvs(326) = b(53,25)+0.3*b(55,25)
      jvs(327) = b(161,27)
      jvs(328) = b(145,28)+2*b(146,28)
      jvs(329) = b(95,32)+0.7*b(97,32)
      jvs(330) = 0
      jvs(331) = 0.63*b(126,35)
      jvs(332) = 0.25*b(128,36)
      jvs(333) = 0.57*b(83,37)+b(85,37)
      jvs(334) = 0.6*b(120,38)
      jvs(335) = b(139,39)+2*b(140,39)+b(141,39)
      jvs(336) = 0
      jvs(337) = 0
      jvs(338) = 0
      jvs(339) = 0.5*b(104,47)+0.5*b(108,47)+0.5*b(116,47)
      jvs(340) = -b(49,49)-b(50,49)-b(51,49)-b(52,49)-b(166,49)
      jvs(341) = b(141,50)+b(156,50)
      jvs(342) = 0.2*b(122,51)+0.15*b(124,51)+0.28*b(125,51)
      jvs(343) = -b(166,54)
      jvs(344) = b(57,55)+b(59,55)+0.66*b(63,55)+2*b(140,55)+2   &
                *b(146,55)+b(156,55)+b(161,55)
      jvs(345) = 0
      jvs(346) = 0
      jvs(347) = b(81,58)+0.57*b(83,58)+0.7*b(97,58)+0.6*b(120,58)   &
                +0.15*b(124,58)
      jvs(348) = b(57,60)+b(103,60)+0.5*b(104,60)+0.63*b(126,60)+0.25   &
                *b(128,60)+b(139,60)+b(145,60)
      jvs(349) = b(48,61)-b(51,61)+0.3*b(55,61)+1.56*b(82,61)+b(85,61)   &
                +b(95,61)
      jvs(350) = -b(52,62)+b(59,62)+b(107,62)+0.5*b(108,62)+0.28   &
                *b(125,62)
      jvs(351) = 0
      jvs(352) = b(103,64)+b(107,64)+0.7*b(115,64)
      jvs(353) = b(159,27)
      jvs(354) = b(145,28)+b(146,28)
      jvs(355) = b(137,34)+0.035*b(138,34)
      jvs(356) = b(139,39)+b(140,39)+1.85*b(142,39)
      jvs(357) = b(147,45)+b(148,45)+b(149,45)+b(150,45)+b(151,45)
      jvs(358) = b(137,46)
      jvs(359) = -b(152,50)-b(153,50)-b(154,50)-b(155,50)-b(156,50)   &
                -b(157,50)-b(158,50)
      jvs(360) = b(151,54)
      jvs(361) = b(140,55)+b(146,55)+b(149,55)-b(156,55)
      jvs(362) = -b(153,56)
      jvs(363) = -b(154,58)
      jvs(364) = b(139,60)+b(145,60)
      jvs(365) = 0.035*b(138,61)+b(150,61)-b(157,61)
      jvs(366) = b(148,62)
      jvs(367) = b(147,63)-b(155,63)
      jvs(368) = 0.2*b(127,29)
      jvs(369) = 0.91*b(126,35)+b(132,35)
      jvs(370) = 0.65*b(120,38)
      jvs(371) = -b(122,51)-b(123,51)-b(124,51)-b(125,51)
      jvs(372) = 0.65*b(120,58)-b(124,58)
      jvs(373) = 0.91*b(126,60)+0.2*b(127,60)
      jvs(374) = -b(123,61)
      jvs(375) = -b(125,62)
      jvs(376) = 0
      jvs(377) = b(70,10)
      jvs(378) = b(95,32)+b(96,32)+0.62*b(97,32)
      jvs(379) = 0.13*b(83,37)
      jvs(380) = 0.2*b(120,38)
      jvs(381) = b(76,41)
      jvs(382) = 0.19*b(84,42)
      jvs(383) = b(78,43)+b(79,43)+b(80,43)
      jvs(384) = b(67,48)+b(68,48)
      jvs(385) = 0.97*b(122,51)+0.5*b(123,51)+0.11*b(124,51)+0.07   &
                *b(125,51)
      jvs(386) = -b(69,52)-b(71,52)-b(72,52)-b(73,52)-b(74,52)
      jvs(387) = 0
      jvs(388) = -b(69,56)
      jvs(389) = 0
      jvs(390) = 0.13*b(83,58)+0.19*b(84,58)+0.62*b(97,58)+0.2   &
                *b(120,58)+0.11*b(124,58)
      jvs(391) = 0
      jvs(392) = -b(71,60)+b(103,60)
      jvs(393) = b(67,61)+b(79,61)+b(95,61)+0.5*b(123,61)
      jvs(394) = b(68,62)-b(72,62)+b(80,62)+b(107,62)+0.07*b(125,62)
      jvs(395) = -b(73,63)
      jvs(396) = b(103,64)+b(107,64)+0.7*b(115,64)
      jvs(397) = b(129,35)
      jvs(398) = b(131,36)
      jvs(399) = 0
      jvs(400) = 0
      jvs(401) = -b(98,53)-b(99,53)
      jvs(402) = 0
      jvs(403) = b(110,59)
      jvs(404) = 0
      jvs(405) = -b(99,61)
      jvs(406) = 0
      jvs(407) = b(110,63)+b(111,63)+b(129,63)+b(131,63)
      jvs(408) = b(111,64)
      jvs(409) = b(160,27)+b(161,27)
      jvs(410) = b(141,39)
      jvs(411) = -b(151,45)
      jvs(412) = 0
      jvs(413) = -b(166,49)
      jvs(414) = b(141,50)+b(153,50)+b(154,50)+b(155,50)+b(156,50)
      jvs(415) = 0
      jvs(416) = -b(151,54)-b(162,54)-b(163,54)-b(164,54)-b(165,54)   &
                -b(166,54)
      jvs(417) = b(156,55)+b(161,55)
      jvs(418) = b(153,56)-b(163,56)
      jvs(419) = 0
      jvs(420) = b(154,58)
      jvs(421) = b(160,60)-b(164,60)
      jvs(422) = 0
      jvs(423) = 0
      jvs(424) = b(155,63)-b(165,63)
      jvs(425) = 0
      jvs(426) = 0.73*b(143,19)
      jvs(427) = 0.7*b(55,25)
      jvs(428) = -b(161,27)
      jvs(429) = -b(146,28)
      jvs(430) = b(137,34)+0.035*b(138,34)
      jvs(431) = 0.07*b(83,37)
      jvs(432) = -b(140,39)
      jvs(433) = b(76,41)
      jvs(434) = 0.1*b(84,42)
      jvs(435) = -b(149,45)
      jvs(436) = b(137,46)
      jvs(437) = b(66,48)
      jvs(438) = b(152,50)-b(156,50)
      jvs(439) = 0.7*b(122,51)+0.05*b(124,51)
      jvs(440) = b(71,52)+b(72,52)+b(74,52)
      jvs(441) = 0
      jvs(442) = b(162,54)
      jvs(443) = -b(57,55)-b(59,55)-b(61,55)-b(63,55)-b(140,55)   &
                -b(146,55)-b(149,55)-b(156,55)-b(161,55)
      jvs(444) = 0
      jvs(445) = 0
      jvs(446) = 0.07*b(83,58)+0.1*b(84,58)+0.05*b(124,58)
      jvs(447) = 0
      jvs(448) = -b(57,60)+b(71,60)
      jvs(449) = b(46,61)+0.7*b(55,61)+0.035*b(138,61)+0.73*b(143,61)
      jvs(450) = -b(59,62)+b(72,62)
      jvs(451) = -b(61,63)
      jvs(452) = 0
      jvs(453) = b(70,10)
      jvs(454) = b(6,14)+b(43,14)
      jvs(455) = -b(94,16)
      jvs(456) = b(5,17)+b(28,17)+b(36,17)
      jvs(457) = 0.95*b(91,18)
      jvs(458) = b(26,24)
      jvs(459) = 0
      jvs(460) = b(160,27)
      jvs(461) = b(145,28)
      jvs(462) = 1.2*b(127,29)
      jvs(463) = b(4,33)
      jvs(464) = 0
      jvs(465) = 0.91*b(126,35)
      jvs(466) = b(128,36)
      jvs(467) = 0
      jvs(468) = b(139,39)
      jvs(469) = b(105,40)+b(109,40)
      jvs(470) = 0
      jvs(471) = 0
      jvs(472) = b(58,44)+b(60,44)
      jvs(473) = 0
      jvs(474) = -b(15,46)-b(16,46)+b(17,46)
      jvs(475) = 1.5*b(104,47)+1.5*b(108,47)+0.5*b(116,47)
      jvs(476) = 0
      jvs(477) = 0
      jvs(478) = -b(153,50)
      jvs(479) = 0
      jvs(480) = -b(69,52)+b(71,52)+b(72,52)
      jvs(481) = 0
      jvs(482) = -b(163,54)
      jvs(483) = b(57,55)+b(59,55)
      jvs(484) = -b(1,56)-b(15,56)-b(16,56)-b(19,56)-b(24,56)-b(34,56)   &
                -b(35,56)-b(39,56)-b(69,56)-b(94,56)-b(153,56)   &
                -b(163,56)
      jvs(485) = b(101,57)
      jvs(486) = b(18,58)-b(19,58)
      jvs(487) = 0.84*b(102,59)+b(106,59)
      jvs(488) = b(17,60)+b(18,60)+b(33,60)+2*b(37,60)+b(57,60)   &
                +b(58,60)+b(71,60)+0.95*b(91,60)+0.84*b(102,60)   &
                +b(103,60)+1.5*b(104,60)+b(105,60)+0.91*b(126,60)+1.2   &
                *b(127,60)+b(128,60)+b(139,60)+b(145,60)+b(160,60)
      jvs(489) = -b(24,61)+b(25,61)+b(26,61)+b(28,61)
      jvs(490) = 0.89*b(2,62)+b(25,62)+2*b(37,62)-b(39,62)+2*b(40,62)   &
                +0.7*b(41,62)+b(59,62)+b(60,62)+b(72,62)+b(106,62)   &
                +b(107,62)+1.5*b(108,62)+b(109,62)
      jvs(491) = b(33,63)-b(34,63)-b(35,63)+0.7*b(41,63)
      jvs(492) = b(103,64)+b(107,64)
      jvs(493) = b(94,16)
      jvs(494) = 0.05*b(91,18)
      jvs(495) = 0
      jvs(496) = 0.8*b(127,29)+b(130,29)+b(133,29)
      jvs(497) = 0.09*b(126,35)
      jvs(498) = 0
      jvs(499) = 0.5*b(104,47)+0.5*b(108,47)+b(112,47)+0.5*b(116,47)
      jvs(500) = 0.93*b(125,51)
      jvs(501) = b(94,56)
      jvs(502) = -b(100,57)-b(101,57)
      jvs(503) = 0
      jvs(504) = 0.16*b(102,59)
      jvs(505) = 0.05*b(91,60)+0.16*b(102,60)+0.5*b(104,60)+0.09   &
                *b(126,60)+0.8*b(127,60)
      jvs(506) = -b(100,61)
      jvs(507) = 0.5*b(108,62)+0.93*b(125,62)
      jvs(508) = b(112,63)+b(130,63)
      jvs(509) = 0
      jvs(510) = -b(81,22)
      jvs(511) = -b(97,32)
      jvs(512) = -b(83,37)
      jvs(513) = -b(120,38)
      jvs(514) = -b(84,42)
      jvs(515) = b(13,46)-b(14,46)
      jvs(516) = -b(154,50)
      jvs(517) = -b(124,51)
      jvs(518) = 0.4*b(73,52)
      jvs(519) = 0
      jvs(520) = 0
      jvs(521) = 0
      jvs(522) = -b(19,56)
      jvs(523) = 0
      jvs(524) = -b(7,58)-b(8,58)-b(14,58)-b(18,58)-b(19,58)-b(20,58)   &
                -b(21,58)-b(81,58)-b(83,58)-b(84,58)-b(97,58)   &
                -b(120,58)-b(124,58)-b(154,58)
      jvs(525) = 0
      jvs(526) = -b(18,60)
      jvs(527) = -b(20,61)
      jvs(528) = 0
      jvs(529) = -b(21,63)+0.4*b(73,63)
      jvs(530) = 0
      jvs(531) = b(75,30)
      jvs(532) = 0
      jvs(533) = 0.03*b(83,37)
      jvs(534) = 0
      jvs(535) = 0.09*b(84,42)
      jvs(536) = 0
      jvs(537) = 0
      jvs(538) = 0.77*b(99,53)
      jvs(539) = 0
      jvs(540) = 0.03*b(83,58)+0.09*b(84,58)
      jvs(541) = -b(102,59)-b(106,59)-b(110,59)-b(114,59)
      jvs(542) = -b(102,60)
      jvs(543) = b(75,61)+0.77*b(99,61)
      jvs(544) = -b(106,62)
      jvs(545) = -b(110,63)
      jvs(546) = 0
      jvs(547) = -b(91,18)
      jvs(548) = b(3,24)
      jvs(549) = -b(160,27)
      jvs(550) = -b(145,28)
      jvs(551) = -b(127,29)
      jvs(552) = 0
      jvs(553) = -b(126,35)
      jvs(554) = -b(128,36)
      jvs(555) = 0
      jvs(556) = -b(139,39)
      jvs(557) = -b(105,40)
      jvs(558) = 0
      jvs(559) = 0
      jvs(560) = -b(58,44)
      jvs(561) = b(15,46)-b(17,46)
      jvs(562) = -b(104,47)
      jvs(563) = b(153,50)
      jvs(564) = 0
      jvs(565) = -b(71,52)
      jvs(566) = 0
      jvs(567) = -b(164,54)
      jvs(568) = -b(57,55)
      jvs(569) = b(1,56)+b(15,56)+b(38,56)+b(153,56)
      jvs(570) = 0
      jvs(571) = -b(18,58)
      jvs(572) = -b(102,59)
      jvs(573) = -b(17,60)-b(18,60)-b(23,60)-b(33,60)-b(37,60)   &
                -b(57,60)-b(58,60)-b(71,60)-b(91,60)-b(102,60)   &
                -b(103,60)-b(104,60)-b(105,60)-b(126,60)-b(127,60)   &
                -b(128,60)-b(139,60)-b(145,60)-b(160,60)-b(164,60)
      jvs(574) = -b(23,61)
      jvs(575) = 0.11*b(2,62)-b(37,62)+b(38,62)
      jvs(576) = -b(33,63)
      jvs(577) = -b(103,64)
      jvs(578) = 2*b(12,6)
      jvs(579) = -b(65,7)
      jvs(580) = -b(45,8)
      jvs(581) = -b(47,9)
      jvs(582) = -b(144,11)
      jvs(583) = -b(89,12)
      jvs(584) = 2*b(9,13)-b(30,13)
      jvs(585) = -b(90,15)
      jvs(586) = -b(28,17)
      jvs(587) = -b(143,19)
      jvs(588) = b(54,21)-0.5*b(56,21)
      jvs(589) = 0.12*b(81,22)-b(82,22)
      jvs(590) = -b(48,23)
      jvs(591) = b(3,24)-b(26,24)
      jvs(592) = b(53,25)-0.7*b(55,25)
      jvs(593) = -b(92,26)
      jvs(594) = -b(75,30)
      jvs(595) = -b(44,31)
      jvs(596) = -b(95,32)+0.08*b(97,32)
      jvs(597) = b(4,33)-b(27,33)
      jvs(598) = -b(135,34)-b(138,34)
      jvs(599) = 0
      jvs(600) = 0
      jvs(601) = 0.33*b(83,37)-b(85,37)
      jvs(602) = -b(119,38)+0.27*b(120,38)
      jvs(603) = -b(77,41)
      jvs(604) = 0.6*b(84,42)-b(86,42)
      jvs(605) = -b(79,43)
      jvs(606) = 0
      jvs(607) = -b(150,45)
      jvs(608) = 0
      jvs(609) = 0
      jvs(610) = -b(67,48)
      jvs(611) = -b(51,49)
      jvs(612) = b(155,50)-b(157,50)
      jvs(613) = -b(123,51)+0.27*b(124,51)
      jvs(614) = b(98,53)-0.77*b(99,53)
      jvs(615) = 0
      jvs(616) = 0
      jvs(617) = -b(24,56)
      jvs(618) = -b(100,57)
      jvs(619) = -b(20,58)+b(21,58)+0.12*b(81,58)+0.33*b(83,58)+0.6   &
                *b(84,58)+0.08*b(97,58)+0.27*b(120,58)+0.27*b(124,58)
      jvs(620) = 0
      jvs(621) = -b(23,60)+b(33,60)
      jvs(622) = -b(20,61)-b(22,61)-b(23,61)-b(24,61)-b(25,61)   &
                -b(26,61)-b(27,61)-b(28,61)-b(29,61)-b(30,61)-b(44,61)   &
                -b(45,61)-b(46,61)-b(47,61)-b(48,61)-b(51,61)-0.7   &
                *b(55,61)-0.5*b(56,61)-b(65,61)-b(67,61)-b(75,61)   &
                -b(77,61)-b(79,61)-b(82,61)-b(85,61)-b(86,61)-b(89,61)   &
                -b(90,61)-b(92,61)-b(95,61)-0.77*b(99,61)-b(100,61)   &
                -b(119,61)-b(123,61)-b(135,61)-b(138,61)-b(143,61)   &
                -b(144,61)-b(150,61)-b(157,61)
      jvs(623) = -b(25,62)+0.7*b(41,62)
      jvs(624) = b(21,63)-b(29,63)+b(33,63)+0.7*b(41,63)+b(155,63)
      jvs(625) = 0
      jvs(626) = b(6,14)+b(43,14)
      jvs(627) = -b(93,26)
      jvs(628) = b(27,33)
      jvs(629) = -b(136,34)
      jvs(630) = -b(87,37)
      jvs(631) = -b(121,38)
      jvs(632) = -b(109,40)
      jvs(633) = -b(88,42)
      jvs(634) = -b(80,43)
      jvs(635) = -b(60,44)
      jvs(636) = -b(148,45)
      jvs(637) = b(16,46)
      jvs(638) = -b(108,47)
      jvs(639) = -b(68,48)
      jvs(640) = -b(52,49)
      jvs(641) = 0
      jvs(642) = -b(125,51)
      jvs(643) = -b(72,52)
      jvs(644) = 0
      jvs(645) = 0
      jvs(646) = -b(59,55)
      jvs(647) = b(16,56)+b(19,56)-b(38,56)-b(39,56)
      jvs(648) = 0
      jvs(649) = b(19,58)
      jvs(650) = -b(106,59)
      jvs(651) = -b(37,60)
      jvs(652) = -b(25,61)+b(27,61)
      jvs(653) = -b(2,62)-b(25,62)-b(37,62)-b(38,62)-b(39,62)-2   &
                *b(40,62)-b(41,62)-b(52,62)-b(59,62)-b(60,62)-b(68,62)   &
                -b(72,62)-b(80,62)-b(87,62)-b(88,62)-b(93,62)   &
                -b(106,62)-b(107,62)-b(108,62)-b(109,62)-b(121,62)   &
                -b(125,62)-b(136,62)-b(148,62)
      jvs(654) = -b(41,63)
      jvs(655) = -b(107,64)
      jvs(656) = b(65,7)
      jvs(657) = b(45,8)
      jvs(658) = 0.2*b(89,12)
      jvs(659) = b(30,13)
      jvs(660) = 0.55*b(90,15)
      jvs(661) = b(5,17)+b(36,17)
      jvs(662) = 0.95*b(91,18)
      jvs(663) = 0.27*b(143,19)
      jvs(664) = b(54,21)
      jvs(665) = 0.22*b(81,22)+b(82,22)
      jvs(666) = b(48,23)
      jvs(667) = b(53,25)
      jvs(668) = 0.6*b(92,26)
      jvs(669) = b(161,27)
      jvs(670) = b(146,28)
      jvs(671) = 0.8*b(127,29)-b(130,29)
      jvs(672) = b(44,31)
      jvs(673) = 2*b(95,32)+b(96,32)+0.76*b(97,32)
      jvs(674) = 0.965*b(138,34)
      jvs(675) = 0.91*b(126,35)-b(129,35)
      jvs(676) = b(128,36)-b(131,36)
      jvs(677) = 0.26*b(83,37)+b(85,37)
      jvs(678) = 0.07*b(120,38)
      jvs(679) = b(140,39)
      jvs(680) = -b(113,40)
      jvs(681) = 0.22*b(84,42)+b(86,42)
      jvs(682) = b(78,43)
      jvs(683) = b(58,44)+b(60,44)-b(62,44)+0.6*b(64,44)
      jvs(684) = -b(147,45)
      jvs(685) = 0
      jvs(686) = 0.5*b(104,47)+0.5*b(108,47)-b(112,47)
      jvs(687) = b(66,48)
      jvs(688) = 2*b(49,49)+b(51,49)+b(52,49)+b(166,49)
      jvs(689) = -b(155,50)+b(156,50)
      jvs(690) = 0.33*b(122,51)+0.1*b(124,51)+0.93*b(125,51)
      jvs(691) = -b(73,52)
      jvs(692) = 0.9*b(98,53)
      jvs(693) = -b(165,54)+b(166,54)
      jvs(694) = b(57,55)+b(59,55)-b(61,55)+0.32*b(63,55)+b(140,55)   &
                +b(146,55)+b(156,55)+b(161,55)
      jvs(695) = -b(34,56)-b(35,56)
      jvs(696) = 0.9*b(101,57)
      jvs(697) = b(20,58)-b(21,58)+0.22*b(81,58)+0.26*b(83,58)+0.22   &
                *b(84,58)+0.76*b(97,58)+0.07*b(120,58)+0.1*b(124,58)
      jvs(698) = 0.76*b(102,59)+0.9*b(106,59)-b(110,59)+0.54*b(114,59)
      jvs(699) = -b(33,60)+b(57,60)+b(58,60)+0.95*b(91,60)+0.76   &
                *b(102,60)+0.5*b(104,60)+0.91*b(126,60)+0.8*b(127,60)   &
                +b(128,60)
      jvs(700) = b(20,61)+b(22,61)+b(25,61)-b(29,61)+b(30,61)+b(44,61)   &
                +b(45,61)+b(48,61)+b(51,61)+b(65,61)+b(82,61)+b(85,61)   &
                +b(86,61)+0.2*b(89,61)+0.55*b(90,61)+0.6*b(92,61)+2   &
                *b(95,61)+0.965*b(138,61)+0.27*b(143,61)
      jvs(701) = b(25,62)-b(41,62)+b(52,62)+b(59,62)+b(60,62)+0.9   &
                *b(106,62)+0.5*b(108,62)+0.93*b(125,62)
      jvs(702) = -b(21,63)-b(29,63)-2*b(31,63)-2*b(32,63)-b(33,63)   &
                -b(34,63)-b(35,63)-b(41,63)-b(61,63)-b(62,63)-b(73,63)   &
                -b(110,63)-b(111,63)-b(112,63)-b(113,63)-b(129,63)   &
                -b(130,63)-b(131,63)-b(147,63)-b(155,63)-b(165,63)
      jvs(703) = -b(111,64)
      jvs(704) = b(77,41)
      jvs(705) = 0.11*b(84,42)
      jvs(706) = 0
      jvs(707) = 0
      jvs(708) = 0
      jvs(709) = 0.11*b(84,58)
      jvs(710) = 0
      jvs(711) = -b(103,60)
      jvs(712) = b(77,61)
      jvs(713) = -b(107,62)
      jvs(714) = -b(111,63)
      jvs(715) = -b(103,64)-b(107,64)-b(111,64)-b(115,64)
      return
      end subroutine cbmz_v02r06_jacob                                    


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r06_decomp( n, v, ier,   &
          lu_crow_v, lu_diag_v, lu_icol_v )
!
!   computes l-u-decomposition of sparse jacobian
!   for mechanism-version-regime = cbmz_v02r06
!
      use module_data_cbmz
      implicit none

!   subr parameters
!     n = number of variable species [input]
      integer n
!     ier = status flag [output]
!         0 = success    other = failure [output]
      integer ier

!     v = non-zero elements of the sparse jacobian [input]
      real v(lu_nonzero_v_r06_kpp)

      integer lu_crow_v(nvar_r06_kpp + 1)
      integer lu_diag_v(nvar_r06_kpp + 1)
      integer lu_icol_v(lu_nonzero_v_r06_kpp)

!   local variables
      integer k, kk, j, jj
      real a, w(nvar_r06_kpp + 1)

      ier = 0
      do k=1,n
        if ( v( lu_diag_v(k) ) .eq. 0. ) then
            ier = k
            return
        end if
        do kk = lu_crow_v(k), lu_crow_v(k+1)-1
              w( lu_icol_v(kk) ) = v(kk)
        end do
        do kk = lu_crow_v(k), lu_diag_v(k)-1
            j = lu_icol_v(kk)
            a = -w(j) / v( lu_diag_v(j) )
            w(j) = -a
            do jj = lu_diag_v(j)+1, lu_crow_v(j+1)-1
               w( lu_icol_v(jj) ) = w( lu_icol_v(jj) ) + a*v(jj)
            end do
         end do
         do kk = lu_crow_v(k), lu_crow_v(k+1)-1
            v(kk) = w( lu_icol_v(kk) )
         end do
      end do
      return
      end subroutine cbmz_v02r06_decomp            


!-----------------------------------------------------------------------------
      subroutine cbmz_v02r06_solve( jvs, x )
!
!   does back-solve for mechanism-version-regime = cbmz_v02r06
!
      implicit none

!   subr parameters
!     jvs = the non-zero elements of the l-u-decomposition
!           of the augmented jacobian [input]
      real jvs(*)
!     x = the right-hand side of the linear equation set being solved [on input]
!     x = concentrations of variable species [on output]
      real x(*)


      x(18) = x(18)-jvs(64)*x(12)-jvs(65)*x(15)
      x(26) = x(26)-jvs(108)*x(12)-jvs(109)*x(15)
      x(28) = x(28)-jvs(117)*x(11)-jvs(118)*x(19)
      x(30) = x(30)-jvs(129)*x(15)-jvs(130)*x(20)-jvs(131)*x(29)
      x(31) = x(31)-jvs(147)*x(22)
      x(32) = x(32)-jvs(163)*x(18)-jvs(164)*x(26)
      x(33) = x(33)-jvs(170)*x(14)-jvs(171)*x(26)
      x(39) = x(39)-jvs(206)*x(34)
      x(40) = x(40)-jvs(214)*x(12)-jvs(215)*x(15)-jvs(216)*x(22)   &
             -jvs(217)*x(26)-jvs(218)*x(32)-jvs(219)*x(37)-jvs(220)   &
             *x(38)
      x(41) = x(41)-jvs(233)*x(36)
      x(43) = x(43)-jvs(250)*x(15)-jvs(251)*x(32)-jvs(252)*x(36)   &
             -jvs(253)*x(37)-jvs(254)*x(42)
      x(44) = x(44)-jvs(264)*x(9)-jvs(265)*x(21)-jvs(266)*x(37)   &
             -jvs(267)*x(42)
      x(45) = x(45)-jvs(277)*x(19)-jvs(278)*x(34)
      x(46) = x(46)-jvs(286)*x(6)-jvs(287)*x(34)
      x(47) = x(47)-jvs(294)*x(37)-jvs(295)*x(42)
      x(48) = x(48)-jvs(303)*x(7)-jvs(304)*x(21)-jvs(305)*x(22)   &
             -jvs(306)*x(29)-jvs(307)*x(32)-jvs(308)*x(36)-jvs(309)   &
             *x(37)-jvs(310)*x(38)-jvs(311)*x(42)-jvs(312)*x(44)   &
             -jvs(313)*x(47)
      x(49) = x(49)-jvs(324)*x(22)-jvs(325)*x(23)-jvs(326)*x(25)   &
             -jvs(327)*x(27)-jvs(328)*x(28)-jvs(329)*x(32)-jvs(330)   &
             *x(34)-jvs(331)*x(35)-jvs(332)*x(36)-jvs(333)*x(37)   &
             -jvs(334)*x(38)-jvs(335)*x(39)-jvs(336)*x(42)-jvs(337)   &
             *x(45)-jvs(338)*x(46)-jvs(339)*x(47)
      x(50) = x(50)-jvs(353)*x(27)-jvs(354)*x(28)-jvs(355)*x(34)   &
             -jvs(356)*x(39)-jvs(357)*x(45)-jvs(358)*x(46)
      x(51) = x(51)-jvs(368)*x(29)-jvs(369)*x(35)-jvs(370)*x(38)
      x(52) = x(52)-jvs(377)*x(10)-jvs(378)*x(32)-jvs(379)*x(37)   &
             -jvs(380)*x(38)-jvs(381)*x(41)-jvs(382)*x(42)-jvs(383)   &
             *x(43)-jvs(384)*x(48)-jvs(385)*x(51)
      x(53) = x(53)-jvs(397)*x(35)-jvs(398)*x(36)-jvs(399)*x(38)   &
             -jvs(400)*x(51)
      x(54) = x(54)-jvs(409)*x(27)-jvs(410)*x(39)-jvs(411)*x(45)   &
             -jvs(412)*x(46)-jvs(413)*x(49)-jvs(414)*x(50)-jvs(415)   &
             *x(51)
      x(55) = x(55)-jvs(426)*x(19)-jvs(427)*x(25)-jvs(428)*x(27)   &
             -jvs(429)*x(28)-jvs(430)*x(34)-jvs(431)*x(37)-jvs(432)   &
             *x(39)-jvs(433)*x(41)-jvs(434)*x(42)-jvs(435)*x(45)   &
             -jvs(436)*x(46)-jvs(437)*x(48)-jvs(438)*x(50)-jvs(439)   &
             *x(51)-jvs(440)*x(52)-jvs(441)*x(53)-jvs(442)*x(54)
      x(56) = x(56)-jvs(453)*x(10)-jvs(454)*x(14)-jvs(455)*x(16)   &
             -jvs(456)*x(17)-jvs(457)*x(18)-jvs(458)*x(24)-jvs(459)   &
             *x(26)-jvs(460)*x(27)-jvs(461)*x(28)-jvs(462)*x(29)   &
             -jvs(463)*x(33)-jvs(464)*x(34)-jvs(465)*x(35)-jvs(466)   &
             *x(36)-jvs(467)*x(38)-jvs(468)*x(39)-jvs(469)*x(40)   &
             -jvs(470)*x(42)-jvs(471)*x(43)-jvs(472)*x(44)-jvs(473)   &
             *x(45)-jvs(474)*x(46)-jvs(475)*x(47)-jvs(476)*x(48)   &
             -jvs(477)*x(49)-jvs(478)*x(50)-jvs(479)*x(51)-jvs(480)   &
             *x(52)-jvs(481)*x(53)-jvs(482)*x(54)-jvs(483)*x(55)
      x(57) = x(57)-jvs(493)*x(16)-jvs(494)*x(18)-jvs(495)*x(26)   &
             -jvs(496)*x(29)-jvs(497)*x(35)-jvs(498)*x(38)-jvs(499)   &
             *x(47)-jvs(500)*x(51)-jvs(501)*x(56)
      x(58) = x(58)-jvs(510)*x(22)-jvs(511)*x(32)-jvs(512)*x(37)   &
             -jvs(513)*x(38)-jvs(514)*x(42)-jvs(515)*x(46)-jvs(516)   &
             *x(50)-jvs(517)*x(51)-jvs(518)*x(52)-jvs(519)*x(53)   &
             -jvs(520)*x(54)-jvs(521)*x(55)-jvs(522)*x(56)-jvs(523)   &
             *x(57)
      x(59) = x(59)-jvs(531)*x(30)-jvs(532)*x(35)-jvs(533)*x(37)   &
             -jvs(534)*x(38)-jvs(535)*x(42)-jvs(536)*x(47)-jvs(537)   &
             *x(51)-jvs(538)*x(53)-jvs(539)*x(57)-jvs(540)*x(58)
      x(60) = x(60)-jvs(547)*x(18)-jvs(548)*x(24)-jvs(549)*x(27)   &
             -jvs(550)*x(28)-jvs(551)*x(29)-jvs(552)*x(34)-jvs(553)   &
             *x(35)-jvs(554)*x(36)-jvs(555)*x(38)-jvs(556)*x(39)   &
             -jvs(557)*x(40)-jvs(558)*x(42)-jvs(559)*x(43)-jvs(560)   &
             *x(44)-jvs(561)*x(46)-jvs(562)*x(47)-jvs(563)*x(50)   &
             -jvs(564)*x(51)-jvs(565)*x(52)-jvs(566)*x(53)-jvs(567)   &
             *x(54)-jvs(568)*x(55)-jvs(569)*x(56)-jvs(570)*x(57)   &
             -jvs(571)*x(58)-jvs(572)*x(59)
      x(61) = x(61)-jvs(578)*x(6)-jvs(579)*x(7)-jvs(580)*x(8)-jvs(581)   &
             *x(9)-jvs(582)*x(11)-jvs(583)*x(12)-jvs(584)*x(13)   &
             -jvs(585)*x(15)-jvs(586)*x(17)-jvs(587)*x(19)-jvs(588)   &
             *x(21)-jvs(589)*x(22)-jvs(590)*x(23)-jvs(591)*x(24)   &
             -jvs(592)*x(25)-jvs(593)*x(26)-jvs(594)*x(30)-jvs(595)   &
             *x(31)-jvs(596)*x(32)-jvs(597)*x(33)-jvs(598)*x(34)   &
             -jvs(599)*x(35)-jvs(600)*x(36)-jvs(601)*x(37)-jvs(602)   &
             *x(38)-jvs(603)*x(41)-jvs(604)*x(42)-jvs(605)*x(43)   &
             -jvs(606)*x(44)-jvs(607)*x(45)-jvs(608)*x(46)-jvs(609)   &
             *x(47)-jvs(610)*x(48)-jvs(611)*x(49)-jvs(612)*x(50)   &
             -jvs(613)*x(51)-jvs(614)*x(53)-jvs(615)*x(54)-jvs(616)   &
             *x(55)-jvs(617)*x(56)-jvs(618)*x(57)-jvs(619)*x(58)   &
             -jvs(620)*x(59)-jvs(621)*x(60)
      x(62) = x(62)-jvs(626)*x(14)-jvs(627)*x(26)-jvs(628)*x(33)   &
             -jvs(629)*x(34)-jvs(630)*x(37)-jvs(631)*x(38)-jvs(632)   &
             *x(40)-jvs(633)*x(42)-jvs(634)*x(43)-jvs(635)*x(44)   &
             -jvs(636)*x(45)-jvs(637)*x(46)-jvs(638)*x(47)-jvs(639)   &
             *x(48)-jvs(640)*x(49)-jvs(641)*x(50)-jvs(642)*x(51)   &
             -jvs(643)*x(52)-jvs(644)*x(53)-jvs(645)*x(54)-jvs(646)   &
             *x(55)-jvs(647)*x(56)-jvs(648)*x(57)-jvs(649)*x(58)   &
             -jvs(650)*x(59)-jvs(651)*x(60)-jvs(652)*x(61)
      x(63) = x(63)-jvs(656)*x(7)-jvs(657)*x(8)-jvs(658)*x(12)   &
             -jvs(659)*x(13)-jvs(660)*x(15)-jvs(661)*x(17)-jvs(662)   &
             *x(18)-jvs(663)*x(19)-jvs(664)*x(21)-jvs(665)*x(22)   &
             -jvs(666)*x(23)-jvs(667)*x(25)-jvs(668)*x(26)-jvs(669)   &
             *x(27)-jvs(670)*x(28)-jvs(671)*x(29)-jvs(672)*x(31)   &
             -jvs(673)*x(32)-jvs(674)*x(34)-jvs(675)*x(35)-jvs(676)   &
             *x(36)-jvs(677)*x(37)-jvs(678)*x(38)-jvs(679)*x(39)   &
             -jvs(680)*x(40)-jvs(681)*x(42)-jvs(682)*x(43)-jvs(683)   &
             *x(44)-jvs(684)*x(45)-jvs(685)*x(46)-jvs(686)*x(47)   &
             -jvs(687)*x(48)-jvs(688)*x(49)-jvs(689)*x(50)-jvs(690)   &
             *x(51)-jvs(691)*x(52)-jvs(692)*x(53)-jvs(693)*x(54)   &
             -jvs(694)*x(55)-jvs(695)*x(56)-jvs(696)*x(57)-jvs(697)   &
             *x(58)-jvs(698)*x(59)-jvs(699)*x(60)-jvs(700)*x(61)   &
             -jvs(701)*x(62)
      x(64) = x(64)-jvs(704)*x(41)-jvs(705)*x(42)-jvs(706)*x(51)   &
             -jvs(707)*x(53)-jvs(708)*x(57)-jvs(709)*x(58)-jvs(710)   &
             *x(59)-jvs(711)*x(60)-jvs(712)*x(61)-jvs(713)*x(62)   &
             -jvs(714)*x(63)
      x(64) = x(64)/jvs(715)
      x(63) = (x(63)-jvs(703)*x(64))/(jvs(702))
      x(62) = (x(62)-jvs(654)*x(63)-jvs(655)*x(64))/(jvs(653))
      x(61) = (x(61)-jvs(623)*x(62)-jvs(624)*x(63)-jvs(625)*x(64))/   &
             (jvs(622))
      x(60) = (x(60)-jvs(574)*x(61)-jvs(575)*x(62)-jvs(576)*x(63)   &
             -jvs(577)*x(64))/(jvs(573))
      x(59) = (x(59)-jvs(542)*x(60)-jvs(543)*x(61)-jvs(544)*x(62)   &
             -jvs(545)*x(63)-jvs(546)*x(64))/(jvs(541))
      x(58) = (x(58)-jvs(525)*x(59)-jvs(526)*x(60)-jvs(527)*x(61)   &
             -jvs(528)*x(62)-jvs(529)*x(63)-jvs(530)*x(64))/(jvs(524))
      x(57) = (x(57)-jvs(503)*x(58)-jvs(504)*x(59)-jvs(505)*x(60)   &
             -jvs(506)*x(61)-jvs(507)*x(62)-jvs(508)*x(63)-jvs(509)   &
             *x(64))/(jvs(502))
      x(56) = (x(56)-jvs(485)*x(57)-jvs(486)*x(58)-jvs(487)*x(59)   &
             -jvs(488)*x(60)-jvs(489)*x(61)-jvs(490)*x(62)-jvs(491)   &
             *x(63)-jvs(492)*x(64))/(jvs(484))
      x(55) = (x(55)-jvs(444)*x(56)-jvs(445)*x(57)-jvs(446)*x(58)   &
             -jvs(447)*x(59)-jvs(448)*x(60)-jvs(449)*x(61)-jvs(450)   &
             *x(62)-jvs(451)*x(63)-jvs(452)*x(64))/(jvs(443))
      x(54) = (x(54)-jvs(417)*x(55)-jvs(418)*x(56)-jvs(419)*x(57)   &
             -jvs(420)*x(58)-jvs(421)*x(60)-jvs(422)*x(61)-jvs(423)   &
             *x(62)-jvs(424)*x(63)-jvs(425)*x(64))/(jvs(416))
      x(53) = (x(53)-jvs(402)*x(58)-jvs(403)*x(59)-jvs(404)*x(60)   &
             -jvs(405)*x(61)-jvs(406)*x(62)-jvs(407)*x(63)-jvs(408)   &
             *x(64))/(jvs(401))
      x(52) = (x(52)-jvs(387)*x(53)-jvs(388)*x(56)-jvs(389)*x(57)   &
             -jvs(390)*x(58)-jvs(391)*x(59)-jvs(392)*x(60)-jvs(393)   &
             *x(61)-jvs(394)*x(62)-jvs(395)*x(63)-jvs(396)*x(64))/   &
             (jvs(386))
      x(51) = (x(51)-jvs(372)*x(58)-jvs(373)*x(60)-jvs(374)*x(61)   &
             -jvs(375)*x(62)-jvs(376)*x(63))/(jvs(371))
      x(50) = (x(50)-jvs(360)*x(54)-jvs(361)*x(55)-jvs(362)*x(56)   &
             -jvs(363)*x(58)-jvs(364)*x(60)-jvs(365)*x(61)-jvs(366)   &
             *x(62)-jvs(367)*x(63))/(jvs(359))
      x(49) = (x(49)-jvs(341)*x(50)-jvs(342)*x(51)-jvs(343)*x(54)   &
             -jvs(344)*x(55)-jvs(345)*x(56)-jvs(346)*x(57)-jvs(347)   &
             *x(58)-jvs(348)*x(60)-jvs(349)*x(61)-jvs(350)*x(62)   &
             -jvs(351)*x(63)-jvs(352)*x(64))/(jvs(340))
      x(48) = (x(48)-jvs(315)*x(51)-jvs(316)*x(53)-jvs(317)*x(57)   &
             -jvs(318)*x(58)-jvs(319)*x(59)-jvs(320)*x(60)-jvs(321)   &
             *x(61)-jvs(322)*x(62)-jvs(323)*x(63))/(jvs(314))
      x(47) = (x(47)-jvs(297)*x(57)-jvs(298)*x(58)-jvs(299)*x(60)   &
             -jvs(300)*x(61)-jvs(301)*x(62)-jvs(302)*x(63))/(jvs(296))
      x(46) = (x(46)-jvs(289)*x(56)-jvs(290)*x(58)-jvs(291)*x(60)   &
             -jvs(292)*x(61)-jvs(293)*x(62))/(jvs(288))
      x(45) = (x(45)-jvs(280)*x(46)-jvs(281)*x(54)-jvs(282)*x(55)   &
             -jvs(283)*x(61)-jvs(284)*x(62)-jvs(285)*x(63))/(jvs(279))
      x(44) = (x(44)-jvs(269)*x(53)-jvs(270)*x(57)-jvs(271)*x(58)   &
             -jvs(272)*x(59)-jvs(273)*x(60)-jvs(274)*x(61)-jvs(275)   &
             *x(62)-jvs(276)*x(63))/(jvs(268))
      x(43) = (x(43)-jvs(256)*x(51)-jvs(257)*x(53)-jvs(258)*x(58)   &
             -jvs(259)*x(60)-jvs(260)*x(61)-jvs(261)*x(62)-jvs(262)   &
             *x(63)-jvs(263)*x(64))/(jvs(255))
      x(42) = (x(42)-jvs(247)*x(58)-jvs(248)*x(61)-jvs(249)*x(62))/   &
             (jvs(246))
      x(41) = (x(41)-jvs(235)*x(42)-jvs(236)*x(51)-jvs(237)*x(53)   &
             -jvs(238)*x(57)-jvs(239)*x(58)-jvs(240)*x(59)-jvs(241)   &
             *x(60)-jvs(242)*x(61)-jvs(243)*x(62)-jvs(244)*x(63)   &
             -jvs(245)*x(64))/(jvs(234))
      x(40) = (x(40)-jvs(222)*x(42)-jvs(223)*x(43)-jvs(224)*x(51)   &
             -jvs(225)*x(53)-jvs(226)*x(57)-jvs(227)*x(58)-jvs(228)   &
             *x(59)-jvs(229)*x(60)-jvs(230)*x(61)-jvs(231)*x(62)   &
             -jvs(232)*x(63))/(jvs(221))
      x(39) = (x(39)-jvs(208)*x(46)-jvs(209)*x(50)-jvs(210)*x(55)   &
             -jvs(211)*x(60)-jvs(212)*x(61)-jvs(213)*x(62))/(jvs(207))
      x(38) = (x(38)-jvs(203)*x(58)-jvs(204)*x(61)-jvs(205)*x(62))/   &
             (jvs(202))
      x(37) = (x(37)-jvs(199)*x(58)-jvs(200)*x(61)-jvs(201)*x(62))/   &
             (jvs(198))
      x(36) = (x(36)-jvs(194)*x(51)-jvs(195)*x(60)-jvs(196)*x(61)   &
             -jvs(197)*x(63))/(jvs(193))
      x(35) = (x(35)-jvs(189)*x(38)-jvs(190)*x(60)-jvs(191)*x(61)   &
             -jvs(192)*x(63))/(jvs(188))
      x(34) = (x(34)-jvs(185)*x(46)-jvs(186)*x(61)-jvs(187)*x(62))/   &
             (jvs(184))
      x(33) = (x(33)-jvs(173)*x(34)-jvs(174)*x(43)-jvs(175)*x(45)   &
             -jvs(176)*x(48)-jvs(177)*x(49)-jvs(178)*x(51)-jvs(179)   &
             *x(54)-jvs(180)*x(56)-jvs(181)*x(61)-jvs(182)*x(62)   &
             -jvs(183)*x(63))/(jvs(172))
      x(32) = (x(32)-jvs(166)*x(58)-jvs(167)*x(60)-jvs(168)*x(61)   &
             -jvs(169)*x(62))/(jvs(165))
      x(31) = (x(31)-jvs(149)*x(32)-jvs(150)*x(36)-jvs(151)*x(37)   &
             -jvs(152)*x(38)-jvs(153)*x(42)-jvs(154)*x(43)-jvs(155)   &
             *x(48)-jvs(156)*x(49)-jvs(157)*x(51)-jvs(158)*x(54)   &
             -jvs(159)*x(58)-jvs(160)*x(60)-jvs(161)*x(61)-jvs(162)   &
             *x(62))/(jvs(148))
      x(30) = (x(30)-jvs(133)*x(35)-jvs(134)*x(37)-jvs(135)*x(38)   &
             -jvs(136)*x(42)-jvs(137)*x(47)-jvs(138)*x(51)-jvs(139)   &
             *x(53)-jvs(140)*x(57)-jvs(141)*x(58)-jvs(142)*x(59)   &
             -jvs(143)*x(60)-jvs(144)*x(61)-jvs(145)*x(62)-jvs(146)   &
             *x(63))/(jvs(132))
      x(29) = (x(29)-jvs(125)*x(38)-jvs(126)*x(60)-jvs(127)*x(62)   &
             -jvs(128)*x(63))/(jvs(124))
      x(28) = (x(28)-jvs(120)*x(34)-jvs(121)*x(55)-jvs(122)*x(60)   &
             -jvs(123)*x(61))/(jvs(119))
      x(27) = (x(27)-jvs(114)*x(50)-jvs(115)*x(55)-jvs(116)*x(60))/   &
             (jvs(113))
      x(26) = (x(26)-jvs(111)*x(61)-jvs(112)*x(62))/(jvs(110))
      x(25) = (x(25)-jvs(104)*x(45)-jvs(105)*x(55)-jvs(106)*x(61)   &
             -jvs(107)*x(63))/(jvs(103))
      x(24) = (x(24)-jvs(98)*x(54)-jvs(99)*x(56)-jvs(100)*x(60)   &
             -jvs(101)*x(61)-jvs(102)*x(63))/(jvs(97))
      x(23) = (x(23)-jvs(92)*x(37)-jvs(93)*x(42)-jvs(94)*x(55)-jvs(95)   &
             *x(58)-jvs(96)*x(61))/(jvs(91))
      x(22) = (x(22)-jvs(89)*x(58)-jvs(90)*x(61))/(jvs(88))
      x(21) = (x(21)-jvs(85)*x(44)-jvs(86)*x(61)-jvs(87)*x(63))/   &
             (jvs(84))
      x(20) = (x(20)-jvs(73)*x(30)-jvs(74)*x(37)-jvs(75)*x(42)-jvs(76)   &
             *x(47)-jvs(77)*x(53)-jvs(78)*x(57)-jvs(79)*x(58)-jvs(80)   &
             *x(59)-jvs(81)*x(60)-jvs(82)*x(61)-jvs(83)*x(62))/   &
             (jvs(72))
      x(19) = (x(19)-jvs(70)*x(34)-jvs(71)*x(61))/(jvs(69))
      x(18) = (x(18)-jvs(67)*x(60)-jvs(68)*x(61))/(jvs(66))
      x(17) = (x(17)-jvs(61)*x(56)-jvs(62)*x(61)-jvs(63)*x(63))/   &
             (jvs(60))
      x(16) = (x(16)-jvs(56)*x(26)-jvs(57)*x(56)-jvs(58)*x(61)-jvs(59)   &
             *x(62))/(jvs(55))
      x(15) = (x(15)-jvs(54)*x(61))/(jvs(53))
      x(14) = (x(14)-jvs(51)*x(56)-jvs(52)*x(62))/(jvs(50))
      x(13) = (x(13)-jvs(47)*x(45)-jvs(48)*x(61)-jvs(49)*x(63))/   &
             (jvs(46))
      x(12) = (x(12)-jvs(45)*x(61))/(jvs(44))
      x(11) = (x(11)-jvs(42)*x(19)-jvs(43)*x(61))/(jvs(41))
      x(10) = (x(10)-jvs(39)*x(52)-jvs(40)*x(56))/(jvs(38))
      x(9) = (x(9)-jvs(36)*x(44)-jvs(37)*x(61))/(jvs(35))
      x(8) = (x(8)-jvs(33)*x(50)-jvs(34)*x(61))/(jvs(32))
      x(7) = (x(7)-jvs(31)*x(61))/(jvs(30))
      x(6) = (x(6)-jvs(29)*x(58))/(jvs(28))
      x(5) = (x(5)-jvs(27)*x(39))/(jvs(26))
      x(4) = (x(4)-jvs(18)*x(45)-jvs(19)*x(49)-jvs(20)*x(50)-jvs(21)   &
            *x(54)-jvs(22)*x(56)-jvs(23)*x(60)-jvs(24)*x(61)-jvs(25)   &
            *x(63))/(jvs(17))
      x(3) = (x(3)-jvs(12)*x(37)-jvs(13)*x(42)-jvs(14)*x(52)-jvs(15)   &
            *x(58)-jvs(16)*x(63))/(jvs(11))
      x(2) = (x(2)-jvs(6)*x(22)-jvs(7)*x(37)-jvs(8)*x(38)-jvs(9)*x(51)   &
            -jvs(10)*x(58))/(jvs(5))
      x(1) = (x(1)-jvs(2)*x(8)-jvs(3)*x(54)-jvs(4)*x(61))/(jvs(1))
      return
      end subroutine cbmz_v02r06_solve          


!-----------------------------------------------------------------------
      end module module_cbmz_rodas_prep
