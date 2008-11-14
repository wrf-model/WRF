
      MODULE module_ftuv_subs

      implicit none

!     data for inter2, inter3 and interi
      integer, private, save :: nintervals
      integer, private, allocatable, save :: xi(:), xcnt(:)
      real(8), private, allocatable, save :: xfrac(:,:)


!     data for lyman
      integer, private, parameter :: nla = 2
      integer, private, parameter :: ngast = 17

!     data for schu_inti, schu
      integer, private, parameter :: tdim = 501
      real(8), private, parameter :: tdim_m1 = tdim - 1.0D+0
      real(8), private, parameter :: t_del = 5.0D+0/tdim_m1
      real(8), private, parameter :: t_fac = tdim_m1/5.0D+0
      integer, private :: ii, jj
      real(8), private, save :: d_table(0:tdim,ngast-1)
      real(8), private, save :: x_table(0:tdim,ngast-1)
      real(8), private, save :: o2_table(tdim)
      real(8), private, dimension(12,ngast-1), save :: a_schu, b_schu
!-----------------------------------------------------------------------------
! a_schu(16,12) coefficients for rj(m) (table 1 in kockarts 1994)
! b_schu(16,12) rj(o2)(table 2 in kockarts 1994)
! rjm attenuation coefficients rj(m)
! rjo2 rj(o2)
!-----------------------------------------------------------------------------
      data ((a_schu(jj,ii),jj=1,12),ii=1,ngast-1) /                             &
!a 57000-56500.5 cm-1
       1.13402e-01,1.00088e-20,3.48747e-01,2.76282e-20,3.47322e-01,1.01267e-19, &
       1.67351e-01,5.63588e-19,2.31433e-02,1.68267e-18,0.00000e+00,0.00000e+00, &
!a 56500-56000.5 cm-1
       2.55268e-03,1.64489e-21,1.85483e-01,2.03591e-21,2.60603e-01,4.62276e-21, &
       2.50337e-01,1.45106e-20,1.92340e-01,7.57381e-20,1.06363e-01,7.89634e-19, &
!a 56000-55500.5 cm-1
       4.21594e-03,8.46639e-22,8.91886e-02,1.12935e-21,2.21334e-01,1.67868e-21, &
       2.84446e-01,3.94782e-21,2.33442e-01,1.91554e-20,1.63433e-01,2.25346e-19, &
!a 55500-55000.5 cm-1
       3.93529e-03,6.79660e-22,4.46906e-02,9.00358e-22,1.33060e-01,1.55952e-21, &
       3.25506e-01,3.43763e-21,2.79405e-01,1.62086e-20,2.10316e-01,1.53883e-19, &
!a 55000-54500.5 cm-1
       2.60939e-03,2.33791e-22,2.08101e-02,3.21734e-22,1.67186e-01,5.77191e-22, &
       2.80694e-01,1.33362e-21,3.26867e-01,6.10533e-21,1.96539e-01,7.83142e-20, &
!a 54500-54000.5 cm-1
       9.33711e-03,1.32897e-22,3.63980e-02,1.78786e-22,1.46182e-01,3.38285e-22, &
       3.81762e-01,8.93773e-22,2.58549e-01,4.28115e-21,1.64773e-01,4.67537e-20, &
!a 54000-53500.5 cm-1
       9.51799e-03,1.00252e-22,3.26320e-02,1.33766e-22,1.45962e-01,2.64831e-22, &
       4.49823e-01,6.42879e-22,2.14207e-01,3.19594e-21,1.45616e-01,2.77182e-20, &
!a 53500-53000.5 cm-1
       7.87331e-03,3.38291e-23,6.91451e-02,4.77708e-23,1.29786e-01,8.30805e-23, &
       3.05103e-01,2.36167e-22,3.35007e-01,8.59109e-22,1.49766e-01,9.63516e-21, &
!a 53000-52500.5 cm-1
       6.92175e-02,1.56323e-23,1.44403e-01,3.03795e-23,2.94489e-01,1.13219e-22, &
       3.34773e-01,3.48121e-22,9.73632e-02,2.10693e-21,5.94308e-02,1.26195e-20, &
!a 52500-52000.5 cm-1
       1.47873e-01,8.62033e-24,3.15881e-01,3.51859e-23,4.08077e-01,1.90524e-22, &
       8.08029e-02,9.93062e-22,3.90399e-02,6.38738e-21,8.13330e-03,9.93644e-22, &
!a 52000-51500.5 cm-1
       1.50269e-01,1.02621e-23,2.39823e-01,3.48120e-23,3.56408e-01,1.69494e-22, &
       1.61277e-01,6.59294e-22,8.89713e-02,2.94571e-21,3.25063e-03,1.25548e-20, &
!a 51500-51000.5 cm-1
       2.55746e-01,8.49877e-24,2.94733e-01,2.06878e-23,2.86382e-01,9.30992e-23, &
       1.21011e-01,3.66239e-22,4.21105e-02,1.75700e-21,0.00000e+00,0.00000e+00, &
!a 51000-50500.5 cm-1
       5.40111e-01,7.36085e-24,2.93263e-01,2.46742e-23,1.63417e-01,1.37832e-22, &
       3.23781e-03,2.15052e-21,0.00000e+00,0.00000e+00,0.00000e+00,0.00000e+00, &
!a 50500-50000.5 cm-1
       8.18514e-01,7.17937e-24,1.82262e-01,4.17496e-23,0.00000e+00,0.00000e+00, &
       0.00000e+00,0.00000e+00,0.00000e+00,0.00000e+00,0.00000e+00,0.00000e+00, &
!a 50000-49500.5 cm-1
       8.73680e-01,7.13444e-24,1.25583e-01,2.77819e-23,0.00000e+00,0.00000e+00, &
       0.00000e+00,0.00000e+00,0.00000e+00,0.00000e+00,0.00000e+00,0.00000e+00, &
!a 49500-49000.5 cm-1
       3.32476e-04,7.00362e-24,9.89000e-01,6.99600e-24,0.00000e+00,0.00000e+00, &
       0.00000e+00,0.00000e+00,0.00000e+00,0.00000e+00,0.00000e+00,0.00000e+00 /
      data ((b_schu(jj,ii),jj=1,12),ii=1,ngast-1) /                             &
! 57000-56500.5 cm-1
       1.07382e-21,9.95029e-21,7.19430e-21,2.48960e-20,2.53735e-20,7.54467e-20, &
       4.48987e-20,2.79981e-19,9.72535e-20,9.29745e-19,2.30892e-20,4.08009e-17, &
! 56500-56000.5 cm-1
       3.16903e-22,1.98251e-21,5.87326e-22,3.44057e-21,2.53094e-21,8.81484e-21, &
       8.82299e-21,4.17179e-20,2.64703e-20,2.43792e-19,8.73831e-20,1.46371e-18, &
! 56000-55500.5 cm-1
       1.64421e-23,9.26011e-22,2.73137e-22,1.33640e-21,9.79188e-22,2.99706e-21, &
       3.37768e-21,1.39438e-20,1.47898e-20,1.04322e-19,4.08014e-20,6.31023e-19, &
! 55500-55000.5 cm-1
       8.68729e-24,7.31056e-22,8.78313e-23,1.07173e-21,8.28170e-22,2.54986e-21, &
       2.57643e-21,9.42698e-21,9.92377e-21,5.21402e-20,3.34301e-20,2.91785e-19, &
! 55000-54500.5 cm-1
       1.20679e-24,2.44092e-22,2.64326e-23,4.03998e-22,2.53514e-22,8.53166e-22, &
       1.29834e-21,3.74482e-21,5.12103e-21,2.65798e-20,2.10948e-20,2.35315e-19, &
! 54500-54000.5 cm-1
       2.79656e-24,1.40820e-22,3.60824e-23,2.69510e-22,4.02850e-22,8.83735e-22, &
       1.77198e-21,6.60221e-21,9.60992e-21,8.13558e-20,4.95591e-21,1.22858e-17, &
! 54000-53500.5 cm-1
       2.36959e-24,1.07535e-22,2.83333e-23,2.16789e-22,3.35242e-22,6.42753e-22, &
       1.26395e-21,5.43183e-21,4.88083e-21,5.42670e-20,3.27481e-21,1.58264e-17, &
! 53500-53000.5 cm-1
       8.65018e-25,3.70310e-23,1.04351e-23,6.43574e-23,1.17431e-22,2.70904e-22, &
       4.88705e-22,1.65505e-21,2.19776e-21,2.71172e-20,2.65257e-21,2.13945e-17, &
! 53000-52500.5 cm-1
       9.63263e-25,1.54249e-23,4.78065e-24,2.97642e-23,6.40637e-23,1.46464e-22, &
       1.82634e-22,7.12786e-22,1.64805e-21,2.37376e-17,9.33059e-22,1.13741e-20, &
! 52500-52000.5 cm-1
       1.08414e-24,8.37560e-24,9.15550e-24,2.99295e-23,9.38405e-23,1.95845e-22, &
       2.84356e-22,3.39699e-21,1.94524e-22,2.72227e-19,1.18924e-21,3.20246e-17, &
! 52000-51500.5 cm-1
       1.52817e-24,1.01885e-23,1.22946e-23,4.16517e-23,9.01287e-23,2.34869e-22, &
       1.93510e-22,1.44956e-21,1.81051e-22,5.17773e-21,9.82059e-22,6.22768e-17, &
! 51500-51000.5 cm-1
       2.12813e-24,8.48035e-24,5.23338e-24,1.93052e-23,1.99464e-23,7.48997e-23, &
       4.96642e-22,6.15691e-17,4.47504e-23,2.76004e-22,8.26788e-23,1.65278e-21, &
! 51000-50500.5 cm-1
       3.81336e-24,7.32307e-24,5.60549e-24,2.04651e-23,3.36883e-22,6.15708e-17, &
       2.09877e-23,1.07474e-22,9.13562e-24,8.41252e-22,0.00000e+00,0.00000e+00, &
! 50500-50000.5 cm-1
       5.75373e-24,7.15986e-24,5.90031e-24,3.05375e-23,2.97196e-22,8.92000e-17, &
       8.55920e-24,1.66709e-17,0.00000e+00,0.00000e+00,0.00000e+00,0.00000e+00, &
! 50000-49500.5 cm-1
       6.21281e-24,7.13108e-24,3.30780e-24,2.61196e-23,1.30783e-22,9.42550e-17, &
       2.69241e-24,1.46500e-17,0.00000e+00,0.00000e+00,0.00000e+00,0.00000e+00, &
! 49500-49000.5 cm-1
       6.81118e-24,6.98767e-24,7.55667e-25,2.75124e-23,1.94044e-22,1.45019e-16, &
       1.92236e-24,3.73223e-17,0.00000e+00,0.00000e+00,0.00000e+00,0.00000e+00 /

!     data for set_o2_xsect      
      integer, public,  parameter :: nwint = 105

      real(8), public, save  :: wlint(nwint)
      real(8), private, save :: xso2int(nwint)
      real(8), private, save :: wlla(nla)
      real(8), private, save :: wlgast(ngast)

! o2 parameters
      data (wlint(ii),ii=1,105)/  &
            0.0000E+00, 0.1166E+03, 0.1167E+03, 0.1173E+03, 0.1179E+03,  &
            0.1187E+03, 0.1194E+03, 0.1202E+03, 0.1208E+03, 0.1214E+03,  &
            0.1219E+03, 0.1223E+03, 0.1231E+03, 0.1238E+03, 0.1246E+03,  &
            0.1254E+03, 0.1262E+03, 0.1270E+03, 0.1278E+03, 0.1286E+03,  &
            0.1294E+03, 0.1303E+03, 0.1311E+03, 0.1320E+03, 0.1329E+03,  &
            0.1338E+03, 0.1346E+03, 0.1356E+03, 0.1365E+03, 0.1374E+03,  &
            0.1384E+03, 0.1399E+03, 0.1418E+03, 0.1439E+03, 0.1459E+03,  &
            0.1481E+03, 0.1504E+03, 0.1526E+03, 0.1550E+03, 0.1574E+03,  &
            0.1600E+03, 0.1626E+03, 0.1653E+03, 0.1681E+03, 0.1709E+03,  &
            0.1731E+03, 0.1746E+03, 0.1754E+03, 0.1770E+03, 0.1786E+03,  &
            0.1802E+03, 0.1818E+03, 0.1835E+03, 0.1852E+03, 0.1869E+03,  &
            0.1887E+03, 0.1905E+03, 0.1923E+03, 0.1942E+03, 0.1961E+03,  &
            0.1980E+03, 0.2000E+03, 0.2020E+03, 0.2041E+03, 0.2050E+03,  &
            0.2060E+03, 0.2070E+03, 0.2080E+03, 0.2090E+03, 0.2100E+03,  &
            0.2110E+03, 0.2120E+03, 0.2130E+03, 0.2140E+03, 0.2150E+03,  &
            0.2160E+03, 0.2170E+03, 0.2180E+03, 0.2190E+03, 0.2200E+03,  &
            0.2210E+03, 0.2220E+03, 0.2230E+03, 0.2240E+03, 0.2250E+03,  &
            0.2260E+03, 0.2270E+03, 0.2280E+03, 0.2290E+03, 0.2300E+03,  &
            0.2310E+03, 0.2320E+03, 0.2330E+03, 0.2340E+03, 0.2350E+03,  &
            0.2360E+03, 0.2370E+03, 0.2380E+03, 0.2390E+03, 0.2400E+03,  &
            0.2410E+03, 0.2420E+03, 0.2430E+03, 0.2430E+03, 0.1000E+39 /

! o2 parameters
      data (xso2int(ii),ii=1,105)/  &
            0.0000E+00, 0.1000E-19, 0.6350E-18, 0.7525E-18, 0.1425E-18,  &
            0.2025E-18, 0.2413E-17, 0.6400E-17, 0.5251E-17, 0.7329E-18,  &
            0.3445E-18, 0.3425E-18, 0.3925E-18, 0.8918E-17, 0.9197E-17,  &
            0.6625E-18, 0.2700E-18, 0.1575E-18, 0.3240E-18, 0.4990E-18,  &
            0.4875E-18, 0.5525E-18, 0.1068E-17, 0.1850E-17, 0.2275E-17,  &
            0.3425E-17, 0.5890E-17, 0.8365E-17, 0.1090E-16, 0.1275E-16,  &
            0.1340E-16, 0.1380E-16, 0.1440E-16, 0.1445E-16, 0.1350E-16,  &
            0.1220E-16, 0.1071E-16, 0.9075E-17, 0.7410E-17, 0.5775E-17,  &
            0.4210E-17, 0.2765E-17, 0.1655E-17, 0.9760E-18, 0.5900E-18,  &
            0.3660E-18, 0.2061E-18, 0.3889E-19, 0.4092E-20, 0.2273E-20,  &
            0.1256E-20, 0.6902E-21, 0.3750E-21, 0.2097E-21, 0.1226E-21,  &
            0.6508E-22, 0.4579E-22, 0.3220E-22, 0.2507E-22, 0.1941E-22,  &
            0.1609E-22, 0.1319E-22, 0.9907E-23, 0.7419E-23, 0.7240E-23,  &
            0.7090E-23, 0.6955E-23, 0.6770E-23, 0.6595E-23, 0.6375E-23,  &
            0.6145E-23, 0.5970E-23, 0.5805E-23, 0.5655E-23, 0.5470E-23,  &
            0.5240E-23, 0.5005E-23, 0.4760E-23, 0.4550E-23, 0.4360E-23,  &
            0.4175E-23, 0.3990E-23, 0.3780E-23, 0.3560E-23, 0.3330E-23,  &
            0.3095E-23, 0.2875E-23, 0.2875E-23, 0.2875E-23, 0.2700E-23,  &
            0.2530E-23, 0.2340E-23, 0.2175E-23, 0.2020E-23, 0.1860E-23,  &
            0.1705E-23, 0.1555E-23, 0.1410E-23, 0.1280E-23, 0.1160E-23,  &
            0.1055E-23, 0.9550E-24, 0.4500E-24, 0.0000E+00, 0.0000E+00 /

! the lyman-alpha grid
      data (wlla(ii),ii=1,2)/ 121.4, 121.9 /

! o2 parameters
      data (wlgast(ii),ii=1,17)/  &
            0.1754E+03, 0.1770E+03, 0.1786E+03, 0.1802E+03, 0.1818E+03,  &
            0.1835E+03, 0.1852E+03, 0.1869E+03, 0.1887E+03, 0.1905E+03,  &
            0.1923E+03, 0.1942E+03, 0.1961E+03, 0.1980E+03, 0.2000E+03,  &
            0.2020E+03, 0.2041E+03 /

      CONTAINS

      subroutine photoin( nz,             & 
                          zen,            & 
                          o3toms,         & 
                          esfact,         & 
                          o3top,          & 
                          o2top,          & 
                          albedo,         &
                          z,              &  
                          tlev,           & 
                          tlay,           & 
                          airlev,         &
                          rh,             & 
                          xlwc,           & 
                          o3,             &
                          acb1,           & 
                          acb2,           & 
                          aoc1,           & 
                          aoc2,           & 
                          aant,           & 
                          aso4,           & 
                          asal,           & 
                          prate )

      use module_wave_data, only : nw, tuv_jmax, deltaw, sflx,   &
                                   c20, c40, c60, c80, sq
!-----------------------------------------------
! ... inter-face routine
!-----------------------------------------------
      implicit none
!----------------------------------------------------------
! ... dummy arguments
!----------------------------------------------------------
      integer, intent(in) :: nz      
      real(8), intent(in)    :: zen     
      real(8), intent(in)    :: o3toms  
      real(8), intent(in)    :: esfact  
      real(8), intent(in)    :: o3top   
      real(8), intent(in)    :: o2top  
      real(8), intent(in)    :: albedo(nw-1)
      real(8), intent(in)    :: z(nz)
      real(8), intent(in)    :: tlev(nz)
      real(8), intent(in)    :: tlay(nz-1)
      real(8), intent(in)    :: airlev(nz)
      real(8), intent(in)    :: rh(nz)
      real(8), intent(in)    :: xlwc(nz)
      real(8), intent(in)    :: o3(nz)
      real(8), intent(in)    :: acb1(nz)
      real(8), intent(in)    :: acb2(nz)
      real(8), intent(in)    :: aoc1(nz)
      real(8), intent(in)    :: aoc2(nz)
      real(8), intent(in)    :: aant(nz)
      real(8), intent(in)    :: aso4(nz)
      real(8), intent(in)    :: asal(nz)
      real(8), intent(out)   :: prate(nz,tuv_jmax)
!----------------------------------------------------------
! ... local variables
!----------------------------------------------------------
      integer :: i, j, k, iw, n
      real(8) :: factor, delzint
      character(len=8 ) :: cdate(4)
      character(len=10) :: ctime(4)
!----------------------------------------------------------
! ... altitude grid
!----------------------------------------------------------
      integer :: iz, izi
      real(8), dimension(nz) :: colinc, vcol, scol, to3
!----------------------------------------------------------
! ... fast-j adj
!----------------------------------------------------------
      real(8), dimension(nz,tuv_jmax) :: adjcoe, adjcoe1, adjcoe2
!----------------------------------------------------------
! ... solar zenith angle
! slant pathlengths in spherical geometry
!----------------------------------------------------------
      integer :: nid(0:nz-1)
      real(8) :: dsdh(0:nz-1,nz-1)
!----------------------------------------------------------
! ... extra terrestrial solar flux and earth-sun distance ^-2
!----------------------------------------------------------
      real(8), dimension(nw-1) :: etf, delw, xsec
!--------------------------------------------------------------
! ... o2 absorption cross section
!--------------------------------------------------------------
      real(8) :: xso2(nw-1,nz)
!--------------------------------------------------------------
! ... atmospheric optical parameters:
!--------------------------------------------------------------
      real(8), dimension(nz-1,nw-1) :: dtrl, dto3, dto2
      real(8), dimension(nz-1,nw-1) :: dtcld, omcld, gcld
      real(8), dimension(nz-1,nw-1) :: dtcb1, omcb1, gcb1
      real(8), dimension(nz-1,nw-1) :: dtcb2, omcb2, gcb2
      real(8), dimension(nz-1,nw-1) :: dtoc1, omoc1, goc1
      real(8), dimension(nz-1,nw-1) :: dtoc2, omoc2, goc2
      real(8), dimension(nz-1,nw-1) :: dtant, omant, gant
      real(8), dimension(nz-1,nw-1) :: dtso4, omso4, gso4
      real(8), dimension(nz-1,nw-1) :: dtsal, omsal, gsal
!--------------------------------------------------------------
! ... spectral irradiance and actinic flux (scalar irradiance):
!--------------------------------------------------------------
      real(8), dimension(nz,nw-1) :: radfld, radxx
!-------------------------------------------------------------
! ... j-values:
!-------------------------------------------------------------
      integer :: m
!-------------------------------------------------------------
! ... location and time
!-------------------------------------------------------------
      integer :: iyear, imonth, iday
      real(8) :: dtime, ut0
!-------------------------------------------------------------
! ... earth-sun distance effect
!-------------------------------------------------------------
      etf(1:nw-1) = sflx(1:nw-1) * esfact 
!-------------------------------------------------------
! ... air profile and rayleigh optical depths (inter-face)
!-------------------------------------------------------
      call setair( nz, z, airlev, dtrl, colinc, o2top )
!-------------------------------------------------------------
! ... ozone optical depths (must give temperature) (inter-face)
!-------------------------------------------------------------
      call setozo( nz, z, tlay, dto3, to3, o3, airlev, o3top, o3toms )

!-------------------------------------------------------------
! ... cloud optical depths (must cloud water g/m3) (inter-face)
!-------------------------------------------------------------
      call setcld( nz, z, xlwc, dtcld, omcld, gcld )

!-------------------------------------------------------------
! ... aerosol optical depths ...
!-------------------------------------------------------------
      call setaer( nz,                     &
                   z,                      &
                   airlev,                 &
                   rh,                     &
                   acb1,                   &
                   acb2,                   &
                   aoc1,                   &
                   aoc2,                   &
                   aant,                   &
                   aso4,                   &
                   asal,                   &
                   dtcb1, omcb1, gcb1,     &
                   dtcb2, omcb2, gcb2,     &
                   dtoc1, omoc1, goc1,     &
                   dtoc2, omoc2, goc2,     &
                   dtant, omant, gant,     &
                   dtso4, omso4, gso4,     &
                   dtsal, omsal, gsal )

!------------------------------------------------------------
! ... photo-chemical and photo-biological weigting functions.
! for pchem, need to know temperature and pressure profiles.
! output:
! from pchem: sq(nw,nz,nj) - for each reaction jlabel(nj)
!-------------------------------------------------------------
      call pchem( nz, tlev, airlev )

!-------------------------------------------------------------
! ... slant path lengths for spherical geometry
!-------------------------------------------------------------
       call spheres( nz, z, zen, dsdh, nid )

       call airmas( nz, z, zen, dsdh, nid, colinc, vcol, scol )

!---------------------------------------------------------------
! ... modification of coefficent of j-vales function of to3 and zenith
!---------------------------------------------------------------
      if( zen <= 20.5 ) then
         call setz( nz, to3, tlev, c20, 20, adjcoe )
      else if( zen > 20.5 .and. zen < 40.5 ) then
         call setz( nz, to3, tlev, c20, 20, adjcoe1 )
         call setz( nz, to3, tlev, c40, 40, adjcoe2 )
         factor = (zen - 20.5) / (40.5 - 20.5)
         adjcoe(:nz,:28) = adjcoe1(:nz,:28) + (adjcoe2(:nz,:28) - adjcoe1(:nz,:28)) * factor
      else if( zen >= 40.5 .and. zen < 60.5 ) then
         call setz( nz, to3, tlev, c40, 40, adjcoe1 )
         call setz( nz, to3, tlev, c60, 60, adjcoe2 )
         factor = (zen - 40.5) / (60.5 - 40.5)
         adjcoe(:nz,:28) = adjcoe1(:nz,:28) + (adjcoe2(:nz,:28) - adjcoe1(:nz,:28)) * factor
      else if( zen >= 60.5 .and. zen < 80. ) then
         call setz( nz, to3, tlev, c60, 60, adjcoe1 )
         call setz( nz, to3, tlev, c80, 80, adjcoe2 )
         factor = (zen - 60.5) / (80.5 - 60.5)
         adjcoe(:nz,:28) = adjcoe1(:nz,:28) + (adjcoe2(:nz,:28) - adjcoe1(:nz,:28)) * factor
      else if( zen >= 80. ) then
         call setz( nz, to3, tlev, c80, 80, adjcoe )
      end if
!------------------------------------------------------------------
! ... effective o2 optical depth (sr bands, must know zenith angle!)
! assign o2 cross section to sq(*,*,1)
!------------------------------------------------------------------
       call set_o2_xsect( nz, z, colinc, vcol, scol, dto2, xso2 )
       sq(1:nw-1,1:nz,1) = xso2(1:nw-1,1:nz)

!-----------------------------------------------
! ... main wavelength loop
!-----------------------------------------------
      delw(1:nw-1) = deltaw(1:nw-1) * etf(1:nw-1)
!---------------------------------------------------
! ... monochromatic radiative transfer:
! outputs are fdir, fdn, fup
!---------------------------------------------------
      call rtlink( nz,                  &
                   nw,                  &
                   zen,                 &
                   albedo,              & 
                   z,                   &
                   nid,                 &
                   dsdh,                & 
                   dtrl,                & ! opt depth of air (rayleigh)
                   dto3,                & ! opt depth of o3
                   dto2,                & ! opt depth of o2
                   dtcld, omcld, gcld,  & ! opt depth of cloud
                   dtcb1, omcb1, gcb1,  &
                   dtcb2, omcb2, gcb2,  &
                   dtoc1, omoc1, goc1,  &
                   dtoc2, omoc2, goc2,  &
                   dtant, omant, gant,  &
                   dtso4, omso4, gso4,  &
                   dtsal, omsal, gsal,  &
                   radfld )               ! output of abs. scart flux.
!----------------------------------------------------------
! Interplation the top level
!----------------------------------------------------------
         delzint=(z(nz-2)-z(nz-3))/(z(nz-1)-z(nz-2))
         do iw = 1,nw-1
            radfld(1,iw) = radfld(2,iw) + (radfld(2,iw)-radfld(3,iw))*delzint
            radfld(1,iw) = max(radfld(1,iw),radfld(2,iw))
         end do

!----------------------------------------------------------
! ... j-val calculation
! spherical irradiance (actinic flux)
! as a function of altitude
! convert to quanta s-1 nm-1 cm-2
! (1.e-4 * (wc*1e-9) / (hc = 6.62e-34 * 2.998e8))
!----------------------------------------------------------
      do m = 1,tuv_jmax
        do iz = 1,nz
          izi = nz - iz + 1
          xsec(:nw-1) = sq(:nw-1,izi,m) * delw(:nw-1)
          prate(iz,m) = dot_product( radfld(iz,:nw-1), xsec(:nw-1) )
        end do
        prate(1:nz,m) = prate(1:nz,m) * adjcoe(nz:1:-1,m)
      end do

      end subroutine photoin

      subroutine setaer( nzlev,                  &
                         z,                      &
                         airden,                 &
                         rh,                     &
                         acb1,                   &
                         acb2,                   &
                         aoc1,                   &
                         aoc2,                   &
                         aant,                   &
                         aso4,                   &
                         asal,                   &
                         op_cb1, om_cb1, g_cb1,  &
                         op_cb2, om_cb2, g_cb2,  &
                         op_oc1, om_oc1, g_oc1,  &
                         op_oc2, om_oc2, g_oc2,  &
                         op_ant, om_ant, g_ant,  &
                         op_so4, om_so4, g_so4,  &
                         op_sal, om_sal, g_sal )

      use module_wave_data, only : nw, wc

!-----------------------------------------------------------------------------
!=  PURPOSE:                                                                 
!
!=  Cal aer opt depth
!-----------------------------------------------------------------------------
!
!=  PARAMETERS:                                                              
!=  NZLEV   - INTEGER, number of specified altitude levels in the working (I)
!=            grid                                                           
!=  Z       - real(8), specified altitude working grid (km)                  (I)
!=  axxx    - aerosol mix ratio                                           (I)
!
!=  op      - real(8), optical depth (absorption)                            (O)
!=  om      - real(8), single albedo             
!=  g       - asysmetric factor                                           (O)
!
!-----------------------------------------------------------------------------
!
!     VERTICAL DOMAIN is from bottom(1)  to TOP (TOP=nzlev)
!        CCM from top(1) to bottom(nzlev)
!-----------------------------------------------------------------------------
      implicit none

!-----------------------------------------------------------------------------
!	... Dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: nzlev
      real(8), intent(in)  :: z(nzlev)
      real(8), intent(in)  :: airden(nzlev)
      real(8), intent(in)  :: rh(nzlev)
      real(8), intent(in)  :: acb1(nzlev)
      real(8), intent(in)  :: acb2(nzlev)
      real(8), intent(in)  :: aoc1(nzlev)
      real(8), intent(in)  :: aoc2(nzlev)
      real(8), intent(in)  :: aant(nzlev)
      real(8), intent(in)  :: aso4(nzlev)
      real(8), intent(in)  :: asal(nzlev)
      real(8), intent(out) :: op_cb1(nzlev-1,nw-1), om_cb1(nzlev-1,nw-1), g_cb1(nzlev-1,nw-1)
      real(8), intent(out) :: op_cb2(nzlev-1,nw-1), om_cb2(nzlev-1,nw-1), g_cb2(nzlev-1,nw-1)
      real(8), intent(out) :: op_oc1(nzlev-1,nw-1), om_oc1(nzlev-1,nw-1), g_oc1(nzlev-1,nw-1)
      real(8), intent(out) :: op_oc2(nzlev-1,nw-1), om_oc2(nzlev-1,nw-1), g_oc2(nzlev-1,nw-1)
      real(8), intent(out) :: op_ant(nzlev-1,nw-1), om_ant(nzlev-1,nw-1), g_ant(nzlev-1,nw-1)
      real(8), intent(out) :: op_so4(nzlev-1,nw-1), om_so4(nzlev-1,nw-1), g_so4(nzlev-1,nw-1)
      real(8), intent(out) :: op_sal(nzlev-1,nw-1), om_sal(nzlev-1,nw-1), g_sal(nzlev-1,nw-1)

!-----------------------------------------------------------------------------
! 	... local variables
!-----------------------------------------------------------------------------
      real(8)    :: dz(nzlev)

      real(8)    :: num_oc1, wei_oc1
      real(8)    :: num_oc2, wei_oc2
      real(8)    :: num_cb1, wei_cb1
      real(8)    :: num_cb2, wei_cb2
      real(8)    :: num_ant, wei_ant
      real(8)    :: num_so4, wei_so4
      real(8)    :: num_sal, wei_sal

      real(8)    :: rw_oc1, rw_oc2, rw_cb1, rw_cb2, rw_ant, rw_so4, rw_sal

      real(8)    :: sig_oc2, sig_cb2

      real(8)    :: opt_oc1, opt_oc2, opt_cb1, opt_cb2, opt_ant, opt_so4, opt_sal  

      real(8)    :: aoc1x, aoc2x, acb1x, acb2x, aantx, aso4x, asalx
      real(8)    :: denx,  rhx

      integer :: k, iw

!-----------------------------------------------------------------------------
! 	... assign the constants
!-----------------------------------------------------------------------------
      real(8), parameter :: rm_cb1 = 0.035
      real(8), parameter :: rm_cb2 = 0.035
      real(8), parameter :: rm_oc1 = 0.10
      real(8), parameter :: rm_oc2 = 0.10
      real(8), parameter :: rm_ant = 0.15
      real(8), parameter :: rm_so4 = 0.24
      real(8), parameter :: rm_sal = 1.30

      real(8), parameter :: de_cb1 = 1.0
      real(8), parameter :: de_cb2 = 1.0
      real(8), parameter :: de_oc1 = 1.5 
      real(8), parameter :: de_oc2 = 1.5 
      real(8), parameter :: de_ant = 1.7  
      real(8), parameter :: de_so4 = 1.7
      real(8), parameter :: de_sal = 2.2

      real(8), parameter :: ml_cb1 = 12.
      real(8), parameter :: ml_cb2 = 12.
      real(8), parameter :: ml_oc1 = 48. 
      real(8), parameter :: ml_oc2 = 48. 
      real(8), parameter :: ml_ant = 90.  
      real(8), parameter :: ml_so4 = 96.
      real(8), parameter :: ml_sal = 22.

      real(8), parameter :: qx_cb1 = 2.138
      real(8), parameter :: qx_cb2 = 2.138
      real(8), parameter :: qw_cb1 = 0.510
      real(8), parameter :: qw_cb2 = 0.510
      real(8), parameter :: sx_cb2 = 0.120
      real(8), parameter :: sw_cb2 = 1.000

      real(8), parameter :: qx_oc1 = 0.675
      real(8), parameter :: qx_oc2 = 0.675
      real(8), parameter :: qw_oc1 = 1.118
      real(8), parameter :: qw_oc2 = 1.118
      real(8), parameter :: sx_oc2 = 0.980
      real(8), parameter :: sw_oc2 = 1.000

      real(8), parameter :: qx_ant = 0.726
      real(8), parameter :: qw_ant = 1.770

      real(8), parameter :: qx_so4 = 2.830
      real(8), parameter :: qw_so4 = 3.605

      real(8), parameter :: qx_sal = 2.600
      real(8), parameter :: qw_sal = 2.575

      real(8), parameter :: pi = 3.1416

!-----------------------------------------------------------------------------
! 	... hydroscropic growth
!-----------------------------------------------------------------------------
      real(8), parameter :: a1_cb2 = 1.000
      real(8), parameter :: a2_cb2 =-0.829
      real(8), parameter :: a3_cb2 = 1.350

      real(8), parameter :: a1_oc2 = 1.005
      real(8), parameter :: a2_oc2 =-0.0666
      real(8), parameter :: a3_oc2 = 0.7608

      real(8), parameter :: a1_ant = 1.000
      real(8), parameter :: a2_ant = 0.493
      real(8), parameter :: a3_ant = 0.527

      real(8), parameter :: a1_so4 = 1.000
      real(8), parameter :: a2_so4 = 0.493
      real(8), parameter :: a3_so4 = 0.528

      real(8), parameter :: a1_sal = 1.011
      real(8), parameter :: a2_sal = 0.457 
      real(8), parameter :: a3_sal = 1.102

!-----------------------------------------------------------------------
!     Assign the wave-length dep values for aerososl
!-----------------------------------------------------------------------
      real(8), parameter :: d1_cb = 1.61892
      real(8), parameter :: d2_cb =-0.0006853
      real(8), parameter :: d3_cb =-1.07806e-6

      real(8), parameter :: o1_cb = 1.29185
      real(8), parameter :: o2_cb = 7.6863e-5
      real(8), parameter :: o3_cb =-1.28567e-6

      real(8), parameter :: g1_cb = 2.87326
      real(8), parameter :: g2_cb =-0.004482
      real(8), parameter :: g3_cb = 1.50361e-6

      real(8), parameter :: d1_oc = 10.5098
      real(8), parameter :: d2_oc =-0.0301455
      real(8), parameter :: d3_oc = 2.22837e-5

      real(8), parameter :: g1_oc = 1.67125
      real(8), parameter :: g2_oc =-0.0008341
      real(8), parameter :: g3_oc =-1.18801e-6

      real(8), parameter :: d1_ant = 9.44794
      real(8), parameter :: d2_ant =-0.0268029
      real(8), parameter :: d3_ant = 1.97106e-5

      real(8), parameter :: g1_ant = 1.34742
      real(8), parameter :: g2_ant =-1.85850e-5
      real(8), parameter :: g3_ant =-1.54297e-6

      real(8), parameter :: d1_so4 = 0.942351
      real(8), parameter :: d2_so4 = 0.00220168
      real(8), parameter :: d3_so4 =-3.9622e-6

      real(8), parameter :: d1_sal = 1.0
      real(8), parameter :: d2_sal = 0.0
      real(8), parameter :: d3_sal = 0.0

!-----------------------------------------------------------------------------
! 	... calculate vertical interveral dz
!-----------------------------------------------------------------------------    
      do k = 1, nzlev-1
         dz(k) = abs((z(k+1)-z(k))) * 1.e5  ! dz in center Meters
      enddo

!=======  Main heigh lopp ===========
      HEIGHT_LOOP : do k = 1, nzlev-1

         acb1x = acb1(k)
         acb2x = acb2(k)
         aoc1x = aoc1(k)
         aoc2x = aoc2(k)
         aantx = aant(k)
         aso4x = aso4(k)
         asalx = asal(k)

         denx  = airden(k)      
         rhx   = rh(k)

!        -------- CB1 -------------

         rw_cb1 = rm_cb1

         call cnum(acb1x, denx, de_cb1, ml_cb1, rm_cb1, rw_cb1, num_cb1, wei_cb1)
      
         opt_cb1 = qx_cb1*wei_cb1 + (1.-wei_cb1)*qw_cb1     
         opt_cb1 = opt_cb1*num_cb1*rw_cb1*rw_cb1 * pi * dz(k)*1.e-8          ! all in cm unites 

!        -------- CB2 -------------

         rw_cb2 = rm_cb2*(a1_cb2+a2_cb2*rhx+a3_cb2*rhx*rhx)

         call cnum(acb2x, denx, de_cb2, ml_cb2, rm_cb2, rw_cb2, num_cb2, wei_cb2)
      
         opt_cb2 = qx_cb2*wei_cb2 + (1.-wei_cb2)*qw_cb2     
         opt_cb2 = opt_cb2*num_cb2*rw_cb2*rw_cb2 * pi * dz(k)*1.e-8          ! all in cm unites 
        
         sig_cb2 = sx_cb2*wei_cb2 + (1.-wei_cb2)*sw_cb2   

!        -------- OC1 -------------
      
         rw_oc1 = rm_oc1

         call cnum(aoc1x, denx, de_oc1, ml_oc1, rm_oc1, rw_oc1, num_oc1, wei_oc1)
      
         opt_oc1 = qx_oc1*wei_oc1 + (1.-wei_oc1)*qw_oc1     
         opt_oc1 = opt_oc1*num_oc1*rw_oc1*rw_oc1 * pi * dz(k)*1.e-8          ! all in cm unites 

!        -------- OC2 -------------
      
         rw_oc2 = rm_oc2*(a1_oc2+a2_oc2*rhx+a3_oc2*rhx*rhx)

         call cnum(aoc2x, denx, de_oc2, ml_oc2, rm_oc2, rw_oc2, num_oc2, wei_oc2)
      
         opt_oc2 = qx_oc2*wei_oc2 + (1.-wei_oc2)*qw_oc2     
         opt_oc2 = opt_oc2*num_oc2*rw_oc2*rw_oc2 * pi * dz(k)*1.e-8          ! all in cm unites 

         sig_oc2 = sx_oc2*wei_oc2 + (1.-wei_cb2)*sw_oc2   

!        -------- ANT-NH4NO3 -------------
      
         rw_ant=rm_ant*(a1_ant+a2_ant*rhx+a3_ant*rhx*rhx)

         call cnum(aantx, denx, de_ant, ml_ant, rm_ant, rw_ant, num_ant, wei_ant)
      
         opt_ant = qx_ant*wei_ant + (1.-wei_ant)*qw_ant     
         opt_ant = opt_ant*num_ant*rw_ant*rw_ant * pi * dz(k)*1.e-8          ! all in cm unites 

!        -------- SO4 -------------
      
         rw_so4=rm_so4*(a1_so4+a2_so4*rhx+a3_so4*rhx*rhx)

         call cnum(aso4x, denx, de_so4, ml_so4, rm_so4, rw_so4, num_so4, wei_so4)
      
         opt_so4 = qx_so4*wei_so4 + (1.-wei_so4)*qw_so4     
         opt_so4 = opt_so4*num_so4*rw_so4*rw_so4 * pi * dz(k)*1.e-8          ! all in cm unites 

!        -------- SALT -------------
      
         rw_sal=rm_sal*(a1_sal+a2_sal*rhx+a3_sal*rhx*rhx)

         call cnum(asalx, denx, de_sal, ml_sal, rm_sal, rw_sal, num_sal, wei_sal)
      
         opt_sal = qx_sal*wei_sal + (1.-wei_sal)*qw_sal     
         opt_sal = opt_sal*num_sal*rw_sal*rw_sal * pi * dz(k)*1.e-8          ! all in cm unites 

!        ======== end of aerosol ====

         WAVE_LENGTH_LOOP : do iw = 1,nw-1

            op_cb1(k,iw) = opt_cb1*(d1_cb + d2_cb*wc(iw) + d3_cb*wc(iw)*wc(iw))
            om_cb1(k,iw) = 0.12*(o1_cb + o2_cb*wc(iw) + o3_cb*wc(iw)*wc(iw))
             g_cb1(k,iw) = 0.12*(g1_cb + g2_cb*wc(iw) + g3_cb*wc(iw)*wc(iw))

            op_cb2(k,iw) = opt_cb2*(d1_cb + d2_cb*wc(iw) + d3_cb*wc(iw)*wc(iw))
            om_cb2(k,iw) = sig_cb2*(o1_cb + o2_cb*wc(iw) + o3_cb*wc(iw)*wc(iw))
             g_cb2(k,iw) = 0.12*(g1_cb + g2_cb*wc(iw) + g3_cb*wc(iw)*wc(iw))

            op_oc1(k,iw) = opt_oc1*(d1_oc + d2_oc*wc(iw) + d3_oc*wc(iw)*wc(iw))
            om_oc1(k,iw) = 0.98
             g_oc1(k,iw) = 0.50*(g1_oc + g2_oc*wc(iw) + g3_oc*wc(iw)*wc(iw))

            op_oc2(k,iw) = opt_oc2*(d1_oc + d2_oc*wc(iw) + d3_oc*wc(iw)*wc(iw))
            om_oc2(k,iw) = sig_oc2
             g_oc2(k,iw) = 0.50*(g1_oc + g2_oc*wc(iw) + g3_oc*wc(iw)*wc(iw))

            op_ant(k,iw) = opt_ant*(d1_ant + d2_ant*wc(iw) + d3_ant*wc(iw)*wc(iw))
            om_ant(k,iw) = 1.00
             g_ant(k,iw) = 0.63*(g1_ant + g2_ant*wc(iw) + g3_ant*wc(iw)*wc(iw))

            op_so4(k,iw) = opt_so4*(d1_so4 + d2_so4*wc(iw) + d3_so4*wc(iw)*wc(iw))
            om_so4(k,iw) = 1.00
             g_so4(k,iw) = 0.75

            op_sal(k,iw) = opt_sal*(d1_sal + d2_sal*wc(iw) + d3_sal*wc(iw)*wc(iw))
            om_sal(k,iw) = 1.00
             g_sal(k,iw) = 0.77

         enddo WAVE_LENGTH_LOOP

      enddo HEIGHT_LOOP

      end subroutine setaer

      subroutine cnum( mix, denx, cden, molw, rm, rw, num, weight )
!-----------------------------------------------------------------------------
!   PURPOSE:                                                                 
!
!   Cal aer num and percent weigh
!-----------------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------------
!	... Dummy arguments
!-----------------------------------------------------------------------------
      real(8), intent(in)  :: cden, molw, rm, rw
      real(8), intent(in)  :: mix, denx
      real(8), intent(out) :: num,weight
!-----------------------------------------------------------------------------
!	... Local variables
!-----------------------------------------------------------------------------
      real(8) :: factor, mas, masw

!     Convert volumn mix ratio to mass
      if( mix < 1.0e-15 ) then
        num = 0.0
        weight = 0.0
        return
      endif

!===============================================================================
! XUEXI
!     UNLIKE MOZART mix = mix ratio
!                   mix = ug/m3
!     1e-12 for  ug/m3 --> gram/cm3 
!===============================================================================

      factor = 4./3. * 3.14 * cden * 1.e-12 * rm*rm*rm
      mas    = mix * 1.e-12                              ! 1e-12 for  ug/m3 --> gram/cm3 
      num    = mas / factor

      if( abs(rw-rm) < 1.0e-6 ) then
        masw = 0.0
      else
        masw = 4./3.*3.14*(rw*rw*rw - rm*rm*rm)*1.e-12 * num   ! water mass gram/cm3
      endif

      weight = mas/( mas + masw )                              ! weight percentage (%*0.01)
      weight = max( 0.d0, min(1.d0,weight) )

      end subroutine cnum

      subroutine setair( nz, z, airlev, dtrl, &
                         cz, o2top )
      use module_wave_data, only : nw, wc
!-----------------------------------------------------------------------------
! purpose:
! set up an altitude profile of air molecules. subroutine includes a
! shape-conserving scaling method that allows scaling of the entire
! profile to a given sea-level pressure.
!-----------------------------------------------------------------------------
! parameters:
! pmbnew - real(8), sea-level pressure (mb) to which profile should be
! scaled. if pmbnew < 0, no scaling is done
! nz - integer, number of specified altitude levels in the working (i)
! grid
! z - real(8), specified altitude working grid (km) (i)
! nw - integer, number of specified intervals + 1 in working (i)
! wavelength grid
! wl - real(8), vector of lower limits of wavelength intervals in (i)
! working wavelength grid
! airlev - real(8), air density (molec/cc) at each specified altitude (o)
! dtrl - real(8), rayleigh optical depth at each specified altitude (o)
! and each specified wavelength
! cz - real(8), number of air molecules per cm^2 at each specified (o)
! altitude layer
!-----------------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: nz
      real(8), intent(in) :: o2top
      real(8), intent(in) :: z(nz)
      real(8), intent(in) :: airlev(nz)
!-----------------------------------------------------------------------------
! ... air density (molec cm-3) at each grid level
! rayleigh optical depths
!-----------------------------------------------------------------------------
      real(8), intent(out) :: dtrl(nz-1,nw-1)
      real(8), intent(out) :: cz(nz)
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
      integer :: i, ip1, iw
      real(8) :: hscale
      real(8) :: srayl
      real(8) :: deltaz
      real(8) :: wmicrn, xx
!-----------------------------------------------------
! ... compute column increments (logarithmic integrals)
!-----------------------------------------------------
      do i = 1,nz-1
         ip1 = i + 1
         deltaz = 1.e5 * (z(ip1) - z(i))
         cz(i) = (airlev(ip1) - airlev(i)) / log(airlev(ip1)/airlev(i)) * deltaz
      end do
!-----------------------------------------------------
! ... include exponential tail integral from infinity to 50 km,
! fold tail integral into top layer
! specify scale height near top of data. (scale height at 40km????)
!-----------------------------------------------------
! hscale = 8.05e5
      cz(nz) = o2top/.2095
!-----------------------------------------------------
! ... compute rayleigh cross sections and depths:
!-----------------------------------------------------
      do iw = 1,nw-1
!-----------------------------------------------------
! ... rayleigh scattering cross section from wmo 1985 (originally from
! nicolet, m., on the molecular scattering in the terrestrial atmosphere:
! an empirical formula for its calculation in the homoshpere, planet.
! space sci., 32, 1467-1468, 1984.
!-----------------------------------------------------
         wmicrn = 1.e-3*wc(iw)
         if( wmicrn <= .55 ) then
            xx = 3.6772 + 0.389*wmicrn + 0.09426/wmicrn
         else
            xx = 4.04
         end if
         srayl = 4.02e-28/(wmicrn)**xx
         dtrl(1:nz-2,iw) = cz(1:nz-2)*srayl
         dtrl(nz-1,iw) = (cz(nz-1) + cz(nz))*srayl
      end do
      end subroutine setair

      subroutine zenith( lat, long, idate, ut, zen )
!-----------------------------------------------------------------------------
! purpose:
! calculate solar zenith angle for a given time and location.
! calculation is based on equations given in: paltridge and platt, radia-
! tive processes in meteorology and climatology, elsevier, pp. 62,63, 1976.
! fourier coefficients originally from: spencer, j.w., 1971, fourier
! series representation of the position of the sun, search, 2:172.
! note: this approximate program does not account fro changes from year
! to year.
!-----------------------------------------------------------------------------
! parameters:
! lat - real(8), latitude of location (degrees) (i)
! long - real(8), longitude of location (degrees) (i)
! idate - integer, date in the form yymmdd (i)
! ut - real(8), local time in decimal ut (e.g., 16.25 means 15 minutes (i)
! after 4 pm)
! zen - real(8), solar zenith angle (degrees) (o)
!-----------------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: idate
      real(8), intent(in) :: lat,long
      real(8), intent(in) :: ut
      real(8), intent(out) :: zen
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
      real(8), parameter :: pi = 3.1415926
      real(8), parameter :: d2r = pi/180.0
      real(8), parameter :: r2d = 1.0/d2r
      integer :: i
      integer :: iiyear, imth, iday, ijd
      integer :: imn(12) = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
      real(8) :: lbut,lzut
      real(8) :: rlt
      real(8) :: d, tz, rdecl, eqr, eqh, zpt
      real(8) :: csz, zr, caz, raz
      real(8) :: sintz, costz, sin2tz, cos2tz, sin3tz, cos3tz
!-----------------------------------------------------------------------------
! ... convert to radians
!-----------------------------------------------------------------------------
      rlt = lat*d2r
!-----------------------------------------------------------------------------
! ... parse date
!-----------------------------------------------------------------------------
      iiyear = idate/10000
      imth = (idate - iiyear*10000)/100
      iday = idate - iiyear*10000 - imth*100
!-----------------------------------------------------------------------------
! ... identify and correct leap years
!-----------------------------------------------------------------------------
      if( mod(iiyear,4) == 0 ) then
         imn(2) = 29
      else
         imn(2) = 28
      end if
!-----------------------------------------------------------------------------
! ... compute current (julian) day of year ijd = 1 to 365
!-----------------------------------------------------------------------------
      ijd = 0
      do i = 1,imth-1
         ijd = ijd + imn(i)
      end do
      ijd = ijd + iday
!-----------------------------------------------------------------------------
! ... calculate decimal julian day from start of year:
!-----------------------------------------------------------------------------
      d = dble(ijd-1) + ut/24.
!-----------------------------------------------------------------------------
! ... equation 3.8 for "day-angle"
!-----------------------------------------------------------------------------
      tz = 2.*pi*d/365.
!-----------------------------------------------------------------------------
! ... calculate sine and cosine from addition theoremes for
! better performance; the computation of sin2tz,
! sin3tz, cos2tz and cos3tz is about 5-6 times faster
! than the evaluation of the intrinsic functions
!
! it is sin(x+y) = sin(x)*cos(y)+cos(x)*sin(y)
! and cos(x+y) = cos(x)*cos(y)-sin(x)*sin(y)
!
! sintz = sin(tz) costz = cos(tz)
! sin2tz = sin(2.*tz) cos2tz = sin(2.*tz)
! sin3tz = sin(3.*tz) cos3tz = cos(3.*tz)
!-----------------------------------------------------------------------------
      sintz = sin( tz )
      costz = cos( tz )
      sin2tz = 2.*sintz*costz
      cos2tz = costz*costz - sintz*sintz
      sin3tz = sintz*cos2tz + costz*sin2tz
      cos3tz = costz*cos2tz - sintz*sin2tz
!-----------------------------------------------------------------------------
! ... equation 3.7 for declination in radians
!-----------------------------------------------------------------------------
      rdecl = 0.006918 - 0.399912*costz + 0.070257*sintz &
                       - 0.006758*cos2tz + 0.000907*sin2tz &
                       - 0.002697*cos3tz + 0.001480*sin3tz
!-----------------------------------------------------------------------------
! ... equation 3.11 for equation of time in radians
!-----------------------------------------------------------------------------
      eqr = 0.000075 + 0.001868*costz - 0.032077*sintz &
                       - 0.014615*cos2tz - 0.040849*sin2tz
!-----------------------------------------------------------------------------
! ... convert equation of time to hours
!-----------------------------------------------------------------------------
      eqh = eqr*24./(2.*pi)
!-----------------------------------------------------------------------------
! ... calculate local hour angle (hours):
!-----------------------------------------------------------------------------
      lbut = 12. - eqh - long*24./360
!-----------------------------------------------------------------------------
! ... convert to angle from ut
!-----------------------------------------------------------------------------
      lzut = 15.*(ut - lbut)
      zpt = lzut*d2r
!-----------------------------------------------------------------------------
! ... equation 2.4 for cosine of zenith angle
!-----------------------------------------------------------------------------
      csz = sin(rlt)*sin(rdecl) + cos(rlt)*cos(rdecl)*cos(zpt)
      zr = acos(csz)
      zen = zr*r2d
      end subroutine zenith

      subroutine calc_zenith( lat, long, ijd, gmt, azimuth, zenith )
        ! this subroutine calculates solar zenith and azimuth angles for a
        ! part
        ! time and location.  must specify:
        ! input:
        ! lat - latitude in decimal degrees
        ! long - longitude in decimal degrees
        ! gmt  - greenwich mean time - decimal military eg.
        ! 22.75 = 45 min after ten pm gmt
        ! output
        ! zenith
        ! azimuth
        ! .. Scalar Arguments ..
        real(8) :: azimuth, gmt, lat, long, zenith
        integer :: ijd
        ! .. Local Scalars ..
      real(8), parameter :: d2r = 3.1415926/180.0
      real(8), parameter :: r2d = 1.0/d2r
        real(8) :: caz, csz, cw, d, decl, ec, epsi, eqt, eyt, feqt, feqt1,       &
          feqt2, feqt3, feqt4, feqt5, feqt6, feqt7, lbgmt, lzgmt, ml, pepsi,     &
          pi, ra, raz, rdecl, reqt, rlt, rml, rphi, rra, ssw, sw, tab, w, wr,    &
          yt, zpt, zr
        integer :: jd
        ! .. Intrinsic Functions ..
        intrinsic acos, atan, cos, min, sin, tan

        ! convert to radians
        rlt = lat*d2r
        rphi = long*d2r

        ! print julian days current ijd

        ! ???? + (yr - yref)

        jd = ijd



        d = jd + gmt/24.0
        ! calc geom mean longitude
        ml = 279.2801988 + .9856473354*d + 2.267E-13*d*d
        rml = ml*d2r

        ! calc equation of time in sec
        ! w = mean long of perigee
        ! e = eccentricity
        ! epsi = mean obliquity of ecliptic
        w = 282.4932328 + 4.70684E-5*d + 3.39E-13*d*d
        wr = w*d2r
        ec = 1.6720041E-2 - 1.1444E-9*d - 9.4E-17*d*d
        epsi = 23.44266511 - 3.5626E-7*d - 1.23E-15*d*d
        pepsi = epsi*d2r
        yt = (tan(pepsi/2.0))**2
        cw = cos(wr)
        sw = sin(wr)
        ssw = sin(2.0*wr)
        eyt = 2.*ec*yt
        feqt1 = sin(rml)*(-eyt*cw-2.*ec*cw)
        feqt2 = cos(rml)*(2.*ec*sw-eyt*sw)
        feqt3 = sin(2.*rml)*(yt-(5.*ec**2/4.)*(cw**2-sw**2))
        feqt4 = cos(2.*rml)*(5.*ec**2*ssw/4.)
        feqt5 = sin(3.*rml)*(eyt*cw)
        feqt6 = cos(3.*rml)*(-eyt*sw)
        feqt7 = -sin(4.*rml)*(.5*yt**2)
        feqt = feqt1 + feqt2 + feqt3 + feqt4 + feqt5 + feqt6 + feqt7
        eqt = feqt*13751.0

        ! convert eq of time from sec to deg
        reqt = eqt/240.
        ! calc right ascension in rads
        ra = ml - reqt
        rra = ra*d2r
        ! calc declination in rads, deg
        tab = 0.43360*sin(rra)
        rdecl = atan(tab)
        decl = rdecl/d2r
        ! calc local hour angle
        lbgmt = 12.0 - eqt/3600. + long*24./360.
        lzgmt = 15.0*(gmt-lbgmt)
        zpt = lzgmt*d2r
        csz = sin(rlt)*sin(rdecl) + cos(rlt)*cos(rdecl)*cos(zpt)
        if(csz.gt.1)print *,'calczen,csz ',csz
        csz = min(1.d0,csz)
!       zr = acos(csz)
!       zenith = zr/d2r
        zr = acos(csz)
        zenith = zr/d2r
        ! calc local solar azimuth
        caz = (sin(rdecl)-sin(rlt)*cos(zr))/(cos(rlt)*sin(zr))
        if(caz.lt.-0.999999)then
          azimuth=180.
        elseif(caz.gt.0.999999)then
          azimuth=0.
        else
          raz = acos(caz)
          azimuth = raz/d2r
        endif
!       caz = min(1.,(sin(rdecl)-sin(rlt)*cos(zr))/(cos(rlt)*sin(zr)))
!       if(caz.lt.-1)print *,calczen ,caz
!       caz = max(-1.,caz)
!       raz = acos(caz)
!       azimuth = raz/d2r

        if(lzgmt>0) azimuth = azimuth + (2*(180.-azimuth))
        ! 200     format( ,f7.2,2(12x,f7.2))
        RETURN

      end subroutine calc_zenith

      subroutine sundis( julday, esrm2 )
!-----------------------------------------------------------------------------
! purpose:
! calculate earth-sun distance variation for a given date. based on
! fourier coefficients originally from: spencer, j.w., 1971, fourier
! series representation of the position of the sun, search, 2:172
!-----------------------------------------------------------------------------
! parameters:
! idate - integer, specification of the date, from yymmdd (i)
! esrm2 - real(8), variation of the earth-sun distance (o)
! esrm2 = (average e/s dist)^2 / (e/s dist on day idate)^2
!-----------------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: julday
      real(8), intent(out) :: esrm2
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
      real(8), parameter :: pi = 3.1415926
      real(8) :: dayn, thet0
      real(8) :: sinth, costh, sin2th, cos2th
!-----------------------------------------------------------------------------
! ... parse date to find day number (julian day)
!-----------------------------------------------------------------------------
      dayn = dble(julday - 1) + .5
!-----------------------------------------------------------------------------
! ... define angular day number and compute esrm2:
!-----------------------------------------------------------------------------
      thet0 = 2.*pi*dayn/365.
!-----------------------------------------------------------------------------
! ... calculate sin(2*thet0), cos(2*thet0) from
! addition theorems for trig functions for better
! performance; the computation of sin2th, cos2th
! is about 5-6 times faster than the evaluation
! of the intrinsic functions sin and cos
!-----------------------------------------------------------------------------
      sinth = sin( thet0 )
      costh = cos( thet0 )
      sin2th = 2.*sinth*costh
      cos2th = costh*costh - sinth*sinth
      esrm2 = 1.000110 + .034221*costh + .001280*sinth + .000719*cos2th + .000077*sin2th
      end subroutine sundis

      subroutine spheres( nz, z, zen, dsdh, nid )
!-----------------------------------------------------------------------------
! purpose:
! calculate slant path over vertical depth ds/dh in spherical geometry.
! calculation is based on: a.dahlback, and k.stamnes, a new spheric model
! for computing the radiation field available for photolysis and heating
! at twilight, planet.space sci., v39, n5, pp. 671-683, 1991 (appendix b)
!-----------------------------------------------------------------------------
! parameters:
! nz - integer, number of specified altitude levels in the working (i)
! grid
! z - real(8), specified altitude working grid (km) (i)
! zen - real(8), solar zenith angle (degrees) (i)
! dsdh - real(8), slant path of direct beam through each layer crossed (o)
! when travelling from the top of the atmosphere to layer i;
! dsdh(i,j), i = 0..nz-1, j = 1..nz-1
! nid - integer, number of layers crossed by the direct beam when (o)
! travelling from the top of the atmosphere to layer i;
! nid(i), i = 0..nz-1
!-----------------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: nz
      real(8), intent(in) :: zen
      real(8), intent(in) :: z(nz)
      integer, intent(out) :: nid(0:nz-1)
      real(8), intent(out) :: dsdh(0:nz-1,nz-1)
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
      real(8), parameter :: radius = 6.371e+3 ! radius earth (km)
      real(8), parameter :: d2r = 3.1415926/180.0
      integer :: i, j, k
      integer :: id
      real(8) :: re, ze(nz)
      real(8) :: zd(0:nz-1)
      real(8) :: zenrad, rpsinz, rj, rjp1, dsj, dhj, ga, gb, sm
      zenrad = zen*d2r
!-----------------------------------------------------------------------------
! ... include the elevation above sea level to the radius of the earth:
!-----------------------------------------------------------------------------
      re = radius + z(1)
!-----------------------------------------------------------------------------
! ... correspondingly z changed to the elevation above earth surface:
!-----------------------------------------------------------------------------
      ze(1:nz) = z(1:nz) - z(1)
!-----------------------------------------------------------------------------
! ... inverse coordinate of z
!-----------------------------------------------------------------------------
      zd(0) = ze(nz)
      do k = 1,nz-1
        zd(k) = ze(nz - k)
      end do
!-----------------------------------------------------------------------------
! ... initialize dsdh(i,j), nid(i)
!-----------------------------------------------------------------------------
      dsdh(0:nz-1,1:nz-1) = 0.
      nid(0:nz-1) = 0
!-----------------------------------------------------------------------------
! ... calculate ds/dh of every layer
!-----------------------------------------------------------------------------
      do i = 0,nz-1
        rpsinz = (re + zd(i)) * sin(zenrad)
        if( zen > 90. .and. rpsinz < re ) then
           nid(i) = -1
        else
!-----------------------------------------------------------------------------
! ... find index of layer in which the screening height lies
!-----------------------------------------------------------------------------
           id = i
           if( zen > 90. ) then
              do j = 1,nz-1
                 if( (rpsinz < ( zd(j-1) + re ) ) .and. (rpsinz >= ( zd(j) + re )) ) then
                    id = j
                 end if
              end do
           end if
           do j = 1,id
             if( j == id .and. id == i .and. zen > 90.) then
                sm = -1.
             else
                sm = 1.
             end if
             rj = re + zd(j-1)
             rjp1 = re + zd(j)
             dhj = zd(j-1) - zd(j)
             ga = max( 0.d0,rj*rj - rpsinz*rpsinz )
             gb = max( 0.d0,rjp1*rjp1 - rpsinz*rpsinz )
             if( id > i .and. j == id ) then
                dsj = sqrt( ga )
             else
                dsj = sqrt( ga ) - sm*sqrt( gb )
             end if
             dsdh(i,j) = dsj / dhj
           end do
           nid(i) = id
        end if
      end do
      end subroutine spheres

      subroutine setz( nz, cz, tlev, c, ndx, adjcoe )

      use module_wave_data, only : tuv_jmax
!-----------------------------------------------------------------------------
! adjcoe - real(8), coross section adjust coefficients
!-----------------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: nz
      integer, intent(in) :: ndx
      real(8), intent(in) :: cz(nz)
      real(8), intent(in) :: tlev(nz)
      real(8), intent(in) :: c(5,tuv_jmax)
      real(8), intent(inout) :: adjcoe(nz,tuv_jmax)
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
      integer :: k, m
      real(8) :: tt, adjin
      real(8) :: c0, c1, c2
      real(8) :: xz(nz)
!-----------------------------------------------------------------------------
! 1 o2 + hv -> o + o
! 2 o3 -> o2 + o(1d)
! 3 o3 -> o2 + o(3p)
! 4 no2 -> no + o(3p)
! 5 no3 -> no + o2
! 6 no3 -> no2 + o(3p)
! 7 n2o5 -> no3 + no + o(3p)
! 8 n2o5 -> no3 + no2
! 9 n2o + hv -> n2 + o(1d)
! 10 ho2 + hv -> oh + o
! 11 h2o2 -> 2 oh
! 12 hno2 -> oh + no
! 13 hno3 -> oh + no2
! 14 hno4 -> ho2 + no2
! 15 ch2o -> h + hco
! 16 ch2o -> h2 + co
! 17 ch3cho -> ch3 + hco
! 18 ch3cho -> ch4 + co
! 19 ch3cho -> ch3co + h
! 20 c2h5cho -> c2h5 + hco
! 21 chocho -> products
! 22 ch3cocho -> products
! 23 ch3coch3
! 24 ch3ooh -> ch3o + oh
! 25 ch3ono2 -> ch3o+no2
! 26 pan + hv -> products
! 27 mvk + hv -> products
! 28 macr + hv -> products
!-----------------------------------------------------------------------------
      xz(1:nz) = cz(1:nz)*1.e-18
      do k = 1,tuv_jmax
         adjcoe(1:nz,k) = 1.
      end do
      tt = tlev(1)/281.
      do m = 1,tuv_jmax
         if( m /= 2 .or. m /= 11 ) then
            adjin = 1.
         else if( m == 2 ) then
!----------------------------------------------------------------------
! ... temperature modification
! t0.9 (1.05) t0.95(1.025) t1.0(1.0) t1.15(1.02) t1.1(1.04)
!----------------------------------------------------------------------
            select case( ndx )
               case( 20 )
                  c0 = 4.52372 ; c1 = -5.94317 ; c2 = 2.63156
               case( 40 )
                  c0 = 4.99378 ; c1 = -7.92752 ; c2 = 3.94715
               case( 60 )
                  c0 = .969867 ; c1 = -.841035 ; c2 = .878835
               case( 80 )
                  c0 = 1.07801 ; c1 = -2.39580 ; c2 = 2.32632
            end select
            adjin = c0 + tt*(c1 + c2*tt)
         else if( m == 11 ) then
!----------------------------------------------------------------------
! ... temperature modification
! t0.9 (1.05) t0.95(1.025) t1.0(1.0) t1.15(1.02) t1.1(1.04)
!----------------------------------------------------------------------
            select case( ndx )
               case( 20 )
                  c0 = 2.43360 ; c1 = -3.61363 ; c2 = 2.19018
               case( 40 )
                  c0 = 3.98265 ; c1 = -6.90516 ; c2 = 3.93602
               case( 60 )
                  c0 = 3.49843 ; c1 = -5.98839 ; c2 = 3.50262
               case( 80 )
                  c0 = 3.06312 ; c1 = -5.26281 ; c2 = 3.20980
            end select
            adjin = c0 + tt*(c1 + c2*tt)
         end if
         call calcoe( nz, c(1,m), xz, tt, adjin, adjcoe(1,m) )
      end do
      end subroutine setz


      subroutine setozo( nz, z, tlay, dto3, &
                         to3, o3, airlev, o3top, o3toms )
      use module_wave_data, only : nw, wl, xso3, s226, s263, s298
!-----------------------------------------------------------------------------
! purpose:
! set up an altitude profile of ozone, and corresponding absorption
! optical depths. subroutine includes a shape-conserving scaling method
! that allows scaling of the entire profile to a given overhead ozone
! column amount.
!-----------------------------------------------------------------------------
! parameters:
! dobnew - real(8), overhead ozone column amount (du) to which profile (i)
! should be scaled. if dobnew < 0, no scaling is done
! nz - integer, number of specified altitude levels in the working (i)
! grid
! z - real(8), specified altitude working grid (km)
! nw - integer, number of specified intervals + 1 in working (i)
! wavelength grid
! wl - real(8), vector of lower limits of wavelength intervals in (i)
! working wavelength grid
! xso3 - real(8), molecular absoprtion cross section (cm^2) of o3 at (i)
! each specified wavelength (wmo value at 273)
! s226 - real(8), molecular absoprtion cross section (cm^2) of o3 at (i)
! each specified wavelength (value from molina and molina at 226k)
! s263 - real(8), molecular absoprtion cross section (cm^2) of o3 at (i)
! each specified wavelength (value from molina and molina at 263k)
! s298 - real(8), molecular absoprtion cross section (cm^2) of o3 at (i)
! each specified wavelength (value from molina and molina at 298k)
! tlay - real(8), temperature (k) at each specified altitude layer (i)
! dto3 - real(8), optical depth due to ozone absorption at each (o)
! specified altitude at each specified wavelength
! to3 - real(8), totol ozone column (o)
!-----------------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: nz
      real(8), intent(in) :: o3top
      real(8), intent(in) :: o3toms
      real(8), intent(in) :: z(nz)
      real(8), intent(in) :: tlay(nz-1)
      real(8), intent(in) :: airlev(nz)
      real(8), intent(out) :: dto3(nz-1,nw-1)
      real(8), intent(out) :: to3(nz)
      real(8), intent(in) :: o3(nz)
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
      integer :: i, iw, nd, ilat
      real(8) :: so3
      real(8) :: scale
      real(8) :: cz(nz), o3den(nz)
!-----------------------------------------------------------------------------
! ... compute column increments
!-----------------------------------------------------------------------------
!     o3(1:nz) = o3(1:nz)*airlev(1:nz)
!     cz(1:nz-1) = (o3(2:nz)) * 1.e5 * (z(2:nz) - z(1:nz-1))

      o3den(1:nz) = o3(1:nz)*airlev(1:nz)
      cz(1:nz-1) = 0.5*(o3den(2:nz) + o3den(1:nz-1))*1.e5*(z(2:nz) - z(1:nz-1))
!-----------------------------------------------------------------------------
! ... include exponential tail integral from infinity to 50 km,
! fold tail integral into top layer
! specify scale height near top of data.
!-----------------------------------------------------------------------------
      cz(nz-1) = cz(nz-1) + o3top

      to3(nz) = o3top
      to3(nz-1) = cz(nz-1)
      do i = nz-2,1,-1
         to3(i) = to3(i+1) + cz(i)
      end do

      if( o3toms <= 0.0 ) then
        scale = 1.0
      else
        scale = o3toms/(to3(1)/2.687e16)
      endif
!-----------------------------------------------------------------------------
! ... scale o3 using toms data
!-----------------------------------------------------------------------------
      cz(1:nz-1) = cz(1:nz-1)*scale
      to3(1:nz) = to3(1:nz)*scale
!-----------------------------------------------------------------------------
! ... calculate ozone optical depth for each layer, with temperature
! correction. output, dto3(kz,kw)
!-----------------------------------------------------------------------------
      do iw = 1,nw-1
         so3 = xso3(iw)
         do i = 1,nz-1
            if( wl(iw) > 240.5 .and. wl(iw+1) < 350. ) then
               if( tlay(i) < 263.) then
                  so3 = s226(iw) + (s263(iw) - s226(iw)) / (263. - 226.) * (tlay(i) - 226.)
               else
                  so3 = s263(iw) + (s298(iw) - s263(iw)) / (298. - 263.) * (tlay(i) - 263.)
               end if
            end if
            dto3(i,iw) = cz(i)*so3
         end do
      end do
      end subroutine setozo

      subroutine set_o2_xsect( nz, z, cz, vcol, scol, dto2, xso2 )

      use module_wave_data, only : nw, wl
!-----------------------------------------------------------------------------
! purpose:
! compute equivalent optical depths for o2 absorption, parameterized in
! the sr bands and the lyman-alpha line.
!-----------------------------------------------------------------------------
! parameters:
! nz - integer, number of specified altitude levels in the working (i)
! grid
! z - real(8), specified altitude working grid (km)
! nw - integer, number of specified intervals + 1 in working
! wavelength grid
! wl - real(8), vector of lower limits of wavelength intervals in
! working wavelength grid
! cz - real(8), number of air molecules per cm^2 at each specified
! altitude layer
! zen - real(8), solar zenith angle
! dto2 - real(8), optical depth due to o2 absorption at each specified (o)
! vertical layer at each specified wavelength
! xso2 - real(8), molecular absorption cross section in sr bands at (o)
! each specified altitude and wavelength. includes herzberg
! continuum.
!-----------------------------------------------------------------------------
! edit history:
! 02/98 included lyman-alpha parameterization
! 03/97 fix dto2 problem at top level (nz)
! 02/97 changed offset for grid-end interpolation to relative number
! (x * (1 +- deltax))
! 08/96 modified for early exit, no redundant read of data and smaller
! internal grid if possible; internal grid uses user grid points
! whenever possible
! 07/96 modified to work on internal grid and interpolate final values
! onto the user-defined grid
!-----------------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: nz
      real(8), intent(in) :: cz(nz)
      real(8), intent(in) :: z(nz)
      real(8), intent(in) :: vcol(nz)
      real(8), intent(in) :: scol(nz)
      real(8), intent(out) :: dto2(nz-1,nw-1)
      real(8), intent(out) :: xso2(nw-1,nz)
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
      integer :: iw, iz, igast
      real(8) :: secchi(nz)
!-----------------------------------------------------------------------------
! ... o2 optical depth and equivalent cross section on kockarts grid
!-----------------------------------------------------------------------------
      real(8) :: dto2k(nz-1,ngast-1)
      real(8) :: xso2k(nz,ngast-1)
!-----------------------------------------------------------------------------
! ... o2 optical depth and equivalent cross section in the lyman-alpha region
!-----------------------------------------------------------------------------
      real(8) :: dto2la(nz-1,nla-1)
      real(8) :: xso2la(nz,nla-1)
!-----------------------------------------------------------------------------
! ... temporary one-dimensional storage for optical depth and cross section values
! xxtmp - on internal grid
! xxuser - on user defined grid
!-----------------------------------------------------------------------------
      real(8), dimension(2*nwint) :: dttmp, xstmp    !	CHANGED by Guohui
      real(8), dimension(nwint) :: dtuser, xsuser    !	CHANGED by Guohui
      real(8) :: o2col(nz)
      real(8) :: x, y
      real(8) :: delo2
!-----------------------------------------------------------------------------
! ... check, whether user grid is in the o2 absorption band at all...
! if not, set cross section and optical depth values to zero and return
!-----------------------------------------------------------------------------
      dto2(:nz-1,:nw-1) = 0.
      xso2(:nw-1,:nz) = 0.
      if( wl(1) > 243. ) then
         return
      end if
!-----------------------------------------------------------------------------
! ... sec xhi or chapman calculation
! for zen > 95 degrees, use zen = 95. (this is only to compute effective o2
! cross sections. still, better than setting dto2 = 0. as was done up to
! version 4.0) sm 1/2000
! in future could replace with mu2(iz) (but mu2 is also wavelength-depenedent)
! or imporved chapman function
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
! ... slant o2 column
!-----------------------------------------------------------------------------
      o2col(1:nz) = 0.2095 * scol(1:nz)
!-----------------------------------------------------------------------------
! ... effective secant of solar zenith angle. use 2.0 if no direct sun.
! for nz, use value at nz-1
!-----------------------------------------------------------------------------
      secchi(1:nz-1) = scol(1:nz-1)/vcol(1:nz-1)
      where( secchi(1:nz-1) == 0. )
         secchi(1:nz-1) = 2.
      endwhere
      secchi(nz) = secchi(nz-1)
!-----------------------------------------------------------------------------
! ... if necessary:
! kockarts parameterization of the sr bands, output values of o2
! optical depth and o2 equivalent cross section are on his grid
!-----------------------------------------------------------------------------
      if( wl(1) < wlgast(ngast) .and. wl(nw) > wlgast(1) ) then
           call schu( nz, o2col, secchi, dto2k, xso2k )
      else
         dto2k(:,:) = 0.
         xso2k(:,:) = 0.
      end if
!-----------------------------------------------------------------------------
! ... lyman-alpha parameterization, output values of o2 opticaldepth
! and o2 effective (equivalent) cross section
!-----------------------------------------------------------------------------
      if( wl(1) <= wlla(nla) .and. wl(nw) >= wlla(1) ) then
         call lymana( nz, o2col, secchi, dto2la, xso2la )
      else
         dto2la(:,:) = 0.
         xso2la(:,:) = 0.
      end if
!-----------------------------------------------------------------------------
! ... loop through the altitude levels
!-----------------------------------------------------------------------------
      do iz = 1,nz
         igast = 0
!-----------------------------------------------------------------------------
! ... loop through the internal wavelength grid
!-----------------------------------------------------------------------------
         do iw = 1,nwint-1
!-----------------------------------------------------------------------------
! ... if outside kockarts grid and outside lyman-alpha, use the
! jpl/brasseur+solomon data, if inside
! kockarts grid, use the parameterized values from the call to schu,
! if inside lyman-alpha, use the paraemterized values from call to lymana
!-----------------------------------------------------------------------------
            if( wlint(iw+1) <= wlgast(1) .or. wlint(iw) >= wlgast(ngast) ) then
              if( wlint(iw+1) <= wlla(1) .or. wlint(iw) >= wlla(nla) ) then
                 xstmp(iw) = xso2int(iw)
              else
                 xstmp(iw) = xso2la(iz,1)
              end if
            else
               igast = igast + 1
               xstmp(iw) = xso2k(iz,igast)
            end if
!-----------------------------------------------------------------------------
! ... compute the area in each bin (for correct interpolation purposes only!)
!-----------------------------------------------------------------------------
            xstmp(iw) = xstmp(iw) * (wlint(iw+1) - wlint(iw))
         end do
!-----------------------------------------------------------------------------
! ... interpolate o2 cross section from the internal grid onto the user grid
!-----------------------------------------------------------------------------
         call inter3( nw, wl, xsuser, nwint, wlint, xstmp )
         xso2(1:nw-1,iz) = xsuser(1:nw-1)/(wl(2:nw) - wl(1:nw-1))
      end do
      do iz = 1,nz-1
         igast = 0
         delo2 = .2095 * cz(iz) ! vertical o2 column
!-----------------------------------------------------------------------------
! ... loop through the internal wavelength grid
!-----------------------------------------------------------------------------
         do iw = 1,nwint-1
!-----------------------------------------------------------------------------
! ... if outside kockarts grid and outside lyman-alpha, use the
! jpl/brasseur+solomon data, if inside
! kockarts grid, use the parameterized values from the call to schu,
! if inside lyman-alpha, use the paraemterized values from call to lymana
!-----------------------------------------------------------------------------
            if( wlint(iw+1) <= wlgast(1) .or. wlint(iw) >= wlgast(ngast) ) then
              if( wlint(iw+1) <= wlla(1) .or. wlint(iw) >= wlla(nla) ) then
                 dttmp(iw) = xso2int(iw) * delo2
              else
                 dttmp(iw) = dto2la(iz,1)
              end if
            else
               igast = igast + 1
               dttmp(iw) = dto2k(iz,igast)
            end if
!-----------------------------------------------------------------------------
! ... compute the area in each bin (for correct interpolation purposes only!)
!-----------------------------------------------------------------------------
            dttmp(iw) = dttmp(iw) * (wlint(iw+1) - wlint(iw))
         end do
!-----------------------------------------------------------------------------
! ... interpolate o2 optical depth from the internal grid onto the user grid
!-----------------------------------------------------------------------------
         call inter3( nw, wl, dtuser, nwint, wlint, dttmp )
         dto2(iz,1:nw-1) = dtuser(1:nw-1)/(wl(2:nw) - wl(1:nw-1))
      end do
      end subroutine set_o2_xsect

      subroutine setcld( nz, z, xlwc, dtcld, omcld, gcld )
      use module_wave_data, only : nw
!-----------------------------------------------------------------------------
!= PURPOSE:
!= Set up cloud optical depth, single albedo and g
!-----------------------------------------------------------------------------
!= PARAMETERS:
!= PARAMETERS:
!= NZ - INTEGER, number of specified altitude levels in the working (I)
!= grid
!= Z - real(8), specified altitude working grid (km) (I)
!= XLWC Cloud water content g/M3 (I)
!=
!= dtcld - cloud optical depth
!= omcld - cloud droplet single albedo
!= gcld  - g
!-----------------------------------------------------------------------------
!
! VERTICAL DOMAIN is from bottom(1) to TOP (TOP=nz)
! CCM from top(1) to bottom(nz)
!-----------------------------------------------------------------------------
!****************************************************************************
      implicit none
!-----------------------------------------------------------------------------
! ... Dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: nz
      real(8), intent(in) :: z(nz)
      real(8), intent(in) :: xlwc(nz)
      real(8), intent(out) :: dtcld(nz-1,nw-1)
      real(8), intent(out) :: omcld(nz-1,nw-1)
      real(8), intent(out) :: gcld(nz-1,nw-1)
!-----------------------------------------------------------------------------
! ... Local variables
!-----------------------------------------------------------------------------
      real(8), parameter :: wden = 1.0 * 1.e6 ! g/m3 (1 m3 water = 1e6 g water)
      real(8), parameter :: re = 10.0 * 1.e-6 ! assuming cloud drop radius = 10 um to M
      integer :: i  ! vertical index for each blk
      real(8) :: dz(nz-1)
!-----------------------------------------------------------------------------
! ... calculate vertical interveral dz
!-----------------------------------------------------------------------------
      do i=1,nz-1
          dz(i) = (z(i+1)-z(i)) * 1.e3 ! dz in Meters
      end do
!----------------------------------------------------
! ....calculate cloud optical depth T
! following Liao et al. JGR, 104, 23697, 1999
!----------------------------------------------------
      do i = 1,nz-1
        dtcld(i,:) = 1.5 * xlwc(i)*dz(i)/ (wden * re)
        dtcld(i,:) = max(dtcld(i,:), 0.d0)
        omcld(i,:) = .9999
        gcld (i,:) = .85
      end do

      end subroutine setcld

      subroutine schu_inti
!-----------------------------------------------------------------------------
! ... initialize the tables
!-----------------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
      integer :: i, iw, k, j1, jp1, j
      real(8) :: col
      real(8), dimension(6) :: a0, a1, b0, b1
      do iw = 1,ngast-1
         x_table(0,iw) = sum( a_schu(1:11:2,iw) )
         d_table(0,iw) = sum( b_schu(1:11:2,iw) )
         do k = 1,tdim
            col = 22. + t_del*dble(k-1)
            o2_table(k) = col
            col = 10.**col
            a1(:) = a_schu(2:12:2,iw)*col
            b1(:) = b_schu(2:12:2,iw)*col
            where( a1(:) < 500. )
               a0(:) = exp( -a1(:) )
            elsewhere
               a0(:) = 0.
            endwhere
            where( b1(:) < 500. )
               b0(:) = exp( -b1(:) )
            elsewhere
               b0(:) = 0.
            endwhere
            x_table(k,iw) = dot_product( a_schu(1:11:2,iw),a0(:) )
            d_table(k,iw) = dot_product( b_schu(1:11:2,iw),b0(:) )
         end do
      end do
      end subroutine schu_inti
      subroutine schu( nz, o2col, secchi, dto2, xscho2 )
!-----------------------------------------------------------------------------
! purpose:
! calculate the equivalent absorption cross section of o2 in the sr bands.
! the algorithm is based on: g.kockarts, penetration of solar radiation
! in the schumann-runge bands of molecular oxygen: a robust approximation,
! annales geophysicae, v12, n12, pp. 1207ff, dec 1994. calculation is
! done on the wavelength grid used by kockarts (1994). final values do
! include effects from the herzberg continuum.
!-----------------------------------------------------------------------------
! parameters:
! nz - integer, number of specified altitude levels in the working (i)
! grid
! o2col - real(8), slant overhead o2 column (molec/cc) at each specified (i)
! altitude
! dto2 - real(8), optical depth due to o2 absorption at each specified (o)
! vertical layer at each specified wavelength
! xscho2 - real(8), molecular absorption cross section in sr bands at (o)
! each specified wavelength. includes herzberg continuum
!-----------------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: nz
      real(8), intent(inout) :: dto2(nz-1,ngast-1)
      real(8), intent(inout) :: xscho2(nz,ngast-1)
      real(8), intent(in) :: o2col(nz)
      real(8), intent(in) :: secchi(nz)
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
      integer :: i, iw, j, j1, jp1, k, ki, kp1
      integer :: index(nz)
      integer :: minind(1), maxind(1)
      real(8) :: a0, a1, b0, b1
      real(8), dimension(6) :: ac, bc
      real(8), dimension(nz,6) :: aa, bb
      real(8), dimension(nz) :: rjm, rjo2
      real(8), dimension(nz) :: rjmi, rjo2i, lo2col, dels
!-----------------------------------------------------------------------------
! ... initialize r(m)
!-----------------------------------------------------------------------------
      rjm(1:nz) = 0.
      rjo2(1:nz) = 0.
!-----------------------------------------------------------------------------
! ... initialize the table interpolation
!-----------------------------------------------------------------------------
      where( o2col(:) /= 0 )
         lo2col(:) = log10( o2col(:) )
      endwhere
      do ki = 1,nz
         if( o2col(ki) /= 0. ) then
            if( lo2col(ki) <= o2_table(1) ) then
               dels(ki) = 0.
               index(ki) = 1
            else if( lo2col(ki) >= o2_table(tdim) ) then
               dels(ki) = 1.
               index(ki) = tdim-1
            else
               do k = 2,tdim
                  if( lo2col(ki) <= o2_table(k) ) then
                     dels(ki) = t_fac*(lo2col(ki) - o2_table(k-1))
                     index(ki) = k-1
                     exit
                  end if
               end do
            end if
         else
            index(ki) = 0
            dels(ki) = 0.
         end if
      end do
!-----------------------------------------------------------------------------
! ... calculate sum of exponentials (eqs 7 and 8 of kockarts 1994)
!-----------------------------------------------------------------------------
      do iw = 1,ngast-1
         do k = 1,nz
            ki = index(k)
            rjm(k) = x_table(ki,iw) + dels(k)*(x_table(ki+1,iw) - x_table(ki,iw))
            rjo2(k) = d_table(ki,iw) + dels(k)*(d_table(ki+1,iw) - d_table(ki,iw))
         end do
         do k = 1,nz-1
            if( rjm(k) > 1.e-100 ) then
               kp1 = k + 1
               if( rjm(kp1) > 0. ) then
                  dto2(k,iw) = log( rjm(kp1) ) / secchi(kp1) - log( rjm(k) ) * secchi(k)
               else
                  dto2(k,iw) = 1000.
               end if
            else
               dto2(k,iw) = 1000.
            end if
         end do
         do k = 1,nz
            if( rjm(k) > 1.e-100 ) then
               if( rjo2(k) > 1.e-100 ) then
                  xscho2(k,iw) = rjo2(k)/rjm(k)
               else
                  xscho2(k,iw) = 0.
               end if
            else
               xscho2(k,iw) = 0.
            end if
         end do
      end do
      end subroutine schu



      subroutine rtlink( nz,                  &
                         nw,                  &
                         zen,                 &
                         albedo,              & 
                         z,                   &
                         nid,                 &
                         dsdh,                & 
                         dtrl,                &
                         dto3,                &
                         dto2,                &
                         dtcld, omcld, gcld,  &
                         dtcb1, omcb1, gcb1,  &
                         dtcb2, omcb2, gcb2,  &
                         dtoc1, omoc1, goc1,  &
                         dtoc2, omoc2, goc2,  &
                         dtant, omant, gant,  &
                         dtso4, omso4, gso4,  &
                         dtsal, omsal, gsal,  &
                         radfld )              

      implicit none
!-----------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------
      integer, intent(in) :: nz
      integer, intent(in) :: nw
      real(8), intent(in)    :: zen
      real(8), intent(in)    :: albedo(nw-1)
      real(8), intent(in)    :: z(nz)
      integer, intent(in) :: nid(0:nz-1)
      real(8), intent(in)    :: dsdh(0:nz-1,nz-1)
      real(8), intent(in)    :: dtrl(nz-1,nw-1)
      real(8), intent(in)    :: dto3(nz-1,nw-1)
      real(8), intent(in)    :: dto2(nz-1,nw-1)
      real(8), intent(in)    :: dtcld(nz-1,nw-1), omcld(nz-1,nw-1), gcld(nz-1,nw-1)
      real(8), intent(in)    :: dtcb1(nz-1,nw-1), omcb1(nz-1,nw-1), gcb1(nz-1,nw-1)
      real(8), intent(in)    :: dtcb2(nz-1,nw-1), omcb2(nz-1,nw-1), gcb2(nz-1,nw-1)
      real(8), intent(in)    :: dtoc1(nz-1,nw-1), omoc1(nz-1,nw-1), goc1(nz-1,nw-1)
      real(8), intent(in)    :: dtoc2(nz-1,nw-1), omoc2(nz-1,nw-1), goc2(nz-1,nw-1)
      real(8), intent(in)    :: dtant(nz-1,nw-1), omant(nz-1,nw-1), gant(nz-1,nw-1)
      real(8), intent(in)    :: dtso4(nz-1,nw-1), omso4(nz-1,nw-1), gso4(nz-1,nw-1)
      real(8), intent(in)    :: dtsal(nz-1,nw-1), omsal(nz-1,nw-1), gsal(nz-1,nw-1)
      real(8), intent(out)   :: radfld(nz,nw-1)           
!-----------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------
      real(8), parameter :: smallest = tiny( 1.0 )
      integer :: i, ii, iw
      real(8) :: dtsct, dtabs
      real(8) :: dscld, dacld
      real(8) :: dscb1, dacb1
      real(8) :: dscb2, dacb2
      real(8) :: dsoc1, daoc1
      real(8) :: dsoc2, daoc2
      real(8) :: dsant, daant
      real(8) :: dsso4, daso4
      real(8) :: dssal, dasal
      real(8), dimension(nz-1,nw-1) :: dt, om, g
!-----------------------------------------------------------------------
! ... set any coefficients specific to rt scheme
!-----------------------------------------------------------------------
      do iw = 1,nw-1
         do i = 1,nz-1

            dscld = dtcld(i,iw)*omcld(i,iw)
            dacld = dtcld(i,iw)*(1. - omcld(i,iw))

            dscb1 = dtcb1(i,iw)*omcb1(i,iw)
            dacb1 = dtcb1(i,iw)*(1. - omcb1(i,iw))

            dscb2 = dtcb2(i,iw)*omcb2(i,iw)
            dacb2 = dtcb2(i,iw)*(1. - omcb2(i,iw))

            dsoc1 = dtoc1(i,iw)*omoc1(i,iw)
            daoc1 = dtoc1(i,iw)*(1. - omoc1(i,iw))

            dsoc2 = dtoc2(i,iw)*omoc2(i,iw)
            daoc2 = dtoc2(i,iw)*(1. - omoc2(i,iw))

            dsant = dtant(i,iw)*omant(i,iw)
            daant = dtant(i,iw)*(1. - omant(i,iw))

            dsso4 = dtso4(i,iw)*omso4(i,iw)
            daso4 = dtso4(i,iw)*(1. - omso4(i,iw))

            dssal = dtsal(i,iw)*omsal(i,iw)
            dasal = dtsal(i,iw)*(1. - omsal(i,iw))

            dtsct = dtrl(i,iw) + dscld +                                   &
                    dscb1 + dscb2 + dsoc1 + dsoc2 + dsant + dsso4 + dssal
            dtabs = dto3(i,iw) + dto2(i,iw) + dacld +                      &
                    dacb1 + dacb2 + daoc1 + daoc2 + daant + daso4 + dasal

            dtabs = max( dtabs,smallest )
            dtsct = max( dtsct,smallest )
!-----------------------------------------------------------------------
! ... invert z-coordinate
!-----------------------------------------------------------------------
            ii = nz - i

            dt(ii,iw) = dtsct + dtabs
            if( dtsct /= smallest ) then
               om(ii,iw) = dtsct/(dtsct + dtabs)
               g(ii,iw) = ( gcld(i,iw)*dscld + &
                            gcb1(i,iw)*dscb1 + &
                            gcb2(i,iw)*dscb2 + &
                            goc1(i,iw)*dsoc1 + &
                            goc2(i,iw)*dsoc2 + &
                            gant(i,iw)*dsant + &
                            gso4(i,iw)*dsso4 + &
                            gsal(i,iw)*dssal ) / dtsct
               g(ii,iw) = max( smallest, g(ii,iw) )
               g(ii,iw) = min( 1.d0, g(ii,iw) )
            else
               om(ii,iw) = smallest
               g(ii,iw)  = smallest
            end if
         end do
      end do
!-----------------------------------------------------------------------
! ... call rt routine
!-----------------------------------------------------------------------
      call ps2str( nz, nw, zen, albedo, dt, om, &
                   g, dsdh, nid, radfld )

      end subroutine rtlink


      subroutine tridec( syscnt, order, lower, main, upper )
!--------------------------------------------------------------------
! dimension of lower(syscnt,order), main(syscnt,order), upper(syscnt,order)
! arguments
!
! latest revision june 1995
!
! purpose trdi computes the solution of the tridiagonal
! linear system,
! b(1)*x(1)+c(1)*x(2) = y(1)
! a(i)*x(i-1)+b(i)*x(i)+c(i)*x(i+1) = y(i)
! i=2,3,...,n-1
! a(n)*x(n-1)+b(n)*x(n) = y(n)
!
! usage call trdi (n, a, b, c, x )
!
! arguments
!
! on input n
! the number of unknowns. this subroutine
! requires that n be greater than 2.
!
! a
! the subdiagonal of the matrix is stored in
! locations a(2) through a(n).
!
! b
! the diagonal of the matrix is stored in
! locations b(1) through b(n).
!
! c
! the super-diagonal of the matrix is stored in
! locations c(1) through c(n-1).
!
! x
! the right-hand side of the equations is
! stored in y(1) through y(n).
!
! on output x
! an array which contains the solution to the
! system of equations.
!
! special conditions this subroutine executes satisfactorily
! if the input matrix is diagonally dominant
! and non-singular. the diagonal elements
! are used to pivot, and no tests are made to
! determine singularity. if a singular, or
! nearly singular, matrix is used as input,
! a divide by zero or floating point overflow
! may result.
!
! precision compiler dependent
!
! language fortran
!
! history originally written by nancy werner at ncar
! in october, 1973. modified by s. walters at
! ncar in june 1989 to functionally replace
! the cal routine tridla.
!
! portability fortran 90
!
! algorithm an lu-decomposition is obtained using the
! algorithm described in the reference below.
!
! to avoid unnecessary divisions, the alpha
! values used in the routine are the
! reciprocals of the alpha values described
! in the reference below.
!
! accuracy every component of the residual of the linear
! system (i.e. the difference between y and
! the matrix applied to x) should be less in
! magnitude than ten times the machine precision
! times the matrix order times the maximum
! absolute component of the solution vector
! times the largest absolute row sum of the
! input matrix.
!
! timing the timing is roughly proportional to the
! order n of the linear system.
!
! references analysis of numerical methods by
! e. isaacson and h. keller
! (john wiley and sons, 1966) pp. 55-58.
!--------------------------------------------------------------------
      implicit none
!--------------------------------------------------------------------
! ... dummy args
!--------------------------------------------------------------------
      integer, intent(in) :: syscnt, order
      real(8), intent(in), dimension(syscnt,order) :: lower
      real(8), intent(inout), dimension(syscnt,order) :: main, upper
!--------------------------------------------------------------------
! ... local variables
!--------------------------------------------------------------------
      integer :: i
!----------------------------------------------------------------------
! ... lu-decomposition
!----------------------------------------------------------------------
      main(:,1) = 1. / main(:,1)
      upper(:,1) = upper(:,1)*main(:,1)
      do i = 2,order-1
         main(:,i) = 1. / (main(:,i) - lower(:,i)*upper(:,i-1))
         upper(:,i) = upper(:,i)*main(:,i)
      end do
      end subroutine tridec
      subroutine trislv( syscnt, order, lower, main, upper, x )
      implicit none
!----------------------------------------------------------------------
! ... dummy args
!----------------------------------------------------------------------
      integer, intent(in) :: syscnt, order
      real(8), intent(in), dimension(syscnt,order) :: lower, &
                                                       main, &
                                                       upper
      real(8), intent(inout), dimension(syscnt,order) :: x
!----------------------------------------------------------------------
! ... local variables
!----------------------------------------------------------------------
      integer :: i, im1, j, n, nm1
      nm1 = order - 1
      n = order
!----------------------------------------------------------------------
! ... solve the system
!----------------------------------------------------------------------
      x(:,1) = x(:,1)*main(:,1)
      do i = 2,nm1
         x(:,i) = (x(:,i) - lower(:,i)*x(:,i-1))*main(:,i)
      end do
      x(:,n) = (x(:,n) - lower(:,n)*x(:,nm1))/(main(:,n) - lower(:,n)*upper(:,nm1))
      do i = nm1,1,-1
         x(:,i) = x(:,i) - upper(:,i)*x(:,i+1)
      end do
      end subroutine trislv

      subroutine ps2str( nz, nw, zen, rsfc, tauu, omu, &
                         gu, dsdh, nid, radfld )
!-----------------------------------------------------------------------------
! purpose:
! solve two-stream equations for multiple layers. the subroutine is based
! on equations from: toon et al., j.geophys.res., v94 (d13), nov 20, 1989.
! it contains 9 two-stream methods to choose from. a pseudo-spherical
! correction has also been added.
!-----------------------------------------------------------------------------
! parameters:
! nz - integer, number of specified altitude levels in the working (i)
! grid
! zen - real(8), solar zenith angle (degrees) (i)
! rsfc - real(8), surface albedo at current wavelength (i)
! tauu - real(8), unscaled optical depth of each layer (i)
! omu - real(8), unscaled single scattering albedo of each layer (i)
! gu - real(8), unscaled asymmetry parameter of each layer (i)
! dsdh - real(8), slant path of direct beam through each layer crossed (i)
! when travelling from the top of the atmosphere to layer i;
! dsdh(i,j), i = 0..nz-1, j = 1..nz-1
! nid - integer, number of layers crossed by the direct beam when (i)
! travelling from the top of the atmosphere to layer i;
! nid(i), i = 0..nz-1
! delta - logical, switch to use delta-scaling (i)
! .true. -> apply delta-scaling
! .false.-> do not apply delta-scaling
! fdr - real(8), contribution of the direct component to the total (o)
! actinic flux at each altitude level
! fup - real(8), contribution of the diffuse upwelling component to (o)
! the total actinic flux at each altitude level
! fdn - real(8), contribution of the diffuse downwelling component to (o)
! the total actinic flux at each altitude level
! edr - real(8), contribution of the direct component to the total (o)
! spectral irradiance at each altitude level
! eup - real(8), contribution of the diffuse upwelling component to (o)
! the total spectral irradiance at each altitude level
! edn - real(8), contribution of the diffuse downwelling component to (o)
! the total spectral irradiance at each altitude level
!-----------------------------------------------------------------------------

      implicit none
!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: nz, nw
      integer, intent(in) :: nid(0:nz-1)
      real(8), intent(in) :: zen
      real(8), intent(in) :: rsfc(nw-1)
      real(8), dimension(nz-1,nw-1), intent(in) :: tauu, omu, gu
      real(8), intent(in) :: dsdh(0:nz-1,nz-1)
      real(8), intent(out) :: radfld(nz,nw-1)
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
! ... mu = cosine of solar zenith angle
! rsfc = surface albedo
! tauu = unscaled optical depth of each layer
! omu = unscaled single scattering albedo
! gu = unscaled asymmetry factor
! klev = max dimension of number of layers in atmosphere
! nlayer = number of layers in the atmosphere
! nlevel = nlayer + 1 = number of levels
!-----------------------------------------------------------------------------
      real(8), parameter :: smallest = tiny( 1.0 )
      real(8), parameter :: largest  = huge( 1.0 )
      real(8), parameter :: d2r = 3.1415926/180.0
      integer :: mrows, nzm1, nzm2
      real(8), parameter :: eps = 1.e-3
      real(8), parameter :: pifs = 1.
      real(8), parameter :: fdn0 = 0.
      integer :: row
      integer :: lev
      integer :: i, ip1, iw
      integer :: j, jl, ju
      real(8) :: precis, wrk
      real(8) :: tempg
      real(8) :: mu, suma
      real(8) :: g, om
      real(8) :: gam1, gam2, gam3, gam4
      real(8), dimension(nz-1) :: f, gi, omi
      real(8), dimension(0:nz) :: tauc, mu2
      real(8), dimension(nz-1) :: lam, taun, bgam
      real(8), dimension(nz-1) :: cdn
      real(8), dimension(0:nz,nw-1) :: tausla
      real(8), dimension(nz-1,nw-1) :: cup, cuptn, cdntn
      real(8), dimension(nz-1,nw-1) :: e1, e2, e3, e4
      real(8), dimension(2*(nz-1)) :: a, b, d, e
      real(8), dimension(nw-1,2*(nz-1)) :: sub, main, super, y
!-----------------------------------------------------------------------------
! ... for calculations of associated legendre polynomials for gama1,2,3,4
! in delta-function, modified quadrature, hemispheric constant,
! hybrid modified eddington-delta function metods, p633,table1.
! w.e.meador and w.r.weaver, gas,1980,v37,p.630
! w.j.wiscombe and g.w. grams, gas,1976,v33,p2440,
! uncomment the following two lines and the appropriate statements
! further down.
!-----------------------------------------------------------------------------
      real(8) :: expon, expon0, expon1, divisr, temp, up, dn
      real(8) :: ssfc
!-----------------------------------------------------------------------------
! ... initial conditions: pi*solar flux = 1; diffuse incidence = 0
!-----------------------------------------------------------------------------
      nzm1 = nz - 1
      nzm2 = nz - 2
      mrows = 2*nzm1
      precis = epsilon( precis )
      mu = cos( zen*d2r )
wave_loop : &
      do iw = 1,nw-1
!-----------------------------------------------------------------------------
! ... compute coefficients for each layer:
! gam1 - gam4 = 2-stream coefficients, different for different approximations
! expon0 = calculation of e when tau is zero
! expon1 = calculation of e when tau is taun
! cup and cdn = calculation when tau is zero
! cuptn and cdntn = calc. when tau is taun
! divisr = prevents division by zero
!-----------------------------------------------------------------------------
         tauc(0:nz) = 0.
         tausla(0:nz,iw) = 0.
         mu2(0:nz) = sqrt( smallest )
!-----------------------------------------------------------------------------
! ... delta-scaling. have to be done for delta-eddington approximation,
! delta discrete ordinate, practical improved flux method, delta function,
! and hybrid modified eddington-delta function methods approximations
!-----------------------------------------------------------------------------
         f(1:nzm1) = gu(:,iw)*gu(:,iw)
         gi(1:nzm1) = (gu(:,iw) - f(1:nzm1))/(1. - f(1:nzm1))
         omi(1:nzm1) = (1. - f(1:nzm1))*omu(1:nzm1,iw)/(1. - omu(1:nzm1,iw)*f(1:nzm1))
         taun(1:nzm1) = (1. - omu(1:nzm1,iw)*f(1:nzm1))*tauu(1:nzm1,iw)
!-----------------------------------------------------------------------------
! ... calculate slant optical depth at the top of the atmosphere when zen>90.
! in this case, higher altitude of the top layer is recommended which can
! be easily changed in gridz.f.
!-----------------------------------------------------------------------------
         if( zen > 90. ) then
            if( nid(0) < 0 ) then
               tausla(0,iw) = largest
            else
               ju = nid(0)
               tausla(0,iw) = 2.*dot_product( taun(1:ju),dsdh(0,1:ju) )
            end if
         end if
level_loop : &
         do i = 1,nzm1
            g = gi(i)
            om = omi(i)
            tauc(i) = tauc(i-1) + taun(i)
!-----------------------------------------------------------------------------
! ... stay away from 1 by precision. for g, also stay away from -1
!-----------------------------------------------------------------------------
            tempg = min( abs(g),1. - precis )
            g = sign( tempg,g )
            om = min( om,1.-precis )
!-----------------------------------------------------------------------------
! ... calculate slant optical depth
!-----------------------------------------------------------------------------
            if( nid(i) < 0 ) then
               tausla(i,iw) = largest
            else
               ju = min( nid(i),i )
               suma = dot_product( taun(1:ju),dsdh(i,1:ju) )
               jl = min( nid(i),i ) + 1
               tausla(i,iw) = suma + 2.*dot_product( taun(jl:nid(i)),dsdh(i,jl:nid(i)) )
               if( abs(tausla(i,iw)-tausla(i-1,iw)) < 1.0e-30 ) then
                 mu2(i) = sqrt( largest )
               else
                 mu2(i) = (tauc(i) - tauc(i-1))/(tausla(i,iw) - tausla(i-1,iw))
                 mu2(i) = sign( max( abs(mu2(i)),sqrt(smallest) ),mu2(i) )
               end if
            end if
!-----------------------------------------------------------------------------
! ... the following gamma equations are from pg 16,289, table 1
! eddington approximation(joseph et al., 1976, jas, 33, 2452):
!-----------------------------------------------------------------------------
            gam1 = .25*(7. - om*(4. + 3.*g))
            gam2 = -.25*(1. - om*(4. - 3.*g))
            gam3 = .25*(2. - 3.*g*mu)
            gam4 = 1. - gam3
!-----------------------------------------------------------------------------
! ... lambda = pg 16,290 equation 21
! big gamma = pg 16,290 equation 22
!-----------------------------------------------------------------------------
            lam(i) = sqrt( gam1*gam1 - gam2*gam2 )
            bgam(i) = (gam1 - lam(i))/gam2
            wrk = lam(i)*taun(i)
            if( wrk < 500. ) then
               expon = exp( -wrk )
            else
               expon = 0.
            end if
!-----------------------------------------------------------------------------
! ... e1 - e4 = pg 16,292 equation 44
!-----------------------------------------------------------------------------
            e1(i,iw) = 1. + bgam(i)*expon
            e2(i,iw) = 1. - bgam(i)*expon
            e3(i,iw) = bgam(i) + expon
            e4(i,iw) = bgam(i) - expon
!-----------------------------------------------------------------------------
! ... the following sets up for the c equations 23, and 24
! found on page 16,290
! prevent division by zero (if lambda=1/mu, shift 1/mu^2 by eps = 1.e-3
! which is approx equiv to shifting mu by 0.5*eps* (mu)**3
!-----------------------------------------------------------------------------
            if( tausla(i-1,iw) < 500. ) then
               expon0 = exp( -tausla(i-1,iw) )
            else
               expon0 = 0.
            end if
            if( tausla(i,iw) < 500. ) then
               expon1 = exp( -tausla(i,iw) )
            else
               expon1 = 0.
            end if

            divisr = lam(i)*lam(i) - 1./(mu2(i)*mu2(i))
            temp = max( eps,abs(divisr) )
            divisr = 1./sign( temp,divisr )
            up = om*pifs*((gam1 - 1./mu2(i))*gam3 + gam4*gam2)*divisr
            dn = om*pifs*((gam1 + 1./mu2(i))*gam4 + gam2*gam3)*divisr
!-----------------------------------------------------------------------------
! ... cup and cdn are when tau is equal to zero
! cuptn and cdntn are when tau is equal to taun
!-----------------------------------------------------------------------------
            cup(i,iw) = up*expon0
            cdn(i) = dn*expon0
            cuptn(i,iw) = up*expon1
            cdntn(i,iw) = dn*expon1

         end do level_loop
!-----------------------------------------------------------------------------
! ... set up matrix
! ssfc = pg 16,292 equation 37 where pi fs is one (unity).
!-----------------------------------------------------------------------------
        if( tausla(nzm1,iw) < 500. ) then
           ssfc = rsfc(iw)*mu*exp( -tausla(nzm1,iw) )*pifs
        else
           ssfc = 0.
        end if
!-----------------------------------------------------------------------------
! ... the following are from pg 16,292 equations 39 - 43.
! set up first row of matrix:
!-----------------------------------------------------------------------------
        a(1) = 0.
        b(1) = e1(1,iw)
        d(1) = -e2(1,iw)
        e(1) = fdn0 - cdn(1)
!-----------------------------------------------------------------------------
! ... set up odd rows 3 thru (mrows - 1):
!-----------------------------------------------------------------------------
        a(3:mrows-1:2) = e2(1:nzm2,iw)*e3(1:nzm2,iw) - e4(1:nzm2,iw)*e1(1:nzm2,iw)
        b(3:mrows-1:2) = e1(1:nzm2,iw)*e1(2:nzm1,iw) - e3(1:nzm2,iw)*e3(2:nzm1,iw)
        d(3:mrows-1:2) = e3(1:nzm2,iw)*e4(2:nzm1,iw) - e1(1:nzm2,iw)*e2(2:nzm1,iw)
        e(3:mrows-1:2) = e3(1:nzm2,iw)*(cup(2:nzm1,iw) - cuptn(1:nzm2,iw)) + e1(1:nzm2,iw)*(cdntn(1:nzm2,iw) - cdn(2:nzm1))
!-----------------------------------------------------------------------------
! ... set up even rows 2 thru (mrows - 2):
!-----------------------------------------------------------------------------
        a(2:mrows-2:2) = e2(2:nzm1,iw)*e1(1:nzm2,iw) - e3(1:nzm2,iw)*e4(2:nzm1,iw)
        b(2:mrows-2:2) = e2(1:nzm2,iw)*e2(2:nzm1,iw) - e4(1:nzm2,iw)*e4(2:nzm1,iw)
        d(2:mrows-2:2) = e1(2:nzm1,iw)*e4(2:nzm1,iw) - e2(2:nzm1,iw)*e3(2:nzm1,iw)
        e(2:mrows-2:2) = (cup(2:nzm1,iw) - cuptn(1:nzm2,iw))*e2(2:nzm1,iw) - (cdn(2:nzm1) - cdntn(1:nzm2,iw))*e4(2:nzm1,iw)
!-----------------------------------------------------------------------------
! ... set up last row of matrix at mrows:
!-----------------------------------------------------------------------------
        a(mrows) = e1(nzm1,iw) - rsfc(iw)*e3(nzm1,iw)
        b(mrows) = e2(nzm1,iw) - rsfc(iw)*e4(nzm1,iw)
        d(mrows) = 0.
        e(mrows) = ssfc - cuptn(nzm1,iw) + rsfc(iw)*cdntn(nzm1,iw)
        sub(iw,1:mrows) = a(1:mrows)
        main(iw,1:mrows) = b(1:mrows)
        super(iw,1:mrows) = d(1:mrows)
        y(iw,1:mrows) = e(1:mrows)
      end do wave_loop
!-----------------------------------------------------------------------------
! ... solve the system
!-----------------------------------------------------------------------------
      call tridec( nw-1, mrows, sub, main, super )
      call trislv( nw-1, mrows, sub, main, super, y )
!-----------------------------------------------------------------------------
! ... unfold solution of matrix, compute output fluxes
!-----------------------------------------------------------------------------
      do iw = 1,nw-1
!-----------------------------------------------------------------------------
! ... the following equations are from pg 16,291 equations 31 & 32
!-----------------------------------------------------------------------------
         e(:mrows) = y(iw,:mrows)
         if( tausla(0,iw) < 500. ) then
            radfld(1,iw) = 2.*(fdn0 + e(1)*e3(1,iw) - e(2)*e4(1,iw) + cup(1,iw)) + exp( -tausla(0,iw) )
         else
            radfld(1,iw) = 2.*(fdn0 + e(1)*e3(1,iw) - e(2)*e4(1,iw) + cup(1,iw))
         end if
         where( tausla(1:nzm1,iw) < 500. )
            cdn(1:nzm1) = exp( -tausla(1:nzm1,iw) )
         elsewhere
            cdn(1:nzm1) = 0.
         endwhere
         radfld(2:nz,iw) = 2.*(e(1:mrows-1:2)*(e3(1:nzm1,iw) + e1(1:nzm1,iw)) &
                            + e(2:mrows:2)*(e4(1:nzm1,iw) + e2(1:nzm1,iw)) &
                            + cdntn(1:nzm1,iw) + cuptn(1:nzm1,iw)) + cdn(1:nzm1)
      end do
      end subroutine ps2str


      subroutine pchem( nz, tlev, airlev )

      use module_wave_data, only : r01, r04, r44, r08, r06, r10,  &
                                   r11, r14, r15, r17, r18,       &
                                   xs_mvk, xs_macr, nj

!-----------------------------------------------------------------------------
! purpose:
! load various "weighting functions" (products of cross section and
! quantum yield at each altitude and for wavelength). the altitude
! dependence is necessary to ensure the consideration of pressure and
! temperature dependence of the cross sections or quantum yields.
! the actual reading, evaluation and interpolation is done is separate
! subroutines for ease of management and manipulation. please refer to
! the inline documentation of the specific subroutines for detail information.
!-----------------------------------------------------------------------------
! parameters:
! nw - integer, number of specified intervals + 1 in working (i)
! wavelength grid
! wl - real(8), vector of lower limits of wavelength intervals in (i)
! working wavelength grid
! nz - integer, number of altitude levels in working altitude grid (i)
! tlev - real(8), temperature (k) at each specified altitude level (i)
! airlev - real(8), air density (molec/cc) at each altitude level (i)
! j - integer, counter for number of weighting functions defined (io)
! sq - real(8), cross section x quantum yield (cm^2) for each (o)
! photolysis reaction defined, at each defined wavelength and
! at each defined altitude level
! jlabel - character*40, string identifier for each photolysis reaction (o)
! defined
!-----------------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: nz
      real(8), intent(in) :: tlev(nz)
      real(8), intent(in) :: airlev(nz)
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
      integer :: j
      character(len=40) :: jlabel(nj)
!-----------------------------------------------------------------------------
! ... (1) o2 + hv -> o + o
! reserve first position. cross section parameterization in
! schumman-runge and lyman-alpha regions are zenith-angle dependent
!-----------------------------------------------------------------------------
      j = 1
      jlabel(j) = 'o2 + hv -> o + o '
!-----------------------------------------------------------------------------
! ... (2,3) o3 + hv -> (both channels) (tmp dep)
!-----------------------------------------------------------------------------
      call r01( nz, tlev, airlev, j, jlabel )
!-----------------------------------------------------------------------------
! ... (4) no2 + hv -> no + o(3p)
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'no2 + hv -> no + o(3p) '
!-----------------------------------------------------------------------------
! ... (5) no3 + hv -> no2 + o(3p)
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'no3 + hv -> no2 + o(3p) '
!-----------------------------------------------------------------------------
! ... (6) no3 + hv -> no + o2
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'no3 + hv -> no + o2 '
!-----------------------------------------------------------------------------
! ... (7,8) n2o5 + hv -> (both channels) (tmp dep)
!-----------------------------------------------------------------------------
      call r04( nz,tlev,airlev,j,jlabel )
!-----------------------------------------------------------------------------
! ... (9) n2o + hv -> n2 + o(1d) (tmp dep)
!-----------------------------------------------------------------------------
      call r44( nz,tlev,airlev,j,jlabel )
!-----------------------------------------------------------------------------
! ... (10) ho2 + hv -> oh + o
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'ho2 + hv -> oh + o '
!-----------------------------------------------------------------------------
! ... (11) h2o2 + hv -> 2 oh
!-----------------------------------------------------------------------------
      call r08( nz,tlev,airlev,j,jlabel )
!-----------------------------------------------------------------------------
! ... (12) hno2 + hv -> oh + no
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'hno2 + hv -> oh + no '
!-----------------------------------------------------------------------------
! ... (13) hno3 + hv -> oh + no2
!-----------------------------------------------------------------------------
      call r06( nz,tlev,airlev,j,jlabel )
!-----------------------------------------------------------------------------
! ... (14) hno4 + hv -> ho2 + no2
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'hno4 + hv -> ho2 + no2 '
!-----------------------------------------------------------------------------
! ... (15,16) ch2o + hv -> (both channels)
!-----------------------------------------------------------------------------
      call r10( nz,tlev,airlev,j,jlabel )
!-----------------------------------------------------------------------------
! ... (17,18,19) ch3cho + hv -> (all three channels)
!-----------------------------------------------------------------------------
      call r11( nz,tlev,airlev,j,jlabel )
!-----------------------------------------------------------------------------
! ... (20) c2h5cho + hv -> c2h5 + hco
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'c2h5cho + hv -> c2h5 + hco '
!-----------------------------------------------------------------------------
! ... (21) chocho + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'chocho + hv -> products '
!-----------------------------------------------------------------------------
! ... (22) ch3cocho + hv -> products
!-----------------------------------------------------------------------------
      call r14( nz,tlev,airlev,j,jlabel )
!-----------------------------------------------------------------------------
! ... (23) ch3coch3 + hv -> products
!-----------------------------------------------------------------------------
      call r15( nz,tlev,airlev,j,jlabel )
!-----------------------------------------------------------------------------
! ... (24) ch3ooh + hv -> ch3o + oh
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'ch3ooh + hv -> ch3o + oh '
!-----------------------------------------------------------------------------
! ... (25) ch3ono2 + hv -> ch3o + no2
!-----------------------------------------------------------------------------
      call r17( nz,tlev,airlev,j,jlabel )
!-----------------------------------------------------------------------------
! ... (26) pan + hv -> products
!-----------------------------------------------------------------------------
      call r18( nz,tlev,airlev,j,jlabel )
!-----------------------------------------------------------------------------
! ... (27) mvk + hv -> products
! (28) macr + hv -> products
!-----------------------------------------------------------------------------
      call xs_mvk( nz, tlev, airlev )
      call xs_macr( nz, tlev, airlev )
!-----------------------------------------------------------------------------
! ... (27) cloo + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'cloo + hv -> products '
!-----------------------------------------------------------------------------
! ... (28,29) clono2 + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'clono2 + hv -> cl + no3'
      j = j + 1
      jlabel(j) = 'clono2 + hv -> clo + no2'
!-----------------------------------------------------------------------------
! ... (30) ch3cl + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'ch3cl + hv -> products '
!-----------------------------------------------------------------------------
! ... (31) ccl2o + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'ccl2o + hv -> products'
!-----------------------------------------------------------------------------
! ... (32) ccl4 + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'ccl4 + hv -> products '
!-----------------------------------------------------------------------------
! ... (33) cclfo + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'cclfo + hv -> products '
!-----------------------------------------------------------------------------
! ... (34) ccf2o + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'ccf2o + hv -> products '
!-----------------------------------------------------------------------------
! ... (35) cf2clcfcl2 (cfc-113) + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'cf2clcfcl2 (cfc-113) + hv -> products'
!-----------------------------------------------------------------------------
! ... (36) cf2clcf2cl (cfc-114) + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'cf2clcf2cl (cfc-114) + hv -> products '
!-----------------------------------------------------------------------------
! ... (37) cf3cf2cl (cfc-115) + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'cf3cf2cl (cfc-115) + hv -> products '
!-----------------------------------------------------------------------------
! ... (38) ccl3f (cfc-111) + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'ccl3f (cfc-111) + hv -> products'
!-----------------------------------------------------------------------------
! ... (39) ccl2f2 (cfc-112) + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j) = 'ccl2f2 (cfc-112) + hv -> products'
!-----------------------------------------------------------------------------
! (40) ch3ccl3 + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j)='ch3ccl3 + hv -> products'
!-----------------------------------------------------------------------------
! (41) cf3chcl2 (hcfc-123) + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j)='cf3chcl2 (hcfc-123) + hv -> products'
!-----------------------------------------------------------------------------
! (42) cf3chfcl (hcfc-124) + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j)='cf3chfcl (hcfc-124) + hv -> products'
!-----------------------------------------------------------------------------
! (43) ch3cfcl2 (hcfc-141b) + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j)='ch3cfcl2 (hcfc-141b) + hv -> products'
!-----------------------------------------------------------------------------
! (44) ch3cf2cl (hcfc-142b) + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j)='ch3cf2cl (hcfc-142b) + hv -> products'
!-----------------------------------------------------------------------------
! (45) cf3cf2chcl2 (hcfc-225ca) + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j)='cf3cf2chcl2 (hcfc-225ca) + hv -> products'
!-----------------------------------------------------------------------------
! (46) cf2clcf2chfcl (hcfc-225cb) + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j)='cf2clcf2chfcl (hcfc-225cb) + hv -> products'
!-----------------------------------------------------------------------------
! (47) chclf2 (hcfc-22) + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j)='chclf2 (hcfc-22) + hv -> products'
!-----------------------------------------------------------------------------
! (48) brono2 + hv -> br + o + no2
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j)='brono2 + hv -> br + o + no2'
!-----------------------------------------------------------------------------
! (49) brono2 + hv -> bro + no2
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j)='brono2 + hv -> bro + no2'
!-----------------------------------------------------------------------------
! (50) ch3br + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j)=' ch3br + hv -> products'
!-----------------------------------------------------------------------------
! (51) chbr3 + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j)='chbr3 + hv -> products'
!-----------------------------------------------------------------------------
! (52) cf3br (halon-1301) + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j)='cf3br (halon-1301) + hv -> products'
!-----------------------------------------------------------------------------
! (53) cf2brcf2br (halon-2402) + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j)='cf2brcf2br (halon-2402) + hv -> products'
!-----------------------------------------------------------------------------
! (54) cf2br2 (halon-1202) + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j)='cf2br2 (halon-1202) + hv -> products'
!-----------------------------------------------------------------------------
! (55) cf2brcl (halon-1211) + hv -> products
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j)='cf2brcl (halon-1211) + hv -> products '
!-----------------------------------------------------------------------------
! (56) cl2 + hc -> cl + cl
!-----------------------------------------------------------------------------
      j = j + 1
      jlabel(j)='cl2 + hc -> cl + cl '

      end subroutine pchem

      subroutine lymana( nz, o2col, secchi, dto2la, xso2la )
!-----------------------------------------------------------------------------
! purpose:
! calculate the effective absorption cross section of o2 in the lyman-alpha
! bands and an effective o2 optical depth at all altitudes. parameterized
! after: chabrillat, s., and g. kockarts, simple parameterization of the
! absorption of the solar lyman-alpha line, geophysical research letters,
! vol.24, no.21, pp 2659-2662, 1997.
!-----------------------------------------------------------------------------
! parameters:
! nz - integer, number of specified altitude levels in the working (i)
! grid
! o2col - real(8), slant overhead o2 column (molec/cc) at each specified (i)
! altitude
! dto2la - real(8), optical depth due to o2 absorption at each specified (o)
! vertical layer
! xso2la - real(8), molecular absorption cross section in la bands (o)
!-----------------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: nz
      real(8), intent(in) :: o2col(nz)
      real(8), intent(in) :: secchi(nz)
      real(8), intent(out) :: dto2la(nz-1,nla-1)
      real(8), intent(out) :: xso2la(nz,nla-1)
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
      integer :: i, k, kp1
      real(8), dimension(nz) :: rm, ro2
      real(8), save :: b(3), c(3), d(3), e(3)
      data b / 6.8431d-01, 2.29841d-01, 8.65412d-02/, &
           c /8.22114d-21, 1.77556d-20, 8.22112d-21/, &
           d / 6.0073d-21, 4.28569d-21, 1.28059d-20/, &
           e /8.21666d-21, 1.63296d-20, 4.85121d-17/
!-----------------------------------------------------------------------------
! ... calculate reduction factors at every altitude
!-----------------------------------------------------------------------------
      rm(:) = 0.
      ro2(:) = 0.
      do k = 1,nz
        do i = 1,3
          rm(k) = rm(k) + b(i) * exp( -c(i)*o2col(k) )
          ro2(k) = ro2(k) + d(i) * exp( -e(i)*o2col(k) )
        end do
      end do
!-----------------------------------------------------------------------------
! ... calculate effective o2 optical depths and effective o2 cross sections
!-----------------------------------------------------------------------------
      do k = 1,nz-1
         if( rm(k) > 1.e-100 ) then
            kp1 = k + 1
            if( rm(kp1) > 0. ) then
               dto2la(k,1) = log( rm(kp1) )/secchi(kp1) - log( rm(k) )/secchi(k)
            else
               dto2la(k,1) = 1000.
            end if
         else
            dto2la(k,1) = 1000.
         end if
      end do
      do k = 1,nz
         if( rm(k) > 1.e-100 ) then
            if( ro2(k) > 1.e-100 ) then
               xso2la(k,1) = ro2(k)/rm(k)
            else
               xso2la(k,1) = 0.
            end if
         else
            xso2la(k,1) = 0.
         end if
      end do
      end subroutine lymana


      subroutine inter2( ng, xg, yg, n, x, y, ierr )
!-----------------------------------------------------------------------------
! purpose:
! map input data given on single, discrete points onto a set of target
! bins.
! the original input data are given on single, discrete points of an
! arbitrary grid and are being linearly interpolated onto a specified set
! of target bins. in general, this is the case for most of the weighting
! functions (action spectra, molecular cross section, and quantum yield
! data), which have to be matched onto the specified wavelength intervals.
! the average value in each target bin is found by averaging the trapezoi-
! dal area underneath the input data curve (constructed by linearly connec-
! ting the discrete input values).
! some caution should be used near the endpoints of the grids. if the
! input data set does not span the range of the target grid, an error
! message is printed and the execution is stopped, as extrapolation of the
! data is not permitted.
! if the input data does not encompass the target grid, use addpnt to
! expand the input array.
!-----------------------------------------------------------------------------
! parameters:
! ng - integer, number of bins + 1 in the target grid (i)
! xg - real(8), target grid (e.g., wavelength grid); bin i is defined (i)
! as [xg(i),xg(i+1)] (i = 1..ng-1)
! yg - real(8), y-data re-gridded onto xg, yg(i) specifies the value for (o)
! bin i (i = 1..ng-1)
! n - integer, number of points in input grid (i)
! x - real(8), grid on which input data are defined (i)
! y - real(8), input y-data (i)
!-----------------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: ng, n
      integer, intent(out) :: ierr
      real(8), intent(in) :: x(n)
      real(8), intent(in) :: y(n)
      real(8), intent(in) :: xg(ng)
      real(8), intent(out) :: yg(ng)
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
      integer :: ngintv
      integer :: i, k, jstart
      real(8) :: area, xgl, xgu
      real(8) :: darea, slope
      real(8) :: a1, a2, b1, b2
      ierr = 0
!-----------------------------------------------------------------------------
! ... test for correct ordering of data, by increasing value of x
!-----------------------------------------------------------------------------
      do i = 2,n
         if( x(i) <= x(i-1) ) then
            ierr = 1
            write(*,*) 'inter2: x coord not monotonically increasing'
            return
         end if
      end do
      do i = 2,ng
        if( xg(i) <= xg(i-1) ) then
           ierr = 2
           write(*,*) 'inter2: xg coord not monotonically increasing'
           return
        end if
      end do
!-----------------------------------------------------------------------------
! ... check for xg-values outside the x-range
!-----------------------------------------------------------------------------
      if( x(1) > xg(1) .or. x(n) < xg(ng) ) then
          write(*,*) 'inter2: data does not span grid'
          write(*,*) '        use addpnt to expand data and re-run'
! call endrun
      end if
!-----------------------------------------------------------------------------
! ... find the integral of each grid interval and use this to
! calculate the average y value for the interval
! xgl and xgu are the lower and upper limits of the grid interval
!-----------------------------------------------------------------------------
      jstart = 1
      ngintv = ng - 1
      do i = 1,ngintv
!-----------------------------------------------------------------------------
! ... initalize:
!-----------------------------------------------------------------------------
         area = 0.
         xgl = xg(i)
         xgu = xg(i+1)
!-----------------------------------------------------------------------------
! ... discard data before the first grid interval and after the
! last grid interval
! for internal grid intervals, start calculating area by interpolating
! between the last point which lies in the previous interval and the
! first point inside the current interval
!-----------------------------------------------------------------------------
         k = jstart
         if( k <= n-1 ) then
!-----------------------------------------------------------------------------
! ... if both points are before the first grid, go to the next point
!-----------------------------------------------------------------------------
            do
               if( x(k+1) <= xgl ) then
                  jstart = k - 1
                  k = k+1
                  if( k <= n-1 ) then
                     cycle
                  else
                     exit
                  end if
               else
                  exit
               end if
            end do
!-----------------------------------------------------------------------------
! ... if the last point is beyond the end of the grid,
! complete and go to the next grid
!-----------------------------------------------------------------------------
            do
               if( k <= n-1 .and. x(k) < xgu ) then
                  jstart = k-1
!-----------------------------------------------------------------------------
! ... compute x-coordinates of increment
!-----------------------------------------------------------------------------
                  a1 = max( x(k),xgl )
                  a2 = min( x(k+1),xgu )
!-----------------------------------------------------------------------------
! ... if points coincide, contribution is zero
!-----------------------------------------------------------------------------
                  if( x(k+1) == x(k) ) then
                     darea = 0.
                  else
                     slope = (y(k+1) - y(k))/(x(k+1) - x(k))
                     b1 = y(k) + slope*(a1 - x(k))
                     b2 = y(k) + slope*(a2 - x(k))
                     darea = .5*(a2 - a1)*(b2 + b1)
                  end if
!-----------------------------------------------------------------------------
! ... find the area under the trapezoid from a1 to a2
!-----------------------------------------------------------------------------
                  area = area + darea
                  k = k+1
                  cycle
               else
                  exit
               end if
            end do
         end if
!-----------------------------------------------------------------------------
! ... calculate the average y after summing the areas in the interval
!-----------------------------------------------------------------------------
         yg(i) = area/(xgu - xgl)
      end do
      end subroutine inter2
      subroutine inter_inti( ng, xg, n, x )
!-----------------------------------------------------------------------------
! ... initialization
!-----------------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: ng, n
      real(8), intent(in) :: xg(ng)
      real(8), intent(in) :: x(n)
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
      integer :: i, ii, iil, astat
      integer :: ndim(1)
      allocate( xi(ng), xcnt(ng-1), stat=astat )
      if( astat /= 0 ) then
         write(*,*) 'inter_inti: failed to allocate wrk arrays; error = ',astat
         stop
      else
         xi(:) = 0
         xcnt(:) = 0
      end if
      iil = 1
      do i = 1,ng
         do ii = iil,n-1
            if( xg(i) < x(ii) ) then
               xi(i) = ii - 1
               iil = ii
               exit
            end if
         end do
      end do
      nintervals = count( xi(:) /= 0 )
      if( nintervals == 0 ) then
         write(*,*) 'inter_inti: wavelength grids do not overlap'
         stop
      else
         nintervals = nintervals - 1
      end if
      xcnt(1:nintervals) = xi(2:nintervals+1) - xi(1:nintervals) + 1
      ndim(:) = maxval( xcnt(1:nintervals) )
      allocate( xfrac(ndim(1),nintervals),stat=astat )
      if( astat /= 0 ) then
         write(*,*) 'inter_inti: failed to allocate wrk array; error = ',astat
         stop
      else
         xfrac(:,:) = 1.
      end if
      do i = 1,nintervals
        iil = xi(i)
        xfrac(1,i) = (min( x(iil+1),xg(i+1) ) - xg(i))/(x(iil+1) - x(iil))
        if( xcnt(i) > 1 ) then
           iil = xi(i) + xcnt(i) - 1
           xfrac(xcnt(i),i) = (xg(i+1) - x(iil))/(x(iil+1) - x(iil))
        end if
      end do
!     write(*,*) 'inter_inti: diagnostics; ng,n,nintervals = ',ng,n,nintervals
!     write(*,'('' xi'')')
!     write(*,'(10i4)') xi(1:nintervals+1)
!     write(*,'('' xcnt'')')
!     write(*,'(10i4)') xcnt(1:nintervals)
!     write(*,'('' xfrac'')')
!     do i = 1,nintervals
!        write(*,'(1p,5e21.13)') xfrac(1:xcnt(i),i)
!     end do
      end subroutine inter_inti
      subroutine inter3( ng, xg, yg, n, x, y )
!-----------------------------------------------------------------------------
! purpose:
! map input data given on a set of bins onto a different set of target
! bins.
! the input data are given on a set of bins (representing the integral
! of the input quantity over the range of each bin) and are being matched
! onto another set of bins (target grid). a typical example would be an
! input data set spcifying the extra-terrestrial flux on wavelength inter-
! vals, that has to be matched onto the working wavelength grid.
! the resulting area in a given bin of the target grid is calculated by
! simply adding all fractional areas of the input data that cover that
! particular target bin.
! some caution should be used near the endpoints of the grids. if the
! input data do not span the full range of the target grid, the area in
! the "missing" bins will be assumed to be zero. if the input data extend
! beyond the upper limit of the target grid, the user has the option to
! integrate the "overhang" data and fold the remaining area back into the
! last target bin. using this option is recommended when re-gridding
! vertical profiles that directly affect the total optical depth of the
! model atmosphere.
!-----------------------------------------------------------------------------
! parameters:
! ng - integer, number of bins + 1 in the target grid (i)
! xg - real(8), target grid (e.g. working wavelength grid); bin i (i)
! is defined as [xg(i),xg(i+1)] (i = 1..ng-1)
! yg - real(8), y-data re-gridded onto xg; yg(i) specifies the (o)
! y-value for bin i (i = 1..ng-1)
! n - integer, number of bins + 1 in the input grid (i)
! x - real(8), input grid (e.g. data wavelength grid); bin i is (i)
! defined as [x(i),x(i+1)] (i = 1..n-1)
! y - real(8), input y-data on grid x; y(i) specifies the (i)
! y-value for bin i (i = 1..n-1)
!-----------------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: n, ng
      real(8), intent(in) :: xg(ng)
      real(8), intent(in) :: x(n)
      real(8), intent(in) :: y(n)
      real(8), intent(out) :: yg(ng)
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
      integer :: i, ii, iil
!-----------------------------------------------------------------------------
! ... do interpolation
!-----------------------------------------------------------------------------
      yg(:) = 0.
      do i = 1,nintervals
         iil = xi(i)
         ii = xcnt(i)
         if( ii == 1 ) then
            yg(i) = xfrac(1,i)*y(iil)
         else
            yg(i) = dot_product( xfrac(1:ii,i),y(iil:iil+ii-1) )
         end if
      end do
      end subroutine inter3

      subroutine calcoe( nz, c, xz, tt, adjin, adjcoe )
!-----------------------------------------------------------------------------
! parameters:
! adjcoe - real(8), coross section adjust coefficients (in and out)
! c(5,28)-polynomal coef
! tt -nomarlized temperature
!-----------------------------------------------------------------------------*
      implicit none
!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: nz
      real(8), intent(in) :: adjin
      real(8), intent(in) :: tt
      real(8), intent(in) :: c(5)
      real(8), intent(in) :: xz(nz)
      real(8), intent(inout) :: adjcoe(nz)
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
      integer :: k
      real(8) :: x
      do k = 1,nz
         x = xz(k)
         adjcoe(k) = adjin * (1. + .01*(c(1) + x*(c(2) + x*(c(3) + x*(c(4) + x*c(5))))))
      end do
      end subroutine calcoe


      subroutine airmas( nz, z, zen, dsdh, nid, cz, &
                         vcol, scol )
!-----------------------------------------------------------------------------
! purpose:
! calculate vertical and slant air columns, in spherical geometry, as a
! function of altitude.
!-----------------------------------------------------------------------------
! parameters:
! nz - integer, number of specified altitude levels in the working (i)
! grid
! z - real(8), specified altitude working grid (km) (i)
! zen - real(8), solar zenith angle (degrees) (i)
! dsdh - real(8), slant path of direct beam through each layer crossed (o)
! when travelling from the top of the atmosphere to layer i;
! dsdh(i,j), i = 0..nz-1, j = 1..nz-1
! nid - integer, number of layers crossed by the direct beam when (o)
! travelling from the top of the atmosphere to layer i;
! nid(i), i = 0..nz-1
! vcol - real(8), output, vertical air column, molec cm-2, above level iz
! scol - real(8), output, slant air column in direction of sun, above iz
! also in molec cm-2
!-----------------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------------
! ... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in) :: nz
      integer, intent(in) :: nid(0:nz-1)
      real(8), intent(in) :: z(nz)
      real(8), intent(in) :: zen
      real(8), intent(in) :: dsdh(0:nz-1,nz-1)
      real(8), intent(in) :: cz(nz)
      real(8), intent(out) :: vcol(nz)
      real(8), intent(out) :: scol(nz)
!-----------------------------------------------------------------------------
! ... parameters
!-----------------------------------------------------------------------------
      real(8), parameter :: largest = huge(1.0)
!-----------------------------------------------------------------------------
! ... local variables
!-----------------------------------------------------------------------------
      integer :: id, j
      real(8) :: sum, ssum, vsum, ratio
!-----------------------------------------------------------------------------
! ... calculate vertical and slant column from each level: work downward
!-----------------------------------------------------------------------------
      vsum = 0.
      ssum = 0.
      do id = 1,nz-1
         vsum = vsum + cz(nz-id)
         vcol(nz-id) = vsum
         sum = 0.
         if( nid(id) < 0 ) then
            sum = largest
         else
!-----------------------------------------------------------------------------
! ... single pass layers:
!-----------------------------------------------------------------------------
            do j = 1,min( nid(id),id )
               sum = sum + cz(nz-j)*dsdh(id,j)
            end do
!-----------------------------------------------------------------------------
! ... double pass layers:
!-----------------------------------------------------------------------------
            do j = min( nid(id),id )+1,nid(id)
               sum = sum + 2.*cz(nz-j)*dsdh(id,j)
            end do
         end if
         scol(nz - id) = sum
      end do
!-----------------------------------------------------------------------------
! ... special section to set scol(nz)
!-----------------------------------------------------------------------------
      if( scol(nz-2) /= 0. ) then
         ratio = scol(nz-1)/scol(nz-2)
         scol(nz) = ratio * scol(nz-1)
      else
         scol(nz) = 0.
      end if
      end subroutine airmas


      END MODULE module_ftuv_subs 
