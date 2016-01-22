subroutine da_cld_eff_radius(t,rho,qci,qrn,qsn,qgr,snow,xice,xland,method, &
                             reff_water,reff_ice,reff_rain,reff_snow,reff_grau)

   !---------------------------------------------------------------------------
   ! Purpose: compute effective radius of cloud liquid water and cloud ice
   !
   ! METHOD: 
   !   liquid water: adapted from WRFV2/phys/module_ra_cam.F, analytic formula following 
   !                 the formulation originally developed by J. T. Kiehl, and 
   !   ice (method 1): adapted from WRFV2/phys/module_ra_cam.F,  Kristjansson and Mitchell
   !   ice (method 2): WSM3, Hong et al., MWR 2004
   !   rain/snow/graupel: assume exponential particle size distribution and
   !                             spherical particles
   !                      use Gauss-Laguerre Quadrature for integration
   ! HISTORY: 12/15/2008 effective radius unit is micron.       Zhiquan Liu
   !---------------------------------------------------------------------------

   integer, intent(in)  :: method
   real*8,  intent(in)  :: t         ! temperature
   real,    intent(in)  :: rho       ! air density  kg/m3
   real*8,  intent(in)  :: qci       ! cloud ice mixing ratio kg/kg
   real*8,  intent(in)  :: qrn       ! cloud rain mixing ratio
   real*8,  intent(in)  :: qsn       ! cloud snow mixing ratio
   real*8,  intent(in)  :: qgr       ! cloud graupel mixing ratio
   real,    intent(in)  :: snow         ! snow water equivalent
   real*8,  intent(in)  :: xice         ! ice percentage
   real*8,  intent(in)  :: xland        ! landsea percentage
   real*8,  intent(out) :: reff_water   ! effective radius liquid water
   real*8,  intent(out) :: reff_ice     ! effective radius ice
   real*8,  intent(out) :: reff_rain    ! effective radius rain
   real*8,  intent(out) :: reff_snow    ! effective radius snow
   real*8,  intent(out) :: reff_grau    ! effective radius graupel

   !  local variables
   integer                      :: index, nk
   real                         :: snowh, corr
   real, parameter              :: rliqland  = 8.0     ! liquid drop size if over land
   real, parameter              :: rliqocean = 14.0    ! liquid drop size if over ocean
   real, parameter              :: rliqice   = 14.0    ! liquid drop size if over sea ice
   ! Tabulated values of re(T) in the temperature interval
   ! 180 K -- 274 K; hexagonal columns assumed:
   real, dimension(95), parameter  :: retab =                   &
         (/ 5.92779, 6.26422, 6.61973, 6.99539, 7.39234,        &
         7.81177, 8.25496, 8.72323, 9.21800, 9.74075, 10.2930,  &
         10.8765, 11.4929, 12.1440, 12.8317, 13.5581, 14.2319,  &
         15.0351, 15.8799, 16.7674, 17.6986, 18.6744, 19.6955,  &
         20.7623, 21.8757, 23.0364, 24.2452, 25.5034, 26.8125,  &
         27.7895, 28.6450, 29.4167, 30.1088, 30.7306, 31.2943,  &
         31.8151, 32.3077, 32.7870, 33.2657, 33.7540, 34.2601,  &
         34.7892, 35.3442, 35.9255, 36.5316, 37.1602, 37.8078,  &
         38.4720, 39.1508, 39.8442, 40.5552, 41.2912, 42.0635,  &
         42.8876, 43.7863, 44.7853, 45.9170, 47.2165, 48.7221,  &
         50.4710, 52.4980, 54.8315, 57.4898, 60.4785, 63.7898,  &
         65.5604, 71.2885, 75.4113, 79.7368, 84.2351, 88.8833,  &
         93.6658, 98.5739, 103.603, 108.752, 114.025, 119.424,  &
         124.954, 130.630, 136.457, 142.446, 148.608, 154.956,  &
         161.503, 168.262, 175.248, 182.473, 189.952, 197.699,  &
         205.728, 214.055, 222.694, 231.661, 240.971, 250.639 /)
   !
   ! constants from da_control.f:  pi,
   !
   real, parameter :: n0_rain  = 0.08      ! cm(-4)
   real, parameter :: n0_snow  = 0.04      ! cm(-4)
   real, parameter :: n0_grau  = 0.04      ! cm(-4)
   real, parameter :: rho_w    =  1000.0   ! kg m(-3)
   real, parameter :: rho_ice  =   900.0   ! kg m(-3)
   real, parameter :: rho_snow =   100.0   ! kg m(-3)
   real, parameter :: rho_grau =   400.0   ! kg m(-3)

   ! Abscissas of Gauss-Laguerre Integration
   real, dimension(32) :: xk = (/ 0.0444893658333, 0.23452610952,  &
      0.576884629302, 1.07244875382, 1.72240877644, 2.52833670643, &
      3.49221327285, 4.61645677223, 5.90395848335, 7.3581268086,   &
      8.98294126732, 10.783012089, 12.763745476, 14.9309117981,    &
      17.2932661372, 19.8536236493, 22.6357789624, 25.6201482024,  &
      28.8739336869, 32.3333294017, 36.1132042245, 40.1337377056,  &
      44.5224085362, 49.2086605665, 54.3501813324, 59.8791192845,  &
      65.9833617041, 72.6842683222, 80.1883747906, 88.735192639,   &
      98.8295523184, 111.751398227 /)

   ! total weights (weight*exp(xk)) of Gauss-Laguerre Integration
   real, dimension(32) :: totalw = (/ 0.114187105768, 0.266065216898, &
      0.418793137325, 0.572532846497, 0.727648788453, 0.884536718946, &
      1.04361887597, 1.20534920595, 1.37022171969, 1.53877595906,     &
      1.71164594592, 1.8895649683, 2.07318851235, 2.26590144444,      &
      2.46997418988, 2.64296709494, 2.76464437462, 3.22890542981,     &
      2.92019361963, 4.3928479809, 4.27908673189, 5.20480398519,      &
      5.11436212961, 4.15561492173, 6.19851060567, 5.34795780128,     &
      6.28339212457, 6.89198340969, 7.92091094244, 9.20440555803,     &
      11.1637432904, 15.3902417688 /)

   real, parameter :: limit = 1.0e-6
   real :: piover6   ! pi/6
   real :: sum1_rain, sum2_rain, sum1_snow, sum2_snow, sum1_grau, sum2_grau
   real :: lamda_rain, lamda_snow, lamda_grau
   real, dimension(32) :: psd_rain, psd_snow, psd_grau   ! partical size distribution

! initialize
   reff_water = 0.0
   reff_ice = 0.0
   reff_rain = 0.0
   reff_snow = 0.0
   reff_grau = 0.0
   lamda_rain = 0.0
   lamda_snow = 0.0
   lamda_grau = 0.0

! cloud liquid effective radius

   snowh = 0.001 * snow  ! here the snowh (in meter) is water-equivalent snow depth, 
                         ! which is different from the actually snow depth defined in the wrf output file
   
   ! Start with temperature-dependent value appropriate for continental air
   reff_water = rliqland + (rliqocean-rliqland) * min(1.0_8,max(0.0_8,(t_triple-t)*0.05_8))
   ! Modify for snow depth over land
   reff_water = reff_water + (rliqocean-reff_water) * min(1.0,max(0.0,snowh*10.))
   ! Ramp between polluted value over land to clean value over ocean.
   reff_water = reff_water + (rliqocean-reff_water) * min(1.0_8,max(0.0_8,1.0_8-xland))
   ! Ramp between the resultant value and a sea ice value in the presence of ice.
   reff_water = reff_water + (rliqice-reff_water) * min(1.0_8,max(0.0_8,xice))

! cloud ice effective radius

   if ( method == 1 ) then
      index = int(t-179.)
      index = min(max(index,1),94)
      corr = t - int(t)
      reff_ice = retab(index)*(1.-corr) + retab(index+1)*corr
   end if

! cloud ice effective radius
! rho*qci = 2.08*10**22 * Dice**8

   if ( method == 2 ) then
      ! 0.5 for diameter - radii conversion
      ! 1.0e6 for meter - micron conversion
      ! 0.125 = 1/8
      reff_ice = 1.0e6 * 0.5 * ( rho * qci / 2.08e22 ) ** 0.125
   end if
!
! cloud rain/snow/graupel effective radius
!
   piover6 = pi/6.
   if ( qrn > limit ) then
      lamda_rain = (piover6*rho_w*n0_rain*rho/qrn)**0.25
   end if
   if ( qsn > limit ) then
      lamda_snow = (piover6*rho_snow*n0_snow*rho/qsn)**0.25
   end if
   if ( qgr > limit ) then
      lamda_grau = (piover6*rho_grau*n0_grau*rho/qgr)**0.25
   end if
   sum1_rain = 0.0
   sum2_rain = 0.0
   sum1_snow = 0.0
   sum2_snow = 0.0
   sum1_grau = 0.0
   sum2_grau = 0.0
   if ( qrn > limit ) then
      do nk = 1, 32
         psd_rain(nk) = n0_rain*exp(-2.0*lamda_rain*xk(nk))
         sum1_rain = sum1_rain + totalw(nk) * (xk(nk)**3) * psd_rain(nk)
         sum2_rain = sum2_rain + totalw(nk) * (xk(nk)**2) * psd_rain(nk)
      end do
      reff_rain = 10000.0 * sum1_rain/sum2_rain    ! micron
   end if
   if ( qsn > limit ) then
      do nk = 1, 32
         psd_snow(nk) = n0_snow*exp(-2.0*lamda_snow*xk(nk))
         sum1_snow = sum1_snow + totalw(nk) * (xk(nk)**3) * psd_snow(nk)
         sum2_snow = sum2_snow + totalw(nk) * (xk(nk)**2) * psd_snow(nk)
      end do
      reff_snow = 10000.0 * sum1_snow/sum2_snow    ! micron
   end if
   if ( qgr > limit ) then
      do nk = 1, 32
         psd_grau(nk) = n0_grau*exp(-2.0*lamda_grau*xk(nk))
         sum1_grau = sum1_grau + totalw(nk) * (xk(nk)**3) * psd_grau(nk)
         sum2_grau = sum2_grau + totalw(nk) * (xk(nk)**2) * psd_grau(nk)
      end do
      reff_grau = 10000.0 * sum1_grau/sum2_grau    ! micron
   end if

end subroutine da_cld_eff_radius
