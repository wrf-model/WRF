  module module_ra_farms

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !                                                 !
    ! Purpose: Couples the FARMS model to WRF         !
    !                                                 !
    ! Author: Yu Xie coded the FARMS model            !
    !         Pedro A. Jimenez coupled FARMS to WRF   !
    !                                                 !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use module_model_constants, only : G

    implicit none

    private

    public :: Farms_driver

    real, parameter :: THREE_OVER_TWO = 3.0 / 2.0
    integer, parameter :: TAU_ICE_METHOD = 1
    logical, parameter :: USE_REST2 = .false.

    real, parameter :: PTAU_MIN = 0.000001
    real, parameter :: DELTA_MIN = 0.000001
    real, parameter :: DE_ICE_MIN = 5.0, DE_ICE_MAX = 140.0
    real, parameter :: DE_CLOUD_MIN = 5.0, DE_CLOUD_MAX = 120.0
    real, parameter :: TAU_MIN = 0.0001, TAU_MAX = 300.0
    real, parameter :: AOD550_VAL = 0.12, ANGEXP_VAL = 1.3, AERSSA_VAL = 0.85, AERASY_VAL = 0.9

  contains

    subroutine Farms_driver (ims, ime, jms, jme, its, ite, jts, jte, kms, kme, kts, kte,  &
           p8w, rho, dz8w, albedo, aer_opt, aerssa2d, aerasy2d, aod5502d, angexp2d,  &
           coszen_loc, qv, qi, qs, qc, re_cloud, re_ice, re_snow,    &
           julian, swdown2, swddir2, swddni2, swddif2, swdownc2, swddnic2)

      implicit None

      integer, intent(in) :: ims, ime, jms, jme, its, ite, jts, jte, kms, kme, &
          kts, kte

      integer, intent(in) :: aer_opt
      real,    intent(in) :: julian

      real, dimension(ims:ime, jms:jme), intent(in) :: albedo, coszen_loc

      real, dimension(ims:ime, kms:kme, jms:jme ), intent(in) :: qv, qi, qs, qc, &
          p8w, rho, dz8w, re_cloud, re_ice, re_snow 

      real, dimension(ims:ime, jms:jme), intent(inout) :: aerssa2d, aerasy2d, aod5502d, angexp2d
      real, dimension(ims:ime,jms:jme), intent(inout) :: swddir2, swdown2, &
                                                         swddif2, swddni2, &
                                                         swdownc2, swddnic2

        ! Local
      integer :: i, j
      real    :: tau_qv, tau_qi, tau_qs, pmw, swp, iwp, lwp, beta
      real    :: re_cloud_path, re_ice_path, re_snow_path, q_aux
      real, dimension(kms:kme) :: rhodz


      j_loop: do j = jts, jte
         i_loop: do i = its, ite
           daytime_if: if (coszen_loc(i, j) <= 0.0 ) then
             swdown2(i, j) = 0.0
             swddni2(i, j) = 0.0
             swddir2(i, j) = 0.0
             swddif2(i, j) = 0.0
             swdownc2(i, j) = 0.0
             swddnic2(i, j) = 0.0
           else
             rhodz(:) = rho(i, :, j) * dz8w(i, :, j) / (1. + qv(i, :, j))
               ! PMW
             pmw = integrate_1var (rhodz, qv(i, :, j), kms, kme, kts, kte)

               ! Calc effective radius water
             q_aux = integrate_1var (rhodz, qc(i, :, j), kms, kme, kts, kte)
             lwp = q_aux

             if (q_aux > 0.0) then
               re_cloud_path = integrate_2var (rhodz, qc(i, :, j), &
                   re_cloud(i, :, j), kms, kme, kts, kte)
               re_cloud_path = re_cloud_path / q_aux
             else
               re_cloud_path = 0.0
             end if

               ! Calc effective radius ice
             q_aux = integrate_1var (rhodz, qi(i, :, j), kms, kme, kts, kte)
             iwp = q_aux

             if (q_aux > 0.0) then
               re_ice_path = integrate_2var (rhodz, qi(i, :, j), &
                   re_ice(i, :, j), kms, kme, kts, kte)
               re_ice_path = re_ice_path / q_aux
             else
               re_ice_path = 0.0
             end if

               ! Calc effective radius snow
             q_aux = integrate_1var (rhodz, qs(i, :, j), kms, kme, kts, kte)
             swp = q_aux

             if (q_aux > 0.0) then
               re_snow_path = integrate_2var (rhodz, qs(i, :, j), &
                   re_snow(i, :, j), kms, kme, kts, kte)
               re_snow_path = re_snow_path / q_aux
             else
               re_snow_path = 0.0
             end if

               ! optical thickness  water
             if (re_cloud_path > 0.0) then
               tau_qv = THREE_OVER_TWO * lwp / re_cloud_path / 1000.0
             else
               tau_qv = 0.0
             end if

               ! Optical thickness ice 
             if (re_ice_path > 0.0) then
               if (TAU_ICE_METHOD == 1) then
                   ! Eq 10 in Matrosov et al. (2002)
                 tau_qi = iwp * 1000.0 * (0.02 + 4.2 / (2.0 * re_ice_path * 1.0e+6))
               else
                 tau_qi = iwp * 1000.0 * (-0.006656 + 3.686 / (2.0 * re_ice_path * 1.0e+6))
               end if
             else
               tau_qi = 0.0
             end if

               ! Optical thickness snow 
             if (re_snow_path > 0.0) then
               if (TAU_ICE_METHOD == 1) then
                   ! Eq 10 in Matrosov et al. (2002)
                 tau_qs = swp * 1000.0 * (0.02 + 4.2 / (2.0 * re_snow_path * 1.0e+6))
               else
                 tau_qs = swp * 1000.0 * (-0.006656 + 3.686 / (2.0 * re_snow_path * 1.0e+6))
               end if
             else
               tau_qs = 0.0
             end if

               ! Aerosols
             if (aer_opt == 1) then
                angexp2d(i, j) = ANGEXP_VAL
                aerssa2d(i, j) = AERSSA_VAL
                aerasy2d(i, j) = AERASY_VAL
             else if (aer_opt == 0) then
                aod5502d(i, j) = 0.0
                angexp2d(i, j) = 0.0
                aerssa2d(i, j) = 0.0
                aerasy2d(i, j) = 0.0
             end if

             beta = aod5502d(i, j) * (1000.0/ 550.0) ** (- angexp2d(i, j))
              
             Call Farms (p8w(i, 1, j), albedo(i, j), aerssa2d(i, j), &
                 aerasy2d(i, j), coszen_loc(i, j), beta,   &
                 angexp2d(i, j), pmw, tau_qv, tau_qi, tau_qs,        &
                 re_cloud_path, re_ice_path, re_snow_path, int(julian),   &
                 swdown2(i, j), swddni2(i, j), swddif2(i, j), swddir2(i, j), &
                 swdownc2(i, j), swddnic2(i, j))

          end if daytime_if
        end do i_loop
      end do j_loop

    end subroutine Farms_driver


    function Integrate_1var(rhodz, var1_1d, kms, kme, kts, kte) &
        result (return_value)
      
      implicit none

      integer, intent(in) :: kts, kte, kms, kme
      real, dimension(kms:kme), intent(in) :: var1_1d, rhodz

        ! Local
      real :: return_value
      integer :: k

      return_value = 0.0
      do k = kts, kte - 1
        return_value = return_value + var1_1d(k) * rhodz(k)
      end do

    end function Integrate_1var


    function Integrate_2var(rhodz, var1_1d, var2_1d, kms, kme, kts, kte) &
        result (return_value)

      implicit none

      integer, intent(in) :: kts, kte, kms, kme
      real, dimension(kms:kme), intent(in) :: var1_1d, var2_1d, rhodz

        ! Local
      real :: return_value
      integer :: k

      return_value = 0.0
      do k = kts, kte - 1
        return_value = return_value + var1_1d(k) * var2_1d(k) * rhodz(k)
      end do

    end function Integrate_2var


    subroutine FARMS (p_pa, albdo, ssa, g, solarangle, beta, alpha, w_mm, &
        tau_qv, tau_qi, tau_qs, re_cloud_path_m, re_ice_path_m, re_snow_path_m, &
        juday, ghi, dni, dif, dir, ghi_clear, dni_clear)

        !!!!!! This Fast All-sky Radiation Model for Solar applications (FARMS) was developed by
        !!!!!! Yu Xie (Yu.Xie@nrel.gov). Please contact him for more
        !!!!!! information.

        ! Adapted by PAJ to couple it with WRF

        ! Input values
        ! p_pa: surface air pressure (Pa)
        ! albdo: surface albedo
        ! SSA: single-scattering albedo for aerosol
        ! g: asymmetric factor of aerosol
        ! Z: solar zenith angle 
        ! beta: Angstrom turbidity coeff., i.e. AOD at 1000 nm.
        ! alpha: Angstrom wavelength exponent
        !        For a cloudy sky, the suggested values are beta=0.1 and alpha=1.2,
        !        unless you have more accurate measurements.
        ! w: PWV (cm)
        ! tau_qv: cloud optical thickness (liquid water)
        ! tau_qi: cloud optical thickness (ice)
        ! tau_qs: cloud optical thickness (snow)
        ! re_cloud_path_m: effective radious liquid
        ! re_ice_path_m: effective radious ice
        ! re_snow_path_m: effective radious snow
        ! juday: day of year (1-366)
        ! ozone: ozone amount (1000DU, i.e. 0.4=400DU)
        ! phase: cloud thermodynamic phase (1=water, 2=ice)
        ! De: Cloud effective particle size (micron) 
        !     (For a water cloud, De=2*effective radius)
        !     (For an ice cloud, the defination of De follows Fu. 1996.
        !     Another useful defination is given by "Determination of ice cloud models using MODIS and MISR data" ) 


      implicit none
  
      real, intent(in) :: p_pa, albdo, ssa, g, solarangle, beta, alpha, w_mm, &
          tau_qv, tau_qi, tau_qs, re_cloud_path_m, re_ice_path_m, re_snow_path_m
      integer, intent(in) :: juday
      real, intent(out) :: ghi, dni, dir, dif, ghi_clear, dni_clear

        ! Local vars
      real :: de_cloud, de_cloud2, de_ice, de_ice2, de_snow, de_snow2, f0, f1, ftotal, ozone, &
          p, radius, Ruucld, pi, Ruucld_water, Ruucld_ice, Ruucld_snow, Ruuclr, tau_qv2, tau_qi2, &
          tau_qs2, tau, Tddcld, Tddclr, Tducld, Tducld_ice, tducld_snow, tducld_water, Tduclr, Tuuclr, &
          w, Z, tau_tot


      PI = acos(-1.0)

      p = p_pa / 100.0
      ozone = 0.265
      w = w_mm / 10.0

      de_cloud = 2.0 * re_cloud_path_m / 1.0e-6
      de_ice = 2.0 * re_ice_path_m / 1.0e-6
      de_snow = 2.0 * re_snow_path_m / 1.0e-6

      de_cloud = 2.0 * re_cloud_path_m / 1.0e-6
      de_cloud2 = Max (de_cloud, DE_CLOUD_MIN)
      de_cloud2 = Min (de_cloud2, DE_CLOUD_MAX)

      de_ice = 2.0 * re_ice_path_m / 1.0e-6
      de_ice2 = Max (de_ice, DE_ICE_MIN)
      de_ice2 = Min (de_ice2, DE_ICE_MAX)

      de_snow = 2.0 * re_snow_path_m / 1.0e-6
      de_snow2 = Max (de_snow, DE_ICE_MIN)
      de_snow2 = Min (de_snow2, DE_ICE_MAX)

      tau_qv2 = Max (tau_qv, TAU_MIN)
      tau_qv2 = Min (tau_qv2, TAU_MAX)
      tau_qi2 = Max (tau_qi, TAU_MIN)
      tau_qi2 = Min (tau_qi2, TAU_MAX)
      tau_qs2 = Max (tau_qs, TAU_MIN)
      tau_qs2 = Min (tau_qs2, TAU_MAX)

      tau_tot = 0.0
      if (tau_qv > TAU_MIN) tau_tot = tau_tot + tau_qv2
      if (tau_qi > TAU_MIN) tau_tot = tau_tot + tau_qi2
      if (tau_qs > TAU_MIN) tau_tot = tau_tot + tau_qs2
      tau_tot = Min (tau_tot, TAU_MAX)

      Z = acos(solarangle) * 180.0 / PI

      call SUNEARTH( juday, Radius )
      F0 = 1361.2/(Radius*Radius)

      call CLEARSKYALL(p, albdo, SSA, g, Z, Radius, beta,&
          alpha, ozone, w, Tddclr, Tduclr, Ruuclr, Tuuclr)

        ! Liquid hydrometeors
      if (tau_qv > TAU_MIN) then
        Call Watermodel (tau_tot, de_cloud2, solarangle, Tducld_water, Ruucld_water)
      else
        Tducld_water = 1.0
        Ruucld_water = 0.0
        tau_qv2 = 0.0
      end if

        ! Ice hydrometeors
      if (tau_qi > TAU_MIN) then
        call Icemodel (tau_tot, de_ice2, solarangle, Tducld_ice, Ruucld_ice)
      else
        Tducld_ice = 1.0
        Ruucld_ice = 0.0
        tau_qi2 = 0.0
      end if

        ! Snow hydrometeors
      if (tau_qs > TAU_MIN) then
        call Icemodel (tau_tot, de_snow2, solarangle, Tducld_snow, Ruucld_snow)
      else
        Tducld_snow = 1.0
        Ruucld_snow = 0.0
        tau_qs2 = 0.0
      end if

      if (tau_tot > 0.0) then
        Tducld =  (tau_qv2 * Tducld_water + tau_qi2 * Tducld_ice + tau_qs2 * Tducld_snow) / tau_tot
        Tducld = min(Tducld, 1.0)
        Tducld = max(Tducld, 0.0)

        Ruucld = (tau_qv2 * Ruucld_water + tau_qi2 * Ruucld_ice + tau_qs2 * Ruucld_snow)  / tau_tot
        Ruucld = min(Ruucld, 1.0) 
        Ruucld = max(Ruucld, 0.0) 
      end if

      tau = tau_qv2 + tau_qi2 + tau_qs2 
      tau = Min (tau, TAU_MAX)
      tau = Max (tau, TAU_MIN)

      Tddcld = exp(-tau/solarangle)

      dni_clear = F0 * Tddclr
      dni = dni_clear * Tddcld
      dir = dni * solarangle

      F1 = solarangle*F0*( Tddcld*(Tddclr+Tduclr) + Tducld*Tuuclr )

      Ftotal = F1/(1.0-albdo*(Ruuclr + Ruucld*Tuuclr*Tuuclr))
      ghi = Ftotal
      ghi_clear =  solarangle * F0 * ((Tddclr + Tduclr) / (1.0 - albdo * Ruuclr))

      dif = ghi - dir

    end subroutine farms


    subroutine SUNEARTH( juday, R )

      implicit none

      integer, intent(in) :: juday
      real, intent(out) :: R

      real :: pi, b, R1


      PI = acos(-1.0)

      b = 2.0*PI*juday/365.0
      R1 = 1.00011 + 0.034221*cos(b) + 0.001280*sin(b) + &
           0.000719*cos(2.0*b) +0.000077*sin(2.0*b)

      R = R1**(-0.5)

    end subroutine SUNEARTH


    subroutine CLEARSKYALL( p, albdo, SSA, g, Z, Radius, beta,&
        alpha, ozone, w, Tddclr, Tduclr, Ruuclr, Tuuclr )

        implicit none

        real, intent(in) :: p, albdo, SSA, g, Z, Radius, beta, alpha, ozone, w
        real, intent(out) :: Tddclr, Tduclr, Ruuclr, Tuuclr

          ! Local vars
        integer, parameter :: nangle = 10
        integer :: i
        real :: mu(nangle), angle(nangle), aa(nangle)
        real :: PI


        PI = acos(-1.0)

        do i=1, nangle
          mu(i) = (i-1.0)*0.1+0.1
          angle(i) = acos(mu(i))*180.0/PI
        end do

        do i=1, nangle
            ! PAJ: can use both REST2 and BIRD
          if (USE_REST2) then
            call CLEARSKY (p, albdo, SSA, g, angle(i), Radius, beta,&
            alpha, ozone, w, Tddclr, Tduclr, Ruuclr)
          else
            call BIRD(p, albdo, SSA, g, angle(i), Radius, beta,&
            alpha, ozone, w, Tddclr, Tduclr, Ruuclr)
          end if
          aa(i) = Tddclr
        end do

        Tuuclr = 0.0
        do i=1, nangle
          Tuuclr = Tuuclr + 2.0*mu(i)*aa(i)*0.1
        end do 
 
          ! PAJ: can use both REST2 and BIRD
        if (USE_REST2) then
          call CLEARSKY(p, albdo, SSA, g, Z, Radius, beta,&
          alpha, ozone, w, Tddclr, Tduclr, Ruuclr)
        else
          call BIRD(p, albdo, SSA, g, Z, Radius, beta,&
          alpha, ozone, w, Tddclr, Tduclr, Ruuclr)
        end if
      
    end subroutine CLEARSKYALL 


    subroutine CLEARSKY(p, albdo, SSA, g, Z, Radius, beta,&
        alpha, ozone, w, Tddclr, Tduclr, Ruuclr)

      implicit none

      real, intent(in) :: p, albdo, SSA, g, Z, Radius, beta, alpha, ozone, w
      real, intent(out) :: Tddclr, Tduclr, Ruuclr
     

      return

    end subroutine CLEARSKY


    subroutine BIRD(p, albdo, SSA, g, Z, Radius, beta,&
        alpha, ozone, w, Tddclr, Tduclr, Ruuclr)

        ! This clear-sky model follows the equations given by Bird (1981)
        ! This subroutine for the all-sky fast model is given by
        ! Yu Xie (Yu.Xie@nrel.gov)

      implicit none

      real, intent (in) :: p, albdo, SSA, g, Z, Radius, beta, alpha, ozone, w
      real, intent (out) :: Tddclr, Tduclr, Ruuclr

        ! Local vars
      real :: degrad, airmass, airmassp, T_rayleigh, x0, T_o, T_gases, xw, T_water, &
          tau038, tau050, taua, T_aerosol, T_AA, T_AS, F0, Fdif, Ftotal, Fddclr


      degrad=.017453293d+00
      airmass = 1/(cos(Z*degrad)+0.15*(93.885-Z)**(-1.25))
      airmassp = p*airmass/1013.0

      T_rayleigh = exp(-0.0903*(airmassp**0.84)*(1.0 + airmassp -&
          airmassp**1.01))

      x0 = ozone*airmass
      T_o = 1.0 - 0.1611*x0*(1.0+139.48*x0)**(-0.3035) -&
          0.002715*x0/(1.0+0.044*x0 + 0.0003*x0**2.0)
      T_gases = exp( -0.0127*(airmassp**0.26) )
      xw = w*airmass
      T_water = 1.0 - 2.4959*xw/( (1.0+79.034*xw)**0.6828 + 6.385*xw )

      tau038 = beta*(0.38**(-alpha))
      tau050 = beta*(0.50**(-alpha))
      taua = 0.2758*tau038 + 0.35*tau050

      T_aerosol = exp( -(taua**0.873)*(1.0+taua-taua**0.7088)&
          *(airmass**0.9108) )
      T_AA = 1.0 - 0.1*(1.0 - airmass + airmass**1.06)*&
          (1.0 - T_aerosol)
      T_AS = T_aerosol/T_AA

      Ruuclr = 0.0685 + (1.0-g)*(1.0-T_AS)

      F0 = 1.0
      Fddclr = F0*(cos(Z*degrad))*0.9662*T_rayleigh*T_o*T_gases*T_water*T_aerosol

      Fdif = F0*( cos(Z*degrad) )*0.79*T_o*T_gases*T_water*T_AA*&
          (0.5*(1.0-T_rayleigh) + g*(1.0-T_AS) )&
         /(1.0 - airmass + airmass**1.02 )

      Ftotal = ( Fddclr + Fdif )/(1.0 - albdo*Ruuclr)

      Tddclr = Fddclr/( F0*cos(Z*degrad) )  
      Tduclr = (Ftotal - Fddclr)/(F0*cos(Z*degrad))

    end subroutine BIRD


    subroutine WATERMODEL( tau, De, solarangle, Tducld, Ruucld )
        
      implicit none

      real, intent (in) :: tau, De, solarangle
      real, intent (out) :: Tducld, Ruucld

        ! Local vars
      real :: solarconst, Ptau, PDHI, delta, y, PPDHI


      solarconst = 1385.72180
      Ptau = (2.8850+0.002*(De-60.0))*solarangle-0.007347
      Ptau = max(Ptau, PTAU_MIN)
      PDHI = (1.0+(De-60.0)*0.0002)*1087.24*solarangle**1.1605

      delta = -0.644531*solarangle+1.20117+0.129807/solarangle &
          -0.00121096/(solarangle*solarangle) + &
          1.52587e-07/(solarangle*solarangle*solarangle)
      delta = Max (delta, DELTA_MIN)

      y = 0.012*(tau-Ptau)*solarangle
      PPDHI = (1.0+SINH(y))*PDHI*&
          exp(-( (log10(tau)-log10(Ptau))**2.0 )/delta)

      Tducld = PPDHI/(solarconst*solarangle)

      if (tau .lt. 1.0) then 
        Ruucld = 0.107359*tau
      endif

      if (tau .ge. 1.0) then 
        Ruucld = 1.03 - exp(-(0.5+log10(tau))*(0.5+log10(tau))/3.105 )
      endif

    end subroutine WATERMODEL


    subroutine ICEMODEL( tau, De, solarangle, Tducld, Ruucld )

      implicit none

      real, intent (in) :: tau, De, solarangle
      real, intent (out) :: Tducld, Ruucld

        ! Local vars

      real :: solarconst, Ptau, PDHI, delta, y, PPDHI

      solarconst = 1385.72180

      if ( De .le. 26.0 ) then 
        Ptau = 2.8487*solarangle- 0.0029
      endif
      if ( De .gt. 26.0 ) then 
        Ptau =  (2.8355 + (100.0-De)*0.006)*solarangle - 0.00612
        Ptau = max(Ptau, PTAU_MIN)
      endif

      PDHI = 1047.6367*solarangle**1.0883

      delta = -0.0549531*solarangle+0.617632+0.17876/(solarangle) &
          -0.002174/(solarangle*solarangle) 
      delta = Max (delta, DELTA_MIN)


      y = 0.01*(tau-Ptau)*solarangle
      PPDHI = (1.0+SINH(y))*PDHI*&
          exp(-( (log10(tau)-log10(Ptau))**2.0 )/delta)

      Tducld = PPDHI/(solarconst*solarangle)

      if (tau .lt. 1.0) then 
        Ruucld = 0.094039*tau
      endif

      if (tau .ge. 1.0) then 
        Ruucld = 1.02 - exp(-(0.5+log10(tau))*(0.5+log10(tau))/3.25 )
      endif

    end subroutine ICEMODEL

  end module module_ra_farms



