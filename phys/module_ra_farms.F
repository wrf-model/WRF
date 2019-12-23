  module module_ra_farms

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !                                                 !
    ! Purpose: Couples the FARMS model to WRF         !
    !                                                 !
    ! Author: Yu Xie coded the FARMS model            !
    !         Pedro A. Jimenez coupled FARMS to WRF.  !
    !                                                 !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use module_model_constants, only : G

    real, parameter :: ONE_OVER_G = 1.0 / G
    real, parameter :: THREE_OVER_TWO = 3.0 / 2.0
    logical, parameter :: DEBUG = .false.
    integer, parameter :: TAU_ICE_METHOD = 1
    logical, parameter :: USE_REST2 = .false.

    contains

    subroutine Farms_driver (ims, ime, jms, jme, its, ite, jts, jte, kms, kme, kts, kte,  &
           p8w, albedo, aer_opt, aerssa2d, aerasy2d, aod5502d, angexp2d,  &
           coszen_loc, qv, qi, qs, qc, re_cloud, re_ice, re_snow,    &
           swdown, swddir, swddni, swddif, &
           julian, mut, dnw, swdown2, swddir2, swddni2, swddif2)

      implicit None

      
      integer, intent(in) :: ims, ime, jms, jme, its, ite, jts, jte, kms, kme, &
          kts, kte

      integer, intent(in) :: aer_opt
      real,    intent(in) :: julian

      real, dimension(ims:ime, jms:jme), intent(in) :: albedo, coszen_loc, &
          aerssa2d, aerasy2d, aod5502d, angexp2d, mut
      real, dimension(kms:kme), intent(in) :: dnw

      real, dimension(ims:ime, kms:kme, jms:jme ), intent(in) :: qv, qi, qs, qc, &
          p8w, re_cloud, re_ice, re_snow 

      real, dimension(ims:ime,jms:jme), intent(inout) :: swddir2, swdown2, &
                                                         swddif2, swddni2
      real, dimension(ims:ime,jms:jme), intent(in) :: swddir, swdown, &
                                                         swddif, swddni

        ! Local
      integer :: i, j
      real    :: tau_qv, tau_qi, tau_qs, pmw, swp, iwp, lwp, beta
      real    :: re_cloud_path, re_ice_path, re_snow_path, q_aux

      j_loop: do j = jts, jte
         i_loop: do i = its, ite
           daytime_if: if (coszen_loc(i, j) < 0.0 ) then
             swdown2(i, j) = 0.0
             swddni2(i, j) = 0.0
             swddir2(i, j) = 0.0
             swddif2(i, j) = 0.0
           else
             if (DEBUG) print *, ' '
               ! PMW
             pmw = ONE_OVER_G * integrate_1var (mut(i, j), dnw, qv(i, :, j), kms, kme, kts, kte)
             if (DEBUG) print *, 'PMW = ', pmw

               ! Calc effective radius water
             q_aux = integrate_1var (mut(i, j), dnw, qc(i, :, j), kms, kme, kts, kte)
             lwp = ONE_OVER_G * q_aux
             if (DEBUG) print *, 'LWP = ', lwp

             if (q_aux > 0.0) then
               re_cloud_path = integrate_2var (mut(i, j), dnw, qc(i, :, j), &
                   re_cloud(i, :, j), kms, kme, kts, kte)
               re_cloud_path = re_cloud_path / q_aux
             else
               re_cloud_path = 0.0
             end if
             if (DEBUG) print *, 're cloud = ', re_cloud_path

               ! Calc effective radius ice
             q_aux = integrate_1var (mut(i, j), dnw, qi(i, :, j), kms, kme, kts, kte)
             iwp = ONE_OVER_G * q_aux
             if (DEBUG) print *, 'IWP = ', iwp

             if (q_aux > 0.0) then
               re_ice_path = integrate_2var (mut(i, j), dnw, qi(i, :, j), &
                   re_ice(i, :, j), kms, kme, kts, kte)
               re_ice_path = re_ice_path / q_aux
             else
               re_ice_path = 0.0
             end if
             if (DEBUG) print *, 're ice = ', re_ice_path

               ! Calc effective radius snow
             q_aux = integrate_1var (mut(i, j), dnw, qs(i, :, j), kms, kme, kts, kte)
             swp = ONE_OVER_G * q_aux
             if (DEBUG) print *, 'SWP = ', swp

             if (q_aux > 0.0) then
               re_snow_path = integrate_2var (mut(i, j), dnw, qs(i, :, j), &
                   re_snow(i, :, j), kms, kme, kts, kte)
               re_snow_path = re_snow_path / q_aux
             else
               re_snow_path = 0.0
             end if
             if (DEBUG) print *, 're snow = ', re_snow_path

               ! optical thickness  water
             if (re_cloud_path > 0.0) then
               tau_qv = THREE_OVER_TWO * lwp / re_cloud_path / 1000.0
             else
               tau_qv = 0.0
             end if
             if (DEBUG) print *, 'Tau water = ', tau_qv

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
             if (DEBUG) print *, 'Tau ice = ', tau_qi

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
             if (DEBUG) print *, 'Tau snow = ', tau_qs

             beta = aod5502d(i, j) * (1000.0/ 550.0) ** (- angexp2d(i, j))

             Call Farms (p8w(i, 1, j), albedo(i, j), aerssa2d(i, j), &
                 aerasy2d(i, j), coszen_loc(i, j), beta,   &
                 angexp2d(i, j), pmw, tau_qv, tau_qi, tau_qs,        &
                 re_cloud_path, re_ice_path, re_snow_path, int(julian),   &
                 swdown(i, j), swddir(i, j), swddni(i, j), swddif(i, j), &
                 swdown2(i, j), swddni2(i, j), swddif2(i, j), swddir2(i, j))

          end if daytime_if
        end do i_loop
      end do j_loop

    end subroutine Farms_driver


    function integrate_1var(mut_val, dnw, var1_1d, kms, kme, kts, kte) &
        result (return_value)
      
      implicit none

      real, intent(in) :: mut_val
      integer, intent(in) :: kts, kte, kms, kme
      real, dimension(kms:kme), intent(in) :: var1_1d, dnw

        ! Local
      real :: return_value
      integer :: k

      return_value = 0.0
      do k = kts, kte - 1
        return_value = return_value + var1_1d(k) * dnw(k)
      end do
      return_value = - mut_val * return_value

    end function integrate_1var


    function integrate_2var(mut_val, dnw, var1_1d, var2_1d, kms, kme, kts, kte) &
        result (return_value)

      implicit none

      real, intent(in) :: mut_val
      integer, intent(in) :: kts, kte, kms, kme
      real, dimension(kms:kme), intent(in) :: var1_1d, var2_1d, dnw

        ! Local
      real :: return_value
      integer :: k

      return_value = 0.0
      do k = kts, kte - 1
        return_value = return_value + var1_1d(k) * var2_1d(k) * dnw(k)
      end do
      return_value = - mut_val * return_value

    end function integrate_2var


    subroutine FARMS (p_pa, albdo, ssa, g, solarangle, beta, alpha, w_mm, &
                      tau_qv, tau_qi, tau_qs, re_cloud_path_m, re_ice_path_m, &
                      re_snow_path_m, juday, ghi_wrf, dir_wrf, dni_wrf, dif_wrf,  ghi, dni, dif, dir)

  
    real, intent(in) :: p_pa, albdo, ssa, g, solarangle, beta, alpha, w_mm, &
        tau_qv, tau_qi, tau_qs, re_cloud_path_m, re_ice_path_m, re_snow_path_m
    integer, intent(in) :: juday
    real, intent(out) :: ghi, dni, dir, dif
    real, intent(in) :: ghi_wrf, dni_wrf, dir_wrf, dif_wrf


!!!!!! This Fast All-sky Radiation Model for Solar applications (FARMS) was developed by
!!!!!! Yu Xie (Yu.Xie@nrel.gov). Please contact him for more
!!!!!! information.

! Input values
! p: surface air pressure (mbar)
! albdo: surface albedo
! SSA: single-scattering albedo for aerosol
! g: asymmetric factor of aerosol
! Z: solar zenith angle 
! beta: Angstrom turbidity coeff., i.e. AOD at 1000 nm.
! alpha: Angstrom wavelength exponent
!        For a cloudy sky, the suggested values are beta=0.1 and alpha=1.2,
!        unless you have more accurate measurements.
! ozone: ozone amount (1000DU, i.e. 0.4=400DU)
! w: PWV (cm)
! phase: cloud thermodynamic phase (1=water, 2=ice)
! juday: day of year (1-366)
! tau: cloud optical thickness
! De: Cloud effective particle size (micron) 
!     (For a water cloud, De=2*effective radius)
!     (For an ice cloud, the defination of De follows Fu. 1996.
!     Another useful defination is given by "Determination of ice cloud models using MODIS and MISR data" ) 

        PI = acos(-1.0)

        p = p_pa / 100.0
        ozone = 0.265
        w = w_mm / 10.0

        de_cloud = 2.0 * re_cloud_path_m / 1.0e-6
        de_ice = 2.0 * re_ice_path_m / 1.0e-6
        de_snow = 2.0 * re_snow_path_m / 1.0e-6
        if (DEBUG) print *, 'de_cloud = ', de_cloud
        if (DEBUG) print *, 'de_ice = ', de_ice
        if (DEBUG) print *, 'de_snow = ', de_snow

  ! PAJ:
        Z = acos(solarangle) * 180.0 / PI

        if (DEBUG) print *, 'Pressure ', p
        if (DEBUG) print *, 'Albedo ', albdo
        if (DEBUG) print *, 'ssa'    , ssa
        if (DEBUG) print *, 'g', g
        if (DEBUG) print *, 'Z', Z
        if (DEBUG) print *, 'w', w
        if (DEBUG) print *, 'beta', beta
        if (DEBUG) print *, 'alpha', alpha
        if (DEBUG) print *, 'solarangle', solarangle
        if (DEBUG) print *, 'juday', juday

        call SUNEARTH( juday, Radius )
        F0 = 1361.2/(Radius*Radius)

        call CLEARSKYALL(p, albdo, SSA, g, Z, Radius, beta,&
          alpha, ozone, w, Tddclr, Tduclr, Ruuclr, Tuuclr)

        if (de_cloud > 0.0) then
          Call Watermodel (tau_qv, de_cloud, solarangle, Tducld_water, Ruucld_water)
        else
          Tducld_water = 0.0
          Ruucld_water = 0.0
        end if

        if (de_ice > 0.0) then
          call Icemodel (tau_qi, de_ice, solarangle, Tducld_ice, Ruucld_ice)
        else
          Tducld_ice = 0.0
          Ruucld_ice = 0.0
        end if

        if (de_snow > 0.0) then
          call Icemodel (tau_qs, de_snow, solarangle, Tducld_snow, Ruucld_snow)
        else
          Tducld_snow = 0.0
          Ruucld_snow = 0.0
        end if

        if (DEBUG) print *, 'Transmiss water = ', Tducld_water
        if (DEBUG) print *, 'Transmiss ice = ', Tducld_ice
        if (DEBUG) print *, 'Transmiss snow = ', Tducld_snow

        if (DEBUG) print *, 'Reflec water = ', Ruucld_water
        if (DEBUG) print *, 'Reflec ice = ', Ruucld_ice
        if (DEBUG) print *, 'Reflec snow = ', Ruucld_snow

        Tducld = 1.0 - (1.0 - Tducld_water) * (1.0 - Tducld_ice) * (1.0 - Tducld_snow)
        Tducld = min(Tducld, 1.0)
        Tducld = max(Tducld, 0.0)
        if (DEBUG) print *, 'Transmiss mixed phase cloud =', Tducld

        Ruucld = 1.0 - (1.0 - Ruucld_water) * (1.0 - Ruucld_ice) * (1.0 - Ruucld_snow) 
        Ruucld = min(Ruucld, 1.0) 
        Ruucld = max(Ruucld, 0.0) 
        if (DEBUG) print *, 'Reflec mixed phase cloud =', Ruucld

        tau = tau_qv + tau_qi + tau_qs 
        if (DEBUG) print *, 'Tau mixed phase cloud = ', tau

        Tddcld = exp(-tau/solarangle)

        Fd = solarangle*F0*Tddcld*Tddclr
        dir = Fd
        dni = dir / solarangle

        F1 = solarangle*F0*( Tddcld*(Tddclr+Tduclr) + Tducld*Tuuclr )

        Ftotal = F1/(1.0-albdo*(Ruuclr + Ruucld*Tuuclr*Tuuclr))
        ghi = Ftotal
        if (DEBUG) print *, 'F0 =', F0
        if (DEBUG) print *, 'F1 =', F1
        if (DEBUG) print *, 'Albedo =', albdo
        if (DEBUG) print *, 'Ruuclr =', Ruuclr
        if (DEBUG) print *, 'Tuuclr =', Tuuclr
        if (DEBUG) print *, 'Reflection factor = ', (1.0-albdo*(Ruuclr + Ruucld*Tuuclr*Tuuclr))

        dif = ghi - dir

        if (DEBUG) print *, 'dni = ', dni, 'dni WRF =', dni_wrf
        if (DEBUG) print *, 'dir = ', dir, 'dir WRF =', dir_wrf
        if (DEBUG) print *, 'dif = ', dif, 'dif WRF =', dif_wrf
        if (DEBUG) print *, 'ghi = ', ghi, 'ghi WRF =', ghi_wrf

    end subroutine farms

        subroutine SUNEARTH( juday, R )

        PI = acos(-1.0)

        b = 2.0*PI*juday/365.0
        R1 = 1.00011 + 0.034221*cos(b) + 0.001280*sin(b) + &
             0.000719*cos(2.0*b) +0.000077*sin(2.0*b)

        R = R1**(-0.5)
        end subroutine SUNEARTH


        subroutine CLEARSKYALL( p, albdo, SSA, g, Z, Radius, beta,&
        alpha, ozone, w, Tddclr, Tduclr, Ruuclr, Tuuclr )

        parameter( nangle=10 )
        real   mu(nangle), angle(nangle), aa(nangle)
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

        return

      end subroutine CLEARSKY


       subroutine BIRD(p, albdo, SSA, g, Z, Radius, beta,&
      alpha, ozone, w, Tddclr, Tduclr, Ruuclr)

! This clear-sky model follows the equations given by Bird (1981)
! This subroutine for the all-sky fast model is given by
! Yu Xie (Yu.Xie@nrel.gov)


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
        
        solarconst = 1385.72180
        Ptau = (2.8850+0.002*(De-60.0))*solarangle-0.007347
        PDHI = (1.0+(De-60.0)*0.0002)*1087.24*solarangle**1.1605

        delta = -0.644531*solarangle+1.20117+0.129807/solarangle &
        -0.00121096/(solarangle*solarangle) + &
        1.52587e-07/(solarangle*solarangle*solarangle)

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

        solarconst = 1385.72180

        if ( De .le. 26.0 ) then 
        Ptau = 2.8487*solarangle- 0.0029
        endif
        if ( De .gt. 26.0 ) then 
        Ptau =  (2.8355 + (100.0-De)*0.006)*solarangle - 0.00612
        endif

        PDHI = 1047.6367*solarangle**1.0883

        delta = -0.0549531*solarangle+0.617632+0.17876/(solarangle) &
        -0.002174/(solarangle*solarangle) 

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



