#define WRF_PORT
#define MODAL_AERO
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov

module radconstants

! This module contains constants that are specific to the radiative transfer
! code used in the CAM3 model.

use shr_kind_mod,   only: r8 => shr_kind_r8
#ifndef WRF_PORT
  use abortutils,     only: endrun
#else
  use module_cam_support,    only: endrun
#endif

implicit none
private

! public routines

   public :: get_number_sw_bands
   public :: get_sw_spectral_boundaries
   public :: get_ref_solar_band_irrad
   public :: get_true_ref_solar_band_irrad
   public :: get_ref_total_solar_irrad
   public :: get_solar_band_fraction_irrad
   public :: radconstants_init
   public :: rad_gas_index

! optics files specify a type.  What length is it?
integer, parameter, public :: ot_length = 32

! SHORTWAVE DATA

! number of shorwave spectral intervals
integer, parameter, public :: nswbands = 19 

integer, parameter, public :: idx_sw_diag = 8 ! index to sw visible band
integer, parameter, public :: idx_lw_diag = 2 ! index to (H20 window) LW band


! Number of evenly spaced intervals in rh
! The globality of this mesh may not be necessary
! Perhaps it could be specific to the aerosol
! But it is difficult to see how refined it must be
! for lookup.  This value was found to be sufficient
! for Sulfate and probably necessary to resolve the
! high variation near rh = 1.  Alternative methods
! were found to be too slow.
! Optimal approach would be for cam to specify size of aerosol
! based on each aerosol's characteristics.  Radiation 
! should know nothing about hygroscopic growth!
integer, parameter, public :: nrh = 1000  

! LONGWAVE DATA

! number of lw bands
integer, public, parameter  :: nlwbands = 7
! Index of volc. abs., H2O non-window
integer, public, parameter :: idx_LW_H2O_NONWND=1
! Index of volc. abs., H2O window
integer, public, parameter :: idx_LW_H2O_WINDOW=2
! Index of volc. cnt. abs. 0500--0650 cm-1
integer, public, parameter :: idx_LW_0500_0650=3
! Index of volc. cnt. abs. 0650--0800 cm-1
integer, public, parameter :: idx_LW_0650_0800=4
! Index of volc. cnt. abs. 0800--1000 cm-1
integer, public, parameter :: idx_LW_0800_1000=5
! Index of volc. cnt. abs. 1000--1200 cm-1
integer, public, parameter :: idx_LW_1000_1200=6
! Index of volc. cnt. abs. 1200--2000 cm-1
integer, public, parameter :: idx_LW_1200_2000=7

! GASES TREATED BY RADIATION (line spectrae)

! gasses required by radiation
integer, public, parameter :: gasnamelength = 5
integer, public, parameter :: nradgas = 8
character(len=gasnamelength), public, parameter :: gaslist(nradgas) &
   = (/'H2O  ','O3   ', 'O2   ', 'CO2  ', 'N2O  ', 'CH4  ', 'CFC11', 'CFC12'/)

! what is the minimum mass mixing ratio that can be supported by radiation implementation?
real(r8), public, parameter :: minmmr(nradgas) &
   = epsilon(1._r8)

! Solar and SW data for CAMRT

   ! minimum wavelength of band in micrometers
   real(r8), public, parameter :: wavmin(nswbands) = &
        (/   .200_r8,    .245_r8,    .265_r8,    .275_r8,    .285_r8, &
             .295_r8,  .305_r8,    .350_r8,    .640_r8,    .700_r8,    .701_r8, &
             .701_r8,  .701_r8,    .701_r8,    .702_r8,    .702_r8, &
            2.630_r8, 4.160_r8,   4.160_r8/)

   real(r8), public, parameter :: wavmin_true(nswbands) = &
        (/   .200_r8,    .245_r8,    .265_r8,    .275_r8,    .285_r8, &
             .295_r8,  .305_r8,    .350_r8,    .640_r8,    .700_r8,    .700_r8, &
             .700_r8,  .700_r8,    .700_r8,    .700_r8,    .700_r8, &
            2.630_r8, 4.160_r8,   4.160_r8/)

   ! maximum wavelength of band in micrometers
   real(r8), public, parameter :: wavmax(nswbands) = &
        (/   .245_r8,  .265_r8,    .275_r8,    .285_r8,    .295_r8, &
             .305_r8,  .350_r8,    .640_r8,    .700_r8,   5.000_r8,   5.000_r8, &
            5.000_r8, 5.000_r8,   5.000_r8,   5.000_r8,   5.000_r8, &
            2.860_r8, 4.550_r8,   4.550_r8/)

   ! Fraction of solar flux in each stated spectral interval
   real(r8), public, parameter :: frcsol(nswbands) = &
     (/ .001488_r8, .001389_r8, .001290_r8, .001686_r8, .002877_r8, &
        .003869_r8, .026336_r8, .360739_r8, .065392_r8, .526861_r8, &
        .526861_r8, .526861_r8, .526861_r8, .526861_r8, .526861_r8, &
        .526861_r8, .006239_r8, .001834_r8, .001834_r8/)

   ! Weight of h2o in spectral interval
   real(r8), public, parameter :: ph2o(nswbands) = &
             (/    .000_r8,    .000_r8,    .000_r8,    .000_r8,    .000_r8, &
        .000_r8,   .000_r8,    .000_r8,    .000_r8,    .505_r8,     &
        .210_r8,   .120_r8,    .070_r8,    .048_r8,    .029_r8,     &
        .018_r8,   .000_r8,    .000_r8,    .000_r8/)

   ! Weight of co2 in spectral interval
   real(r8), public, parameter :: pco2(nswbands) = &
             (/    .000_r8,    .000_r8,    .000_r8,    .000_r8,    .000_r8, &
        .000_r8,   .000_r8,    .000_r8,    .000_r8,    .000_r8,     &
        .000_r8,   .000_r8,    .000_r8,    .000_r8,    .000_r8,     &
        .000_r8,  1.000_r8,    .640_r8,    .360_r8/)

   ! Weight of o2  in spectral interval
   real(r8), public, parameter :: po2(nswbands) = &
             (/    .000_r8,    .000_r8,    .000_r8,    .000_r8,    .000_r8, &
        .000_r8,   .000_r8,    .000_r8,   1.000_r8,   1.000_r8,     &
        .000_r8,   .000_r8,    .000_r8,    .000_r8,    .000_r8,     &
        .000_r8,   .000_r8,    .000_r8,    .000_r8/)

   real(r8) :: solfrac_true(nswbands)

contains


!------------------------------------------------------------------------------
subroutine get_number_sw_bands(number_of_bands)
   ! number of solar (shortwave) bands in the radiation code
   integer, intent(out) :: number_of_bands

   number_of_bands = nswbands

end subroutine get_number_sw_bands

!------------------------------------------------------------------------------
subroutine get_sw_spectral_boundaries(low_boundaries, high_boundaries, units)
   ! provide spectral boundaries of each shortwave band

   real(r8), intent(out) :: low_boundaries(nswbands), high_boundaries(nswbands)
   character(*), intent(in) :: units ! requested units

   select case (units)
   case ('inv_cm','cm^-1','cm-1')
      low_boundaries = 1.e4_r8/wavmax
      high_boundaries = 1.e4_r8/wavmin_true
   case('m')
      low_boundaries = 1.e-6_r8*wavmin_true
      high_boundaries = 1.e-6_r8*wavmax
   case('nm')
      low_boundaries = 1.e3_r8*wavmin_true
      high_boundaries = 1.e3_r8*wavmax
   case('micrometer','micron','um')
      low_boundaries = wavmin_true
      high_boundaries = wavmax
   case default
      call endrun('rad_constants.F90: spectral units not acceptable'//units)
   end select

end subroutine get_sw_spectral_boundaries

!------------------------------------------------------------------------------
subroutine get_ref_solar_band_irrad( band_irrad )

   ! solar irradiance in each band (W/m^2)
   real(r8), intent(out) :: band_irrad(nswbands)

   band_irrad = frcsol

end subroutine get_ref_solar_band_irrad

!------------------------------------------------------------------------------
subroutine radconstants_init()
! The last bands are implemented as scalings to the solar flux
! so the corresponding actual flux applied to the heating
! is different from the solar in that band.  These are the
! actual solar flux applied to each band

   integer :: ns
   real(r8):: psf(nswbands)      !  scaled fractional solar spectrum in each band applied to unitary heating

   do ns = 1, nswbands
      psf(ns) = 1.0_r8
      if(ph2o(ns)/=0._r8) psf(ns) = psf(ns)*ph2o(ns)
      if(pco2(ns)/=0._r8) psf(ns) = psf(ns)*pco2(ns)
      if(po2 (ns)/=0._r8) psf(ns) = psf(ns)*po2 (ns)
      solfrac_true(ns)   = frcsol(ns)*psf(ns) 
    enddo

end subroutine radconstants_init


!------------------------------------------------------------------------------
subroutine get_true_ref_solar_band_irrad( solfrac_true_out )

   ! solar irradiance in each band (W/m^2)

   real(r8), intent(out) :: solfrac_true_out(nswbands)

   solfrac_true_out(:) = solfrac_true(:)

end subroutine get_true_ref_solar_band_irrad

!------------------------------------------------------------------------------
subroutine get_ref_total_solar_irrad(tsi)
   ! provide Total Solar Irradiance assumed by radiation

   real(r8), intent(out) :: tsi
   real(r8) :: solfrac_true(nswbands)

   call get_true_ref_solar_band_irrad( solfrac_true )
   tsi = sum(solfrac_true)

end subroutine get_ref_total_solar_irrad

!------------------------------------------------------------------------------
subroutine get_solar_band_fraction_irrad(fractional_irradiance)
   ! provide fractional solar irradiance in each band

   ! fraction of solar irradiance in each band
   real(r8), intent(out) :: fractional_irradiance(1:nswbands)
   real(r8) :: tsi ! total solar irradiance

   fractional_irradiance = frcsol

end subroutine get_solar_band_fraction_irrad

!------------------------------------------------------------------------------
integer function rad_gas_index(gasname)

   ! return the index in the gaslist array of the specified gasname

   character(len=*),intent(in) :: gasname
   integer :: igas

   rad_gas_index = -1
   do igas = 1, nradgas
      if (trim(gaslist(igas)).eq.trim(gasname)) then
         rad_gas_index = igas
         return
      endif
   enddo
   call endrun ("rad_gas_index: can not find gas with name "//gasname)
end function rad_gas_index

end module radconstants
