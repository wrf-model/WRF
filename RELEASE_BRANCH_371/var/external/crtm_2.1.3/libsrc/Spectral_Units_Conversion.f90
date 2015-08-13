!
! Spectral_Units_Conversion
!
! Module containing functions to convert between various spectral units.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 14-Jan-2002
!                       paul.vandelst@noaa.gov
!
!

MODULE Spectral_Units_Conversion

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,            ONLY: fp
  USE Fundamental_Constants, ONLY: C => SPEED_OF_LIGHT
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: GHz_to_inverse_cm
  PUBLIC :: inverse_cm_to_GHz
  PUBLIC :: micron_to_inverse_cm
  PUBLIC :: inverse_cm_to_micron


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Spectral_Units_Conversion.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       GHz_to_inverse_cm
!
! PURPOSE:
!       Function to convert frequencies in units of gigahertz (GHz) to units
!       of inverse centimetres (cm^-1)
!
! CALLING SEQUENCE:
!       Wavenumber = GHz_to_inverse_cm( Frequency )
!
! INPUTS:
!       Frequency:      Frequency in gigahertz. Must be > 0.0
!                       UNITS:      GHz
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar or any rank array
!                       ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Wavenumber:     The frequency in inverse centimetres.
!                       Value is 0.0 for an input frequency < or = 0.0.
!                       UNITS:      cm^-1
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Same as input argument.
!
! PROCEDURE:
!       The relationship between wavelength and frequency is given by,
!
!              c
!         l = ---  metres     .....(1)
!              f
!
!       where c = speed of light in m.s^-1
!             f = frequency in Hz (s^-1).
!
!       The conversion of wavelength, l, to frequency, v, in cm^-1, is given by,
!
!                1
!         v = -------  cm^-1     .....(2)
!              100 l
!
!       where the factor of 100 converts l from metres to centimetres.
!       Substituting (2) into (1) gives,
!
!               f
!         v = -------  cm^-1     .....(3)
!              100 c
!
!       If f is expressed as gigahertz, then (3) becomes,
!
!              10^9 f
!         v = -------- cm^-1
!              100  c
!
!                   f
!           = 10^7 --- cm^-1
!                   c
!
!       Therefore the conversion factor from GHz to inverse centimeters is
!       10^7/c where c is in m.s^-1.
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION GHz_to_inverse_cm( Frequency ) RESULT( Wavenumber )
    REAL(fp), INTENT(IN) :: Frequency
    REAL(fp)             :: Wavenumber
    REAL(fp), PARAMETER  :: SCALE_FACTOR = 1.0e+07_fp
    IF ( Frequency < EPSILON(ONE) ) THEN
      Wavenumber = ZERO
      RETURN
    END IF
    Wavenumber = SCALE_FACTOR * Frequency / C
  END FUNCTION GHz_to_inverse_cm



!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       inverse_cm_to_GHz
!
! PURPOSE:
!       Function to convert frequencies in units of inverse centimetres (cm^-1)
!       to units of gigahertz (GHz)
!
! CALLING SEQUENCE:
!       Frequency = inverse_cm_to_GHz( Wavenumber )
!
! INPUTS:
!       Wavenumber:   Frequency in inverse centimetres. Must be > 0.0
!                     UNITS:      cm^-1
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Scalar or any rank array
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Frequency:    The frequency in gigahertz.
!                     Value is 0.0 for an input wavenumber < or = 0.0.
!                     UNITS:      GHz
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Same as input argument
!
! PROCEDURE:
!       The relationship between frequency and wavelength is given by,
!
!              c
!         f = ---  Hz (s^-1)     .....(1)
!              l
!
!       where c = speed of light in m.s^-1
!             l = wavelength in m.
!
!       The conversion of wavelength, l, to frequency, v, in cm^-1, is given by,
!
!                1
!         v = -------  cm^-1     .....(2)
!              100 l
!
!       where the factor of 100 converts l from metres to centimetres.
!       Substituting (2) into (1) gives,
!
!         f = 100 c.v  Hz
!
!           = 10^-9 . 100 c.v  GHz
!
!           = 10^-7 . c.v  GHz
!
!       Therefore the conversion factor from inverse centimeters to GHz is
!       10^-7.c where c is in m.s^-1.
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION inverse_cm_to_GHz( Wavenumber ) RESULT( Frequency )
    REAL(fp), INTENT(IN) :: Wavenumber
    REAL(fp)             :: Frequency
    REAL(fp), PARAMETER  :: SCALE_FACTOR = 1.0e-07_fp
    IF ( Wavenumber < EPSILON(ONE) ) THEN
      Frequency = ZERO
      RETURN
    END IF
    Frequency = SCALE_FACTOR * C * Wavenumber
  END FUNCTION inverse_cm_to_GHz



!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       micron_to_inverse_cm
!
! PURPOSE:
!       Function to convert wavelengths in units of microns (10^-6 m) to 
!       frequencies in units of inverse centimetres (cm^-1).
!
! CALLING SEQUENCE:
!       Wavenumber = micron_to_inverse_cm( Wavelength )
!
! INPUTS:
!       Wavelength:   Wavelength in microns. Must be > 0.0
!                     UNITS:      um (10^-6 m)
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Scalar or any rank array
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Wavenumber:   The frequency in inverse centimetres.
!                     Value is 0.0 for an input wavelength < or = 0.0.
!                     UNITS:      cm^-1
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Same as input argument
!
! PROCEDURE:
!       Given a wavelength of l microns, the frequency, v, in terms of
!       inverse length is its reciprocal, i.e. the number of wavelengths
!       that "fit" within a certain length dimension,
!
!              1 
!         v = --- . 10^6  m^-1
!              l
!
!       where the 10^6 converts the microns to metres. A factor of 100
!       is introduced to produce units of inverse centimetres,
!
!              1     10^6
!         v = --- . ------  m^-1
!              l     10^2
!
!              10^4
!           = ------ cm^-1
!               l
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION micron_to_inverse_cm( Wavelength ) RESULT( Wavenumber )
    REAL(fp), INTENT(IN) :: Wavelength
    REAL(fp)             :: Wavenumber
    REAL(fp), PARAMETER  :: SCALE_FACTOR = 1.0e+04_fp
    IF ( Wavelength < EPSILON(ONE) ) THEN
      Wavenumber = ZERO
      RETURN
    END IF
    Wavenumber = SCALE_FACTOR / Wavelength
  END FUNCTION micron_to_inverse_cm



!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       inverse_cm_to_micron
!
! PURPOSE:
!       Function to convert frequencies in units of inverse centimetres (cm^-1)
!       to wavelengths in units of microns (10^-6 m).
!
! CALLING SEQUENCE:
!       Wavelength = inverse_cm_to_micron( Wavenumber )
!
! INPUTS:
!       Wavenumber:   Frequency in inverse centimetres. Must be > 0.0
!                     UNITS:      cm^-1
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Scalar or any rank array
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Wavelength:   The wavelength in microns.
!                     Value is 0.0 for an input wavenumber < or = 0.0.
!                     UNITS:      um (10^-6 m)
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Same as input argument
!
! PROCEDURE:
!       Given a freqency of v inverse centimetres, the wavelength is its
!       reciprocal,
!
!              1       1
!         l = --- . ------  m
!              v     10^2
!
!       where the 10^2 converts the centimetres to metres. A factor of 10^6
!       is introduced to produce units of microns (10^-6 m),
!
!              1     10^6
!         l = --- . ------  um
!              v     10^2
!
!              10^4
!           = ------ um
!               v
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION inverse_cm_to_micron( Wavenumber ) RESULT( Wavelength )
    REAL(fp), INTENT(IN) :: Wavenumber
    REAL(fp)             :: Wavelength
    REAL(fp), PARAMETER  :: SCALE_FACTOR = 1.0e+04_fp
    IF ( Wavenumber < EPSILON(ONE) ) THEN
      Wavelength = ZERO
      RETURN
    END IF
    Wavelength = SCALE_FACTOR / Wavenumber
  END FUNCTION inverse_cm_to_micron

END MODULE Spectral_Units_Conversion
