!
! CRTM_Land_Parameters
!
! Module of CRTM land parameter definitions.
!
! CREATION HISTORY:
!       Written by:     David Groff, 17-Jan-2013
!                       david.groff@noaa.gov
!

MODULE CRTM_Land_Parameters

  ! ------------------
  ! Default visibility
  ! ------------------
  PRIVATE
  
  ! ...Number of avaliable soil and vegetation types
  INTEGER, PUBLIC, PARAMETER :: N_VALID_SOIL_TYPES = 8
  INTEGER, PUBLIC, PARAMETER :: N_VALID_VEGETATION_TYPES = 12
  
  ! ...The soil type indices
  INTEGER, PUBLIC, PARAMETER :: INVALID_SOIL    =  0
  INTEGER, PUBLIC, PARAMETER :: COARSE          =  1
  INTEGER, PUBLIC, PARAMETER :: MEDIUM          =  2
  INTEGER, PUBLIC, PARAMETER :: FINE            =  3
  INTEGER, PUBLIC, PARAMETER :: COARSE_MEDIUM   =  4
  INTEGER, PUBLIC, PARAMETER :: COARSE_FINE     =  5
  INTEGER, PUBLIC, PARAMETER :: MEDIUM_FINE     =  6
  INTEGER, PUBLIC, PARAMETER :: COARSE_MED_FINE =  7
  INTEGER, PUBLIC, PARAMETER :: ORGANIC         =  8
  ! ...Soil type names
  CHARACTER(*), PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_SOIL_TYPES ) :: &
    SOIL_TYPE_NAME = (/ 'Invalid soil type', &
                        'Coarse           ', &
                        'Medium           ', &
                        'Fine             ', &
                        'Coarse-Medium    ', &
                        'Coarse-Fine      ', &
                        'Medium-Fine      ', &
                        'Coarse-Med-Fine  ', &
                        'Organic          ' /)
  ! ...The vegetation type indices
  INTEGER, PUBLIC, PARAMETER :: INVALID_VEGETATION             =  0
  INTEGER, PUBLIC, PARAMETER :: BROADLEAF_EVERGREEN_TREES      =  1
  INTEGER, PUBLIC, PARAMETER :: BROADLEAF_DECIDUOUS_TREES      =  2
  INTEGER, PUBLIC, PARAMETER :: BROADLEAF_NEEDLELEAF_TREES     =  3
  INTEGER, PUBLIC, PARAMETER :: NEEDLELEAF_EVERGREEN_TREES     =  4
  INTEGER, PUBLIC, PARAMETER :: NEEDLELEAF_DECIDUOUS_TREES     =  5
  INTEGER, PUBLIC, PARAMETER :: BROADLEAF_TREES_GROUNDCOVER    =  6
  INTEGER, PUBLIC, PARAMETER :: GROUNDCOVER                    =  7
  INTEGER, PUBLIC, PARAMETER :: GROADLEAF_SHRUBS_GROUNDCOVER   =  8
  INTEGER, PUBLIC, PARAMETER :: BROADLEAF_SHRUBS_BARE_SOIL     =  9
  INTEGER, PUBLIC, PARAMETER :: DWARF_TREES_SHRUBS_GROUNDCOVER = 10
  INTEGER, PUBLIC, PARAMETER :: BARE_SOIL                      = 11
  INTEGER, PUBLIC, PARAMETER :: CULTIVATIONS                   = 12
  ! ...Vegetation type names
  CHARACTER(*), PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_VEGETATION_TYPES ) :: &
    VEGETATION_TYPE_NAME=(/'Invalid vegetation surface type         ', &
                           'Broadleaf-Evergreen Trees               ', &
                           'Broadleaf-Deciduous Trees               ', &
                           'Broadleaf & Needleleaf Trees            ', &
                           'Needleleaf-Evergreen Trees              ', &
                           'Needleleaf-Deciduous Trees              ', &
                           'Broadleaf Trees with Groundcover        ', &
                           'Groundcover only                        ', &
                           'Broadlead Shrubs + Perennial Groundcover', &
                           'Broadlead Shrubs with Bare Soil         ', &
                           'Dwarf Trees & Shrubs with Groundcover   ', &
                           'Bare Soil                               ', &
                           'Cultivations                            ' /)
                           
END MODULE CRTM_Land_Parameters
