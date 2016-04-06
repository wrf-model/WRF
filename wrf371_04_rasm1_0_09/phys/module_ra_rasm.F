MODULE module_ra_rasm

!    This subroutine created by Andrew Roberts June 2013 to pass variables
!    and types required to synchronize CPL and WRF orbital parameters.

#ifdef CESMCOUPLED

     USE shr_kind_mod
     USE shr_orb_mod

     implicit none

     real(SHR_KIND_R8) :: eccen       ! Eccentricity of orbit
     real(SHR_KIND_R8) :: obliqr      ! Obliquity in radians
     real(SHR_KIND_R8) :: lambm0      ! Long of perh. radians
     real(SHR_KIND_R8) :: mvelpp      ! Locatn of vernal equinox at perihelion 
     real(SHR_KIND_R8) :: DECLIN_SHR  ! SHR_KIND_R8 DECLIN

     real(SHR_KIND_R8) :: nextsw_cday ! calendar of next atm shortwave 
     real(SHR_KIND_R8) :: RADTIM_SHR  ! SHR_KIND_R8 TIME OF RADIATION CALCULATION 

     real(SHR_KIND_R8) :: XXLONG_SHR  ! SHR_KIND_R8 XXLONG
     real(SHR_KIND_R8) :: XXLAT_SHR   ! SHR_KIND_R8 XXLAT

#endif

END MODULE module_ra_rasm
