      module module_data_mosaic_constants

      use module_data_mosaic_kind, only:  r8

      implicit none


      real(r8), parameter :: pi = 3.14159265358979323846_r8   ! from shr_const_mod.F90 of CESM

      real(r8), parameter :: piover4 = 0.25_r8 * pi

      real(r8), parameter :: piover6 = pi/6.0_r8

      real(r8), parameter :: deg2rad = pi/180.0_r8

      real(r8), parameter :: third = 1.0_r8/3.0_r8

      real(r8), parameter :: avogad = 6.02217e23_r8

      end module module_data_mosaic_constants
