      module module_data_mosaic_kind

      implicit none

!     integer, parameter :: r8 = 8
      integer, parameter :: r8 = selected_real_kind(12) ! 8 byte real

!     integer, parameter :: r4 = 4
      integer, parameter :: r4 = selected_real_kind( 6) ! 4 byte real

      end module module_data_mosaic_kind
