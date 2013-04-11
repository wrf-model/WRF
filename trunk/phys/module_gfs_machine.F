      MODULE MODULE_GFS_MACHINE

      IMPLICIT NONE
      SAVE
!  Machine dependant constants
      integer kind_io4,kind_io8,kind_phys,kind_rad,kind_evod
      parameter (kind_rad  = selected_real_kind(13,60)) ! the '60' maps to 64-bit real
      parameter (kind_phys = selected_real_kind(13,60)) ! the '60' maps to 64-bit real
      parameter (kind_io4  = 4)
      parameter (kind_io8  = 8)
      parameter (kind_evod = 8)

      real(kind=kind_evod) mprec ! machine precision to restrict dep
      parameter(mprec  = 1.e-12 )

      END MODULE MODULE_GFS_MACHINE
