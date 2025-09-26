#ifndef _ACCEL
      integer :: ncol_,nlayers_,nbndlw_,ngptlw_
! changed to arguments for thread safety
# ifndef ncol_
#   define ncol_ CHNK
# endif
      integer  :: ngsd(nbndlw)      

! Atmosphere
      real :: taucmcd(ncol_, ngptlw_, nlayers_+1)
   
      real , dimension(ncol_, 0:nlayers_+1) :: pzd      ! level (interface) pressures (hPa, mb)
                                                        !    Dimensions: (ncol,0:nlayers)
      real , dimension(ncol_) :: pwvcmd                 ! precipitable water vapor (cm)
                                                        !    Dimensions: (ncol)
      real , dimension(ncol_,nbndlw_) :: semissd        ! lw surface emissivity
                                                        !    Dimensions: (ncol,nbndlw)
      real , dimension(ncol_,nlayers_+1,nbndlw_) :: planklayd    ! 
                                                        !    Dimensions: (ncol,nlayers+1,nbndlw)
      real , dimension(ncol_,0:nlayers_+1,nbndlw_) :: planklevd    ! 
                                                        !    Dimensions: (ncol,0:nlayers+1,nbndlw)
      real, dimension(ncol_,nbndlw_) :: plankbndd       ! 
                                                        !    Dimensions: (ncol,nbndlw)
   
      real :: gurad(ncol_,ngptlw_,0:nlayers_+1)         ! upward longwave flux (w/m2)
      real :: gdrad(ncol_,ngptlw_,0:nlayers_+1)         ! downward longwave flux (w/m2)
      real :: gclrurad(ncol_,ngptlw_,0:nlayers_+1)      ! clear sky upward longwave flux (w/m2)
      real :: gclrdrad(ncol_,ngptlw_,0:nlayers_+1)      ! clear sky downward longwave flux (w/m2)

      real  :: gdtotuflux_dtd(ncol_, ngptlw_, 0:nlayers_+1) ! change in upward longwave flux (w/m1/k)
                                     ! with respect to surface temperature

      real  :: gdtotuclfl_dtd(ncol_, ngptlw_, 0:nlayers_+1) ! change in clear sky upward longwave flux (w/m2/k)
                                     ! with respect to surface temperature

! Clouds
      integer  :: idrvd                       ! flag for calculation of dF/dt from 
                                                      ! Planck derivative [0=off, 1=on]
      real  :: bpaded
      real  :: heatfacd
      real  :: fluxfacd
      real  :: a0d(nbndlw_), a1d(nbndlw_), a2d(nbndlw_)
      real  :: delwaved(nbndlw_)
      real :: totufluxd(ncol_, 0:nlayers_+1)     ! upward longwave flux (w/m2)
      real :: totdfluxd(ncol_, 0:nlayers_+1)     ! downward longwave flux (w/m2)
      real :: fnetd(ncol_, 0:nlayers_+1)         ! net longwave flux (w/m2)
      real :: htrd(ncol_, 0:nlayers_+1)          ! longwave heating rate (k/day)
      real :: totuclfld(ncol_, 0:nlayers_+1)     ! clear sky upward longwave flux (w/m2)
      real :: totdclfld(ncol_, 0:nlayers_+1)     ! clear sky downward longwave flux (w/m2)
      real :: fnetcd(ncol_, 0:nlayers_+1)        ! clear sky net longwave flux (w/m2)
      real :: htrcd(ncol_, 0:nlayers_+1)         ! clear sky longwave heating rate (k/day)
      real :: dtotuflux_dtd(ncol_, 0:nlayers_+1) ! change in upward longwave flux (w/m2/k)
                                                       ! with respect to surface temperature
      real :: dtotuclfl_dtd(ncol_, 0:nlayers_+1) ! change in clear sky upward longwave flux (w/m2/k)
                                                       ! with respect to surface temperature
      real :: dplankbnd_dtd(ncol_,nbndlw_) 
# undef ncol_
#endif
