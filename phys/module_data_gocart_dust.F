Module module_data_gocart_dust
  INTEGER, PARAMETER :: ndust=5,ndcls=3,ndsrc=1,maxstypes=100
  INTEGER, PARAMETER :: ngsalt=9
! GAC-->
! 20130219 - Drypoint no longer needed.  Use NOAA porosity for all schemes to
! allow use of AFWA dust scheme by all LSMs, not just NOAA and RUC.
!  real, dimension (maxstypes) :: drypoint
!  real, dimension (maxstypes) :: porosity
  real, dimension (19), PARAMETER :: porosity=(/0.339, 0.421, 0.434, 0.476, 0.476, 0.439, &
                                                0.404, 0.464, 0.465, 0.406, 0.468, 0.468, &
                                                0.439, 1.000, 0.200, 0.421, 0.468, 0.200, &
                                                0.339/)
! <--GAC

  REAL :: ch_dust(ndust,12)
  REAL,    PARAMETER :: dyn_visc = 1.5E-5
!  real*8, DIMENSION (5), PARAMETER :: den_dust(5)=(/2500.,2650.,2650.,2650.,2650./)
  real*8, DIMENSION (5)            :: den_dust(5)=(/2500.,2650.,2650.,2650.,2650./)
  real*8, DIMENSION (5), PARAMETER :: reff_dust(5)=(/0.73D-6,1.4D-6,2.4D-6,4.5D-6,8.0D-6/)
  INTEGER, DIMENSION (5), PARAMETER :: ipoint(5)=(/3,2,2,2,2/)
  REAL, DIMENSION (5), PARAMETER :: frac_s(5)=(/0.1,0.25,0.25,0.25,0.25/)

  real*8, DIMENSION (ngsalt), PARAMETER :: reff_salt=(/0.71D-6,1.37D-6,2.63D-6,5.00D-6,9.50D-6,18.1D-6,34.5D-6,65.5D-6,125.D-6/)
  real*8, DIMENSION (ngsalt), PARAMETER :: den_salt=(/2500.,2650.,2650.,2650.,2650.,2650.,2650.,2650.,2650./)
  INTEGER, DIMENSION (ngsalt), PARAMETER :: spoint=(/1,2,2,2,2,2,3,3,3/)  ! 1 Clay, 2 Silt, 3 Sand
  real*8, DIMENSION (ngsalt), PARAMETER :: frac_salt=(/1.,0.2,0.2,0.2,0.2,0.2,0.333,0.333,0.333/)

  real*8, DIMENSION (ndust), PARAMETER :: distr_dust=(/1.074D-1,1.012D-1,2.078D-1,4.817D-1,1.019D-1/)
! real*8, DIMENSION (ndust), PARAMETER :: lo_dust=(/0.1D-6,1.0D-6,1.8D-6,3.0D-6,6.0D-6/)
! real*8, DIMENSION (ndust), PARAMETER :: up_dust=(/1.0D-6,1.8D-6,3.0D-6,6.0D-6,10.0D-6/)

END Module module_data_gocart_dust


