Module module_data_gocart_dust
  INTEGER, PARAMETER :: ndust=5,ndcls=3,ndsrc=1,maxstypes=100
  real, dimension (maxstypes) :: porosity
  REAL :: ch_dust(ndust,12)
  REAL,    PARAMETER :: dyn_visc = 1.5E-5
  real*8, DIMENSION (5), PARAMETER :: den_dust(5)=(/2500.,2650.,2650.,2650.,2650./)
  real*8, DIMENSION (5), PARAMETER :: reff_dust(5)=(/0.73D-6,1.4D-6,2.4D-6,4.5D-6,8.0D-6/)
  INTEGER, DIMENSION (5), PARAMETER :: ipoint(5)=(/3,2,2,2,2/)
  REAL, DIMENSION (5), PARAMETER :: frac_s(5)=(/0.1,0.25,0.25,0.25,0.25/)
END Module module_data_gocart_dust
