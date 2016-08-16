Module module_data_gocart_seas
  real*8, DIMENSION (4), PARAMETER :: ra(4)=(/1.d-1,5.d-1,1.5d0,5.0d0/)
  real*8, DIMENSION (4), PARAMETER :: rb(4)=(/5.d-1,1.5d0,5.d0,1.d1/)
  real*8, DIMENSION (4), PARAMETER :: den_seas(4)=(/2.2d3,2.2d3,2.2d3,2.2d3/)
  real*8, DIMENSION (4), PARAMETER :: reff_seas(4)=(/0.30D-6,1.00D-6,3.25D-6,7.50D-6/)
  real*8,                  PARAMETER :: pi=3.141592653559
  REAL :: ch_ss(4,12)
END Module module_data_gocart_seas
