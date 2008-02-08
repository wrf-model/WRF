Module module_data_gocart_seas
  real*8, DIMENSION (4), PARAMETER :: ra(4)=(/0.1,0.5,1.5,5.0/)
  real*8, DIMENSION (4), PARAMETER :: rb(4)=(/0.5,1.5,5.0,10.0/)
  real*8, DIMENSION (4), PARAMETER :: den_seas(4)=(/2200.,2200.,2200.,2200./)
  real*8, DIMENSION (4), PARAMETER :: reff_seas(4)=(/0.30D-6,1.00D-6,3.25D-6,7.50D-6/)
  real,                  PARAMETER :: pi=3.141592653559
  REAL :: ch_ss(4,12)
END Module module_data_gocart_seas
