subroutine da_get_trh( input_file, dim1, dim2, dim3, k, temp, rh )

   !---------------------------------------------------------------------------
   ! Purpose: Calculates T, RH from input WRF file.
   !---------------------------------------------------------------------------

   implicit none

   character(len=200), intent(in)  :: input_file       ! NETCDF file name.
   integer,            intent(in)  :: dim1, dim2, dim3          ! Dimensions.
   integer,            intent(in)  :: k                         ! Model level.
   real,               intent(out) :: temp(1:dim1,1:dim2)       ! Temperature.
   real,               intent(out) :: rh(1:dim1,1:dim2)         ! Relative humidity.

   character(len=10) :: var                       ! Variable to search for. var = "T"
   integer           :: i, j                      ! Loop counters.

   real              :: thetap(1:dim1,1:dim2)     ! Perturbation potential temperature.
   real              :: pb(1:dim1,1:dim2)         ! Base state pressure.
   real              :: pp(1:dim1,1:dim2)         ! Pressure perturbation.
   real              :: x(1:dim1,1:dim2)          ! Vapor mixing ratio.

   real              :: theta                     ! Potential temperature.
   real              :: p                         ! Pressure.
   real              :: q                         ! Specific humidity.
   real              :: t_c                       ! Temp(Celsius).
   real              :: es                        ! Saturation vapor pressure.
   real              :: qs                        ! Saturation specific humidity.

   if (trace_use) call da_trace_entry("da_get_trh")

   var = "T" ! Perturbation potential temperature in WRF.
   call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, thetap)

   var = "PB"  ! Base state pressure in WRF.
   call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, pb)

   var = "P" ! Perturbation pressure in WRF.
   call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, pp)

   var = "QVAPOR"  ! Water vapor mixing ratio.
   call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, x)

   do j = 1, dim2
      do i = 1, dim1
         ! Convert p', theta' to T:
         theta = t0 + thetap(i,j)                 ! Theta = Theta0 + Thetap
         p = pb(i,j) + pp(i,j)                     ! p = base p + pert p.
         temp(i,j) = theta *( p/base_pres)**kappa ! Theta to T.

         ! Convert to specific humidity.
         q = x(i,j) /( 1.0 + x(i,j))

         ! Calculate relative humidity:
         t_c = temp(i,j) - t_kelvin
         es = es_alpha * exp( es_beta * t_c /( t_c + es_gamma))
         qs = rd_over_rv * es /( p - rd_over_rv1 * es)
         rh(i,j) = 100.0 * q / qs
      end do
   end do

   if (trace_use) call da_trace_exit("da_get_trh")

end subroutine da_get_trh


