! module overland_routing_properties_data.F
! Purpose: This module contains the overland_control_struct class. This types holds
! the mass blance variables used in the overland routing code
! National Water Center
! Responsibility: Donald W Johnson donald.w.johnson@noaa.gov
! Authors: Donald W Johnson, Nels Frazier

module overland_mass_balance
implicit none

   ! holds variables used for mass balance
   type overland_mass_balance_struct
      ! mass balance
      
      ! replaced with accumulated_change_in_soil_moisture
      !real(kind=8) :: dsmctot
      ! total difference in soil moisture each timestep
      real(kind=8) :: accumulated_change_in_soil_moisture
      
      ! replaced with pre_soil_mosture_content
      !real(kind=8) :: smctot1     ! NEED VARIABLE INFO
      ! Pre time step soil moisture content accumulator  (mm) TODO verify unit
      real(kind=8) :: pre_soil_moisture_content

      ! replaced with post_soil_moisture_content
      !real(kind=8) :: smctot2     ! NEED VARIABLE INFO
      ! Post time step soil moisture content accumulator (mm) TODO verify unit
      real(kind=8) :: post_soil_moisture_content

      ! replaced with pre_infiltration_excess
      !real(kind=8) :: suminfxs1  ! NEED VARIABLE INFO
      
      ! Pre time step infiltration excess accumulator    (mm) TODO verify unit
      ! FIX ME -- this variable was declared as real(kind=8) as mis match with parameter specification
      ! in Noah_distributed_routing.F:OverlandRouting matched it with parameter specified as real(kind=4)
      ! this caused an implicit cast to single percision float which was required as the value was then used
      ! in the sum_real mpi call which required single percision. Does this variable need to be a double?
      real :: pre_infiltration_excess
      
      ! replaced with post_infiltration_excess
      !real(kind=8) :: suminfxsrt  ! NEED VARIABLE INFO
      ! Post time step infiltration excess accumulator   (mm) TODO verify unit
      ! FIX ME -- this variable was declared as real(kind=8) as mis match with parameter specification
      ! in Noah_distributed_routing.F:OverlandRouting matched it with parameter specified as real(kind=4)
      ! this caused an implicit cast to single percision float which was required as the value was then used
      ! in the sum_real mpi call which required single percision. Does this variable need to be a double?
      real :: post_infiltration_excess
      
   contains
        procedure :: init => overland_mass_balance_init
        procedure :: destroy => overland_mass_balance_destroy
    end type overland_mass_balance_struct

    contains

! initalize the mass balance variables
subroutine overland_mass_balance_init(this)
    implicit none
    class(overland_mass_balance_struct), intent(inout) :: this ! the type object being initalized
    this%accumulated_change_in_soil_moisture = 0.0
    this%pre_soil_moisture_content = 0.0
    this%post_soil_moisture_content = 0.0
    this%pre_infiltration_excess = 0.0
    this%post_infiltration_excess = 0.0
end subroutine overland_mass_balance_init

! none of the mass balance variables are dynamicly allocated if such are added
! they should be deallocated here
subroutine overland_mass_balance_destroy(this)
    implicit none
    class(overland_mass_balance_struct), intent(inout) :: this ! the type object being destroyed
end subroutine overland_mass_balance_destroy


end module overland_mass_balance
