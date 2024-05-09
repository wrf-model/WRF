! This module defines and instantiates objects
! for a level pool type reservoir's
! parameters/properties. Properties holds
! static/unchanging variables that are
! set when the given reservoir object is
! initialized/instantiated.
module module_levelpool_properties

    use module_reservoir, only: reservoir_properties
    use iso_fortran_env, only: int64
    implicit none

    ! Extend/derive level pool properties from the abstract base
    ! type for reservoir properties.
    type, extends(reservoir_properties) :: levelpool_properties_interface
        real :: lake_area                ! area of lake (km^2)
        real :: weir_elevation           ! bottom of weir elevation (meters AMSL)
        real :: weir_coeffecient         ! weir coefficient
        real :: weir_length              ! weir length (meters)
        real :: dam_length               ! dam length (meters)
        real :: orifice_elevation        ! orifice elevation (meters AMSL)
        real :: orifice_coefficient      ! orifice coefficient
        real :: orifice_area             ! orifice area (meters^2)
        real :: max_depth                ! max depth of reservoir before overtop (meters)
        integer(kind=int64) :: lake_number           ! lake number

    contains

        procedure :: init => levelpool_properties_init
        procedure :: destroy => levelpool_properties_destroy

    end type levelpool_properties_interface

contains

    !Level Pool Properties Constructor
    subroutine levelpool_properties_init(this, lake_area, &
        weir_elevation, weir_coeffecient, weir_length, dam_length, orifice_elevation, &
        orifice_coefficient, orifice_area, max_depth, lake_number)
        implicit none
        class(levelpool_properties_interface), intent(inout) :: this ! the type object being initialized
        real, intent(in)    :: lake_area      	        ! area of lake (km^2)
        real, intent(in)    :: weir_elevation           ! bottom of weir elevation (meters AMSL)
        real, intent(in)    :: weir_coeffecient         ! weir coefficient
        real, intent(in)    :: weir_length              ! weir length (meters)
        real, intent(in)    :: dam_length               ! dam length (meters)
        real, intent(in)    :: orifice_elevation        ! orifice elevation (meters AMSL)
        real, intent(in)    :: orifice_coefficient      ! orifice coefficient
        real, intent(in)    :: orifice_area             ! orifice area (meters^2)
        real, intent(in)    :: max_depth                ! max depth of reservoir before overtop (meters)
        integer(kind=int64), intent(in) :: lake_number              ! lake number

        ! Assign the values passed in to a particular level pool reservoir
        ! properties object's variables.
        this%lake_area = lake_area
        this%weir_elevation = weir_elevation
        this%weir_coeffecient = weir_coeffecient
        this%weir_length = weir_length
        this%orifice_elevation = orifice_elevation
        this%orifice_coefficient = orifice_coefficient
        this%orifice_area = orifice_area
        this%max_depth = max_depth
        this%lake_number = lake_number
        this%dam_length = dam_length

    end subroutine levelpool_properties_init

    !Level Pool Properties Destructor
    subroutine levelpool_properties_destroy(this)
        implicit none
        class(levelpool_properties_interface), intent(inout) :: this ! the type object being destroyed
    end subroutine levelpool_properties_destroy

end module module_levelpool_properties
