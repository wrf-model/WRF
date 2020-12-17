! module overland_data.F
! Purpose: This module contains the overland struct class and its
!          sub types. This types hold and catagorize the variables used
!          in the overland routing code
! National Water Center
! Responsibility: Donald W Johnson donald.w.johnson@noaa.gov
! Authors: Donald W Johnson, Nels Frazier


module overland_data

    use overland_control
    use overland_stream_and_lake_interface
    use overland_routing_properties
    use overland_mass_balance

    implicit none

    ! hold all variables used in overland routing
    type overland_struct

        type (overland_control_struct), pointer :: control => NULL()                             ! i/o and control variables
        type (overland_stream_and_lake_interface_struct), pointer :: streams_and_lakes => NULL() ! channel and lake related
        type (overland_routing_properties_struct), pointer :: properties => NULL()               ! properties used in routing code
        type (overland_mass_balance_struct), pointer :: mass_balance => NULL()                   ! mass balance variables

        ! unused pointer are in an undefined state
        ! this means the result of calling associated(<pointer>)
        ! on a pointer that has not been set is unknown
        ! therefore associated can not be used as a guard
        ! in inital pointer allocation
        logical, private :: pointer_allocation_guard = .false.

    contains
        procedure :: init => overland_struct_init
        procedure :: destroy => overland_struct_destroy
    end type overland_struct

contains

! this procedure allocates the overland_struct
subroutine overland_struct_init(this,lsm_ix,lsm_jx,rt_ix,rt_jx)
    implicit none
    class(overland_struct), intent(inout) :: this ! the type object being initalized
    integer, intent(in) :: lsm_ix                    ! lsm x grid size
    integer, intent(in) :: lsm_jx                    ! lsm y grid size
    integer, intent(in) :: rt_ix                     ! x grid size
    integer, intent(in) :: rt_jx                     ! y grid size    allocate( thist%control )

    if (this%pointer_allocation_guard .eqv. .false. ) then
        this%pointer_allocation_guard = .true.
        allocate( this%control )
        if ( .not. associated( this%control) ) then
            write(0,*) "Failure to allocate overland control structure"
        else
            call this%control%init(lsm_ix,lsm_jx,rt_ix,rt_jx)
        end if

        ! allocate the streams and lakes structure
        allocate( this%streams_and_lakes )
        if ( .not. associated( this%streams_and_lakes) ) then
            write(0,*) "Failure to allocate overland lakes and streams structure"
        else
            call this%streams_and_lakes%init(rt_ix,rt_jx)
        end if

        ! allocate the properties structure
        allocate( this%properties )
        if ( .not. associated( this%properties) ) then
            write(0,*) "Failure to allocate overland properties structure"
        else
            call this%properties%init(rt_ix,rt_jx)
        end if

        ! allocate the mass balance structure
        allocate( this%mass_balance)
        if ( .not. associated( this%mass_balance) ) then
            write(0,*) "Failure to allocate overland mass balance structure"
        else
            call this%mass_balance%init
        end if
    else
        write(0,*) "Warning: Attempt to double allocated overland_struct (overland_struct_init)"
    end if

end subroutine overland_struct_init

! safely deallocate data from the overland struct
subroutine overland_struct_destroy(this)
    implicit none
    ! call the destructors
    class (overland_struct), intent(inout) :: this

    logical :: status = .true.

    if ( associated(this%control ) ) then
        call this%control%destroy
        deallocate( this%control )
    else
        status = .false.
    end if

    if ( associated(this%streams_and_lakes) )then
        call this%streams_and_lakes%destroy
        deallocate( this%streams_and_lakes )
    else
        status = .false.
    end if

    if ( associated(this%properties) ) then
        call this%properties%destroy
        deallocate( this%properties )
    else
        status = .false.
    end if

    if ( associated(this%mass_balance) ) then
        call this%mass_balance%destroy
        deallocate( this%mass_balance )
    else
        status = .false.
    end if

    this%pointer_allocation_guard = .false.

    if ( status .eqv. .false. ) then
        write(0,*) "Warning: Attempt to double free one or more pointers in (overland_struct_destroy)"
    end if

end subroutine overland_struct_destroy

end module overland_data
