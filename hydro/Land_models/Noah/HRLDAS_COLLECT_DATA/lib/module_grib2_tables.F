!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

module module_grib2_tables 

  type table_entry_struct
     integer                   :: product_discipline
     integer                   :: parameter_category
     integer                   :: parameter_index
     character(len=256)        :: category_name
     character(len=256)        :: parameter_name
     character(len=256)        :: parameter_units
     type(table_entry_struct), pointer  :: next
  end type table_entry_struct
  type(table_entry_struct), target :: table

contains

!=================================================================================
!=================================================================================

  subroutine grib2_read_parameter_tables
    implicit none
    integer :: iunit
    character(len=200) :: string
    integer :: ierr
    integer, external :: get_unused_unit
    integer :: p1, p2, p3

    integer            :: product_discipline_index
    integer            :: parameter_category_index
    character(len=128) :: parameter_category_name


    integer           :: parameter_index
    character(len=64) :: parameter_name
    character(len=32) :: parameter_units
    character(len=256) :: grib_root
    character(len=256) :: table_filename

    call getenv("GRIB_ROOT", grib_root)
    if (trim(grib_root)==" ") then
       write(*,'("Not finding environment variable GRIB_ROOT.")')
       write(*,'("Program does not know where to find GRIB parameter tables")')
       stop
    endif
    table_filename = trim(grib_root)//"/GRIB2_PARAMETER_TABLE"

    iunit = get_unused_unit()
    nullify(table%next)

    open(iunit, file=trim(table_filename), status='old', form='formatted', action='read', iostat=ierr)
    if (ierr /= 0) then
       write(*,'(/," ***** ERROR *****",/)')
       write(*,'(" ***** Problem opening file ''", A, "''")') trim(table_filename)
       write(*,'(" ***** Does file exist?  Is file readable?",/)')
       stop
    endif

    READ_TABLE : do
       read(iunit, '(A200)', iostat=ierr) string
       if (ierr /= 0) exit READ_TABLE
       if (string(1:1) == "#") cycle READ_TABLE
       if (string(1:1) == "+") then
          !
          ! read table header info, separated by '+' signs
          !
          p1 = index(string, "+")
          p1 = p1 + 1
          p2 = p1 + index(string(p1:), "+")
          p3 = p2 + index(string(p2:), "+")
          read(string(p1:p2-2), *) product_discipline_index
          read(string(p2:p3-2), *) parameter_category_index
          parameter_category_name = trim(adjustl(string(p3:)))
       else
          p1 = index(string, "|") - 1
          p2 = p1 + index(string(p1+2:), "|")
          read(string(1:p1),*) parameter_index
          parameter_name = trim(adjustl(string(p1+2:p2)))
          parameter_units = trim(adjustl(string(p2+2:)))
          ! print*, parameter_index, trim(parameter_name)," ", trim(parameter_units)

          call add_to_growing_table(product_discipline_index, parameter_category_index, parameter_category_name, &
               parameter_index, parameter_name, parameter_units)

       endif
    enddo READ_TABLE
    close(iunit)

  end subroutine grib2_read_parameter_tables

!=================================================================================
!=================================================================================

  subroutine add_to_growing_table(pdi, pci, pcn, pi, pn, pu)
    implicit none
! Input:
!       pdi:  Parameter Discipline index number
!       pci:  Parameter Category index number in discipline <pdi>
!       pi:   Parameter index number in category <pci> and discipline <pdi>
!       pcn:  Parameter category name
!       pn:   Parameter name
!       pu:   Parameter units

! Input
    integer, intent(in) :: pdi
    integer, intent(in) :: pci
    integer, intent(in) :: pi
    character(len=*), intent(in) :: pcn
    character(len=*), intent(in) :: pn
    character(len=*), intent(in) :: pu

! Local
    type(table_entry_struct), pointer:: table_pointer
    
    ! Start us off by pointing to the top of the table.
    table_pointer => table
    
    ! Seek for the end of the table.
    do while (associated(table_pointer%next))
       table_pointer => table_pointer%next
    enddo

    ! Now that we're at the end of the table, create a new entry
    allocate(table_pointer%next)
    table_pointer => table_pointer%next
    nullify(table_pointer%next)

    ! Fill our new entry with the information passed into this subroutine
    table_pointer%product_discipline = pdi
    table_pointer%parameter_category =   pci
    table_pointer%parameter_index    =   pi
    table_pointer%category_name    =   pcn
    table_pointer%parameter_name     =   pn
    table_pointer%parameter_units    =   pu

  end subroutine add_to_growing_table

!=================================================================================
!=================================================================================
  subroutine grib2_clear_parameter_table
    implicit none
    
! Local
    type(table_entry_struct), pointer:: table_pointer
    type(table_entry_struct), pointer:: next_pointer
    
    ! Start us off by pointing to the top of the table.
    table_pointer => table

    do while ( associated(table_pointer%next) )
       next_pointer => table_pointer%next
       if (associated(table_pointer, table)) then
          table_pointer => next_pointer
          nullify(next_pointer)
       else
          nullify(table_pointer%next)
          deallocate(table_pointer)
          table_pointer => next_pointer
          nullify(next_pointer)
       endif
    enddo

    if (associated(table_pointer)) then
       ! deallocate(table_pointer)
       nullify(table_pointer)
    endif
    if (associated(next_pointer)) then
       nullify(next_pointer)
    endif

  end subroutine grib2_clear_parameter_table

!=================================================================================
!=================================================================================

  subroutine get_parameter_table_information(discipline, category, parameter_index, &
       category_name, parameter_name, parameter_units)
    implicit none
    integer, intent(in) :: discipline, category, parameter_index
    character(len=256), intent(out) :: category_name, parameter_name, parameter_units

    type(table_entry_struct), pointer:: table_pointer
    write(category_name, '("Discipline: ", I3, "   Unknown category: ", I3)') discipline, category
    write(parameter_name, '("Unknown parameter: ", I3)') parameter_index
    parameter_units = " "

    table_pointer => table
    do while (associated(table_pointer%next))
       table_pointer => table_pointer%next
       if ( ( table_pointer%product_discipline == discipline ) .and. &
            ( table_pointer%parameter_category == category ) .and. &
            ( table_pointer%parameter_index    == parameter_index) ) then
          ! We found a match
          category_name = table_pointer%category_name
          parameter_name = table_pointer%parameter_name
          parameter_units = table_pointer%parameter_units
          exit
       endif
    enddo

  end subroutine get_parameter_table_information

!=================================================================================
!=================================================================================

  character(len=32) function interpret_time_unit(i) result(string)
    implicit none
    integer, intent(in) :: i

    select case (i)
    case default
       write(string, '("unrecognized time units: ", I4)') i
    case (0)
       string = "minutes"
    case (1)
       string = "hours"
    case (2)
       string = "days"
    case (3)
       string = "months"
    case (4)
       string = "years"
    case (13)
       string = "seconds"
    end select
  end function interpret_time_unit

!=================================================================================
!=================================================================================

  subroutine get_level_string(surface_type_code, text, units)
    implicit none
    integer, intent(in) :: surface_type_code
    character(len=256), intent(out) :: text
    character(len=256), intent(out) :: units

    units = " "
    select case (surface_type_code)
    case default
       write(text,'("Unrecognized type of surface", I4)') surface_type_code
    case (1)
       write(text,'("Level:  Ground or water surface")') 
    case (2)
       write(text,'("Level:  Cloud base level")') 
    case (3)
       write(text,'("Level:  Cloud top level")') 
    case (4)
       write(text,'("Level:  0~S~o~N~C isotherm level")') 
    case (5)
       write(text,'("Level:  Adiabatic lifting condensation level")') 
    case (6)
       write(text,'("Level:  Maximum wind level")') 
    case (7)
       write(text,'("Level:  Tropopause")') 
    case (8)
       write(text,'("Level:  Nominal top of atmosphere")') 
    case (9)
       write(text,'("Level:  Sea bottom")') 
    case (20)
       write(text,'("Level:  Isothermal level ")')
       units = "K"
    case (100)
       write(text,'("Level:  Isobaric surface ")')
       units = "Pa"
    case (101)
       write(text,'("Level:  Mean sea level")') 
    case (102)
       write(text,'("Level:  Height MSL ")')
       units = "m"
    case (103)
       write(text,'("Level:  Height AGL ")')
       units = "m"
    case (104)
       write(text,'("Level:  Sigma level ")')
       units = "sigma value"
    case (105)
       write(text,'("Level:  Hybrid level ")')
    case (106)
       write(text,'("Level:  Depth below land surface ")')
       units = "m"
    case (107)
       write(text,'("Level:  Isentropic level ")')
       units = "K"
    case (108)
       write(text,'("Level:  Specified pressure difference from ground ")')
       units = "Pa"
    case (109)
       write(text,'("Level:  Potential vorticity surface ")')
       units = "Kg m{2} kg{-1} s{-1}"
    case (111)
       write(text,'("Level:  Eta level ")')
    case (117)
       write(text,'("Level:  Mixed-layer depth ")')
       units = "m"
    case (160)
       write(text,'("Level:  Depth below sea-level ")')
       units = "m"
    end select

  end subroutine get_level_string

!=================================================================================
!=================================================================================

end module module_grib2_tables
