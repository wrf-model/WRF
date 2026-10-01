! This code is part of RRTM for GCM Applications - Parallel (RRTMGP)
!
! Contacts: Robert Pincus and Eli Mlawer
! email:  rrtmgp@aer.com
!
! Copyright 2015-,  Atmospheric and Environmental Research,
! Regents of the University of Colorado, Trustees of Columbia University.  All right reserved.
!
! Use and duplication is permitted under the terms of the
!    BSD 3-clause license, see http://opensource.org/licenses/BSD-3-Clause
! -------------------------------------------------------------------------------------------------
!>
!> Routines for handling strings:
!>
!>   - convert to lower case
!>   - does a string exist within an array of strings?
!>   - what is the location of a string with an array?
!>
! -------------------------------------------------------------------------------------------------
module mo_gas_optics_util_string
  implicit none
  private
  public :: lower_case, string_in_array, string_loc_in_array

  ! List of character for case conversion
  character(len=26), parameter :: LOWER_CASE_CHARS = 'abcdefghijklmnopqrstuvwxyz'
  character(len=26), parameter :: UPPER_CASE_CHARS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

contains
  ! -------------------------------------------------------------------------------------------------
  !> Convert stringo to lower case 
  pure function lower_case( input_string ) result( output_string )
    character(len=*), intent(in) :: input_string
    character(len=len(input_string)) :: output_string
    integer :: i, n

    ! Copy input string
    output_string = input_string

    ! Convert case character by character
    do i = 1, len(output_string)
      n = index(UPPER_CASE_CHARS, output_string(i:i))
      if ( n /= 0 ) output_string(i:i) = LOWER_CASE_CHARS(n:n)
    end do
  end function
  ! --------------------------------------------------------------------------------------
  !
  !> Is string somewhere in array?
  !
  pure function string_in_array(string, array)
    character(len=*),               intent(in) :: string
    character(len=*), dimension(:), intent(in) :: array
    logical                                    :: string_in_array

    integer :: i
    character(len=len_trim(string)) :: lc_string

    string_in_array = .false.
    lc_string = lower_case(trim(string))
    do i = 1, size(array)
      if(lc_string == lower_case(trim(array(i)))) then
        string_in_array = .true.
        exit
      end if
    end do
  end function string_in_array
  ! --------------------------------------------------------------------------------------
  !
  !> what is the location of a string in an array 
  !
  pure function string_loc_in_array(string, array)
    character(len=*),               intent(in) :: string
    character(len=*), dimension(:), intent(in) :: array
    integer                                    :: string_loc_in_array

    integer :: i
    character(len=len_trim(string)) :: lc_string

    string_loc_in_array = -1
    lc_string = lower_case(trim(string))
    do i = 1, size(array)
      if(lc_string == lower_case(trim(array(i)))) then
        string_loc_in_array = i
        exit
      end if
    end do
  end function string_loc_in_array
  ! --------------------------------------------------------------------------------------
end module
