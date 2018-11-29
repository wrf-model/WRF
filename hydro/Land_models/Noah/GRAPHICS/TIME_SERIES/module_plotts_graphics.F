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

module module_plotts_graphics

  type pltts_parameter_type
     character(len=256) :: info
     character(len=256) :: fldname
     integer :: soil_level
     real :: latitude
     real :: longitude
     real :: y_interval
     real :: y_minimum
     real :: y_maximum
     character(len=256) :: color
     logical :: callframe
     character(len=10) :: startdate
     character(len=10) :: enddate
     integer :: time_interval
     integer :: count
  end type pltts_parameter_type

  type(pltts_parameter_type) :: pltts_parameter

  type named_color
     character(len=20) :: name
     real :: r
     real :: g
     real :: b
  end type named_color

  logical :: mapped = .FALSE.

  integer :: global_ncols
  type(named_color), dimension(0:255) :: global_colorlist


contains

!=============================================================================================

  integer function color_index(name) result (icol)
    implicit none
    character(len=*), intent(in) :: name
    integer :: i

    if (global_ncols == 0) then
       global_colorlist(global_ncols) = get_x11_color(name)
       call gscr(1, global_ncols, &
            global_colorlist(global_ncols)%r, &
            global_colorlist(global_ncols)%g, &
            global_colorlist(global_ncols)%b)
       icol = global_ncols
       global_ncols = global_ncols + 1
       return
    endif

    do i = 0, global_ncols-1
       if (name == global_colorlist(i)%name) then
          icol = i
          return
       endif
    enddo

    if (global_ncols < 256) then
       global_colorlist(global_ncols) = get_x11_color(name)
       call gscr(1, global_ncols, &
            global_colorlist(global_ncols)%r, &
            global_colorlist(global_ncols)%g, &
            global_colorlist(global_ncols)%b)
       icol = global_ncols
       global_ncols = global_ncols + 1
       return
    else
       stop "Too many colors."
    endif
    
  end function color_index

  type(named_color) function get_x11_color(name) result (c)
    use kwm_string_utilities
    implicit none
    character(len=*) :: name
    integer :: r, g, b, i
    character(len=20) :: cname

    logical, save :: already_read = .FALSE.
    integer :: ierr, idx
    character(len=80) :: string, stringname
    integer, parameter :: iunit = 22

    real, dimension(2500), save :: readlist_r, readlist_g, readlist_b
    character(len=64), dimension(2500), save :: readlist_name
    integer, save :: readlist_count = 0

    if (name(1:1) == "#") then
       read(name(2:7), '(Z2,Z2,Z2)') r,g,b
       c = named_color(trim(name), float(r)/255., float(g)/255., float(b)/255.)
       return
    else if ((name(1:2) == "Ox") .or. (name(1:2) == "0x")) then
       read(name(3:8), '(Z2,Z2,Z2)') r,g,b
       c = named_color(trim(name), float(r)/255., float(g)/255., float(b)/255.)
       return
    endif

    if (.not. already_read) then
       already_read = .TRUE.
       open(iunit, file="/usr/lib/X11/rgb.txt", form='formatted', &
            status='old', action='read', iostat=ierr)
       if (ierr /= 0) then
          open(iunit, file="/usr/share/X11/rgb.txt", form='formatted', &
               status='old', action='read', iostat=ierr)
          if (ierr /= 0) stop "color table"
       endif
       do

          read(iunit, '(A)', iostat=ierr) string
          if (ierr /= 0) exit
          if (string(1:1) == "!") cycle

          stringname = " "
          read(string, *) r, g, b
          ! Find the index of the first alphabetical character
          idx = verify(string, " 0123456789"//char(9)) ! char(9) is tab
          stringname = trim(string(idx:))
          stringname = downcase(trim(stringname))
          stringname = unblank(trim(stringname))
          
          readlist_count = readlist_count + 1
          readlist_r(readlist_count) = float(r)/255.
          readlist_g(readlist_count) = float(g)/255.
          readlist_b(readlist_count) = float(b)/255.
          readlist_name(readlist_count) = stringname

       enddo

       close(iunit)
    endif

    ! Search through readlist for name
    do i = 1, readlist_count
       if (name == readlist_name(i)) then
          cname = name
          c = named_color(cname, readlist_r(i), readlist_g(i), readlist_b(i))
          return
       endif
    enddo

    write(*, '("color not found: ", A)') trim(name)

  end function get_x11_color

!=============================================================================================

  subroutine init_ncargks(new_cgm_name)
! Starts off the NCAR Graphics package.
! Opens workstation 1 as a CGM workstation.
! Opens workstation 2 as a WISS.
! Defines a couple of colors, and sets a couple of NCAR Graphics parameters
! 
! If you want the gmeta file to be called something other than "gmeta",
! give this subroutine the optional argument.
    implicit none
    character(len=*), optional, intent(in) :: new_cgm_name
    character(len=80) :: newname
    character(len=1) :: hdum
    integer :: icol

    call gopks(6,0)

    if (present(new_cgm_name)) then
       newname = trim(new_cgm_name)
       call gesc(-1391, 1, newname, 1, 1, hdum)
    endif

    call gopwk(1,119,1)
    call gopwk(2,120,3)
    call gacwk(1)
    call gacwk(2)
    global_ncols = 0
    
    
    ! call define_color("white")
    ! call define_color("black")
    ! call gscr(1, 0, 1., 1., 1.) ! White
    ! call gscr(1, 1, 0., 0., 0.) ! Black
    icol = color_index("white")
    icol = color_index("black")
    call pcseti("FN", 21)
    call pcsetc("FC", "~")
  end subroutine init_ncargks

  subroutine close_ncargks
    implicit none
    call gdawk(1)
    call gdawk(2)
    call gclwk(1)
    call gclwk(2)
    call gclks
  end subroutine close_ncargks

end module module_plotts_graphics
