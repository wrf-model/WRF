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

module arguments_module
  implicit none
  private
  public :: arg
  public :: print_help
  character(len=2400), public :: arguments_help

  logical :: adone = .FALSE.
  character(len=120), dimension(0:2000) :: harg
  integer :: numarg

  interface arg
     module procedure &
          argi, argndi,&
          argf,&
          argl,&
          argh, argndh, argnd1h,&
          argsub
  end interface

contains

!==============================================================================

  subroutine argi(string, default, ival)
! Handle the case for optional integer flags,
! i.e., (FLAG, DEFAULT, VALUE) for integers.
    implicit none
    character(len=*) :: string
    integer :: default
    integer :: ival
    integer :: ierr, i, nval

    call initialize

    ival = default

    nval = numarg
    do i = 1, nval
       if (harg(i) == string) then
          read(harg(i+1),*, iostat=ierr) ival
          if (ierr /= 0) then
             call print_help
          endif
          harg(i:numarg-2) = harg(i+2:numarg)
          numarg = numarg - 2
          return
       endif
    enddo

    do i = 1, nval
       if ( (harg(i)(1:len(string)) == string).and.&
            (scan(harg(i)(len(string)+1:),"0123456789") .ne. 0) )then
          read(harg(i)(len(string)+1:),*, iostat=ierr) ival
          if (ierr == 0) then
             harg(i:numarg-1) = harg(i+1:numarg)
             numarg = numarg - 1
             return
          endif
       endif
    enddo

  end subroutine argi

!==============================================================================

  subroutine argndi(string, ival)
! Handle the case of required integer flags,
! i.e., (FLAG, VALUE) for integers.
    implicit none
    character(len=*) :: string
    integer :: ival

    integer :: ierr, i, nval

    call initialize

    nval = numarg
    do i = 1, nval
       if (harg(i) == string) then
          read(harg(i+1),*) ival
          harg(i:numarg-2) = harg(i+2:numarg)
          numarg = numarg - 2
          return
       endif
    enddo

    nval = numarg
    do i = 1, nval

       if ( (harg(i)(1:len(string)) == string).and.&
            (scan(harg(i)(len(string)+1:),"0123456789") .ne. 0) )then
          read(harg(i)(len(string)+1:),*, iostat=ierr) ival
          if (ierr == 0) then
             harg(i:numarg-1) = harg(i+1:numarg)
             numarg = numarg - 1
             return
          endif
       endif
    enddo

    write(*,'(/,"Argument ", A, " MUST be present.",/)') trim(string)
    call print_help
  end subroutine argndi

!==============================================================================

  subroutine argf(string, default, xval)
! Handle the case for optional real flags,
! i.e., (FLAG, DEFAULT, VALUE) for reals.
    implicit none
    character(len=*) :: string
    real :: default
    real :: xval

    integer :: nval, i, ierr

    call initialize

    xval = default

    nval = numarg
    do i = 1, nval
       if (harg(i) == string) then
          read(harg(i+1),*) xval
          harg(i:numarg-2) = harg(i+2:numarg)
          numarg = numarg - 2
          return
       endif
    enddo

    nval = numarg
    do i = 1, nval

       if ( (harg(i)(1:len(string)) == string).and.&
            (scan(harg(i)(len(string)+1:),"0123456789") .ne. 0) )then
          read(harg(i)(len(string)+1:),*, iostat=ierr) xval
          if (ierr == 0) then
             harg(i:numarg-1) = harg(i+1:numarg)
             numarg = numarg - 1
             return
          endif
       endif
    enddo
  end subroutine argf

!==============================================================================

  subroutine argl(string, default, lval)
! Handle the case of optional logical flags,
! i.e., (FLAG, DEFAULT, VALUE) for logicals.
    implicit none
    character(len=*) :: string
    logical :: default
    logical :: lval

    integer :: nval, i, ierr

    call initialize

    lval = default

    nval = numarg
    do i = 1, nval
       if (harg(i) == string) then
          lval = .not.(default)
          harg(i:numarg-1) = harg(i+1:numarg)
          numarg = numarg - 1
          return
       endif
    enddo

  end subroutine argl

!==============================================================================

  subroutine argh(string, default, hval)
! Handle the case of optional flagged string arguments,
! i.e., (FLAG, DEFAULT, VALUE) for strings.
    implicit none
    character(len=*) :: string
    character(len=*) :: default
    character(len=*) :: hval

    integer :: nval, i, ierr

    call initialize

    hval = default

    nval = numarg
    do i = 1, nval
       if (harg(i) == string) then
          hval = harg(i+1)
          harg(i:numarg-2) = harg(i+2:numarg)
          numarg = numarg - 2
          return
       endif
    enddo

    nval = numarg
    do i = 1, nval

       if (harg(i)(1:len(string)) == string) then
          hval = harg(i)(len(string)+1:)
          harg(i:numarg-1) = harg(i+1:numarg)
          numarg = numarg - 1
          return
       endif
    enddo
  end subroutine argh

!==============================================================================

  subroutine argndh(string, hval)
! Handle the case of required flagged string arguments,
! i.e., (FLAG, VALUE) for strings.
!    or
! Handle the case of optional unflagged string arguments.
! i.e., (DEFAULT, VALUE) for strings.

    implicit none
    character(len=*) :: string
    character(len=*) :: hval

    integer :: nval, i, ierr

    call initialize

    if (string(1:1) .eq. "-") then
! STRING is a flag.
! Handle the case of required flagged string arguments

       nval = numarg
       do i = 1, nval
          if (harg(i) == string) then
             if (i == numarg) call print_help
             hval = harg(i+1)
             if (hval(1:1) == '-') call print_help
             harg(i:numarg-2) = harg(i+2:numarg)
             numarg = numarg - 2
             return
          endif
       enddo

       do i = 1, nval

          if (harg(i)(1:len(string)) == string) then
             hval = harg(i)(len(string)+1:)
             harg(i:numarg-1) = harg(i+1:numarg)
             numarg = numarg - 1
             return
          endif
       enddo

       write(*,'(/,"Argument ", A, " MUST be present.",/)') trim(string)
       call print_help
    else
! STRING is a default string value.
! Handle the case of optional unflagged string arguments.
    
       if (numarg > 0) then
          if (harg(1)(1:1) == "-") then
             call print_help
          else
             hval = harg(1)
             harg(1:numarg-1) = harg(2:numarg)
             numarg = numarg - 1
          endif
       else
          hval = string
       endif
    endif

  end subroutine argndh

!==============================================================================
!KWM
!KWM  subroutine arg1h(default, hval)
!KWM! Handle the case of optional unflagged string arguments.
!KWM    implicit none
!KWM    character(len=*) :: default
!KWM    character(len=*) :: hval
!KWM
!KWM    integer :: nval, i, ierr
!KWM
!KWM    call initialize
!KWM
!KWM    if (numarg > 0) then
!KWM       hval = harg(1)
!KWM       harg(1:numarg-1) = harg(2:numarg)
!KWM       numarg = numarg - 1
!KWM    else
!KWM       hval = default
!KWM    endif
!KWM
!KWM  end subroutine arg1h
!KWM
!==============================================================================

  subroutine argnd1h(hval)
! Handle the case of required unflagged string arguments.
    implicit none
    character(len=*) :: hval

    integer :: nval, i, ierr

    call initialize
    
    if (numarg /= 1) then
       write(*,'(/,"A final command-line argument MUST be present.")')
       call print_help
    else
       if (harg(numarg)(1:1) == "-") then
          call print_help
       else
          hval = harg(numarg)
       endif
    endif

  end subroutine argnd1h

!==============================================================================

  subroutine argsub(string, subr)
! Handle the case of a subprogram.
    implicit none
    character(len=*) :: string
    integer :: nval, i
    external subr

    call initialize

    nval = numarg
    do i = 1, nval
       if (harg(i) == string) then
          call subr
       endif
    enddo

  end subroutine argsub
    
!==============================================================================

  subroutine print_help
    write(*,'(/,"Usage: ", A, 1x)', advance="no") trim(harg(0))
    write(*,arguments_help)
    write(*,'(/)')
    stop
  end subroutine print_help

!==============================================================================

  subroutine initialize
    implicit none
    integer :: i
    integer, external :: iargc

    if (.not. adone) then
       numarg = iargc()
       do i = 0, numarg
          call getarg(i, harg(i))
       enddo
       adone = .TRUE.
    endif

  end subroutine initialize

!==============================================================================

end module arguments_module

!KWMprogram test1
!KWM  use arguments_module
!KWM  implicit none
!KWM  integer :: i, j, k
!KWM  real :: x, y, z
!KWM  logical :: g, gg
!KWM  character(len=120) :: h, flnm
!KWM  arguments_help = '("-a i -b b -c c -aa aa -bb bb -cc cc -g -gg -h h flnm")'
!KWM  call arg("-a", i)
!KWM  call arg("-b", 12, k)
!KWM  call arg("-c", 11, j)
!KWM  call arg("-aa", 1.10, x)
!KWM  call arg("-bb", 1.12, y)
!KWM  call arg("-cc", 1.11, z)
!KWM  call arg("-g", .TRUE., g)
!KWM  call arg("-gg", .FALSE., gg)
!KWM  call arg("-h", "Whatever", h)
!KWM  call arg(flnm)
!KWM
!KWM  print*, 'i, j, k = ', i, j, k
!KWM  print*, 'x, y, z = ', x, y, z
!KWM  print*, 'g, gg = ', g, gg
!KWM
!KWM  print*,' h = ', trim(h)
!KWM  print*, 'flnm = ', trim(flnm)
!KWM
!KWMend program test1

