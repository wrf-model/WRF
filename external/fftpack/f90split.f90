program main

!*****************************************************************************80
!
!! MAIN is the main program for F90SPLIT.
!
!  Discussion:
!
!    F90SPLIT splits the modules of a FORTRAN file into separate files.
!
!    A "module" is a blockdata, function, module, program, subroutine,
!    recursive function or recursive subroutine program subunit.
!
!    The command
!
!      f90split extract.f90
!
!    processes the file EXTRACT.F90 line by line.  Each program subunit
!    that is found is written to a separate file whose name is derived
!    from the name of the program subunit.  If the program subunit does
!    not have a name, a default name is assigned.
!
!    The program should be able to split multiple files with a
!    single command, as in:
!
!      f90split *.f90
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 August 2011
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) iarg
  integer ( kind = 4 ) iargc
  character ( len = 255 ) input_file
  integer ( kind = 4 ) numarg

  call timestamp ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'F90SPLIT:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Split a FORTRAN90 program, so that each'
  write ( *, '(a)' ) '  unit is in its own file.'
!
!  Count the number of command line arguments.
!
  numarg = iargc ( )

  if ( numarg < 1 ) then

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'F90SPLIT:'
    write ( *, '(a)' ) '  What is the name of the input file?'
    read ( *, '(a)' ) input_file

    if ( input_file == ' ' ) then
      stop
    end if

    numarg = 1
    call handle ( input_file )

  else

    do iarg = 1, numarg

      call getarg ( iarg, input_file )

      call handle ( input_file )

    end do

  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  Number of files handled = ', numarg
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'F90SPLIT:'
  write ( *, '(a)' ) '  Normal end of execution.'

  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end
subroutine handle ( input_file )

!*****************************************************************************80
!
!! HANDLE handles one file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 August 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) INPUT_FILE, the name of the file to
!    be split up.
!
  implicit none

  integer ( kind = 4 ) duplicate_num
  character ( len = 255 ) extension
  logical f90_line_is_end
  integer ( kind = 4 ) i
  character ( len = 255 ) input_file
  integer ( kind = 4 ) input_unit
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) j
  character ( len = 255 ) line
  integer ( kind = 4 ) line_length
  integer ( kind = 4 ) line_length_loc
  integer ( kind = 4 ) line_length_max
  integer ( kind = 4 ) line_num
  integer ( kind = 4 ) module_num
  logical no_name
  character ( len = 255 ) no_name_file
  integer ( kind = 4 ) no_name_line_last
  integer ( kind = 4 ) no_name_line_num
  logical no_name_open
  integer ( kind = 4 ) no_name_unit
  logical output_exists
  character ( len = 255 ) output_file
  logical output_open
  integer ( kind = 4 ) output_unit

  duplicate_num = 0
!
!  Pick off the extension of the input file.
!
  call file_ext ( input_file, i, j )

  if ( i == 0 ) then
    extension = '.f'
  else if ( 1 < i ) then
    extension = input_file(i-1:j)
  else
    extension = ' '
  end if
!
!  Open the file.
!
  call get_unit ( input_unit )

  open ( unit = input_unit, file = input_file, status = 'old', &
    iostat = ios )

  if ( ios /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'F90SPLIT - Fatal error!'
    write ( *, '(a)' ) &
      '  Could not open the input file "' // trim ( input_file ) // '".'
    stop
  end if

  line_num = 0
  no_name_line_last = -1
  no_name_line_num = 0
  module_num = 0

  output_open = .false.

  no_name_open = .false.
  no_name = .true.
  no_name_file = 'no_name.f90'

  line_length_max = -1;
  line_length_loc = -1;

  do

    read ( input_unit, '(a)', iostat = ios ) line

    if ( ios /= 0 ) then
      exit
    end if

    line_num = line_num + 1

    line_length = len_trim ( line )
    if ( line_length_max < line_length ) then
      line_length_max = line_length
      line_length_loc = line_num
    end if
!
!  If we don't have a module name, then it's not clear what to do.
!  My vote is to discard the information for now.
!
!  It's important to check whether the next line marks the beginning of
!  a named module.
!
    if ( no_name ) then

      call f90_line_is_begin ( line, output_file )

      if ( output_file /= ' ' ) then

        no_name = .false.

        module_num = module_num + 1

        call s_cat ( output_file, extension, output_file )

        write ( *, '(a)' ) trim ( output_file )

        call get_unit ( output_unit )

        open ( unit = output_unit, file = output_file, &
          status = 'replace', iostat = ios )

        output_open = .true.

      end if

    end if
!
!  If an output file is not currently open...
!
    if ( .not. output_open ) then

      call f90_line_is_begin ( line, output_file )

      if ( output_file == ' ' ) then

        no_name = .true.

        if ( .not. no_name_open ) then
          write ( *, '(a)' ) trim ( no_name_file )
          call get_unit ( no_name_unit )
          open ( unit = no_name_unit, file = no_name_file, status = 'replace', &
            iostat = ios )
          no_name_open = .true.
        end if

      else

        module_num = module_num + 1

        no_name = .false.
        call s_cat ( output_file, extension, output_file )
        call s_low ( output_file )
        write ( *, '(a)' ) trim ( output_file )
!
!  Check for duplicates
!
        inquire ( file = output_file, exist = output_exists )

        if ( output_exists ) then
          duplicate_num = duplicate_num + 1
          write ( *, '(a)' ) '  Duplicate module = "' &
            // trim ( output_file ) // '".'
        end if

        call get_unit ( output_unit )

        open ( unit = output_unit, file = output_file, status = 'replace', &
          iostat = ios )

        output_open = .true.

      end if

    end if
!
!  Write the line.
!
    if ( output_open ) then
      write ( output_unit, '(a)' ) trim ( line )
    else
      write ( no_name_unit, '(a)' ) trim ( line )
      no_name_line_last = line_num
      no_name_line_num = no_name_line_num + 1
    end if

    if ( f90_line_is_end ( line ) ) then
      close ( unit = output_unit )
      output_open = .false.
      no_name = .false.
    end if

  end do
!
!  Close the NO_NAME file, and delete it.
!  Rationale:
!
!    1) I don't write main programs without a PROGRAM statement.
!    2) I don't stick blank or comment lines between routines.
!    3) The stupid ALPHA fortran compiler will FAIL if given
!       a file to compile that contains only blanks and comments!
!
  if ( no_name_open ) then
    close ( unit = no_name_unit, status = 'delete' )
    no_name_open = .false.
  end if

  close ( unit = input_unit )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'F90SPLIT:'
  write ( *, '(a)' ) '  Reached end of ' // trim ( input_file )
  write ( *, '(a,i8)' ) '  Lines read:              ', line_num
  write ( *, '(a,i8)' ) '  Longest line length:     ', line_length_max
  write ( *, '(a,i8)' ) '  Longest line location:   ', line_length_loc
  write ( *, '(a,i8)' ) '  Named modules created:   ', module_num
  write ( *, '(a,i8)' ) '  Lines sent to NO_NAME:   ', no_name_line_num
  if ( 0 < no_name_line_num ) then
    write ( *, '(a,i8)' ) '  Last NO_NAME line:       ', no_name_line_last
  end if
  write ( *, '(a,i8)' ) '  Duplicate modules found: ', duplicate_num

  return
end
subroutine ch_cap ( c )

!*****************************************************************************80
!
!! CH_CAP capitalizes a single character.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character C, the character to capitalize.
!
  implicit none

  character c
  integer ( kind = 4 ) itemp

  itemp = ichar ( c )

  if ( 97 <= itemp .and. itemp <= 122 ) then
    c = char ( itemp - 32 )
  end if

  return
end
subroutine ch_low ( ch )

!*****************************************************************************80
!
!! CH_LOW lowercases a single character.
!
!  Discussion:
!
!    Instead of CHAR and ICHAR, we now use the ACHAR and IACHAR functions,
!    which guarantee the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character CH, the character to be lowercased.
!
  implicit none

  character              ch
  integer   ( kind = 4 ) i

  i = iachar ( ch )

  if ( 65 <= i .and. i <= 90 ) then
    ch = achar ( i + 32 )
  end if

  return
end
subroutine digit_to_ch ( digit, c )

!*****************************************************************************80
!
!! DIGIT_TO_CH returns the character representation of a decimal digit.
!
!  Example:
!
!    DIGIT   C
!    -----  ---
!      0    '0'
!      1    '1'
!    ...    ...
!      9    '9'
!     17    '*'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIGIT, the digit value between 0 and 9.
!
!    Output, character C, the corresponding character, or '*' if DIGIT
!    was illegal.
!
  implicit none

  character              c
  integer   ( kind = 4 ) digit

  if ( 0 <= digit .and. digit <= 9 ) then

    c = char ( digit + 48 )

  else

    c = '*'

  end if

  return
end
subroutine f90_line_is_begin ( line, name )

!*****************************************************************************80
!
!! F90_LINE_IS_BEGIN determines if a line begins a FORTRAN90 routine.
!
!  Discussion:
!
!    This routine will NOT properly handle complicated function
!    statements such as:
!
!      integer ( kind = 4 )*2 function fred ( a, b )
!      recursive real function bob ( c )
!
!    For that matter, if you are so bold as to have a variable whose
!    name is "PROGRAM", "FUNCTION" or a similar "keyword", then this
!    routine will incorrectly treat lines such as:
!
!      function = function + 1
!
!    The routine will also fail if the initial line of the module
!    extends over more than one line:
!
!      recursive double precision fun&
!      ction naomi ( x )
!
!    or if you use some nonstandard keyword such as
!
!      parallel function magoo ( y )
!
!    14 December 2002: This routine was, for convenience and style,
!    lowercasing the line and hence the output name.  I now find that
!    I want to preserve case, so I modified the routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 January 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) LINE, a line of text.
!
!    Output, character ( len = * ) NAME, the name of the module, if this
!    line begins a module, and ' ' otherwise.
!
  implicit none

  integer ( kind = 4 ) i
  character ( len = * ) line
  character ( len = 255 ) line2
  character ( len = * ) name
  logical s_eqi

  name = ' '

  line2 = line
  call s_blank_delete ( line2 )

  if ( s_eqi ( line2(1:9), 'blockdata' ) ) then
    if ( line2(10:) == ' ' ) then
      name = 'blockdata'
    else
      call s_before_ss_copy ( line2(10:), '(', name )
    end if
  else if ( s_eqi ( line2(1:17), 'characterfunction' ) ) then
    call s_before_ss_copy ( line2(18:), '(', name )
  else if ( s_eqi ( line2(1:15), 'complexfunction' ) ) then
    call s_before_ss_copy ( line2(16:), '(', name )
  else if ( s_eqi ( line2(1:23), 'doubleprecisionfunction' ) ) then
    call s_before_ss_copy ( line2(24:), '(', name )
  else if ( s_eqi ( line2(1:8), 'function' ) ) then
    call s_before_ss_copy ( line2(9:), '(', name )
  else if ( s_eqi ( line2(1:15), 'integerfunction' ) ) then
    call s_before_ss_copy ( line2(16:), '(', name )
  else if ( s_eqi ( line2(1:15), 'logicalfunction' ) ) then
    call s_before_ss_copy ( line2(16:), '(', name )
  else if ( s_eqi ( line2(1:6), 'module' ) ) then
    call s_before_ss_copy ( line2(7:), '(', name )
  else if ( s_eqi ( line2(1:7), 'program' ) ) then
    call s_before_ss_copy ( line2(8:), '(', name )
  else if ( s_eqi ( line2(1:12), 'realfunction' ) ) then
    call s_before_ss_copy ( line2(13:), '(', name )
  else if ( s_eqi ( line2(1:17), 'recursivefunction' ) ) then
    call s_before_ss_copy ( line2(18:), '(', name )
  else if ( s_eqi ( line2(1:10), 'subroutine' ) ) then
    call s_before_ss_copy ( line2(11:), '(', name )
  else if ( s_eqi ( line2(1:19), 'recursivesubroutine' ) ) then
    call s_before_ss_copy ( line2(20:), '(', name )
  end if
!
!  In some "clever" cases, people write the name of the routine
!  on one line, continue with an ampersand, and the rest of the
!  routine follows.
!
!  I really should be reading the logical line, not the literal
!  line, but for now, let me just chop off trailing ampersands.
!
  i = index ( name, '&' )

  if ( i /= 0 ) then
    name(i:i) = ' '
  end if

  return
end
function f90_line_is_end ( line )

!*****************************************************************************80
!
!! F90_LINE_IS_END determines if a line ends a FORTRAN90 module.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) LINE, a line of text.
!
!    Output, logical F90_LINE_IS_END, TRUE if the line ends a module.
!
  implicit none

  logical f90_line_is_end
  character ( len = * ) line
  character ( len = 255 ) line2

  f90_line_is_end = .false.

  line2 = line

  call s_low ( line2 )

  call s_blank_delete ( line2 )

  if ( &
    line2       == 'end' .or. &
    line2(1:12) == 'endblockdata' .or. &
    line2(1:11) == 'endfunction' .or. &
    line2(1:9)  == 'endmodule' .or. &
    line2(1:10) == 'endprogram' .or. &
    line2(1:13) == 'endsubroutine' .or. &
    line2(1:4)  == 'end!' ) then

    f90_line_is_end = .true.

  end if

  return
end
subroutine file_ext ( file_name, i, j )

!*****************************************************************************80
!
!! FILE_EXT determines the "extension" of a file name.
!
!  Discussion:
!
!    The "extension" of a filename is the string of characters
!    that appears after the LAST period in the name.  A file
!    with no period, or with a period as the last character
!    in the name, has a "null" extension.
!
!    Blanks are unusual in filenames.  This routine ignores all
!    trailing blanks, but will treat initial or internal blanks
!    as regular characters acceptable in a file name.
!
!  Example:
!
!    FILE_NAME  I  J
!
!    bob.for    5  7
!    N.B.C.D    7  7
!    Naomi.     0  0
!    Arthur     0  0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 February 2000
!
!  Parameters:
!
!    Input, character ( len = * ) FILE_NAME, a file name to be examined.
!
!    Output, integer ( kind = 4 ) I, J, the indices of the first and last 
!    characters in the file extension.
!
!    If at least one period occurs in the filename, and at least one
!    nonblank character follows that period, then I will be the index
!    of the first character after the period, and J the index of the
!    last nonblank character after the period.  The extension is
!    therefore equal to FILE_NAME(I:J).
!
!    Otherwise, I and J will be returned as 0, indicating that the file
!    has no extension.
!
  implicit none

  character ( len = * ) file_name
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) s_index_last

  i = s_index_last ( file_name, '.' )

  if ( i /= 0 ) then

    j = len_trim ( file_name )

    if ( i == j ) then
      i = 0
      j = 0
    else
      i = i + 1
    end if

  else

    j = 0

  end if

  return
end
subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  logical lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

  return
end
subroutine s_before_ss_copy ( s, ss, s2 )

!*****************************************************************************80
!
!! S_BEFORE_SS_COPY copies a string up to a given substring.
!
!  Discussion:
!
!    S and S2 can be the same object, in which case the string is
!    overwritten by a copy of itself up to the substring, followed
!    by blanks.
!
!  Example:
!
!    Input:
!
!      S = 'ABCDEFGH'
!      SS = 'EF'
!
!    Output:
!
!      S2 = 'ABCD'.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be copied.
!
!    Input, character ( len = * ) SS, the substring before which the copy stops.
!
!    Output, character ( len = * ) S2, the copied portion of S.
!
  implicit none

  integer ( kind = 4 ) last
  integer ( kind = 4 ) last_s2
  character ( len = * ) s
  character ( len = * ) s2
  character ( len = * ) ss
!
!  Find the first occurrence of the substring.
!
  last = index ( s, ss )
!
!  If the substring doesn't occur at all, behave as though it begins
!  just after the string terminates.
!
!  Now redefine LAST to point to the last character to copy before
!  the substring begins.
!
  if ( last == 0 ) then
    last = len ( s )
  else
    last = last - 1
  end if
!
!  Now adjust again in case the copy holder is "short".
!
  last_s2 = len ( s2 )

  last = min ( last, last_s2 )
!
!  Copy the beginning of the string.
!  Presumably, compilers now understand that if LAST is 0, we don't
!  copy anything.
!  Clear out the rest of the copy.
!
  s2(1:last) = s(1:last)
  s2(last+1:last_s2) = ' '

  return
end
subroutine s_blank_delete ( s )

!*****************************************************************************80
!
!! S_BLANK_DELETE removes blanks from a string, left justifying the remainder.
!
!  Discussion:
!
!    All TAB characters are also removed.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
  implicit none

  character c
  integer ( kind = 4 ) iget
  integer ( kind = 4 ) iput
  character ( len = * ) s
  character TAB

  TAB = char ( 9 )
  iput = 0

  do iget = 1, len ( s )

    c = s(iget:iget)

    if ( c /= ' ' .and. c /= TAB ) then
      iput = iput + 1
      s(iput:iput) = c
    end if

  end do

  s(iput+1:) = ' '

  return
end
subroutine s_blanks_delete ( s )

!*****************************************************************************80
!
!! S_BLANKS_DELETE replaces consecutive blanks by one blank.
!
!  Discussion:
!
!    The remaining characters are left justified and right padded with blanks.
!    TAB characters are converted to spaces.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) nchar
  character newchr
  character oldchr
  character ( len = * ) s
  character TAB

  nchar = len ( s )
  TAB = char ( 9 )
  j = 0
  newchr = ' '

  do i = 1, nchar

    oldchr = newchr
    newchr = s(i:i)

    if ( newchr == TAB ) then
      newchr = ' '
    end if

    s(i:i) = ' '

    if ( oldchr /= ' ' .or. newchr /= ' ' ) then
      j = j + 1
      s(j:j) = newchr
    end if

  end do

  return
end
subroutine s_cap ( s )

!*****************************************************************************80
!
!! S_CAP replaces any lowercase letters by uppercase ones in a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
  implicit none

  character c
  integer ( kind = 4 ) i
  character ( len = * ) s

  do i = 1, len ( s )

    c = s(i:i)
    call ch_cap ( c )
    s(i:i) = c

  end do

  return
end
subroutine s_cat ( s1, s2, s3 )

!*****************************************************************************80
!
!! S_CAT concatenates two strings to make a third string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, the "prefix" string.
!
!    Input, character ( len = * ) S2, the "postfix" string.
!
!    Output, character ( len = * ) S3, the string made by
!    concatenating S1 and S2, ignoring any trailing blanks.
!
  implicit none

  character ( len = * ) s1
  character ( len = * ) s2
  character ( len = * ) s3

  s3 = trim ( s1 ) // trim ( s2 )

  return
end
function s_eqi ( s1, s2 )

!*****************************************************************************80
!
!! S_EQI is a case insensitive comparison of two strings for equality.
!
!  Example:
!
!    S_EQI ( 'Anjana', 'ANJANA' ) is .TRUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, S2, the strings to compare.
!
!    Output, logical S_EQI, the result of the comparison.
!
  implicit none

  character c1
  character c2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) len1
  integer ( kind = 4 ) len2
  integer ( kind = 4 ) lenc
  logical s_eqi
  character ( len = * ) s1
  character ( len = * ) s2

  len1 = len ( s1 )
  len2 = len ( s2 )
  lenc = min ( len1, len2 )

  s_eqi = .false.

  do i = 1, lenc

    c1 = s1(i:i)
    c2 = s2(i:i)
    call ch_cap ( c1 )
    call ch_cap ( c2 )

    if ( c1 /= c2 ) then
      return
    end if

  end do

  do i = lenc + 1, len1
    if ( s1(i:i) /= ' ' ) then
      return
    end if
  end do

  do i = lenc + 1, len2
    if ( s2(i:i) /= ' ' ) then
      return
    end if
  end do

  s_eqi = .true.

  return
end
function s_index_last ( string, sub )

!*****************************************************************************80
!
!! S_INDEX_LAST finds the LAST occurrence of a given substring.
!
!  Discussion:
!
!    It returns the location in STRING at which the substring SUB is
!    first found, or 0 if the substring does not occur at all.
!
!    The routine is also trailing blank insensitive.  This is very
!    important for those cases where you have stored information in
!    larger variables.  If STRING is of length 80, and SUB is of
!    length 80, then if STRING = 'FRED' and SUB = 'RED', a match would
!    not be reported by the standard FORTRAN INDEX, because it treats
!    both variables as being 80 characters long!  This routine assumes that
!    trailing blanks represent garbage!
!
!    This means that this routine cannot be used to find, say, the last
!    occurrence of a substring 'A ', since it assumes the blank space
!    was not specified by the user, but is, rather, padding by the
!    system.  However, as a special case, this routine can properly handle
!    the case where either STRING or SUB is all blanks.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) STRING, the string to be searched.
!
!    Input, character ( len = * ) SUB, the substring to search for.
!
!    Output, integer ( kind = 4 ) S_INDEX_LAST.  0 if SUB does not occur in
!    STRING.  Otherwise S_INDEX_LAST = I, where STRING(I:I+LENS-1) = SUB,
!    where LENS is the length of SUB, and is the last place
!    this happens.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) llen1
  integer ( kind = 4 ) llen2
  integer ( kind = 4 ) s_index_last
  character ( len = * ) string
  character ( len = * ) sub

  s_index_last = 0

  llen1 = len_trim ( string )
  llen2 = len_trim ( sub )
!
!  In case STRING or SUB is blanks, use LEN
!
  if ( llen1 == 0 ) then
    llen1 = len ( string )
  end if

  if ( llen2 == 0 ) then
    llen2 = len ( sub )
  end if

  if ( llen1 < llen2 ) then
    return
  end if

  do j = 1, llen1+1-llen2

    i = llen1 + 2 - llen2 - j

    if ( string(i:i+llen2-1) == sub ) then
      s_index_last = i
      return
    end if

  end do

  return
end
function s_indexi ( s, sub )

!*****************************************************************************80
!
!! S_INDEXI is a case-insensitive INDEX function.
!
!  Discussion:
!
!    The function returns the location in the string at which the
!    substring SUB is first found, or 0 if the substring does not
!    occur at all.
!
!    The routine is also trailing blank insensitive.  This is very
!    important for those cases where you have stored information in
!    larger variables.  If S is of length 80, and SUB is of
!    length 80, then if S = 'FRED' and SUB = 'RED', a match would
!    not be reported by the standard FORTRAN INDEX, because it treats
!    both variables as being 80 characters long!  This routine assumes that
!    trailing blanks represent garbage!
!
!    Because of the suppression of trailing blanks, this routine cannot be
!    used to find, say, the first occurrence of the two-character
!    string 'A '.  However, this routine treats as a special case the
!    occurrence where S or SUB is entirely blank.  Thus you can
!    use this routine to search for occurrences of double or triple blanks
!    in a string, for example, although INDEX itself would be just as
!    suitable for that problem.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be searched.
!
!    Input, character ( len = * ) SUB, the substring to search for.
!
!    Output, integer ( kind = 4 ) S_INDEXI.  0 if SUB does not occur in
!    the string.  Otherwise S(S_INDEXI:S_INDEXI+LENS-1) = SUB,
!    where LENS is the length of SUB, and is the first place
!    this happens.  However, note that this routine ignores case,
!    unlike the standard FORTRAN INDEX function.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) llen1
  integer ( kind = 4 ) llen2
  character ( len = * ) s
  logical s_eqi
  integer ( kind = 4 ) s_indexi
  character ( len = * ) sub

  s_indexi = 0

  llen1 = len_trim ( s )
  llen2 = len_trim ( sub )
!
!  In case S or SUB is blanks, use LEN.
!
  if ( llen1 == 0 ) then
    llen1 = len ( s )
  end if

  if ( llen2 == 0 ) then
    llen2 = len ( sub )
  end if

  if ( llen1 < llen2 ) then
    return
  end if

  do i = 1, llen1 + 1 - llen2

    if ( s_eqi ( s(i:i+llen2-1), sub ) ) then
      s_indexi = i
      return
    end if

  end do

  return
end
subroutine s_low ( s )

!*****************************************************************************80
!
!! S_LOW replaces all uppercase letters by lowercase ones.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be
!    transformed.  On output, the string is all lowercase.
!
  implicit none

  integer ( kind = 4 ) i
  character ( len = * ) s
  integer ( kind = 4 ) s_length

  s_length = len_trim ( s )

  do i = 1, s_length
    call ch_low ( s(i:i) )
  end do

  return
end
subroutine s_split ( s, sub, s1, s2, s3 )

!*****************************************************************************80
!
!! S_SPLIT divides a string into three parts, given the middle.
!
!  Discussion:
!
!    This version of the routine is case-insensitive.
!
!  Example:
!
!    Input:
!
!      S = 'aBCdEfgh'
!      S2 = 'eF'
!
!    Output:
!
!      S1 = 'aBCd'
!      S2 =  'gh'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be analyzed.
!
!    Input, character ( len = * ) SUB, the substring used to "split" S.
!    Trailing blanks in SUB are ignored.
!
!    Output, character ( len = * ) S1, the entries in the string, up
!    to, but not including, the first occurrence, if any,
!    of SUB.  If SUB occurs immediately, then S1 = ' '.
!    If SUB is not long enough, trailing entries will be lost.
!
!    Output, character ( len = * ) S2, the part of the string that matched SUB.
!    If S2 is ' ', then there wasn't a match.
!
!    Output, character ( len = * ) S3, the part of the string after the match.
!    If there was no match, then S3 is blank.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) lenm
  integer ( kind = 4 ) lens
  character ( len = * ) s
  integer ( kind = 4 ) s_indexi
  character ( len = * ) s1
  character ( len = * ) s2
  character ( len = * ) s3
  character ( len = * ) sub

  lens = len_trim ( s )

  lenm = len_trim ( sub )
  if ( lenm == 0 ) then
    lenm = 1
  end if

  i = s_indexi ( s, sub )
!
!  The substring did not occur.
!
  if ( i == 0 ) then
    s1 = s
    s2 = ' '
    s3 = ' '
!
!  The substring begins immediately.
!
  else if ( i == 1 ) then
    s1 = ' '
    s2 = s(1:lenm)
    s3 = s(lenm+1:)
!
!  What am I checking here?
!
  else if ( lens < i + lenm ) then
    s1 = s
    s2 = ' '
    s3 = ' '
!
!  The substring occurs in the middle.
!
  else
    s1 = s(1:i-1)
    s2 = s(i:i+lenm-1)
    s3 = s(i+lenm: )
  end if
!
!  Drop leading blanks.
!
  s1 = adjustl ( s1 )
  s2 = adjustl ( s2 )
  s3 = adjustl ( s3 )

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
subroutine word_next_read ( line, word, done )

!*****************************************************************************80
!
!! WORD_NEXT_READ "reads" words from a string, one at a time.
!
!  Discussion:
!
!    The following characters are considered to be a single word,
!    whether surrounded by spaces or not:
!
!      " ( ) { } [ ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) LINE, a string, presumably containing words
!    separated by spaces.
!
!    Output, character ( len = * ) WORD.
!    If DONE is FALSE, then WORD contains the "next" word read from LINE.
!    If DONE is TRUE, then WORD is blank, because there was no more to read.
!
!    Input/output, logical DONE.
!    On input with a fresh value of LINE, set DONE to TRUE.
!    On output, the routine sets DONE:
!      FALSE if another word was read from LINE,
!      TRUE if no more words could be read (LINE is exhausted).
!
  implicit none

  logical done
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ), save :: lenc = 0
  character ( len = * ) line
  integer ( kind = 4 ), save :: next = 1
  character TAB
  character ( len = * ) word

  TAB = char ( 9 )
!
!  An input value of DONE = TRUE signals a new line of text to examine.
!
  if ( done ) then

    next = 1
    done = .false.
    lenc = len_trim ( line )

    if ( lenc <= 0 ) then
      done = .true.
      word = ' '
      return
    end if

  end if
!
!  Beginning at index NEXT, search LINE for the next nonblank,
!  which signals the beginning of a word.
!
  ilo = next

  do
!
!  ...LINE(NEXT:) is blank.  Return with WORD = ' ' and DONE = TRUE.
!
    if ( lenc < ilo ) then
      word = ' '
      done = .true.
      next = lenc + 1
      return
    end if
!
!  If the current character is blank, skip to the next one.
!
    if ( line(ilo:ilo) /= ' ' .and. line(ilo:ilo) /= TAB ) then
      exit
    end if

    ilo = ilo + 1

  end do
!
!  ILO is the index of the next nonblank character in the string.
!
!  If this initial nonblank is a special character,
!  then that's the whole word as far as we're concerned,
!  so return immediately.
!
  if ( line(ilo:ilo) == '"' .or. line(ilo:ilo) == '(' .or. &
       line(ilo:ilo) == ')' .or. line(ilo:ilo) == '{' .or. &
       line(ilo:ilo) == '}' .or. line(ilo:ilo) == '[' .or. &
       line(ilo:ilo) == ']' ) then

    word = line(ilo:ilo)
    next = ilo + 1
    return

  end if
!
!  Now search for the last contiguous character that is not a
!  blank, TAB, or special character.
!
  next = ilo + 1

  do

    if ( lenc < next ) then
      word = line(ilo:next-1)
      return
    end if

    if ( line(next:next) == ' ' .or. &
         line(next:next) == TAB .or. &
         line(next:next) == '"' .or. &
         line(next:next) == '(' .or. &
         line(next:next) == ')' .or. &
         line(next:next) == '{' .or. &
         line(next:next) == '}' .or. &
         line(next:next) == '[' .or. &
         line(next:next) == ']' ) then
      exit
    end if

    next = next + 1

  end do

  word = line(ilo:next-1)

  return
end
