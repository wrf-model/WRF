module kwm_string_utilities

contains

  subroutine strrep(string, rep1, rep2)
    ! In string 1, replace expression rep1 with expression rep2.
    ! if rep2 is shorter than rep1, fill the end of the string
    ! with blanks

    implicit none
    character(len=*) :: string, rep1, rep2
    integer :: idx, inlen, len1, len2

    do
       inlen = len(string)
       len1 = len(rep1)
       len2 = len(rep2)
       idx = index(string, rep1)-1

       if (idx == -1) then
          return
       else
          string = string(1:idx)// rep2 // &
               string((idx+len1+1):inlen) // &
               "                                                    "
       endif
    enddo

  end subroutine strrep


  character(len=1024) function unblank(string) result(return_string)
    ! Remove blanks (and tabs [char(9)] from string
    implicit none
    character(len=*), intent(in) :: string
    integer :: i, j,  lenstr

    return_string = " "

    if ( verify(string," "//char(9)) == 0 ) then
       stop 'String is all blanks.'
    endif

    j = 0
    do i = 1, len(string)
       if ((string(i:i).ne.' ').and.(string(i:i).ne.char(9))) then
          j = j + 1
          return_string(j:j) = string(i:i)
       endif
    enddo
    
  end function unblank

!KWM  character function upcase(h)
!KWM    implicit none
!KWM    character :: h
!KWM
!KWM    if ((ichar(h).ge.96) .and. (ichar(h).le.123)) then
!KWM       upcase = char(ichar(h)-32)
!KWM    else
!KWM       upcase = h
!KWM    endif
!KWM
!KWM  end function upcase

  character(len=256) function upcase(h) result(return_string)
    implicit none
    character(len=*), intent(in) :: h
    integer :: i
    
    return_string = " "

    do i = 1, len_trim(h)

       if ((ichar(h(i:i)).ge.96) .and. (ichar(h(i:i)).le.123)) then
          return_string(i:i) = char(ichar(h(i:i))-32)
       else
          return_string(i:i) = h(i:i)
       endif
    enddo

  end function upcase

!KWM  character function downcase(h)
!KWM    implicit none
!KWM    character h
!KWM
!KWM    if ((ichar(h).ge.65) .and. (ichar(h).le.90)) then
!KWM       downcase = char(ichar(h)+32)
!KWM    else
!KWM       downcase = h
!KWM    endif
!KWM  end function downcase

  character(len=256) function downcase(h) result(return_string)
    implicit none
    character(len=*), intent(in) :: h
    integer :: i

    return_string = " "

    do i = 1, len_trim(h)

       if ((ichar(h(i:i)).ge.65) .and. (ichar(h(i:i)).le.90)) then
          return_string(i:i) = char(ichar(h(i:i))+32)
       else
          return_string(i:i) = h(i:i)
       endif
    enddo

  end function downcase


end module kwm_string_utilities
