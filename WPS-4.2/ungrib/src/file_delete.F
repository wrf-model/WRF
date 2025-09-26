subroutine file_delete(hdates, ndates, root, interval)
! Recent changes:                                                             !
!    2001-02-14:                                                              !
!               - Allow file names to have date stamps out to minutes or      !
!                 seconds, if the user requests a time interval (in seconds)  !
!                 that is evenly divisible into minutes or hours.             !
!                 INTERVAL is checked for divisibility into 3600 (for hours)  !
!                 or 60 (for minutes).  The local variable DATELEN is set     !
!                 to be the number of characters to use in our character      !
!                 dates.  Valid values for DATELEN are 13 (for hours),        !
!                 16 (for minutes), and 19 (for seconds).                     !
!                                                                             !
!                 This change also requires changes to pregrid_grib.F,        !
!                 output.F, rrpr.F, datint.F                                  !

  use misc_definitions_module
  use module_debug

  implicit none
  integer :: ndates
  character(len=*), dimension(ndates) :: hdates
  character(len=*) :: root
  integer :: interval

  logical :: lexist
  integer :: idate
  character(len=MAX_FILENAME_LEN) :: flnm

! DATELEN:  length of date strings to use for our output file names.
  integer :: datelen

! Decide the length of date strings to use for output file names.  
! DATELEN is 13 for hours, 16 for minutes, and 19 for seconds.
  if (mod(interval,3600) == 0) then
     datelen = 13
  else if (mod(interval, 60) == 0) then
     datelen = 16
  else
     datelen = 19
  end if

  write(*, '(/,10("*"), /, &
       &    "Deleting temporary files created by ungrib...",/, &
       &    10("*")/)')
  call mprintf(.true.,LOGFILE,"****  Deleting temporary files created by ungrib... ")

  do idate = 1, ndates
     flnm=trim(root)//hdates(idate)(1:datelen)
     write(*, '(A)') 'Deleting file:  '//trim(flnm)
     call mprintf(.true.,LOGFILE," Deleting file: %s ",s1=trim(flnm))

     inquire(file=flnm, exist = lexist)
     if (lexist) then
        open(10, file=flnm, status='old')
        close(10, status="DELETE")
     else
        write(*,'(10x, "File ",A," does not exist.",/)') flnm
        call mprintf(.true.,LOGFILE," File  %s does not exist ",s1=flnm)
     endif
  enddo

  write(*, '(/,10("*"), /, &
       &    "Done deleting temporary files.",/, &
       &    10("*")/)')
  call mprintf(.true.,LOGFILE,"****  Done deleting temporary files. ")

end subroutine file_delete
