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

module module_nudging_utils

real :: totalNudgeTime
integer :: sysClockCountRate, sysClockCountMax
character(len=4) :: clockType

contains

!===================================================================================================
! NOTE for whichUtilites
! whUniLoop was fastest for single index searches.
! I still havent tested multiple index searches (which and whichLoop)


!===================================================================================================
! Program Names: 
!   functions: whichPack and whichLoop 
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   Identify indices in a vector which are TRUE, reutrns zero length vector
!   if there are no matches.
! History Log: 
!   6/04/15 -Created, JLM.
! Usage: 
! Parameters:  
! Input Files:
! Output Files: 
! Condition codes: 
! User controllable options: None. 
! Notes: 
!   JLM: Recent catastrophic failure reported for pack on ifort, with work arround.
!   JLM: https://software.intel.com/en-us/forums/topic/559308#comments

subroutine whichPack(theMask, which, nWhich)
implicit none
logical, allocatable, intent(in),  dimension(:) :: theMask
integer,              intent(out), dimension(:) :: which
integer,              intent(out)               :: nwhich

integer :: ii
which = -9999
nWhich = sum( (/ (1, ii=1,size(theMask)) /), mask=theMask)
if(nWhich .gt. size(which)) then
   which = -9999
   return
end if
which(1:nWhich) = pack( (/ (ii, ii=1,size(theMask)) /), mask=theMask)
end subroutine whichPack

subroutine whichLoop(theMask, which, nWhich)
implicit none
logical, intent(in),  dimension(:) :: theMask
integer, intent(out), dimension(:) :: which
integer, intent(out)               :: nwhich

integer :: ii
which = -9999
nWhich = 1
do ii=1,size(theMask)
   if(nWhich .gt. size(which)) then
      which = -9999
      return
   end if
   if(theMask(ii)) then 
      which(nWhich)=ii
      nWhich = nWhich + 1
   endif
end do
nWhich = nWhich-1
end subroutine whichLoop

!===================================================================================================
! Program Names: 
!   function: whUnique
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   Identify THE index in a logical vector which is TRUE. Returns
!   -1 if not unique or none are true.
! History Log: 
!   6/04/15 -Created, JLM.
! Usage: 
! Parameters:  
! Input Files:
! Output Files: 
! Condition codes: 
! User controllable options: None. 
! Notes: 

function whUnique(theMask, unsafe)
  implicit none
  integer                             :: whUnique !! return value
  logical, allocatable, dimension(:), intent(in)  :: theMask
  logical, optional, intent(in)  :: unsafe
  integer, allocatable, dimension(:) :: whUniques
  integer :: i, nMatches
  if(present(unsafe)) then
     !whUniques=pack( (/ (i, i=1,size(theMask)) /), mask= theMask)     
     !whUnique = whUniques(1)
     whUnique=sum( (/ (i, i=1,size(theMask)) /), mask= theMask)     
  else 
     nMatches = sum( (/ (1, i=1,size(theMask)) /), mask= theMask )
     if (nMatches .gt. 1 .OR. nMatches .eq. 0) then
        whUnique=-1
     else 
        whUnique=sum( (/ (i, i=1,size(theMask)) /), mask= theMask)     
     end if
  end if
end function whUnique


!===================================================================================================
! Program Names: 
!   function: whUnique
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   Simply returns the first match, no check for uniques. On gfortran this
!    was the fastest of the bunch even/especially for max indices on huge arrays.
! History Log: 
!   6/04/15 -Created, JLM.
! Usage: 
! Parameters:  
! Input Files:
! Output Files: 
! Condition codes: 
! User controllable options: None. 
! Notes: 

function whUniLoop(theMask)
  implicit none
  integer                                         :: whUniLoop !! return value
  logical, allocatable, dimension(:), intent(in)  :: theMask
  integer :: ii
  whUniLoop = -9999
  do ii=1,size(theMask)
     if(theMask(ii)) then
        whUniLoop = ii
        return
     end if
  end do
end function whUniLoop

!===================================================================================================
! Program Names: 
!   function: whInLoop
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   Identify the indices of elements in a first vector which are present in the 
!   second vector, returns 0 for no matches. This can be slow, it's a double do/for loop.
! History Log: 
!   6/04/15 -Created, JLM.
! Usage: 
! Parameters:  
! Input Files:
! Output Files: 
! Condition codes: 
! User controllable options: None. 
! Notes: Can be slow, use with caution.

! parallelize this? ||||||||||||||||||||||||||||||||||
subroutine whichInLoop(vecToSearch, vecToMatch, which, nWhich)
implicit none
character(len=15), intent(in),  dimension(:) :: vecToSearch
character(len=15), intent(in),  dimension(:) :: vecToMatch
integer, intent(out), dimension(:) :: which
integer, intent(out)               :: nWhich
integer :: ii, jj
which = -9999
nWhich = 0
do ii=1,size(vecToSearch)
   do jj=1,size(vecToMatch)
      if(trim(adjustl(vecToSearch(ii))) .eq. trim(adjustl(vecToMatch(jj)))) then
         which(ii)=ii
         nWhich = nWhich + 1
         exit
      end if
   end do
end do
end subroutine whichInLoop


! parallelize this? ||||||||||||||||||||||||||||||||||
subroutine whichInLoop2(vecToSearch, vecToMatch, which, nWhich)
implicit none
character(len=15), intent(in),  dimension(:) :: vecToSearch
character(len=15), intent(in),  dimension(:) :: vecToMatch
integer, intent(out), dimension(:) :: which
integer, intent(out)               :: nWhich
integer :: ii, jj
which = -9999
nWhich = 0
do ii=1,size(vecToSearch)
   if(any(vecToMatch .eq. vecToSearch(ii))) then
      which(ii)=ii
      nWhich = nWhich + 1
   end if
end do
end subroutine whichInLoop2


!===================================================================================================
! Program Names: 
!   accum_nudging_time
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   Tally up the total cpu or wall time used by nudging.
! History Log: 
!   8/20/15 -Created, JLM.
! Usage: 
! Parameters:  
!   start, end: real times for end-diff timing & accumulation
!   sectionLabel: prints a message with the timing for the section
!      print*, 'Ndg: ' // sectionLabel // '(seconds ' // trim(clockType) // ' time):', diff
!   optional - accum: accumulate this towards the overall time or simply print the above 
!      message? Do not accum for nested sections of code, but still give the diagnostic.
! Input Files:
! Output Files: 
! Condition codes: 
! User controllable options: None. 
! Notes: 
subroutine accum_nudging_time(start, end, sectionLabel, accum)
implicit none
real,              intent(in) :: start, end
character(len=*),  intent(in) :: sectionLabel
logical, optional, intent(in):: accum
logical :: accumLocal
real :: diff
accumLocal=.TRUE.
if(present(accum)) accumLocal = accum
diff=end-start
if(clockType.eq.'wall') then
   if(diff .lt. 0) diff = diff + sysClockCountMax
   diff=diff/sysClockCountRate
endif
if (accumLocal) totalNudgeTime = totalNudgeTime + diff

print*,'Ndg: Timing: ' // sectionLabel // ' (' // trim(clockType) // ' time, seconds):', diff

if(accumLocal) print*,'Ndg: Timing: accum totalNudgeTime: ',totalNudgeTime
end subroutine accum_nudging_time


!===================================================================================================
! Program Names: 
!   nudging_timer
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   Return your choice of cpu time or wall time
! History Log: 
!   8/20/15 -Created, JLM.
! Usage: 
! Parameters:  
! Input Files:
! Output Files: 
! Condition codes: 
! User controllable options: None. 
! Notes: 
subroutine nudging_timer(time)
implicit none
real, intent(inout) :: time
integer :: count 
if(clockType.eq.'cpu') call cpu_time(time)
if(clockType.eq.'wall') then
   call system_clock(count=count) 
   time=real(count)
end if
end subroutine nudging_timer


!===================================================================================================
end module module_nudging_utils

