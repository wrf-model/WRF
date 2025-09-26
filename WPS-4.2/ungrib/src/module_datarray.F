!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE DATARRAY
!
! Purpose: To make allocatable arrays available to subroutines.
!
! Why? -- We do this so that an array can be allocated within a subroutine,
!         and the caller can make use of the array afterward. 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module datarray

   real, allocatable, dimension(:) :: rdatarray

end module datarray
