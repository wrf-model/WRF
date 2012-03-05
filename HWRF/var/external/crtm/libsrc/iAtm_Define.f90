!
! iAtm_Define
!
! Module for defining the Atmosphere module internal data object
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 07-Apr-2009
!                       paul.vandelst@noaa.gov
!

MODULE iAtm_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     , ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters, ONLY: ZERO, SET
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Structures
  PUBLIC :: iAtm_type
  ! Procedures
  PUBLIC :: iAtm_Associated
  PUBLIC :: iAtm_Create
  PUBLIC :: iAtm_Destroy


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: iAtm_Define.f90 6125 2009-12-18 20:19:59Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! --------------------
  ! Structure definition
  ! --------------------
  !:tdoc+:
  TYPE :: iAtm_type
    INTEGER :: n_Layers    = 0  ! K dimension
    INTEGER :: n_Absorbers = 0  ! J dimension
    ! Level arrays
    REAL(fp), ALLOCATABLE :: pl(:)    ! 0:K
    REAL(fp), ALLOCATABLE :: tl(:)    ! 0:K
    REAL(fp), ALLOCATABLE :: al(:,:)  ! 0:K x J
    ! Layer arrays
    REAL(fp), ALLOCATABLE :: p(:)    ! K
    REAL(fp), ALLOCATABLE :: t(:)    ! K
    REAL(fp), ALLOCATABLE :: a(:,:)  ! K x J
    ! Save variables
    REAL(fp)              :: pln_save = ZERO
    REAL(fp)              :: tln_save = ZERO
    REAL(fp), ALLOCATABLE :: aln_save(:)  ! J
    REAL(fp)              :: plint_save = ZERO
    REAL(fp)              :: tlint_save = ZERO
    REAL(fp), ALLOCATABLE :: alint_save(:)  ! J
    REAL(fp), ALLOCATABLE :: a_save(:,:)    ! K x J
    ! Interpolating polynomials
    REAL(fp) :: ilpoly = ZERO  ! Interpolating polynomial for extra levels to user Pl(0)
    REAL(fp) :: elpoly = ZERO  ! Extrapolating polynomial for user "layer 0" values
  END TYPE iAtm_type
  !:tdoc-:
  
  
CONTAINS


!##################################################################################
!##################################################################################
!##
!##                          ## PUBLIC MODULE ROUTINES ##
!##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       iAtm_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of an iAtm object.
!
! CALLING SEQUENCE:
!       Status = iAtm_Associated( iAtm )
!
! OBJECTS:
!       iAtm:      Internal iAtm object which is to have its member's
!                  status tested.
!                  UNITS:      N/A
!                  TYPE:       iAtm_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:    The return value is a logical value indicating the
!                  status of the iAtm members.
!                  .TRUE.  - if ANY of the allocatable or
!                            pointer members are in use.
!                  .FALSE. - if ALL of the allocatable or
!                            pointer members are not in use.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION iAtm_Associated( iAtm ) RESULT( Status )
    ! Arguments
    TYPE(iAtm_type), INTENT(IN) :: iAtm
    ! Function result
    LOGICAL :: Status

    ! Test the structure components
    Status = &
      ALLOCATED(iAtm%pl        ) .OR. &
      ALLOCATED(iAtm%tl        ) .OR. &
      ALLOCATED(iAtm%al        ) .OR. &
      ALLOCATED(iAtm%p         ) .OR. &
      ALLOCATED(iAtm%t         ) .OR. &
      ALLOCATED(iAtm%a         ) .OR. &
      ALLOCATED(iAtm%aln_save  ) .OR. &
      ALLOCATED(iAtm%alint_save) .OR. &
      ALLOCATED(iAtm%a_save    )

  END FUNCTION iAtm_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Destroy_iAtm
! 
! PURPOSE:
!       Elemental subroutine to re-initialize iAtm objects.
!
! CALLING SEQUENCE:
!       CALL iAtm_Destroy( iAtm )
!
! OBJECTS:
!       iAtm:         Re-initialized internal iAtm object.
!                     UNITS:      N/A
!                     TYPE:       iAtm_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE iAtm_Destroy( iAtm )
    TYPE(iAtm_type), INTENT(OUT) :: iAtm
  END SUBROUTINE iAtm_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       iAtm_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of the iAtm object.
!
! CALLING SEQUENCE:
!       CALL iAtm_Create( iAtm       , &
!                         n_Layers   , &
!                         n_Absorbers, &
!                         iAtm         )
!
! OBJECTS:
!       iAtm:         Internal iAtm structure.
!                     UNITS:      N/A
!                     TYPE:       iAtm_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Layers:     Number of layers dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar or same as iAtm object
!                     ATTRIBUTES: INTENT(IN)
!
!       n_Absorbers:  Number of absorbers dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar or same as iAtm object
!                     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE iAtm_Create( &
    iAtm       , &  ! Output
    n_Layers   , &  ! Input
    n_Absorbers  )  ! Input
    ! Arguments
    TYPE(iAtm_type), INTENT(OUT) :: iAtm
    INTEGER        , INTENT(IN)  :: n_Layers    
    INTEGER        , INTENT(IN)  :: n_Absorbers 
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Layers < 1 .OR. n_Absorbers < 1 ) RETURN

    ! Perform the allocation
    ALLOCATE( iAtm%pl(0:n_Layers), iAtm%tl(0:n_Layers), iAtm%al(0:n_Layers, 1:n_Absorbers), &
              iAtm%p(1:n_Layers) , iAtm%t(1:n_Layers) , iAtm%a(1:n_Layers, 1:n_Absorbers) , &
              iAtm%aln_save(1:n_Absorbers), &
              iAtm%alint_save(1:n_Absorbers), &
              iAtm%a_save(1:n_Layers,1:n_Absorbers), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    ! ...Dimensions
    iAtm%n_Layers    = n_Layers
    iAtm%n_Absorbers = n_Absorbers
    ! ...Arrays
    iAtm%pl         = ZERO
    iAtm%tl         = ZERO
    iAtm%al         = ZERO
    iAtm%p          = ZERO
    iAtm%t          = ZERO
    iAtm%a          = ZERO
    iAtm%aln_save   = ZERO
    iAtm%alint_save = ZERO
    iAtm%a_save     = ZERO
    
  END SUBROUTINE iAtm_Create

END MODULE iAtm_Define

