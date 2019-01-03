!
! FitCoeff_Define
!
! Module defining the FitCoeff objects.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 17-Nov-2011
!                       paul.vandelst@noaa.gov
 
MODULE FitCoeff_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp, Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE File_Utility         , ONLY: File_Open, File_Exists
  USE Binary_File_Utility  , ONLY: Open_Binary_File      , &
                                   WriteGAtts_Binary_File, &
                                   ReadGAtts_Binary_File
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Parameters
  PUBLIC :: FITCOEFF_MAX_N_DIMENSIONS
  ! Datatypes
  PUBLIC :: FitCoeff_1D_type
  PUBLIC :: FitCoeff_2D_type
  PUBLIC :: FitCoeff_3D_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: FitCoeff_Associated
  PUBLIC :: FitCoeff_Destroy
  PUBLIC :: FitCoeff_Create
  PUBLIC :: FitCoeff_SetValue
  PUBLIC :: FitCoeff_Inspect
  PUBLIC :: FitCoeff_ValidRelease
  PUBLIC :: FitCoeff_Info
  PUBLIC :: FitCoeff_DefineVersion
  PUBLIC :: FitCoeff_InquireFile
  PUBLIC :: FitCoeff_ReadFile
  PUBLIC :: FitCoeff_WriteFile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE FitCoeff_Associated
    MODULE PROCEDURE FitCoeff_1D_Associated
    MODULE PROCEDURE FitCoeff_2D_Associated
    MODULE PROCEDURE FitCoeff_3D_Associated
  END INTERFACE FitCoeff_Associated
  
  INTERFACE FitCoeff_Destroy
    MODULE PROCEDURE FitCoeff_1D_Destroy
    MODULE PROCEDURE FitCoeff_2D_Destroy
    MODULE PROCEDURE FitCoeff_3D_Destroy
  END INTERFACE FitCoeff_Destroy
  
  INTERFACE FitCoeff_Create
    MODULE PROCEDURE FitCoeff_1D_Create
    MODULE PROCEDURE FitCoeff_2D_Create
    MODULE PROCEDURE FitCoeff_3D_Create
  END INTERFACE FitCoeff_Create
  
  INTERFACE FitCoeff_SetValue
    MODULE PROCEDURE FitCoeff_1D_SetValue
    MODULE PROCEDURE FitCoeff_2D_SetValue
    MODULE PROCEDURE FitCoeff_3D_SetValue
  END INTERFACE FitCoeff_SetValue
  
  INTERFACE FitCoeff_Inspect
    MODULE PROCEDURE FitCoeff_1D_Inspect
    MODULE PROCEDURE FitCoeff_2D_Inspect
    MODULE PROCEDURE FitCoeff_3D_Inspect
  END INTERFACE FitCoeff_Inspect
  
  INTERFACE FitCoeff_ValidRelease
    MODULE PROCEDURE FitCoeff_1D_ValidRelease
    MODULE PROCEDURE FitCoeff_2D_ValidRelease
    MODULE PROCEDURE FitCoeff_3D_ValidRelease
  END INTERFACE FitCoeff_ValidRelease
  
  INTERFACE FitCoeff_Info
    MODULE PROCEDURE FitCoeff_1D_Info
    MODULE PROCEDURE FitCoeff_2D_Info
    MODULE PROCEDURE FitCoeff_3D_Info
  END INTERFACE FitCoeff_Info
  
  INTERFACE FitCoeff_ReadFile
    MODULE PROCEDURE FitCoeff_1D_ReadFile
    MODULE PROCEDURE FitCoeff_2D_ReadFile
    MODULE PROCEDURE FitCoeff_3D_ReadFile
  END INTERFACE FitCoeff_ReadFile
  
  INTERFACE FitCoeff_WriteFile
    MODULE PROCEDURE FitCoeff_1D_WriteFile
    MODULE PROCEDURE FitCoeff_2D_WriteFile
    MODULE PROCEDURE FitCoeff_3D_WriteFile
  END INTERFACE FitCoeff_WriteFile
  
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE FitCoeff_1D_Equal
    MODULE PROCEDURE FitCoeff_2D_Equal
    MODULE PROCEDURE FitCoeff_3D_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: FitCoeff_Define.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  ! Release and version
  INTEGER, PARAMETER :: FITCOEFF_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PARAMETER :: FITCOEFF_VERSION = 1  ! This is just the default data version.
  ! Close status for write errors
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! String lengths
  INTEGER, PARAMETER :: ML = 256 ! Message length
  INTEGER, PARAMETER :: SL =  80 ! String length
  ! Maximum number of dimensions
  INTEGER, PARAMETER :: FITCOEFF_MAX_N_DIMENSIONS = 3  ! Only implemented up to 3-D arrays so far


  ! ----------------------------------
  ! FitCoeff data type definitions
  ! ----------------------------------
  !:tdoc+:
  TYPE :: FitCoeff_1D_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER(Long) :: Release = FITCOEFF_RELEASE
    INTEGER(Long) :: Version = FITCOEFF_VERSION
    ! Dimensions
    INTEGER(Long) :: Dimensions(1) = 0
    ! Data
    REAL(Double), ALLOCATABLE :: C(:)
  END TYPE FitCoeff_1D_type
  !:tdoc-:

  !:tdoc+:
  TYPE :: FitCoeff_2D_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER(Long) :: Release = FITCOEFF_RELEASE
    INTEGER(Long) :: Version = FITCOEFF_VERSION
    ! Dimensions
    INTEGER(Long) :: Dimensions(2) = 0
    ! Data
    REAL(Double), ALLOCATABLE :: C(:,:)
  END TYPE FitCoeff_2D_type
  !:tdoc-:

  !:tdoc+:
  TYPE :: FitCoeff_3D_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER(Long) :: Release = FITCOEFF_RELEASE
    INTEGER(Long) :: Version = FITCOEFF_VERSION
    ! Dimensions
    INTEGER(Long) :: Dimensions(3) = 0
    ! Data
    REAL(Double), ALLOCATABLE :: C(:,:,:)
  END TYPE FitCoeff_3D_type
  !:tdoc-:


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                           ## PUBLIC PROCEDURES ##                          ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       FitCoeff_Associated
!
! PURPOSE:
!       Pure function to test the status of the allocatable components
!       of the FitCoeff structure.
!
! CALLING SEQUENCE:
!       Status = FitCoeff_Associated( FitCoeff )
!
! OBJECTS:
!       FitCoeff:      Structure which is to have its member's
!                      status tested.
!                      UNITS:      N/A
!                      TYPE:       Any FitCoeff type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value indicating the
!                      status of the components.
!                       .TRUE.  - if ANY of the FitCoeff allocatable members
!                                 are in use.
!                       .FALSE. - if ALL of the FitCoeff allocatable members
!                                 are not in use.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE FUNCTION FitCoeff_1D_Associated( self ) RESULT( Status )
    TYPE(FitCoeff_1D_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION FitCoeff_1D_Associated

  PURE FUNCTION FitCoeff_2D_Associated( self ) RESULT( Status )
    TYPE(FitCoeff_2D_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION FitCoeff_2D_Associated

  PURE FUNCTION FitCoeff_3D_Associated( self ) RESULT( Status )
    TYPE(FitCoeff_3D_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION FitCoeff_3D_Associated

  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       FitCoeff_Destroy
!
! PURPOSE:
!       Pure subroutine to re-initialize FitCoeff objects.
!
! CALLING SEQUENCE:
!       CALL FitCoeff_Destroy( FitCoeff )
!
! OBJECTS:
!       FitCoeff:     Re-initialized FitCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       Any FitCoeff type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE SUBROUTINE FitCoeff_1D_Destroy( self )
    TYPE(FitCoeff_1D_type), INTENT(OUT) :: self
    INCLUDE 'FitCoeff_Destroy.inc'
  END SUBROUTINE FitCoeff_1D_Destroy

  PURE SUBROUTINE FitCoeff_2D_Destroy( self )
    TYPE(FitCoeff_2D_type), INTENT(OUT) :: self
    INCLUDE 'FitCoeff_Destroy.inc'
  END SUBROUTINE FitCoeff_2D_Destroy

  PURE SUBROUTINE FitCoeff_3D_Destroy( self )
    TYPE(FitCoeff_3D_type), INTENT(OUT) :: self
    INCLUDE 'FitCoeff_Destroy.inc'
  END SUBROUTINE FitCoeff_3D_Destroy



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       FitCoeff_Create
!
! PURPOSE:
!       Pure subroutine to create an instance of a FitCoeff object.
!
! CALLING SEQUENCE:
!       CALL FitCoeff_Create( FitCoeff, Dimensions )
!
! OBJECTS:
!       FitCoeff:   FitCoeff object structure.
!                   UNITS:      N/A
!                   TYPE:       Any FitCoeff type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       Dimensions: Dimension vector for the fitting coefficient array.
!                   The number of elements of this array must agree with
!                   the rank of the FitCoeff datatype specified, e.g. 2D
!                   type requires 2 dimensions specified.
!                   Values must be > 0.
!                   UNITS:      N/A
!                   TYPE:       INTEGER
!                   DIMENSION:  Rank
!                   ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE SUBROUTINE FitCoeff_1D_Create( &
    self      , &  ! Output
    dimensions  )  ! Input
    ! Arguments
    TYPE(FitCoeff_1D_type), INTENT(OUT) :: self
    INTEGER               , INTENT(IN)  :: dimensions(1)
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( ANY(dimensions < 1) ) RETURN

    ! Perform the allocation
    ALLOCATE( self%C(dimensions(1)), STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    ! ...Dimensions
    self%Dimensions = dimensions
    ! ...Arrays
    self%C = ZERO

    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE FitCoeff_1D_Create


  PURE SUBROUTINE FitCoeff_2D_Create( &
    self      , &  ! Output
    dimensions  )  ! Input
    ! Arguments
    TYPE(FitCoeff_2D_type), INTENT(OUT) :: self
    INTEGER               , INTENT(IN)  :: dimensions(2)
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( ANY(dimensions < 1) ) RETURN

    ! Perform the allocation
    ALLOCATE( self%C(dimensions(1), dimensions(2)), STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    ! ...Dimensions
    self%Dimensions = dimensions
    ! ...Arrays
    self%C = ZERO

    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE FitCoeff_2D_Create


  PURE SUBROUTINE FitCoeff_3D_Create( &
    self      , &  ! Output
    dimensions  )  ! Input
    ! Arguments
    TYPE(FitCoeff_3D_type), INTENT(OUT) :: self
    INTEGER               , INTENT(IN)  :: dimensions(3)
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( ANY(dimensions < 1) ) RETURN

    ! Perform the allocation
    ALLOCATE( self%C(dimensions(1), dimensions(2), dimensions(3)), STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    ! ...Dimensions
    self%Dimensions = dimensions
    ! ...Arrays
    self%C = ZERO

    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE FitCoeff_3D_Create



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       FitCoeff_SetValue
!
! PURPOSE:
!       Subroutine to set the value of coefficients in a FitCoeff object.
!
! CALLING SEQUENCE:
!       CALL FitCoeff_SetValue( FitCoeff, Carray )
!
! OBJECTS:
!       FitCoeff:   FitCoeff object structure which is to have its coefficient
!                   data set.
!                   Note #1: If unallocated, the object is allocated based
!                            on the SHAPE of the Carray input.
!                   Note #2: If already allocated, the dimensions of the
!                            coefficient component must be the same as that
!                            of the Carray input.
!                   UNITS:      N/A
!                   TYPE:       Any FitCoeff type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       Carray:     Coefficient array to be assigned to FitCoeff object.
!                   The rank of this array must agree with the rank of
!                   the FitCoeff datatype specified, e.g. 2D FitCoeff
!                   type requires a rank-2 coefficient array.
!                   UNITS:      N/A
!                   TYPE:       REAL(fp)
!                   DIMENSION:  Rank conforming with FitCoeff type.
!                   ATTRIBUTES: INTENT(IN)
!
! COMMENTS:
!   If an error occurs, the FitCoeff object is deallocated.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE FitCoeff_1D_SetValue( &
    self   , &  ! In/output
    C      , &  ! Input
    Version  )  ! optional input
    ! Arguments
    TYPE(FitCoeff_1D_type), INTENT(IN OUT) :: self
    REAL(fp)              , INTENT(IN)     :: C(:)
    INTEGER,      OPTIONAL, INTENT(IN)     :: Version
    INCLUDE 'FitCoeff_SetValue.inc'
  END SUBROUTINE FitCoeff_1D_SetValue


  SUBROUTINE FitCoeff_2D_SetValue( &
    self   , &  ! In/output
    C      , &  ! Input
    Version  )  ! optional input
    ! Arguments
    TYPE(FitCoeff_2D_type), INTENT(IN OUT) :: self
    REAL(fp)              , INTENT(IN)     :: C(:,:)
    INTEGER,      OPTIONAL, INTENT(IN)     :: Version
    INCLUDE 'FitCoeff_SetValue.inc'
  END SUBROUTINE FitCoeff_2D_SetValue


  SUBROUTINE FitCoeff_3D_SetValue( &
    self   , &  ! In/output
    C      , &  ! Input
    Version  )  ! optional input
    ! Arguments
    TYPE(FitCoeff_3D_type), INTENT(IN OUT) :: self
    REAL(fp)              , INTENT(IN)     :: C(:,:,:)
    INTEGER,      OPTIONAL, INTENT(IN)     :: Version
    INCLUDE 'FitCoeff_SetValue.inc'
  END SUBROUTINE FitCoeff_3D_SetValue


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       FitCoeff_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a FitCoeff object to stdout.
!
! CALLING SEQUENCE:
!       CALL FitCoeff_Inspect( FitCoeff )
!
! OBJECTS:
!       FitCoeff:      FitCoeff object to display.
!                      UNITS:      N/A
!                      TYPE:       Any FitCoeff type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE FitCoeff_1D_Inspect( self )
    TYPE(FitCoeff_1D_type), INTENT(IN) :: self
    WRITE(*,'(1x,"FitCoeff 1D OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version : ",i0,".",i0)') self%Release, self%Version
    ! Dimensions
    WRITE(*,'(3x,"Dimensions : ",10(i5,:))') self%Dimensions
    IF ( .NOT. FitCoeff_1D_Associated(self) ) RETURN
    ! Coefficient data
    WRITE(*,'(3x,"Coefficients:")')
    WRITE(*,'(5(1x,es13.6,:))') self%C
  END SUBROUTINE FitCoeff_1D_Inspect


  SUBROUTINE FitCoeff_2D_Inspect( self )
    TYPE(FitCoeff_2D_type), INTENT(IN) :: self
    INTEGER :: i
    WRITE(*,'(1x,"FitCoeff 2D OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version : ",i0,".",i0)') self%Release, self%Version
    ! Dimensions
    WRITE(*,'(3x,"Dimensions : ",10(i5,:))') self%Dimensions
    IF ( .NOT. FitCoeff_2D_Associated(self) ) RETURN
    ! Coefficient data
    WRITE(*,'(3x,"Coefficients:")')
    DO i = 1, self%Dimensions(2)
      WRITE(*,'(5x,"Outer dimension = ",i0," of ",i0)') i, self%Dimensions(2)
      WRITE(*,'(5(1x,es13.6,:))') self%C(:,i)
    END DO
  END SUBROUTINE FitCoeff_2D_Inspect


  SUBROUTINE FitCoeff_3D_Inspect( self )
    TYPE(FitCoeff_3D_type), INTENT(IN) :: self
    INTEGER :: i, j
    WRITE(*,'(1x,"FitCoeff 3D OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version : ",i0,".",i0)') self%Release, self%Version
    ! Dimensions
    WRITE(*,'(3x,"Dimensions : ",10(i5,:))') self%Dimensions
    IF ( .NOT. FitCoeff_3D_Associated(self) ) RETURN
    ! Coefficient data
    WRITE(*,'(3x,"Coefficients:")')
    DO j = 1, self%Dimensions(3)
      WRITE(*,'(5x,"Outer dimension = ",i0," of ",i0)') j, self%Dimensions(3)
      DO i = 1, self%Dimensions(2)
        WRITE(*,'(7x,"Middle dimension = ",i0," of ",i0)') i, self%Dimensions(2)
        WRITE(*,'(5(1x,es13.6,:))') self%C(:,i,j)
      END DO
    END DO
  END SUBROUTINE FitCoeff_3D_Inspect


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       FitCoeff_ValidRelease
!
! PURPOSE:
!       Function to check the FitCoeff Release value.
!
! CALLING SEQUENCE:
!       IsValid = FitCoeff_ValidRelease( FitCoeff )
!
! INPUTS:
!       FitCoeff:      FitCoeff object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       Any FitCoeff type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       IsValid:       Logical value defining the release validity.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION ValidRelease( Release ) RESULT( IsValid )
    ! Arguments
    INTEGER, INTENT(IN) :: Release
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'FitCoeff_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.

    ! Check release is not too old
    IF ( Release < FITCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A FitCoeff data update is needed. ", &
                  &"FitCoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  Release, FITCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF

    ! Check release is not too new
    IF ( Release > FITCOEFF_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A FitCoeff software update is needed. ", &
                  &"FitCoeff release is ",i0,". Valid release is ",i0,"." )' ) &
                  Release, FITCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF
  END FUNCTION ValidRelease


  FUNCTION FitCoeff_1D_ValidRelease( self ) RESULT( IsValid )
    TYPE(FitCoeff_1D_type), INTENT(IN) :: self
    LOGICAL :: IsValid
    IsValid = ValidRelease( self%Release )
  END FUNCTION FitCoeff_1D_ValidRelease


  FUNCTION FitCoeff_2D_ValidRelease( self ) RESULT( IsValid )
    TYPE(FitCoeff_2D_type), INTENT(IN) :: self
    LOGICAL :: IsValid
    IsValid = ValidRelease( self%Release )
  END FUNCTION FitCoeff_2D_ValidRelease


  FUNCTION FitCoeff_3D_ValidRelease( self ) RESULT( IsValid )
    TYPE(FitCoeff_3D_type), INTENT(IN) :: self
    LOGICAL :: IsValid
    IsValid = ValidRelease( self%Release )
  END FUNCTION FitCoeff_3D_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       FitCoeff_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a FitCoeff object.
!
! CALLING SEQUENCE:
!       CALL FitCoeff_Info( FitCoeff, Info )
!
! OBJECTS:
!       FitCoeff:      FitCoeff object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       Any FitCoeff type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the FitCoeff object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE FitCoeff_1D_Info( self, Info )
    ! Arguments
    TYPE(FitCoeff_1D_type), INTENT(IN)  :: self
    INCLUDE 'FitCoeff_Info.inc'
  END SUBROUTINE FitCoeff_1D_Info
 
 
  SUBROUTINE FitCoeff_2D_Info( self, Info )
    ! Arguments
    TYPE(FitCoeff_2D_type), INTENT(IN)  :: self
    INCLUDE 'FitCoeff_Info.inc'
  END SUBROUTINE FitCoeff_2D_Info
 
 
  SUBROUTINE FitCoeff_3D_Info( self, Info )
    ! Arguments
    TYPE(FitCoeff_3D_type), INTENT(IN)  :: self
    INCLUDE 'FitCoeff_Info.inc'
  END SUBROUTINE FitCoeff_3D_Info
 
 
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       FitCoeff_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL FitCoeff_DefineVersion( Id )
!
! OUTPUTS:
!       Id:    Character string containing the version Id information
!              for the module.
!              UNITS:      N/A
!              TYPE:       CHARACTER(*)
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE FitCoeff_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE FitCoeff_DefineVersion


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       FitCoeff_InquireFile
!
! PURPOSE:
!       Function to inquire FitCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = FitCoeff_InquireFile( &
!                        Filename                   , &
!                        n_Dimensions = n_Dimensions, &
!                        Dimensions   = Dimensions  , &
!                        Release      = Release     , &
!                        Version      = Version     , &
!                        Title        = Title       , &
!                        History      = History     , &
!                        Comment      = Comment       )
!
! INPUTS:
!       Filename:           Character string specifying the name of the
!                           data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Dimensions:       The rank of the coefficient data array.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Dimensions:         The dimension values of the coefficient data array.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL, ALLOCATABLE
!
!       Release:            The data/file release number. Used to check
!                           for data/software mismatch.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The data/file version number. Used for
!                           purposes only in identifying the dataset for
!                           a particular release.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string containing a succinct description
!                           of what is in the dataset.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string containing dataset creation
!                           history.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string containing any comments about
!                           the dataset.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the file inquire was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION FitCoeff_InquireFile( &
    Filename    , &  ! Input
    n_Dimensions, &  ! Optional output
    Dimensions  , &  ! Optional output
    Release     , &  ! Optional output
    Version     , &  ! Optional output
    Title       , &  ! Optional output
    History     , &  ! Optional output
    Comment     ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),                        INTENT(IN)  :: Filename
    INTEGER     ,              OPTIONAL, INTENT(OUT) :: n_Dimensions
    INTEGER     , ALLOCATABLE, OPTIONAL, INTENT(OUT) :: Dimensions(:)
    INTEGER     ,              OPTIONAL, INTENT(OUT) :: Release
    INTEGER     ,              OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*),              OPTIONAL, INTENT(OUT) :: Title  
    CHARACTER(*),              OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*),              OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'FitCoeff_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: alloc_stat
    INTEGER :: fid
    INTEGER :: rel
    INTEGER :: ver
    INTEGER :: n_dims
    INTEGER :: dims(FITCOEFF_MAX_N_DIMENSIONS)
 
    ! Setup
    err_stat = SUCCESS
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( Filename ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the release and version
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      rel, &
      ver
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Release/Version - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF
    IF ( .NOT. ValidRelease( rel ) ) THEN
      msg = 'FitCoeff Release check failed.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the dimension data
    ! ...The number of dimensions
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      n_dims      
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading number of dimensions from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...Check the value
    IF ( n_dims > FITCOEFF_MAX_N_DIMENSIONS ) THEN
      WRITE( msg,'("Number of dimensions (",i0,") in ",a," is greater than maximum allowed (",i0,")")' ) &
                 n_dims, TRIM(Filename), FITCOEFF_MAX_N_DIMENSIONS
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...The dimension values
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      dims(1:n_dims)
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimension values from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the global attributes
    err_stat = ReadGAtts_Binary_File( &
                 fid, &
                 Title   = Title  , &
                 History = History, &
                 Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Assign the return arguments
    IF ( PRESENT(Release     ) ) Release      = rel        
    IF ( PRESENT(Version     ) ) Version      = ver        
    IF ( PRESENT(n_Dimensions) ) n_Dimensions = n_dims
    IF ( PRESENT(Dimensions  ) ) THEN
      ALLOCATE(Dimensions(n_dims), STAT=alloc_stat)
      IF ( alloc_stat /= 0 ) THEN
        WRITE( msg,'("Error allocating output DIMENSIONS argument. STAT=",i0)') alloc_stat
        CALL Inquire_Cleanup(); RETURN
      END IF
      Dimensions = dims(1:n_dims)
    END IF
    
  CONTAINS
  
    SUBROUTINE Inquire_CleanUp()
      ! Close file if necessary
      IF ( File_Open(fid) ) THEN
        CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Inquire_CleanUp
    
  END FUNCTION FitCoeff_InquireFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       FitCoeff_ReadFile
!
! PURPOSE:
!       Function to read FitCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = FitCoeff_ReadFile( &
!                        FitCoeff           , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet   , &
!                        Title    = Title   , &
!                        History  = History , &
!                        Comment  = Comment   )
!
! OBJECTS:
!       FitCoeff:       FitCoeff object containing the data read from file.
!                       While any FitCoeff data type can be used, the rank
!                       of the data in the file must correspond to the 
!                       datatype.
!                       UNITS:      N/A
!                       TYPE:       Any FitCoeff type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       FitCoeff data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the FitCoeff data is embedded within another file.
!                       If == .FALSE., File is closed upon function exit [DEFAULT].
!                          == .TRUE.,  File is NOT closed upon function exit
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       Title:          Character string containing a succinct description
!                       of what is in the dataset.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string containing dataset creation
!                       history.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string containing any comments about
!                       the dataset.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file read was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION FitCoeff_1D_ReadFile( &
    FitCoeff, &  ! Output
    Filename, &  ! Input
    No_Close, &  ! Optional input
    Quiet   , &  ! Optional input
    Title   , &  ! Optional output
    History , &  ! Optional output
    Comment , &  ! Optional output
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(FitCoeff_1D_type), INTENT(OUT) :: FitCoeff
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL     , OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    LOGICAL     , OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'FitCoeff_ReadFile'
    ! Function variables
    TYPE(FitCoeff_1D_type) :: dummy
    ! Insert common code
    INCLUDE 'FitCoeff_ReadFile.inc'
  END FUNCTION FitCoeff_1D_ReadFile


  FUNCTION FitCoeff_2D_ReadFile( &
    FitCoeff, &  ! Output
    Filename, &  ! Input
    No_Close, &  ! Optional input
    Quiet   , &  ! Optional input
    Title   , &  ! Optional output
    History , &  ! Optional output
    Comment , &  ! Optional output
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(FitCoeff_2D_type), INTENT(OUT) :: FitCoeff
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL     , OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    LOGICAL     , OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'FitCoeff_ReadFile'
    ! Function variables
    TYPE(FitCoeff_2D_type) :: dummy
    ! Insert common code
    INCLUDE 'FitCoeff_ReadFile.inc'
  END FUNCTION FitCoeff_2D_ReadFile


  FUNCTION FitCoeff_3D_ReadFile( &
    FitCoeff, &  ! Output
    Filename, &  ! Input
    No_Close, &  ! Optional input
    Quiet   , &  ! Optional input
    Title   , &  ! Optional output
    History , &  ! Optional output
    Comment , &  ! Optional output
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(FitCoeff_3D_type), INTENT(OUT) :: FitCoeff
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL     , OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    LOGICAL     , OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'FitCoeff_ReadFile'
    ! Function variables
    TYPE(FitCoeff_3D_type) :: dummy
    ! Insert common code
    INCLUDE 'FitCoeff_ReadFile.inc'
  END FUNCTION FitCoeff_3D_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       FitCoeff_WriteFile
!
! PURPOSE:
!       Function to write FitCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = FitCoeff_WriteFile( &
!                        FitCoeff           , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet   , &
!                        Title    = Title   , &
!                        History  = History , &
!                        Comment  = Comment   )
!
! OBJECTS:
!       FitCoeff:   FitCoeff object containing the data to write to file.
!                       UNITS:      N/A
!                       TYPE:       FitCoeff_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       FitCoeff format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the FitCoeff data is to be embedded within another file.
!                       If == .FALSE., File is closed upon function exit [DEFAULT].
!                          == .TRUE.,  File is NOT closed upon function exit
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:          Character string containing a succinct description
!                       of what is in the dataset.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string containing dataset creation
!                       history.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string containing any comments about
!                       the dataset.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file write was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION FitCoeff_1D_WriteFile( &
    FitCoeff, &  ! Input
    Filename, &  ! Input
    No_Close, &  ! Optional input
    Quiet   , &  ! Optional input
    Title   , &  ! Optional input
    History , &  ! Optional input
    Comment , &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(FitCoeff_1D_type), INTENT(IN) :: FitCoeff
    CHARACTER(*),           INTENT(IN) :: Filename
    LOGICAL     , OPTIONAL, INTENT(IN) :: No_Close
    LOGICAL     , OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    LOGICAL     , OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'FitCoeff_WriteFile'
    ! Insert common code
    INCLUDE 'FitCoeff_WriteFile.inc'
  END FUNCTION FitCoeff_1D_WriteFile

  
  FUNCTION FitCoeff_2D_WriteFile( &
    FitCoeff, &  ! Input
    Filename, &  ! Input
    No_Close, &  ! Optional input
    Quiet   , &  ! Optional input
    Title   , &  ! Optional input
    History , &  ! Optional input
    Comment , &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(FitCoeff_2D_type), INTENT(IN) :: FitCoeff
    CHARACTER(*),           INTENT(IN) :: Filename
    LOGICAL     , OPTIONAL, INTENT(IN) :: No_Close
    LOGICAL     , OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    LOGICAL     , OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'FitCoeff_WriteFile'
    ! Insert common code
    INCLUDE 'FitCoeff_WriteFile.inc'
  END FUNCTION FitCoeff_2D_WriteFile

  
  FUNCTION FitCoeff_3D_WriteFile( &
    FitCoeff, &  ! Input
    Filename, &  ! Input
    No_Close, &  ! Optional input
    Quiet   , &  ! Optional input
    Title   , &  ! Optional input
    History , &  ! Optional input
    Comment , &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(FitCoeff_3D_type), INTENT(IN) :: FitCoeff
    CHARACTER(*),           INTENT(IN) :: Filename
    LOGICAL     , OPTIONAL, INTENT(IN) :: No_Close
    LOGICAL     , OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    LOGICAL     , OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'FitCoeff_WriteFile'
    ! Insert common code
    INCLUDE 'FitCoeff_WriteFile.inc'
  END FUNCTION FitCoeff_3D_WriteFile

  
!################################################################################
!################################################################################
!##                                                                            ##
!##                          ## PRIVATE PROCEDURES ##                          ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       FitCoeff_Equal
!
! PURPOSE:
!       Pure function to test the equality of two FitCoeff objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = FitCoeff_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two FitCoeff objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       Any FitCoeff type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!--------------------------------------------------------------------------------

  PURE FUNCTION FitCoeff_1D_Equal( x, y ) RESULT( is_equal )
    TYPE(FitCoeff_1D_type), INTENT(IN) :: x, y
    INCLUDE 'FitCoeff_Equal.inc'
  END FUNCTION FitCoeff_1D_Equal


  PURE FUNCTION FitCoeff_2D_Equal( x, y ) RESULT( is_equal )
    TYPE(FitCoeff_2D_type), INTENT(IN) :: x, y
    INCLUDE 'FitCoeff_Equal.inc'
  END FUNCTION FitCoeff_2D_Equal


  PURE FUNCTION FitCoeff_3D_Equal( x, y ) RESULT( is_equal )
    TYPE(FitCoeff_3D_type), INTENT(IN) :: x, y
    INCLUDE 'FitCoeff_Equal.inc'
  END FUNCTION FitCoeff_3D_Equal

END MODULE FitCoeff_Define
