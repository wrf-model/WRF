!
! CRTM_AtmOptics_Define
!
! Module defining the CRTM AtmOptics object.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 11-Oct-2011
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_AtmOptics_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp, Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: DEFAULT_N_SIGFIG, &
                                   OPERATOR(.EqualTo.), &
                                   Compares_Within_Tolerance
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
  ! Datatypes
  PUBLIC :: CRTM_AtmOptics_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(-)
  ! Procedures
  PUBLIC :: CRTM_AtmOptics_Associated
  PUBLIC :: CRTM_AtmOptics_Destroy
  PUBLIC :: CRTM_AtmOptics_Create
  PUBLIC :: CRTM_AtmOptics_Zero
  PUBLIC :: CRTM_AtmOptics_Inspect
  PUBLIC :: CRTM_AtmOptics_ValidRelease
  PUBLIC :: CRTM_AtmOptics_Info
  PUBLIC :: CRTM_AtmOptics_DefineVersion
  PUBLIC :: CRTM_AtmOptics_Compare
  PUBLIC :: CRTM_AtmOptics_InquireFile
  PUBLIC :: CRTM_AtmOptics_ReadFile
  PUBLIC :: CRTM_AtmOptics_WriteFile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_AtmOptics_Equal
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(-)
    MODULE PROCEDURE CRTM_AtmOptics_Subtract
  END INTERFACE OPERATOR(-)

  INTERFACE CRTM_AtmOptics_Inspect
    MODULE PROCEDURE Scalar_Inspect
    MODULE PROCEDURE Rank1_Inspect
  END INTERFACE CRTM_AtmOptics_Inspect

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: CRTM_AtmOptics_Define.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  ! Release and version
  INTEGER, PARAMETER :: ATMOPTICS_RELEASE = 4  ! This determines structure and file formats.
  ! Close status for write errors
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! String lengths
  INTEGER,  PARAMETER :: ML = 256 ! Message length
  INTEGER,  PARAMETER :: SL =  80 ! String length


  ! ------------------------------
  ! AtmOptics data type definition
  ! ------------------------------
  !:tdoc+:
  TYPE :: CRTM_AtmOptics_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release information
    INTEGER :: Release = ATMOPTICS_RELEASE
    ! Dimensions
    INTEGER :: n_Layers           = 0  ! K dimension
    INTEGER :: n_Legendre_Terms   = 0  ! Ic dimension
    INTEGER :: n_Phase_Elements   = 0  ! Ip dimension
    ! ...Max. values to avoid unnecessary reallocation
    INTEGER :: Max_Layers         = 0  ! K-Max dimension
    INTEGER :: Max_Legendre_Terms = 0  ! Ic-Max dimension
    INTEGER :: Max_Phase_Elements = 0  ! Ip-Max dimension
    ! Scalar components
    LOGICAL  :: Include_Scattering = .TRUE.
    INTEGER  :: lOffset = 0   ! Start position in array for Legendre coefficients
    REAL(fp) :: Scattering_Optical_Depth = ZERO
    ! Array components
    REAL(fp), ALLOCATABLE :: Optical_Depth(:)         ! K-Max
    REAL(fp), ALLOCATABLE :: Single_Scatter_Albedo(:) ! K-Max
    REAL(fp), ALLOCATABLE :: Asymmetry_Factor(:)      ! K-Max
    REAL(fp), ALLOCATABLE :: Delta_Truncation(:)      ! K-Max
    REAL(fp), ALLOCATABLE :: Phase_Coefficient(:,:,:) ! 0:Ic-Max x Ip-Max x K-Max
  END TYPE CRTM_AtmOptics_type
  !:tdoc-:


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AtmOptics_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the AtmOptics structure.
!
! CALLING SEQUENCE:
!       Status = CRTM_AtmOptics_Associated( AtmOptics )
!
! OBJECTS:
!       AtmOptics:  Structure which is to have its member's
!                   status tested.
!                   UNITS:      N/A
!                   TYPE:       CRTM_AtmOptics_type
!                   DIMENSION:  Scalar or any rank
!                   ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:     The return value is a logical value indicating the
!                   status of the NLTE members.
!                    .TRUE.  - if ANY of the AtmOptics allocatable members
!                              are in use.
!                    .FALSE. - if ALL of the AtmOptics allocatable members
!                              are not in use.
!                   UNITS:      N/A
!                   TYPE:       LOGICAL
!                   DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_AtmOptics_Associated( self ) RESULT( status )
    TYPE(CRTM_AtmOptics_type), INTENT(IN) :: self
    LOGICAL :: status
    status = self%Is_Allocated
  END FUNCTION CRTM_AtmOptics_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AtmOptics_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize AtmOptics objects.
!
! CALLING SEQUENCE:
!       CALL CRTM_AtmOptics_Destroy( AtmOptics )
!
! OBJECTS:
!       AtmOptics: Re-initialized AtmOptics structure.
!                  UNITS:      N/A
!                  TYPE:       CRTM_AtmOptics_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_AtmOptics_Destroy( self )
    TYPE(CRTM_AtmOptics_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
    self%n_Layers           = 0
    self%n_Legendre_Terms   = 0
    self%n_Phase_Elements   = 0
    self%Max_Layers         = 0
    self%Max_Legendre_Terms = 0
    self%Max_Phase_Elements = 0
  END SUBROUTINE CRTM_AtmOptics_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AtmOptics_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of an AtmOptics object.
!
! CALLING SEQUENCE:
!       CALL CRTM_AtmOptics_Create( AtmOptics       , &
!                                   n_Layers        , &
!                                   n_Legendre_Terms, &
!                                   n_Phase_Elements  )
!
! OBJECTS:
!       AtmOptics:          AtmOptics object structure.
!                           UNITS:      N/A
!                           TYPE:       CRTM_AtmOptics_type
!                           DIMENSION:  Scalar or any rank
!                           ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       n_Layers:           Number of atmospheric layers.
!                           Must be > 0
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Conformable with the AtmOptics object.
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Legendre_Terms:   The number of Legendre polynomial terms for the
!                           phase matrix.
!                           Must be > 0
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Same as n_Layers input.
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Phase_Elements:   The number of phase elements for the phase matrix.
!                           Must be > 0
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Same as n_Layers input.
!                           ATTRIBUTES: INTENT(IN)
!
! COMMENTS:
!       Note the INTENT on the output AtmOptics argument is IN OUT rather than
!       just OUT. If the AtmOptics is already allocated to sufficient size for
!       the passed dimensions, no reallocation is performed, only the internal
!       dimension values are reset.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_AtmOptics_Create( &
    self            , &  ! Output
    n_Layers        , &  ! Input
    n_Legendre_Terms, &  ! Input
    n_Phase_Elements  )  ! Input
    ! Arguments
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: self
    INTEGER                  , INTENT(IN)     :: n_Layers
    INTEGER                  , INTENT(IN)     :: n_Legendre_Terms
    INTEGER                  , INTENT(IN)     :: n_Phase_Elements
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Layers         < 1 .OR. &
         n_Legendre_Terms < 1 .OR. &
         n_Phase_Elements < 1 ) THEN
      CALL CRTM_AtmOptics_Destroy( self )
      RETURN
    END IF

    ! Check the allocation status
    IF ( CRTM_AtmOptics_Associated( self ) ) THEN
      ! ...Does it need to be reallocated?
      IF ( self%Max_Layers         < n_Layers         .OR. &
           self%Max_Legendre_Terms < n_Legendre_Terms .OR. &
           self%Max_Phase_Elements < n_Phase_Elements ) THEN
        CALL AtmOptics_Allocate( self, alloc_stat )
        IF ( alloc_stat /= 0 ) RETURN
      END IF
    ELSE
      ! ...Not allocated, so allocate it.
      CALL AtmOptics_Allocate( self, alloc_stat )
      IF ( alloc_stat /= 0 ) RETURN
    END IF

    ! Initialise dimensions (but not arrays!)
    self%n_Layers         = n_Layers
    self%n_Legendre_Terms = n_Legendre_Terms
    self%n_Phase_Elements = n_Phase_Elements

    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  CONTAINS

!cray/pkb add elemental
    ELEMENTAL PURE SUBROUTINE AtmOptics_Allocate(self,alloc_stat)
      TYPE(CRTM_AtmOptics_type), INTENT(OUT) :: self
      INTEGER                  , INTENT(OUT) :: alloc_stat
      ! Allocate object
      ALLOCATE( self%Optical_Depth( n_Layers ), &
                self%Single_Scatter_Albedo( n_Layers ), &
                self%Asymmetry_Factor( n_Layers ), &
                self%Delta_Truncation( n_Layers ), &
                self%Phase_Coefficient( 0:n_Legendre_Terms, n_Phase_Elements, n_Layers ), &
                STAT = alloc_stat )
      IF ( alloc_stat /= 0 ) RETURN
      ! Set maximum dimension values
      self%Max_Layers         = n_Layers
      self%Max_Legendre_Terms = n_Legendre_Terms
      self%Max_Phase_Elements = n_Phase_Elements
    END SUBROUTINE AtmOptics_Allocate

  END SUBROUTINE CRTM_AtmOptics_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AtmOptics_Zero
!
! PURPOSE:
!       Elemental subroutine to initialise the components of an AtmOptics
!       object to a value of zero.
!
! CALLING SEQUENCE:
!       CALL CRTM_AtmOptics_Zero( AtmOptics )
!
! OBJECTS:
!       AtmOptics:   AtmOptics object which is to have its components
!                    set to a zero value.
!                    UNITS:      N/A
!                    TYPE:       CRTM_AtmOptics_type
!                    DIMENSION:  Scalar or any rank
!                    ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CRTM_AtmOptics_Zero( self )
    TYPE(CRTM_AtmOptics_type), INTENT(IN OUT) :: self
    self%Scattering_Optical_Depth = ZERO
    IF ( .NOT. CRTM_AtmOptics_Associated( self ) ) RETURN
    self%Optical_Depth         = ZERO
    self%Single_Scatter_Albedo = ZERO
    self%Asymmetry_Factor      = ZERO
    self%Delta_Truncation      = ZERO
    self%Phase_Coefficient     = ZERO
  END SUBROUTINE CRTM_AtmOptics_Zero


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AtmOptics_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a AtmOptics object to stdout.
!
! CALLING SEQUENCE:
!       CALL CRTM_AtmOptics_Inspect( AtmOptics )
!
! OBJECTS:
!       AtmOptics:     AtmOptics object to display.
!                      UNITS:      N/A
!                      TYPE:       CRTM_AtmOptics_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Scalar_Inspect(self)
    TYPE(CRTM_AtmOptics_type), INTENT(IN) :: self
    INTEGER :: ip, k
    WRITE(*,'(1x,"AtmOptics OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release          : ",i0)') self%Release
    ! Dimensions
    WRITE(*,'(3x,"n_Layers         : ",i0," (of max. ",i0,")")') self%n_Layers        , self%Max_Layers
    WRITE(*,'(3x,"n_Legendre_Terms : ",i0," (of max. ",i0,")")') self%n_Legendre_Terms, self%Max_Legendre_Terms
    WRITE(*,'(3x,"n_Phase_Elements : ",i0," (of max. ",i0,")")') self%n_Phase_Elements, self%Max_Phase_Elements
    WRITE(*,'(3x,"Scattering Optical Depth : ",es13.6)') self%Scattering_Optical_Depth
    IF ( .NOT. CRTM_AtmOptics_Associated(self) ) RETURN
    ! Dimension arrays
    WRITE(*,'(3x,"Optical_Depth :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Optical_Depth(1:self%n_Layers)
    WRITE(*,'(3x,"Single_Scatter_Albedo :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Single_Scatter_Albedo(1:self%n_Layers)
    WRITE(*,'(3x,"Asymmetry_Factor :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Asymmetry_Factor(1:self%n_Layers)
    WRITE(*,'(3x,"Delta_Truncation :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Delta_Truncation(1:self%n_Layers)
    WRITE(*,'(3x,"Phase_Coefficient Legendre polynomial coefficients :")')
    DO k = 1, self%n_Layers
      DO ip = 1, self%n_Phase_Elements
        WRITE(*,'(5x,"Layer: ",i0,"; Phase element: ",i0)') k, ip
        WRITE(*,'(5(1x,es13.6,:))') self%Phase_Coefficient(0:self%n_Legendre_Terms,ip,k)
      END DO
      WRITE(*,*)
    END DO
  END SUBROUTINE Scalar_Inspect

  SUBROUTINE Rank1_Inspect( self )
    TYPE(CRTM_AtmOptics_type), INTENT(IN) :: self(:)
    INTEGER :: n, n_objects

    n_objects = SIZE(self)
    DO n = 1, n_objects
      WRITE(*, FMT='(1x,"OBJECT INDEX:",i0," - ")', ADVANCE='NO') n
      CALL Scalar_Inspect(self(n))
    END DO
  END SUBROUTINE Rank1_Inspect


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AtmOptics_ValidRelease
!
! PURPOSE:
!       Function to check the AtmOptics object Release value.
!
! CALLING SEQUENCE:
!       IsValid = CRTM_AtmOptics_ValidRelease( AtmOptics )
!
! OBJECTS:
!       AtmOptics:     AtmOptics object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       CRTM_AtmOptics_type
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

  FUNCTION CRTM_AtmOptics_ValidRelease( self ) RESULT( IsValid )
    ! Arguments
    TYPE(CRTM_AtmOptics_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AtmOptics_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( self%Release < ATMOPTICS_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An AtmOptics data update is needed. ", &
                  &"AtmOptics release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, ATMOPTICS_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF


    ! Check release is not too new
    IF ( self%Release > ATMOPTICS_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An AtmOptics software update is needed. ", &
                  &"AtmOptics release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, ATMOPTICS_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF

  END FUNCTION CRTM_AtmOptics_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AtmOptics_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about an AtmOptics object.
!
! CALLING SEQUENCE:
!       CALL CRTM_AtmOptics_Info( AtmOptics, Info )
!
! OBJECTS:
!       AtmOptics:     AtmOptics object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       CRTM_AtmOptics_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the AtmOptics object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_AtmOptics_Info( self, Info )
    ! Arguments
    TYPE(CRTM_AtmOptics_type), INTENT(IN)  :: self
    CHARACTER(*),              INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"AtmOptics RELEASE: ",i2,3x, &
           &"N_LAYERS=",i0,2x,&
           &"N_LEGENDRE_TERMS=",i0,2x,&
           &"N_PHASE_ELEMENTS=",i0 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%Release, &
           self%n_Layers, &
           self%n_Legendre_Terms, &
           self%n_Phase_Elements

    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE CRTM_AtmOptics_Info


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AtmOptics_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_AtmOptics_DefineVersion( Id )
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

  SUBROUTINE CRTM_AtmOptics_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_AtmOptics_DefineVersion


!--------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       CRTM_AtmOptics_Compare
!
! PURPOSE:
!       Elemental function to compare two CRTM_AtmOptics objects to within
!       a user specified number of significant figures.
!
! CALLING SEQUENCE:
!       is_comparable = CRTM_AtmOptics_Compare( x, y, n_SigFig=n_SigFig )
!
! OBJECTS:
!       x, y:          Two CRTM AtmOptics objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_AtmOptics_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       n_SigFig:      Number of significant figures to compare floating point
!                      components.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar or same as input
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_AtmOptics_Compare( &
    x, &
    y, &
    n_SigFig ) &
  RESULT( is_comparable )
    TYPE(CRTM_AtmOptics_type), INTENT(IN) :: x, y
    INTEGER,         OPTIONAL, INTENT(IN) :: n_SigFig
    LOGICAL :: is_comparable
    ! Variables
    INTEGER :: ic, ip, k
    INTEGER :: n

    ! Set up
    is_comparable = .FALSE.
    IF ( PRESENT(n_SigFig) ) THEN
      n = ABS(n_SigFig)
    ELSE
      n = DEFAULT_N_SIGFIG
    END IF

    ! Check the structure association status
    IF ( (.NOT. CRTM_AtmOptics_Associated(x)) .OR. &
         (.NOT. CRTM_AtmOptics_Associated(y))      ) RETURN

    ! Check dimensions
    IF ( (x%n_Layers         /= y%n_Layers        ) .OR. &
         (x%n_Legendre_Terms /= y%n_Legendre_Terms) .OR. &
         (x%n_Phase_Elements /= y%n_Phase_Elements)      ) RETURN

    ! Check scalar components
    IF ( (x%Include_Scattering .NEQV. y%Include_Scattering ) .OR. &
         (x%lOffset             /=    y%lOffset            ) .OR. &
         (.NOT. Compares_Within_Tolerance(x%Scattering_Optical_Depth,&
                                          y%Scattering_Optical_Depth, n)) ) RETURN


    ! Check floating point arrays
    k  = x%n_Layers
    ip = x%n_Phase_Elements
    ic = x%n_Legendre_Terms
    IF ( (.NOT. ALL(Compares_Within_Tolerance( &
                      x%Optical_Depth(1:k), &
                      y%Optical_Depth(1:k), &
                      n                      ))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance( &
                      x%Single_Scatter_Albedo(1:k), &
                      y%Single_Scatter_Albedo(1:k), &
                      n                      ))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance( &
                      x%Asymmetry_Factor(1:k), &
                      y%Asymmetry_Factor(1:k), &
                      n                      ))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance( &
                      x%Delta_Truncation(1:k), &
                      y%Delta_Truncation(1:k), &
                      n                      ))) .OR. &
         (.NOT. ALL(Compares_Within_Tolerance( &
                      x%Phase_Coefficient(0:ic,1:ip,1:k), &
                      y%Phase_Coefficient(0:ic,1:ip,1:k), &
                      n                      ))) ) RETURN


    ! If we get here, the structures are comparable
    is_comparable = .TRUE.

  END FUNCTION CRTM_AtmOptics_Compare


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AtmOptics_InquireFile
!
! PURPOSE:
!       Function to inquire AtmOptics object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_AtmOptics_InquireFile( &
!                        Filename             , &
!                        n_Objects = n_Objects, &
!                        Release   = Release    )
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
!       n_Objects:          Number of AtmOptics objects contained in the file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The data/file release number. Used to check
!                           for data/software mismatch.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
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

  FUNCTION CRTM_AtmOptics_InquireFile( &
    Filename , &  ! Input
    n_Objects, &  ! Optional output
    Release  , &  ! Optional output
    Title    , &  ! Optional output
    History  , &  ! Optional output
    Comment  ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Objects
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AtmOptics_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: n
    TYPE(CRTM_AtmOptics_type) :: atmoptics


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


    ! Read the release
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      atmoptics%Release
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Release - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF
    IF ( .NOT. CRTM_AtmOptics_ValidRelease( AtmOptics ) ) THEN
      msg = 'AtmOptics Release check failed.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the number of objects
    READ( fid, IOSTAT=io_stat,IOMSG=io_msg ) n
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimensions from '//TRIM(Filename)//' - '//TRIM(io_msg)
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
    IF ( PRESENT(n_Objects) ) n_Objects = n
    IF ( PRESENT(Release  ) ) Release   = AtmOptics%Release

  CONTAINS

    SUBROUTINE Inquire_CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION CRTM_AtmOptics_InquireFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AtmOptics_ReadFile
!
! PURPOSE:
!       Function to read AtmOptics object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_AtmOptics_ReadFile( &
!                        AtmOptics          , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet     )
!
! OBJECTS:
!       AtmOptics:      AtmOptics object array containing the data read from file.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmOptics_type
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(OUT), ALLOCATABLE
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       AtmOptics data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the AtmOptics data is embedded within another file.
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

  FUNCTION CRTM_AtmOptics_ReadFile( &
    AtmOptics, &  ! Output
    Filename , &  ! Input
    No_Close , &  ! Optional input
    Quiet    , &  ! Optional input
    Title    , &  ! Optional output
    History  , &  ! Optional output
    Comment  , &  ! Optional output
    Debug    ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_AtmOptics_type), ALLOCATABLE, INTENT(OUT) :: AtmOptics(:)
    CHARACTER(*),                           INTENT(IN)  :: Filename
    LOGICAL,                      OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL,                      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),                 OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*),                 OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*),                 OPTIONAL, INTENT(OUT) :: Comment
    LOGICAL,                      OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AtmOptics_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: count_msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: alloc_stat
    INTEGER :: fid
    INTEGER :: n, n_objects
    TYPE(CRTM_AtmOptics_type) :: dummy

    ! Setup
    err_stat = SUCCESS
    ! ...Check No_Close argument
    close_file = .TRUE.
    IF ( PRESENT(No_Close) ) close_file = .NOT. No_Close
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF


    ! Check if the file is open.
    IF ( File_Open( Filename ) ) THEN
      ! ...Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=fid )
      ! ...Ensure it's valid
      IF ( fid < 0 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its FileID'
        CALL Read_CleanUp(); RETURN
      END IF
    ELSE
      ! ...Open the file if it exists
      IF ( File_Exists( Filename ) ) THEN
        err_stat = Open_Binary_File( Filename, fid )
        IF ( err_Stat /= SUCCESS ) THEN
          msg = 'Error opening '//TRIM(Filename)
          CALL Read_CleanUp(); RETURN
        END IF
      ELSE
        msg = 'File '//TRIM(Filename)//' not found.'
        CALL Read_CleanUp(); RETURN
      END IF
    END IF


    ! Read and check the release and version
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      dummy%Release
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Release/Version - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( .NOT. CRTM_AtmOptics_ValidRelease( dummy ) ) THEN
      msg = 'Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the number of objects
    READ( fid, IOSTAT=io_stat,IOMSG=io_msg ) n_objects
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimensions from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the output array
    ALLOCATE( AtmOptics(n_objects), STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      msg = 'Error allocating output object array'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the global attributes
    err_stat = ReadGAtts_Binary_File( &
                 fid, &
                 Title   = Title  , &
                 History = History, &
                 Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Loop over all the objects
    Read_Loop: DO n = 1, n_objects


      ! Generate count message for error output
      WRITE(count_msg,'("for object (",i0,")")') n


      ! Read the dimensions
      READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
        dummy%n_Layers        , &
        dummy%n_Legendre_Terms, &
        dummy%n_Phase_Elements
      IF ( io_stat /= 0 ) THEN
        msg = 'Error reading dimensions '//TRIM(count_msg)//' from '//&
              TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Allocate the object
      CALL CRTM_AtmOptics_Create( &
             AtmOptics(n)          , &
             dummy%n_Layers        , &
             dummy%n_Legendre_Terms, &
             dummy%n_Phase_Elements  )
      IF ( .NOT. CRTM_AtmOptics_Associated( AtmOptics(n) ) ) THEN
        msg = 'Allocation failed '//TRIM(count_msg)
        CALL Read_Cleanup(); RETURN
      END IF


      ! Read the data
      ! ...Read the scalar data
      READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
        AtmOptics(n)%Scattering_Optical_Depth
      IF ( io_stat /= 0 ) THEN
        msg = 'Error reading scalar data '//TRIM(count_msg)//' from '//&
              TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Read the profile data
      READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
        AtmOptics(n)%Optical_Depth        , &
        AtmOptics(n)%Single_Scatter_Albedo, &
        AtmOptics(n)%Asymmetry_Factor     , &
        AtmOptics(n)%Delta_Truncation
      IF ( io_stat /= 0 ) THEN
        msg = 'Error reading profile data '//TRIM(count_msg)//' from '//&
              TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Read_Cleanup(); RETURN
      END IF
      ! ...Read the scattering phase matrix coefficients
      READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
        AtmOptics(n)%Phase_Coefficient
      IF ( io_stat /= 0 ) THEN
        msg = 'Error reading phase matrix coefficients '//TRIM(count_msg)//' from '//&
              TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Read_Cleanup(); RETURN
      END IF

    END DO Read_Loop


    ! Close the file
    IF ( close_file ) THEN
      CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Read_Cleanup(); RETURN
      END IF
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of objects read from ",a,": ",i0)' ) TRIM(Filename), n_objects
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Read_Cleanup()
      IF ( File_Open(Filename) ) THEN
        CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      IF ( ALLOCATED(AtmOptics) ) THEN
        DEALLOCATE( AtmOptics, STAT=alloc_stat )
        IF ( alloc_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deallocating object array during error cleanup'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION CRTM_AtmOptics_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_AtmOptics_WriteFile
!
! PURPOSE:
!       Function to write AtmOptics object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_AtmOptics_WriteFile( &
!                        AtmOptics          , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet     )
!
! OBJECTS:
!       AtmOptics:      AtmOptics object array containing the data to write to file.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmOptics_type
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       AtmOptics format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the AtmOptics data is to be embedded within another file.
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

  FUNCTION CRTM_AtmOptics_WriteFile( &
    AtmOptics, &  ! Input
    Filename , &  ! Input
    No_Close , &  ! Optional input
    Quiet    , &  ! Optional input
    Title    , &  ! Optional input
    History  , &  ! Optional input
    Comment  , &  ! Optional input
    Debug    ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_AtmOptics_type), INTENT(IN) :: AtmOptics(:)
    CHARACTER(*),              INTENT(IN) :: Filename
    LOGICAL,         OPTIONAL, INTENT(IN) :: No_Close
    LOGICAL,         OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*),    OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*),    OPTIONAL, INTENT(IN) :: History
    CHARACTER(*),    OPTIONAL, INTENT(IN) :: Comment
    LOGICAL,         OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_AtmOptics_WriteFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: count_msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: n, n_objects


    ! Setup
    err_stat = SUCCESS
    ! ...Check No_Close argument
    close_file = .TRUE.
    IF ( PRESENT(No_Close) ) close_file = .NOT. No_Close
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF
    ! ...Check there is data to write
    IF ( .NOT. ALL(CRTM_AtmOptics_Associated( AtmOptics )) ) THEN
      msg = 'Unassociated objects in input array.'
      CALL Write_Cleanup(); RETURN
    END IF
    n_objects= SIZE(AtmOptics)


    ! Check if the file is open.
    IF ( File_Open( FileName ) ) THEN
      ! ...Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=fid )
      ! ...Ensure it's valid
      IF ( fid < 0 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its FileID'
        CALL Write_CleanUp(); RETURN
      END IF
    ELSE
      ! ...Open the file for output
      err_stat = Open_Binary_File( Filename, fid, For_Output=.TRUE. )
      IF ( err_Stat /= SUCCESS ) THEN
        msg = 'Error opening '//TRIM(Filename)
        CALL Write_CleanUp(); RETURN
      END IF
    END IF


    ! Write the release
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) ATMOPTICS_RELEASE
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Release - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the number of objects
    WRITE( fid, IOSTAT=io_stat,IOMSG=io_msg ) SIZE(AtmOptics)
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimensions to '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the global attributes
    err_stat = WriteGAtts_Binary_File( &
                 fid, &
                 Write_Module = MODULE_VERSION_ID, &
                 Title        = Title  , &
                 History      = History, &
                 Comment      = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing global attributes'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Loop over all the objects
    Write_Loop: DO n = 1, n_objects


      ! Generate count message for error output
      WRITE(count_msg,'("for object (",i0,")")') n


      ! Write the dimensions
      WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
        AtmOptics(n)%n_Layers        , &
        AtmOptics(n)%n_Legendre_Terms, &
        AtmOptics(n)%n_Phase_Elements
      IF ( io_stat /= 0 ) THEN
        msg = 'Error writing dimensions '//TRIM(count_msg)//' to '//&
              TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Write_Cleanup(); RETURN
      END IF


      ! Write the data
      ! ...Write the scalar data
      WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
        AtmOptics(n)%Scattering_Optical_Depth
      IF ( io_stat /= 0 ) THEN
        msg = 'Error writing scalar data '//TRIM(count_msg)//' to '//&
              TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Write the profile data
      WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
        AtmOptics(n)%Optical_Depth(1:AtmOptics(n)%n_Layers)        , &
        AtmOptics(n)%Single_Scatter_Albedo(1:AtmOptics(n)%n_Layers), &
        AtmOptics(n)%Asymmetry_Factor(1:AtmOptics(n)%n_Layers)     , &
        AtmOptics(n)%Delta_Truncation(1:AtmOptics(n)%n_Layers)
      IF ( io_stat /= 0 ) THEN
        msg = 'Error writing profile data '//TRIM(count_msg)//' to '//&
              TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Write_Cleanup(); RETURN
      END IF
      ! ...Write the scattering phase matrix coefficients
      WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
        AtmOptics(n)%Phase_Coefficient(0:AtmOptics(n)%n_Legendre_Terms, &
                                       1:AtmOptics(n)%n_Phase_Elements, &
                                       1:AtmOptics(n)%n_Layers)
      IF ( io_stat /= 0 ) THEN
        msg = 'Error writing phase matrix coefficients '//TRIM(count_msg)//' to '//&
              TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Write_Cleanup(); RETURN
      END IF

    END DO Write_Loop


    ! Close the file
    IF ( close_file ) THEN
      CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Output an info message
     IF ( noisy ) THEN
      WRITE( msg,'("Number of objects written to ",a,": ",i0)' ) TRIM(Filename), n_objects
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
     END IF

   CONTAINS

     SUBROUTINE Write_Cleanup()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing output file during error cleanup - '//TRIM(io_msg)
       END IF
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Write_Cleanup

  END FUNCTION CRTM_AtmOptics_WriteFile


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
!       CRTM_AtmOptics_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two AtmOptics objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = CRTM_AtmOptics_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two AtmOptics objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       CRTM_AtmOptics_type
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

  ELEMENTAL FUNCTION CRTM_AtmOptics_Equal( x, y ) RESULT( is_equal )
    TYPE(CRTM_AtmOptics_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.

    ! Check the object association status
    IF ( (.NOT. CRTM_AtmOptics_Associated(x)) .OR. &
         (.NOT. CRTM_AtmOptics_Associated(y))      ) RETURN

    ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) ) RETURN
    ! ...Dimensions
    IF ( (x%n_Layers         /= y%n_Layers        ) .OR. &
         (x%n_Legendre_Terms /= y%n_Legendre_Terms) .OR. &
         (x%n_Phase_Elements /= y%n_Phase_Elements) ) RETURN
    ! ...Scalar data
    IF ( x%Scattering_Optical_Depth .EqualTo. y%Scattering_Optical_Depth ) &
      is_equal = .TRUE.
    ! ...Array data
    is_equal = is_equal .AND. &
               ALL(x%Optical_Depth(1:x%n_Layers)         .EqualTo. y%Optical_Depth(1:y%n_Layers)        ) .AND. &
               ALL(x%Single_Scatter_Albedo(1:x%n_Layers) .EqualTo. y%Single_Scatter_Albedo(1:y%n_Layers)) .AND. &
               ALL(x%Asymmetry_Factor(1:x%n_Layers)      .EqualTo. y%Asymmetry_Factor(1:y%n_Layers)     ) .AND. &
               ALL(x%Delta_Truncation(1:x%n_Layers)      .EqualTo. y%Delta_Truncation(1:y%n_Layers)     ) .AND. &
               ALL(x%Phase_Coefficient(0:x%n_Legendre_Terms, 1:x%n_Phase_Elements, 1:x%n_Layers) .EqualTo. &
                   y%Phase_Coefficient(0:y%n_Legendre_Terms, 1:y%n_Phase_Elements, 1:y%n_Layers) )

  END FUNCTION CRTM_AtmOptics_Equal


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_AtmOptics_Subtract
!
! PURPOSE:
!       Pure function to subtract two CRTM AtmOptics objects.
!       Used in OPERATOR(-) interface block.
!
! CALLING SEQUENCE:
!       aodiff = CRTM_AtmOptics_Subtract( ao1, ao2 )
!
!         or
!
!       aodiff = ao1 - ao2
!
!
! INPUTS:
!       ao1, ao2: The AtmOptics objects to difference.
!                   UNITS:      N/A
!                   TYPE:       CRTM_AtmOptics_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN OUT)
!
! RESULT:
!       aodiff:    AtmOptics object containing the differenced components.
!                   UNITS:      N/A
!                   TYPE:       CRTM_AtmOptics_type
!                   DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CRTM_AtmOptics_Subtract( ao1, ao2 ) RESULT( aodiff )
    TYPE(CRTM_AtmOptics_type), INTENT(IN) :: ao1, ao2
    TYPE(CRTM_AtmOptics_type) :: aodiff
    INTEGER :: ic, ip, k

    ! Check input
    ! ...If input structures not allocated, do nothing
    IF ( (.NOT. CRTM_AtmOptics_Associated(ao1)) .OR. &
         (.NOT. CRTM_AtmOptics_Associated(ao2))      ) RETURN
    ! ...If input structure for different sizes, do nothing
    IF ( ao1%n_Layers         /= ao2%n_Layers         .OR. &
         ao1%n_Legendre_Terms /= ao2%n_Legendre_Terms .OR. &
         ao1%n_Phase_Elements /= ao2%n_Phase_Elements ) RETURN
    ! ...If input structure for different scattering setup, do nothing
    IF ( (ao1%Include_Scattering .NEQV. ao2%Include_Scattering ) .AND. &
         (ao1%lOffset             /=    ao2%lOffset            ) ) RETURN

    ! Copy the first structure
    aodiff = ao1

    ! And subtract the second one's components from it
    ! ...The scalar values
    aodiff%Scattering_Optical_Depth = aodiff%Scattering_Optical_Depth - ao2%Scattering_Optical_Depth
    ! ...The arrays
    k  = aodiff%n_Layers
    ip = aodiff%n_Phase_Elements
    ic = aodiff%n_Legendre_Terms
    aodiff%Optical_Depth(1:k)               = aodiff%Optical_Depth(1:k)               - ao2%Optical_Depth(1:k)
    aodiff%Single_Scatter_Albedo(1:k)       = aodiff%Single_Scatter_Albedo(1:k)       - ao2%Single_Scatter_Albedo(1:k)
    aodiff%Asymmetry_Factor(1:k)            = aodiff%Asymmetry_Factor(1:k)            - ao2%Asymmetry_Factor(1:k)
    aodiff%Delta_Truncation(1:k)            = aodiff%Delta_Truncation(1:k)            - ao2%Delta_Truncation(1:k)
    aodiff%Phase_Coefficient(0:ic,1:ip,1:k) = aodiff%Phase_Coefficient(0:ic,1:ip,1:k) - ao2%Phase_Coefficient(0:ic,1:ip,1:k)

  END FUNCTION CRTM_AtmOptics_Subtract

END MODULE CRTM_AtmOptics_Define
