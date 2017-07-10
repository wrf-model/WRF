!
! LSEatlas_Define
!
! Module defining the LSEatlas object.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 17-Aug-2011
!                       paul.vandelst@noaa.gov
 
MODULE LSEatlas_Define

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
  PUBLIC :: LSEATLAS_DATATYPE
  ! Datatypes
  PUBLIC :: LSEatlas_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: LSEatlas_Associated
  PUBLIC :: LSEatlas_Destroy
  PUBLIC :: LSEatlas_Create
  PUBLIC :: LSEatlas_Inspect
  PUBLIC :: LSEatlas_ValidRelease
  PUBLIC :: LSEatlas_Info
  PUBLIC :: LSEatlas_Name
  PUBLIC :: LSEatlas_DefineVersion
  PUBLIC :: LSEatlas_SetValue
  PUBLIC :: LSEatlas_GetValue
  PUBLIC :: LSEatlas_InquireFile
  PUBLIC :: LSEatlas_ReadFile
  PUBLIC :: LSEatlas_WriteFile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE LSEatlas_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id: LSEatlas_Define.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  ! Datatype information
  CHARACTER(*), PARAMETER :: LSEATLAS_DATATYPE = 'LSEatlas'
  ! Current valid release and version
  INTEGER, PARAMETER :: LSEATLAS_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PARAMETER :: LSEATLAS_VERSION = 1  ! This is just the default data version.
  ! Close status for write errors
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! String lengths
  INTEGER,  PARAMETER :: ML = 256 ! Message length
  INTEGER,  PARAMETER :: SL =  80 ! String length


  ! ----------------------------------
  ! LSEatlas data type definitions
  ! ----------------------------------
  !:tdoc+:
  TYPE :: LSEatlas_type
    PRIVATE
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Datatype information
    CHARACTER(SL) :: Datatype_Name = LSEATLAS_DATATYPE
    ! Release and version information
    INTEGER(Long) :: Release = LSEATLAS_RELEASE
    INTEGER(Long) :: Version = LSEATLAS_VERSION
    ! Dimensions
    INTEGER(Long) :: n_Frequencies = 0  ! I dim.
    INTEGER(Long) :: n_Latitudes   = 0  ! J dim.
    INTEGER(Long) :: n_Longitudes  = 0  ! K dim.
    ! Dimensional vectors
    REAL(Double),  ALLOCATABLE :: Frequency(:)      ! Ix1
    REAL(Double),  ALLOCATABLE :: Latitude(:)       ! Jx1
    REAL(Double),  ALLOCATABLE :: Longitude(:)      ! Kx1
    ! Emissivity LUT data
    REAL(Double),  ALLOCATABLE :: Emissivity(:,:,:)  ! IxJxK
  END TYPE LSEatlas_type
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
!       LSEatlas_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the LSEatlas structure.
!
! CALLING SEQUENCE:
!       Status = LSEatlas_Associated( LSEatlas )
!
! OBJECTS:
!       LSEatlas:   Structure which is to have its member's
!                   status tested.
!                   UNITS:      N/A
!                   TYPE:       LSEatlas_type
!                   DIMENSION:  Scalar or any rank
!                   ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:     The return value is a logical value indicating the
!                   status of the members.
!                    .TRUE.  - if ANY of the allocatable members
!                              are in use.
!                    .FALSE. - if ALL of the allocatable members
!                              are not in use.
!                   UNITS:      N/A
!                   TYPE:       LOGICAL
!                   DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION LSEatlas_Associated( self ) RESULT( Status )
    TYPE(LSEatlas_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION LSEatlas_Associated

  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEatlas_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize LSEatlas objects.
!
! CALLING SEQUENCE:
!       CALL LSEatlas_Destroy( LSEatlas )
!
! OBJECTS:
!       LSEatlas:     Re-initialized LSEatlas structure.
!                     UNITS:      N/A
!                     TYPE:       LSEatlas_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE LSEatlas_Destroy( self )
    TYPE(LSEatlas_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
    self%n_Frequencies = 0
    self%n_Latitudes   = 0
    self%n_Longitudes  = 0
  END SUBROUTINE LSEatlas_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEatlas_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of an LSEatlas object.
!
! CALLING SEQUENCE:
!       CALL LSEatlas_Create( LSEatlas     , &
!                             n_Frequencies, &     
!                             n_Latitudes  , &     
!                             n_Longitudes   )         
!
! OBJECTS:
!       LSEatlas:           LSEatlas object structure.
!                           UNITS:      N/A
!                           TYPE:       LSEatlas_type
!                           DIMENSION:  Scalar or any rank
!                           ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Frequencies:      Number of spectral frequencies for which there are
!                           data.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Conformable with the LSEatlas object
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Latitudes:        Number of latitude values for which there are
!                           data.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Conformable with the LSEatlas object
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Longitudes:       Number of longitude values for which there are
!                           data.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Conformable with the LSEatlas object
!                           ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE LSEatlas_Create( &
    self         , &  ! Output
    n_Frequencies, &  ! Input
    n_Latitudes  , &  ! Input
    n_Longitudes   )  ! Input
    ! Arguments
    TYPE(LSEatlas_type), INTENT(OUT) :: self
    INTEGER            , INTENT(IN)  :: n_Frequencies        
    INTEGER            , INTENT(IN)  :: n_Latitudes          
    INTEGER            , INTENT(IN)  :: n_Longitudes              
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Frequencies < 1 .OR. &
         n_Latitudes   < 1 .OR. &
         n_Longitudes  < 1 ) RETURN

   
    ! Perform the allocation
    ALLOCATE( self%Frequency( n_Frequencies ), &
              self%Latitude( n_Latitudes ), &
              self%Longitude( n_Longitudes ), &
              self%Emissivity( n_Frequencies, n_Latitudes, n_Longitudes ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    self%n_Frequencies = n_Frequencies
    self%n_Latitudes   = n_Latitudes  
    self%n_Longitudes  = n_Longitudes 
    ! ...Arrays
    self%Frequency  = ZERO
    self%Latitude   = ZERO
    self%Longitude  = ZERO
    self%Emissivity = ZERO

    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE LSEatlas_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEatlas_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a LSEatlas object to stdout.
!
! CALLING SEQUENCE:
!       CALL LSEatlas_Inspect( LSEatlas )
!
! OBJECTS:
!       LSEatlas:  LSEatlas object to display.
!                      UNITS:      N/A
!                      TYPE:       LSEatlas_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LSEatlas_Inspect( self)
    TYPE(LSEatlas_type), INTENT(IN) :: self
    WRITE(*,'(1x,"LSEatlas OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version  :",1x,i0,".",i0)') self%Release, self%Version
    ! Dimensions
    WRITE(*,'(3x,"n_Frequencies    :",1x,i0)') self%n_Frequencies
    WRITE(*,'(3x,"n_Latitudes      :",1x,i0)') self%n_Latitudes  
    WRITE(*,'(3x,"n_Longitudes     :",1x,i0)') self%n_Longitudes 
    IF ( .NOT. LSEatlas_Associated(self) ) RETURN
    ! Dimension arrays
    WRITE(*,'(3x,"Frequency :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Frequency
    WRITE(*,'(3x,"Latitude :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Latitude
    WRITE(*,'(3x,"Longitude :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Longitude
    ! Emissivity array
    WRITE(*,'(3x,"Emissivity :")')
    WRITE(*,'(5(1x,es13.6,:))') self%Emissivity
  END SUBROUTINE LSEatlas_Inspect



!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEatlas_ValidRelease
!
! PURPOSE:
!       Function to check the LSEatlas Release value.
!
! CALLING SEQUENCE:
!       IsValid = LSEatlas_ValidRelease( LSEatlas )
!
! INPUTS:
!       LSEatlas:      LSEatlas object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       LSEatlas_type
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

  FUNCTION LSEatlas_ValidRelease( self ) RESULT( IsValid )
    ! Arguments
    TYPE(LSEatlas_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LSEatlas_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( self%Release < LSEatlas_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An LSEatlas data update is needed. ", &
                  &"LSEatlas release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, LSEATLAS_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF


    ! Check release is not too new
    IF ( self%Release > LSEatlas_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("An LSEatlas software update is needed. ", &
                  &"LSEatlas release is ",i0,". Valid release is ",i0,"." )' ) &
                  self%Release, LSEATLAS_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION ); RETURN
    END IF

  END FUNCTION LSEatlas_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEatlas_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a LSEatlas object.
!
! CALLING SEQUENCE:
!       CALL LSEatlas_Info( LSEatlas, Info )
!
! OBJECTS:
!       LSEatlas:      LSEatlas object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       LSEatlas_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the LSEatlas object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LSEatlas_Info( self, Info )
    ! Arguments
    TYPE(LSEatlas_type), INTENT(IN)  :: self
    CHARACTER(*),            INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '(a,1x,"LSEatlas RELEASE.VERSION: ",i2,".",i2.2,a,3x, &
           &"N_FREQUENCIES=",i0,2x,&
           &"N_LATITUDES=",i0,2x,&
           &"N_LONGITUDES=",i0 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%Release, self%Version, &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           self%n_Frequencies, &
           self%n_Latitudes  , &
           self%n_Longitudes 
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE LSEatlas_Info
 
 
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEatlas_Name
!
! PURPOSE:
!       Function to return the datatype name of an LSEatlas object.
!
! CALLING SEQUENCE:
!       datatype_name = LSEatlas_Name( LSEatlas )         
!
! OBJECTS:
!       LSEatlas:     LSEatlas object structure.
!                     UNITS:      N/A
!                     TYPE:       LSEatlas_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:       The return value is a the character string containing
!                     the datatype name of the structure.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION LSEatlas_Name( self ) RESULT( datatype_name )
    ! Arguments
    TYPE(LSEatlas_type), INTENT(OUT) :: self
    ! Function result
    CHARACTER(LEN(self%Datatype_Name)) :: datatype_name
    
    datatype_name = self%Datatype_Name

  END FUNCTION LSEatlas_Name


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEatlas_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL LSEatlas_DefineVersion( Id )
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

  SUBROUTINE LSEatlas_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE LSEatlas_DefineVersion



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEatlas_SetValue
!
! PURPOSE:
!       Subroutine to set the contents of a valid LSEatlas object.
!
! CALLING SEQUENCE:
!       CALL LSEatlas_SetValue( LSEatlas, &
!                               Version    = Version   , &
!                               Frequency  = Frequency , &
!                               Latitude   = Latitude  , &
!                               Longitude  = Longitude , &
!                               Emissivity = Emissivity  )
!
! OBJECTS:
!       LSEatlas:           Valid, allocated LSEatlas object for which
!                           values are to be set.
!                           UNITS:      N/A
!                           TYPE:       LSEatlas_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Version:            Integer indicating the data version. If not specified
!                           the value of the module parameter LSEatlas_VERSION
!                           is used.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Frequency:          Real array to which the Frequency component of the
!                           LSEatlas object is to be set. The size of the
!                           input must match the allocated size of the component,
!                           otherwise all the component values are set to zero.
!                           UNITS:      N/A
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-1 (L)
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Latitude:           Real array to which the Latitude component of the
!                           LSEatlas object is to be set. The size of the
!                           input must match the allocated size of the component,
!                           otherwise all the component values are set to zero.
!                           UNITS:      N/A
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-1 (I)
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Longitude:          Real array to which the Longitude component of the
!                           LSEatlas object is to be set. The size of the
!                           input must match the allocated size of the component,
!                           otherwise all the component values are set to zero.
!                           UNITS:      N/A
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-1 (J)
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Emissivity:         Real array to which the Emissivity component of the
!                           LSEatlas object is to be set. The size of the
!                           input must match the allocated size of the component,
!                           otherwise all the component values are set to zero.
!                           UNITS:      N/A
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-3 (L x I x J)
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LSEatlas_SetValue( &
    self      , &  ! Input
    Version   , &  ! Optional input
    Frequency , &  ! Optional input
    Latitude  , &  ! Optional input
    Longitude , &  ! Optional input
    Emissivity  )  ! Optional input
    ! Arguments
    TYPE(LSEatlas_type), INTENT(IN OUT) :: self
    INTEGER ,  OPTIONAL, INTENT(IN)     :: Version
    REAL(fp),  OPTIONAL, INTENT(IN)     :: Frequency(:)
    REAL(fp),  OPTIONAL, INTENT(IN)     :: Latitude(:)
    REAL(fp),  OPTIONAL, INTENT(IN)     :: Longitude(:)
    REAL(fp),  OPTIONAL, INTENT(IN)     :: Emissivity(:,:,:)
   
    IF ( .NOT. LSEatlas_Associated(self) ) RETURN

    IF ( PRESENT(Version) ) self%Version = Version
    
    IF ( PRESENT(Frequency) ) THEN
      IF ( SIZE(Frequency) == self%n_Frequencies ) THEN
        self%Frequency = Frequency
      ELSE
        self%Frequency = ZERO
      END IF
    END IF
   
    IF ( PRESENT(Latitude) ) THEN
      IF ( SIZE(Latitude) == self%n_Latitudes ) THEN
        self%Latitude = Latitude
      ELSE
        self%Latitude = ZERO
      END IF
    END IF
   
    IF ( PRESENT(Longitude) ) THEN
      IF ( SIZE(Longitude) == self%n_Longitudes ) THEN
        self%Longitude = Longitude
      ELSE
        self%Longitude = ZERO
      END IF
    END IF
   
    IF ( PRESENT(Emissivity) ) THEN
      IF ( SIZE(Emissivity,DIM=1) == self%n_Frequencies .AND. &
           SIZE(Emissivity,DIM=2) == self%n_Latitudes   .AND. &
           SIZE(Emissivity,DIM=3) == self%n_Longitudes        ) THEN
        self%Emissivity = Emissivity
      ELSE
        self%Emissivity = ZERO
      END IF
    END IF
      
  END SUBROUTINE LSEatlas_SetValue
 

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEatlas_GetValue
!
! PURPOSE:
!       Subroutine to get the contents of a valid LSEatlas object.
!
! CALLING SEQUENCE:
!       CALL LSEatlas_GetValue( LSEatlas, &
!                               Version       = Version      , &
!                               n_Frequencies = n_Frequencies, &
!                               n_Latitudes   = n_Latitudes  , &
!                               n_Longitudes  = n_Longitudes , &
!                               Frequency     = Frequency    , & 
!                               Latitude      = Latitude     , & 
!                               Longitude     = Longitude    , & 
!                               Emissivity    = Emissivity     ) 
!
! OBJECTS:
!       LSEatlas:                Valid, allocated LSEatlas object from which
!                                values are to be retrieved.
!                                UNITS:      N/A
!                                TYPE:       LSEatlas_type
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL OUTPUTS:
!       Version:                 Integer indicating the data version of the object.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Frequencies:           Number of spectral frequencies for which there are 
!                                data.                                              
!                                UNITS:      N/A                                    
!                                TYPE:       INTEGER                                
!                                DIMENSION:  Scalar                                 
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Latitudes:             Number of latitude values for which there are      
!                                data.                                              
!                                UNITS:      N/A                                    
!                                TYPE:       INTEGER                                
!                                DIMENSION:  Scalar                                 
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Longitudes:            Number of longitude values for which there are     
!                                data.                                              
!                                UNITS:      N/A                                    
!                                TYPE:       INTEGER                                
!                                DIMENSION:  Scalar                                 
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL                 
! 
!       Frequency:               Real array to which the Frequency component of the
!                                LSEatlas object will be assigned. The actual
!                                argument must be declared as allocatable.
!                                UNITS:      N/A
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Rank-1 (L)
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL, ALLOCATABLE
!
!       Latitude:                Real array to which the Latitude component of the
!                                LSEatlas object will be assigned. The actual
!                                argument must be declared as allocatable.
!                                UNITS:      N/A
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Rank-1 (I)
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL, ALLOCATABLE
!
!       Longitude:               Real array to which the Longitude component of the
!                                LSEatlas object will be assigned. The actual
!                                argument must be declared as allocatable.
!                                UNITS:      N/A
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Rank-1 (J)
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL, ALLOCATABLE
!
!       Emissivity:              Real array to which the Emissivity component of the
!                                LSEatlas object will be assigned. The actual
!                                argument must be declared as allocatable.
!                                UNITS:      N/A
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Rank-3 (L x I x J)
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL, ALLOCATABLE
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LSEatlas_GetValue( &
    self         , &  ! Input
    Version      , &  ! Optional input
    n_Frequencies, &  ! Optional output
    n_Latitudes  , &  ! Optional output
    n_Longitudes , &  ! Optional output
    Frequency    , &  ! Optional output
    Latitude     , &  ! Optional output
    Longitude    , &  ! Optional output
    Emissivity     )  ! Optional output
    ! Arguments
    TYPE(LSEatlas_type),             INTENT(IN)  :: self
    INTEGER ,              OPTIONAL, INTENT(OUT) :: Version
    INTEGER ,              OPTIONAL, INTENT(OUT) :: n_Frequencies
    INTEGER ,              OPTIONAL, INTENT(OUT) :: n_Latitudes  
    INTEGER ,              OPTIONAL, INTENT(OUT) :: n_Longitudes 
    REAL(fp), ALLOCATABLE, OPTIONAL, INTENT(OUT) :: Frequency(:)    
    REAL(fp), ALLOCATABLE, OPTIONAL, INTENT(OUT) :: Latitude(:)     
    REAL(fp), ALLOCATABLE, OPTIONAL, INTENT(OUT) :: Longitude(:)    
    REAL(fp), ALLOCATABLE, OPTIONAL, INTENT(OUT) :: Emissivity(:,:,:) 
    
    IF ( .NOT. LSEatlas_Associated(self) ) RETURN
   
    IF ( PRESENT(Version      ) ) Version       = self%Version
    IF ( PRESENT(n_Frequencies) ) n_Frequencies = self%n_Frequencies
    IF ( PRESENT(n_Latitudes  ) ) n_Latitudes   = self%n_Latitudes  
    IF ( PRESENT(n_Longitudes ) ) n_Longitudes  = self%n_Longitudes 

    IF ( PRESENT(Frequency) ) THEN
      ALLOCATE(Frequency(self%n_Frequencies))
      Frequency = self%Frequency
    END IF

    IF ( PRESENT(Latitude) ) THEN
      ALLOCATE(Latitude(self%n_Latitudes))
      Latitude = self%Latitude
    END IF

    IF ( PRESENT(Longitude) ) THEN
      ALLOCATE(Longitude(self%n_Longitudes))
      Longitude = self%Longitude
    END IF

    IF ( PRESENT(Emissivity) ) THEN
      ALLOCATE(Emissivity(self%n_Frequencies,self%n_Latitudes,self%n_Longitudes))
      Emissivity = self%Emissivity
    END IF

  END SUBROUTINE LSEatlas_GetValue
 


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEatlas_InquireFile
!
! PURPOSE:
!       Function to inquire LSEatlas object files.
!
! CALLING SEQUENCE:
!       Error_Status = LSEatlas_InquireFile( &
!                        Filename                     , &
!                        n_Frequencies = n_Frequencies, &
!                        n_Latitudes   = n_Latitudes  , &
!                        n_Longitudes  = n_Longitudes , &
!                        Release       = Release      , &
!                        Version       = Version        )
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
!       n_Frequencies:      Number of spectral frequencies for which there are 
!                           data.                                              
!                           UNITS:      N/A                                    
!                           TYPE:       INTEGER                                
!                           DIMENSION:  Scalar                                 
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Latitudes:        Number of latitude values for which there are      
!                           data.                                              
!                           UNITS:      N/A                                    
!                           TYPE:       INTEGER                                
!                           DIMENSION:  Scalar                                 
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Longitudes:       Number of longitude values for which there are     
!                           data.                                              
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
!       Version:            The data/file version number. Used for
!                           purposes only in identifying the dataset for
!                           a particular release.
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

  FUNCTION LSEatlas_InquireFile( &
    Filename     , &  ! Input
    n_Frequencies, &  ! Optional output  
    n_Latitudes  , &  ! Optional output  
    n_Longitudes , &  ! Optional output  
    Release      , &  ! Optional output
    Version      , &  ! Optional output
    Title        , &  ! Optional output
    History      , &  ! Optional output
    Comment      ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Frequencies
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Latitudes  
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Longitudes 
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release        
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version        
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title           
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment         
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LSEatlas_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(LSEatlas_type) :: LSEatlas

 
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


    ! Read and check the datatype name
    err_stat = Read_Datatype( fid, LSEatlas%Datatype_name )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading Datatype_Name'
      CALL Inquire_Cleanup(); RETURN
    END IF
    IF ( TRIM(LSEatlas%Datatype_Name) /= LSEATLAS_DATATYPE ) THEN
      msg = LSEATLAS_DATATYPE//' datatype name check failed.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the release and version
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEatlas%Release, &
      LSEatlas%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Release/Version - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF
    IF ( .NOT. LSEatlas_ValidRelease( LSEatlas ) ) THEN
      msg = 'LSEatlas Release check failed.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEatlas%n_Frequencies, &
      LSEatlas%n_Latitudes  , &
      LSEatlas%n_Longitudes 
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
    IF ( PRESENT(n_Frequencies) ) n_Frequencies = LSEatlas%n_Frequencies
    IF ( PRESENT(n_Latitudes  ) ) n_Latitudes   = LSEatlas%n_Latitudes  
    IF ( PRESENT(n_Longitudes ) ) n_Longitudes  = LSEatlas%n_Longitudes     
    IF ( PRESENT(Release      ) ) Release       = LSEatlas%Release        
    IF ( PRESENT(Version      ) ) Version       = LSEatlas%Version        
    
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
    
  END FUNCTION LSEatlas_InquireFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEatlas_ReadFile
!
! PURPOSE:
!       Function to read LSEatlas object files.
!
! CALLING SEQUENCE:
!       Error_Status = LSEatlas_ReadFile( &
!                        LSEatlas           , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet     )
!
! OBJECTS:
!       LSEatlas:       LSEatlas object containing the data read from file.
!                       UNITS:      N/A
!                       TYPE:       LSEatlas_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       LSEatlas data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the LSEatlas data is embedded within another file.
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

  FUNCTION LSEatlas_ReadFile( &
    LSEatlas, &  ! Output
    Filename   , &  ! Input
    No_Close   , &  ! Optional input
    Quiet      , &  ! Optional input
    Title      , &  ! Optional output
    History    , &  ! Optional output
    Comment    , &  ! Optional output
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(LSEatlas_type)   , INTENT(OUT) :: LSEatlas
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LSEatlas_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    TYPE(LSEatlas_type) :: dummy
    
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


    ! Read and check the datatype name
    err_stat = Read_Datatype( fid, dummy%Datatype_name )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading Datatype_Name'
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( TRIM(dummy%Datatype_Name) /= LSEATLAS_DATATYPE ) THEN
      msg = LSEATLAS_DATATYPE//' datatype name check failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    

    ! Read and check the release and version
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      dummy%Release, &
      dummy%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Release/Version - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    IF ( .NOT. LSEatlas_ValidRelease( dummy ) ) THEN
      msg = 'LSEatlas Release check failed.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      dummy%n_Frequencies, &
      dummy%n_Latitudes  , &
      dummy%n_Longitudes 
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading data dimensions - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Allocate the object
    CALL LSEatlas_Create( &
           LSEatlas           , &
           dummy%n_Frequencies, &        
           dummy%n_Latitudes  , &        
           dummy%n_Longitudes   )                  
    IF ( .NOT. LSEatlas_Associated( LSEatlas ) ) THEN
      msg = 'LSEatlas object allocation failed.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Explicitly assign the version number
    LSEatlas%Version = dummy%Version
        

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


    ! Read the coefficient data
    ! ...Read the dimensional vectors
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEatlas%Frequency, &
      LSEatlas%Latitude , &
      LSEatlas%Longitude
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading dimensional vectors - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Read the emissivity data
    READ( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEatlas%Emissivity
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading emissivity data - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF


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
       CALL LSEatlas_Info( LSEatlas, msg )
       CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
     END IF

   CONTAINS
   
     SUBROUTINE Read_CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
       END IF
       CALL LSEatlas_Destroy( LSEatlas )
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE Read_CleanUp

  END FUNCTION LSEatlas_ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LSEatlas_WriteFile
!
! PURPOSE:
!       Function to write LSEatlas object files.
!
! CALLING SEQUENCE:
!       Error_Status = LSEatlas_WriteFile( &
!                        LSEatlas           , &
!                        Filename           , &
!                        No_Close = No_Close, &
!                        Quiet    = Quiet     )
!
! OBJECTS:
!       LSEatlas:       LSEatlas object containing the data to write to file.
!                       UNITS:      N/A
!                       TYPE:       LSEatlas_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       LSEatlas format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       No_Close:       Set this logical argument to *NOT* close the datafile
!                       upon exiting this routine. This option is required if
!                       the LSEatlas data is to be embedded within another file.
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

  FUNCTION LSEatlas_WriteFile( &
    LSEatlas, &  ! Input
    Filename   , &  ! Input
    No_Close   , &  ! Optional input
    Quiet      , &  ! Optional input
    Title      , &  ! Optional input
    History    , &  ! Optional input
    Comment    , &  ! Optional input
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(LSEatlas_type), INTENT(IN) :: LSEatlas
    CHARACTER(*),           INTENT(IN) :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN) :: No_Close
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LSEatlas_WriteFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    

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
    IF ( .NOT. LSEatlas_Associated( LSEatlas ) ) THEN
      msg = 'LSEatlas object is empty.'
      CALL Write_Cleanup(); RETURN
    END IF

   
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


    ! Write the datatype name
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LEN(LSEatlas%Datatype_Name)
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Datatype_Name length - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEatlas%Datatype_Name
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Datatype_Name - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF
    

    ! Write the release and version
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEatlas%Release, &
      LSEatlas%Version
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Release/Version - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEatlas%n_Frequencies, &
      LSEatlas%n_Latitudes  , &
      LSEatlas%n_Longitudes 
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing data dimensions - '//TRIM(io_msg)
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


    ! Write the coefficient data
    ! ...Write the dimensional vectors
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEatlas%Frequency, &
      LSEatlas%Latitude , &
      LSEatlas%Longitude
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimensional vectors - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF
    ! ...Write the emissivity data
    WRITE( fid, IOSTAT=io_stat, IOMSG=io_msg ) &
      LSEatlas%Emissivity
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing emissivity data - '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


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
       CALL LSEatlas_Info( LSEatlas, msg )
       CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//'; '//TRIM(msg), INFORMATION )
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

  END FUNCTION LSEatlas_WriteFile

  
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
!       LSEatlas_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two LSEatlas objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = LSEatlas_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two LSEatlas objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       LSEatlas_type
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

  ELEMENTAL FUNCTION LSEatlas_Equal( x, y ) RESULT( is_equal )
    TYPE(LSEatlas_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
   
    ! Check the object association status
    IF ( (.NOT. LSEatlas_Associated(x)) .OR. &
         (.NOT. LSEatlas_Associated(y))      ) RETURN

   ! Check contents
    ! ...Release/version info
    IF ( (x%Release /= y%Release) .OR. &
         (x%Version /= y%Version) ) RETURN
    ! ...Dimensions
    IF ( (x%n_Frequencies /= y%n_Frequencies ) .OR. &
         (x%n_Latitudes   /= y%n_Latitudes   ) .OR. &
         (x%n_Longitudes  /= y%n_Longitudes  ) ) RETURN
    ! ...Arrays
    IF ( ALL(x%Frequency  .EqualTo. y%Frequency ) .AND. &
         ALL(x%Latitude   .EqualTo. y%Latitude  ) .AND. &
         ALL(x%Longitude  .EqualTo. y%Longitude ) .AND. &
         ALL(x%Emissivity .EqualTo. y%Emissivity ) ) &
      is_equal = .TRUE.

  END FUNCTION LSEatlas_Equal
  
  
  FUNCTION Read_Datatype( fid, datatype_name ) RESULT( err_stat )
    ! Arguments
    INTEGER     , INTENT(IN)  :: fid
    CHARACTER(*), INTENT(OUT) :: datatype_name
    ! Function result
    INTEGER :: err_stat
    ! Local variables
    CHARACTER(1), ALLOCATABLE :: dummy(:)
    INTEGER :: i, strlen
    INTEGER :: io_stat
    INTEGER :: alloc_stat

    ! Set up
    err_stat = FAILURE
    datatype_name = ''

    ! Get the string length
    READ( fid, IOSTAT=io_stat ) strlen
    IF ( io_stat /= 0 ) RETURN
    
    ! Allocate dummy string array
    ALLOCATE( dummy(strlen), STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Read the string into the dummy array
    READ( fid, IOSTAT=io_stat ) dummy
    IF ( io_stat /= 0 ) RETURN

    ! Transfer array into string
    DO i = 1, MIN(strlen,LEN(datatype_name))
      datatype_name(i:i) = dummy(i)
    END DO

    ! Done
    err_stat = SUCCESS
  END FUNCTION Read_Datatype
  
END MODULE LSEatlas_Define
