!
! CRTM_CloudCover_Define
!
! Module defining the CRTM Cloud Cover object and its methods.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 16-Sep-2015
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_CloudCover_Define


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Intrinsic modules
  USE ISO_Fortran_Env       , ONLY: OUTPUT_UNIT
  ! Module use
  USE Type_Kinds            , ONLY: fp
  USE File_Utility          , ONLY: File_Open
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Compare_Float_Numbers , ONLY: DEFAULT_N_SIGFIG, &
                                    OPERATOR(.EqualTo.), &
                                    Compares_Within_Tolerance
  USE CRTM_Parameters       , ONLY: ZERO, ONE, &
                                    WATER_CONTENT_THRESHOLD
  USE CRTM_Atmosphere_Define, ONLY: CRTM_Atmosphere_type, &
                                    CRTM_Atmosphere_Associated
  USE CRTM_Cloud_Define     , ONLY: OPERATOR(==), &
                                    OPERATOR(+), &
                                    CRTM_Cloud_type, &
                                    CRTM_Cloud_Associated, &
                                    CRTM_Cloud_Zero
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Parameters
  PUBLIC :: DEFAULT_OVERLAP_ID
  ! Datatypes
  PUBLIC :: CRTM_CloudCover_type
  ! "Class" methods
  PUBLIC :: CloudCover_Maximum_Overlap
  PUBLIC :: CloudCover_Random_Overlap
  PUBLIC :: CloudCover_MaxRan_Overlap
  PUBLIC :: CloudCover_Average_Overlap
  PUBLIC :: CloudCover_Overcast_Overlap
  PUBLIC :: CloudCover_Overlap_IsValid
  PUBLIC :: CloudCover_Overlap_Name


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_CloudCover_Define.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  ! The valid cloud categories and names
! INTEGER, PARAMETER :: N_OVERLAPS = 4
  INTEGER, PARAMETER :: N_OVERLAPS = 5          
  INTEGER, PARAMETER :: INVALID_OVERLAP_ID = 0
  INTEGER, PARAMETER :: MAXIMUM_OVERLAP_ID = 1
  INTEGER, PARAMETER ::  RANDOM_OVERLAP_ID = 2
  INTEGER, PARAMETER ::  MAXRAN_OVERLAP_ID = 3
  INTEGER, PARAMETER :: AVERAGE_OVERLAP_ID = 4
  INTEGER, PARAMETER :: OVERCAST_OVERLAP_ID =5  
  CHARACTER(*), PARAMETER :: OVERLAP_NAMES(0:N_OVERLAPS) = &
    [ 'Invalid       ', &
      'Maximum       ', &
      'Random        ', &
      'Maximum-random', &
      'Average       ', &  
      'Overcast      ' ]   
  INTEGER, PARAMETER :: DEFAULT_OVERLAP_ID = AVERAGE_OVERLAP_ID
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! File status on close after write error
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'


  ! -----------------------------
  ! CloudCover object definitions
  ! -----------------------------
  ! Private object for intermediate results
  ! Only used and acessible in this module
  TYPE, PRIVATE :: iVar_type
    ! Housekeeping
    LOGICAL :: Is_Allocated = .FALSE.    ! Allocation indicator
    INTEGER :: n_Layers = 0              ! K dimension.
    INTEGER :: n_Clouds = 0              ! N dimension.    
    ! Data
    REAL(fp), ALLOCATABLE :: prod(:)     ! 0:K. Product across layers
    REAL(fp), ALLOCATABLE :: lwc(:)      ! 1:K. Total layer water content for ALL clouds
    REAL(fp), ALLOCATABLE :: wc_sum(:)   ! 0:K. Cumulative sum of lwc at each layer
    REAL(fp), ALLOCATABLE :: cwc_sum(:)  ! 0:K. Cumulative sum of the weighted lwc at each layer
    REAL(fp), ALLOCATABLE :: wc(:,:)     ! 1:N 1:K. layer water content for each cloud type       
    REAL(fp), ALLOCATABLE :: maxcov(:)   ! 1:K. Max cloud fraction between two layers 
  CONTAINS
    PROCEDURE, PASS(self) :: Is_Usable   => iVar_Is_Usable
    PROCEDURE, PASS(self) :: Destroy     => iVar_Destroy
    PROCEDURE, PASS(self) :: Create      => iVar_Create
    PROCEDURE, PASS(self) :: Inspect     => iVar_Inspect
    PROCEDURE, PASS(self) :: Set_To_Zero => iVar_Set_To_Zero
    PROCEDURE :: iVar_Equal
    PROCEDURE :: iVar_NotEqual
    PROCEDURE :: iVar_Compare
    GENERIC :: OPERATOR(==) => iVar_Equal
    GENERIC :: OPERATOR(/=) => iVar_NotEqual
    GENERIC :: OPERATOR(.Compare.) => iVar_Compare
  END TYPE iVar_type


  ! The main object definition
  !:tdoc+:
  TYPE :: CRTM_CloudCover_type
    ! Housekeeping
    LOGICAL :: Is_Allocated = .FALSE.                   ! Allocation indicator
    INTEGER :: n_Layers = 0                             ! K dimension.
    ! Data
    INTEGER  :: Overlap           = DEFAULT_OVERLAP_ID  ! Overlap type identifier
    REAL(fp) :: Total_Cloud_Cover = ZERO                ! Cloud cover used in RT
    REAL(fp), ALLOCATABLE :: Cloud_Fraction(:)          ! K. The physical cloud fraction
    REAL(fp), ALLOCATABLE :: Cloud_Cover(:)             ! K. The overlap cloud cover
    ! Intermediate results
    TYPE(iVar_type) :: iVar                             ! FWD results for TL/AD
  CONTAINS
    PRIVATE
    PROCEDURE, PUBLIC, PASS(self)    :: Overlap_Id
    PROCEDURE, PUBLIC, PASS(self)    :: Overlap_Name
    PROCEDURE, PUBLIC, PASS(self)    :: Compute_CloudCover
    PROCEDURE, PUBLIC, PASS(self_TL) :: Compute_CloudCover_TL
    PROCEDURE, PUBLIC, PASS(self_AD) :: Compute_CloudCover_AD
    PROCEDURE, PUBLIC, PASS(self)    :: Is_Usable
    PROCEDURE, PUBLIC, PASS(self)    :: Destroy
    PROCEDURE, PUBLIC, PASS(self)    :: Create
    PROCEDURE, PUBLIC, PASS(self)    :: Inspect
    PROCEDURE, PUBLIC, PASS(self)    :: Set_To_Zero
    PROCEDURE :: Equal
    PROCEDURE :: NotEqual
    PROCEDURE :: Compare_
    GENERIC, PUBLIC :: OPERATOR(==) => Equal
    GENERIC, PUBLIC :: OPERATOR(/=) => NotEqual
    GENERIC, PUBLIC :: OPERATOR(.Compare.) => Compare_
  END TYPE CRTM_CloudCover_type
  !:tdoc-:

CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                       ## PUBLIC MODULE PROCEDURES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   CloudCover_Maximum_Overlap
!   CloudCover_Random_Overlap
!   CloudCover_MaxRan_Overlap
!   CloudCover_Average_Overlap
!   CloudCover_Overcast_Overlap 
!
! PURPOSE:
!   Group of pure functions to supply the overlap methodology indicator.
!
! CALLING SEQUENCE:
!   id = CloudCover_Maximum_Overlap()
!   id = CloudCover_Random_Overlap()
!   id = CloudCover_MaxRan_Overlap()
!   id = CloudCover_Average_Overlap()
!   id = CloudCover_Overcast_Overlap()
!
! FUNCTION RESULT:
!   id:  The return value is an integer defining the overlap methodology.
!        The actual number value of these integers in a CRTM release can
!        change at any time based upon code updates.
!        UNITS:      N/A
!        TYPE:       INTEGER
!        DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE FUNCTION CloudCover_Maximum_Overlap() RESULT(id)
    INTEGER :: id
    id = MAXIMUM_OVERLAP_ID
  END FUNCTION CloudCover_Maximum_Overlap

  PURE FUNCTION CloudCover_Random_Overlap() RESULT(id)
    INTEGER :: id
    id = RANDOM_OVERLAP_ID
  END FUNCTION CloudCover_Random_Overlap

  PURE FUNCTION CloudCover_MaxRan_Overlap() RESULT(id)
    INTEGER :: id
    id = MAXRAN_OVERLAP_ID
  END FUNCTION CloudCover_MaxRan_Overlap

  PURE FUNCTION CloudCover_Average_Overlap() RESULT(id)
    INTEGER :: id
    id = AVERAGE_OVERLAP_ID
  END FUNCTION CloudCover_Average_Overlap

  PURE FUNCTION CloudCover_Overcast_Overlap() RESULT(id)  
    INTEGER :: id                                         
    id = OVERCAST_OVERLAP_ID                              
  END FUNCTION CloudCover_Overcast_Overlap               


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   CloudCover_Overlap_IsValid
!
! PURPOSE:
!   Pure function to test if an overlap methodology identifier is valid.
!
! CALLING SEQUENCE:
!   is_valid = CloudCover_Overlap_IsValid( id )
!
! INPUTS:
!   id:        The overlap methodology identifier.
!              UNITS:      N/A
!              TYPE:       INTEGER
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   is_valid:  Logical variable indicating whether or not the input overlap
!              methodology identifier is valid.
!              UNITS:      N/A
!              TYPE:       LOGICAL
!              DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE FUNCTION CloudCover_Overlap_IsValid(id) RESULT(is_valid)
    INTEGER, INTENT(IN) :: id
    LOGICAL :: is_valid
    is_valid = (id >= 1 .AND. id <= N_OVERLAPS)
  END FUNCTION CloudCover_Overlap_IsValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   CloudCover_Overlap_Name
!
! PURPOSE:
!   Pure function to return a string description of the overlap methodology
!   given its identifier.
!
! CALLING SEQUENCE:
!   name = CloudCover_Overlap_Name( id )
!
! INPUTS:
!   id:        The overlap methodology identifier.
!              UNITS:      N/A
!              TYPE:       INTEGER
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   name:      Character variable containing a short descriptor of the overlap
!              methodology. If the input identifier is invalid, the returned
!              string is "Invalid".
!              UNITS:      N/A
!              TYPE:       CHARACTER(*)
!              DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE FUNCTION CloudCover_Overlap_Name(id) RESULT(name)
    INTEGER, INTENT(IN) :: id
    CHARACTER(LEN(OVERLAP_NAMES(1))) :: name
    IF ( CloudCover_Overlap_IsValid(id) ) THEN
      name = OVERLAP_NAMES(id)
    ELSE
      name = OVERLAP_NAMES(INVALID_OVERLAP_ID)
    END IF
  END FUNCTION CloudCover_Overlap_Name



!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE METHODS ##                        ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Overlap_Id
!
! PURPOSE:
!   Function method to return the overlap methodology identifier of a
!   CloudCover object.
!
! CALLING SEQUENCE:
!   id = cc_obj%Overlap_Id()
!
! OBJECTS:
!   cc_obj:  Cloud cover object for which the overlap methodology identifier
!            is required.
!            UNITS:      N/A
!            CLASS:      CRTM_CloudCover_type
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!   id:      The return value is an integer defining the overlap methodology.
!            The actual number value of these integers in a CRTM release can
!            change at any time based upon code updates.
!            UNITS:      N/A
!            TYPE:       INTEGER
!            DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE FUNCTION Overlap_Id(self) RESULT(id)
    CLASS(CRTM_CloudCover_type), INTENT(IN) :: self
    INTEGER :: id
    id = self%Overlap
    IF ( id < 1 .OR. id > N_OVERLAPS ) id = INVALID_OVERLAP_ID
  END FUNCTION Overlap_Id


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Overlap_Name
!
! PURPOSE:
!   Function method to return a string description of the overlap methodology
!   that has been set for a CloudCover object.
!
! CALLING SEQUENCE:
!   name = cc_obj%Overlap_Name()
!
! OBJECTS:
!   cc_obj:  Cloud cover object for which the overlap methodology descriptor
!            is required.
!            UNITS:      N/A
!            CLASS:      CRTM_CloudCover_type
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   name:    Character variable containing a short descriptor of the overlap
!            methodology. If the object's overlap methodology identifier is
!            invalid, the returned string is "Invalid".
!            UNITS:      N/A
!            TYPE:       CHARACTER(*)
!            DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE FUNCTION Overlap_Name(self) RESULT(name)
    CLASS(CRTM_CloudCover_type), INTENT(IN) :: self
    CHARACTER(LEN(OVERLAP_NAMES(1))) :: name
    name = OVERLAP_NAMES(self%Overlap_Id())
  END FUNCTION Overlap_Name


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Compute_CloudCover
!
! PURPOSE:
!   Function method to compute the cloud cover profile given a supplied
!   Atmosphere object, and populate the CloudCover object with the
!   results.
!
! CALLING SEQUENCE:
!   err_stat = cc_obj%Compute_CloudCover( &
!                atmosphere       , &
!                Overlap = overlap  )
!
! OBJECTS:
!   cc_obj:      Cloud cover object which is to be populated with cloud
!                cover results.
!                UNITS:      N/A
!                CLASS:      CRTM_CloudCover_type
!                DIMENSION:  Scalar
!                ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!   atmosphere:  Atmopshere object containing the layer cloud fraction
!                profile, and the actual cloud profiles for when cloud
!                water content averaging of the cloud cover is selected.
!                UNITS:      N/A
!                TYPE:       CRTM_Atmosphere_type
!                DIMENSION:  Scalar
!                ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!   overlap:     Set this argument to a flag defining the cloud coverage
!                algorithm used. Supplied module functions providing valid flag
!                output are:
!                  CloudCover_Maximum_Overlap(): Use maximum overlap method.
!                  CloudCover_Random_Overlap() : Use random overlap method.
!                  CloudCover_MaxRan_Overlap() : Use maximum-random overlap method.
!                  CloudCover_Average_Overlap(): Use cloud content weighted averaged method. [DEFAULT]
!                  CloudCover_Overcast_Overlap():Overcast. [Test]
!                If not specified, the default is the cloud content weighted
!                averaged method
!                UNITS:      N/A
!                TYPE:       INTEGER
!                DIMENSION:  Scalar
!                ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!   err_stat:    The return value is an integer defining the error status.
!                The error codes are defined in the Message_Handler module.
!                  If == SUCCESS, the computation was successful
!                     == FAILURE, an unrecoverable error occurred.
!                UNITS:      N/A
!                TYPE:       INTEGER
!                DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Compute_CloudCover( &
    self   , &  ! Output
    atm    , &  ! In/Output      
    overlap) &  ! Optional input
  RESULT(err_stat)
    ! Arguments
    CLASS(CRTM_CloudCover_type), INTENT(OUT)  :: self
    TYPE(CRTM_Atmosphere_type) , INTENT(INOUT):: atm        
    INTEGER,           OPTIONAL, INTENT(IN)   :: overlap
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'CRTM_CloudCover_Define::Compute_CloudCover'
    REAL(fp),     PARAMETER :: MIN_COVERAGE_THRESHOLD = 1.0e-06_fp                              
    REAL(fp),     PARAMETER :: MAX_COVERAGE_THRESHOLD = ONE - MIN_COVERAGE_THRESHOLD                                 
    ! Local variables
    CHARACTER(ML) :: err_msg
    INTEGER :: overlap_method
    INTEGER :: n_layers
    INTEGER :: n_clouds  
    INTEGER :: n       
    ! Check input
    err_stat = SUCCESS
    IF ( .NOT. CRTM_Atmosphere_Associated(atm) ) THEN
      err_msg = 'Input atmosphere is not usable'
      err_stat = FAILURE
      CALL Display_Message(PROCEDURE_NAME, err_msg, err_stat); RETURN
    END IF
    ! ...Overlap keyword
    overlap_method = DEFAULT_OVERLAP_ID
    IF ( PRESENT(overlap) ) overlap_method = overlap
    ! ...and check it.
    IF ( .NOT. CloudCover_Overlap_IsValid(overlap_method) ) THEN
      err_msg = 'Invalid overlap assumption'
      err_stat = FAILURE
      CALL Display_Message(PROCEDURE_NAME, err_msg, err_stat); RETURN
    END IF


    ! Create the output object
    n_layers = Atm%n_Layers
    n_clouds = Atm%n_Clouds 
!   CALL self%Create(n_layers, Forward = .TRUE., Error_Message = err_msg)                    
    CALL self%Create(n_layers, n_clouds, Forward = .TRUE., Error_Message = err_msg)            
    IF ( .NOT. self%Is_Usable() ) THEN
      err_stat = FAILURE
      CALL Display_Message(PROCEDURE_NAME, err_msg, err_stat); RETURN
    END IF


    ! Set the object quantities
    self%Overlap        = overlap_method
    self%Cloud_Fraction = atm%Cloud_Fraction(1:atm%n_Layers)

    DO n = 1, n_clouds 
       self%iVar%wc(n,1:n_layers) = atm%Cloud(n)%Water_Content(1:n_layers) ! save for TL/AD
    END DO

    ! Compute the cloud cover
    SELECT CASE (self%Overlap)
      CASE (MAXIMUM_OVERLAP_ID); CALL Compute_Maximum_Overlap()
      CASE (RANDOM_OVERLAP_ID) ; CALL Compute_Random_Overlap()
      CASE (MAXRAN_OVERLAP_ID) ; CALL Compute_MaxRan_Overlap()
      CASE (AVERAGE_OVERLAP_ID); CALL Compute_Average_Overlap()
      CASE (OVERCAST_OVERLAP_ID);CALL Compute_Overcast_Overlap() 
    END SELECT

    ! Add cloud scaling here!
    ! Partition all hydrometeors into cloudy column
    IF (self%Total_Cloud_Cover > MIN_COVERAGE_THRESHOLD) then
       DO n = 1, n_clouds 
          ! scaled cloud water content
          atm%Cloud(n)%Water_Content(1:n_layers) = atm%Cloud(n)%Water_Content(1:n_layers) / self%Total_Cloud_Cover 
       END DO
    END IF

  CONTAINS

    SUBROUTINE Compute_Maximum_Overlap()
      INTEGER :: k
      self%Cloud_Cover(1) = self%Cloud_Fraction(1)
      DO k = 2, n_layers
        IF ( self%Cloud_Fraction(k) > self%Cloud_Cover(k-1) ) THEN
          self%Cloud_Cover(k) = self%Cloud_Fraction(k)
        ELSE
          self%Cloud_Cover(k) = self%Cloud_Cover(k-1)
        END IF
      END DO
      self%Total_Cloud_Cover = self%Cloud_Cover(n_layers)
    END SUBROUTINE Compute_Maximum_Overlap

!   SUBROUTINE Compute_Random_Overlap()
!     INTEGER  :: k
!     REAL(fp) :: prod
!     prod         = ONE
!     self%iVar%prod(0) = prod
!     DO k = 1, n_layers
!       prod = prod * (ONE - self%Cloud_Fraction(k))
!       self%Cloud_Cover(k) = ONE - prod
!       self%iVar%prod(k) = prod  ! Save for TL/AD
!     END DO
!     self%Total_Cloud_Cover = self%Cloud_Cover(n_layers)
!   END SUBROUTINE Compute_Random_Overlap
    SUBROUTINE Compute_Random_Overlap()
      INTEGER  :: k
      REAL(fp) :: prod(0:n_layers)
      prod(0)           = ONE
      self%iVar%prod(0) = prod(0)
      DO k = 1, n_layers
        if (self%Cloud_Fraction(k) > MIN_COVERAGE_THRESHOLD) then   
           prod(k) = prod(k-1) * (ONE - self%Cloud_Fraction(k))
        else
           prod(k) = prod(k-1)
        endif
        self%Cloud_Cover(k) = ONE - prod(k)
        self%iVar%prod(k)   = prod(k)  ! Save for TL/AD
      END DO
     self%Total_Cloud_Cover = self%Cloud_Cover(n_layers)
    END SUBROUTINE Compute_Random_Overlap

!   SUBROUTINE Compute_MaxRan_Overlap()
!     INTEGER  :: k
!     REAL(fp) :: prod
!     prod              = ONE - self%Cloud_Fraction(1)
!     self%iVar%prod(1) = prod
!     self%Cloud_Cover(1) = ONE - prod
!     DO k = 2, n_layers 
!       IF ( self%Cloud_Fraction(k) > self%Cloud_Fraction(k-1) ) THEN 
!        prod = prod * (ONE - self%Cloud_Fraction(k)) / (ONE - self%Cloud_Fraction(k-1)) 
!       END IF
!       self%Cloud_Cover(k) = ONE - prod
!       self%iVar%prod(k) = prod  ! Save for TL/AD
!     END DO
!     self%Total_Cloud_Cover = self%Cloud_Cover(n_layers)
!   END SUBROUTINE Compute_MaxRan_Overlap
    SUBROUTINE Compute_MaxRan_Overlap()
    INTEGER  :: k
    REAL(fp) :: prod, maxcov

    prod                = ONE - self%Cloud_Fraction(1)
    self%Cloud_Cover(1) = ONE - prod
    self%iVar%prod(1)   = prod
    self%iVar%maxcov(1) = ONE - self%Cloud_Fraction(1)
    DO k= 2, n_layers
       maxcov = (ONE - MAX(self%Cloud_Fraction(k-1), self%Cloud_Fraction(k)))
       prod = prod * maxcov / (one - self%Cloud_Fraction(k-1))                                                    
       self%iVar%maxcov(k) = maxcov 
       self%iVar%prod(k)   = prod
       self%Cloud_Cover(k) = ONE - prod
    ENDDO
    self%Total_Cloud_Cover = self%Cloud_Cover(n_layers)
    END SUBROUTINE Compute_MaxRan_Overlap

    SUBROUTINE Compute_Average_Overlap()
      INTEGER  :: k, n

      ! Give the variables shorter names
      ASSOCIATE( lwc     => self%iVar%lwc      , &
                 wc_sum  => self%iVar%wc_sum   , &
                 cwc_sum => self%iVar%cwc_sum  , &
                 cf      => self%Cloud_Fraction, &
                 cc      => self%Cloud_Cover   , &
                 cloud   => atm%Cloud            )
                 
        ! The total layer water content
        lwc = ZERO
        DO n = 1, SIZE(cloud)
          WHERE (cloud(n)%Water_Content > WATER_CONTENT_THRESHOLD ) &
            lwc = lwc + cloud(n)%Water_Content(1:n_layers)
        END DO

        ! The cloud cover profile
        wc_sum(0)  = ZERO
        cwc_sum(0) = ZERO
        DO k = 1, n_layers
           wc_sum(k) =  wc_sum(k-1) + lwc(k)
          cwc_sum(k) = cwc_sum(k-1) + (cf(k) * lwc(k))
          IF ( wc_sum(k) > ZERO ) cc(k) = cwc_sum(k)/wc_sum(k)
        END DO
      
      END ASSOCIATE
      
      ! Extract out the total cloud cover
      self%Total_Cloud_Cover = self%Cloud_Cover(n_layers)

    END SUBROUTINE Compute_Average_Overlap

    SUBROUTINE Compute_Overcast_Overlap()

      self%Total_Cloud_Cover = ONE 

    END SUBROUTINE Compute_Overcast_Overlap

  END FUNCTION Compute_CloudCover


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Compute_CloudCover_TL
!
! PURPOSE:
!   Function method to compute the tangent-linear cloud cover profile for
!   supplied forward model results and a Atmosphere perturbation, and populate
!   the tangent-linear CloudCover object with the results.
!
! CALLING SEQUENCE:
!   err_stat = cc_obj_TL%Compute_CloudCover_TL( &
!                cc_FWD       , &
!                atmosphere   , &
!                atmosphere_TL  )
!
! OBJECTS:
!   cc_obj_TL:      The tangent-linear cloud cover object which is to be
!                   populated with perturbed cloud cover results.
!                   UNITS:      N/A
!                   CLASS:      CRTM_CloudCover_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!   cc_FWD:         The forward model cloud cover object.
!                   UNITS:      N/A
!                   CLASS:      CRTM_CloudCover_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN)
!
!   atmosphere:     Atmopshere object containing the layer cloud fraction
!                   profile, and the actual cloud profiles for when cloud
!                   water content averaging of the cloud cover is selected.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Atmosphere_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN)
!
!   atmosphere_TL:  The tangent-linear atmosphere object containing the layer
!                   cloud fraction perturbation profile, and the cloud amount
!                   perturbation profiles for when cloud water content averaging
!                   of the cloud cover is selected.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Atmosphere_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   err_stat:       The return value is an integer defining the error status.
!                   The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the computation was successful
!                        == FAILURE, an unrecoverable error occurred.
!                   UNITS:      N/A
!                   TYPE:       INTEGER
!                   DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Compute_CloudCover_TL( &
    self_TL, &  ! Output
    cc_FWD , &  ! Input
    atm    , &  ! Input      
    atm_TL ) &  ! In/Outupt  
  RESULT(err_stat)
    ! Arguments
    CLASS(CRTM_CloudCover_type), INTENT(OUT)  :: self_TL
    CLASS(CRTM_CloudCover_type), INTENT(IN)   :: cc_FWD
    TYPE(CRTM_Atmosphere_type) , INTENT(IN)   :: atm       
    TYPE(CRTM_Atmosphere_type) , INTENT(INOUT):: atm_TL 
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'CRTM_CloudCover_Define::Compute_CloudCover_TL'
    REAL(fp),     PARAMETER :: MIN_COVERAGE_THRESHOLD = 1.0e-06_fp                               
    REAL(fp),     PARAMETER :: MAX_COVERAGE_THRESHOLD = ONE - MIN_COVERAGE_THRESHOLD      
    ! Local variables
    CHARACTER(ML) :: err_msg
    INTEGER :: n_layers
    INTEGER :: n_clouds 
    INTEGER :: n
    ! Check inputs
    err_stat = SUCCESS
    IF ( .NOT. cc_FWD%Is_Usable( Include_iVar=.TRUE. ) ) THEN
      err_msg = 'Input forward cloud cover object is not usable'
      err_stat = FAILURE
      CALL Display_Message(PROCEDURE_NAME, err_msg, err_stat); RETURN
    END IF
    IF ( .NOT. CRTM_Atmosphere_Associated(atm) ) THEN
      err_msg = 'Input atmosphere object is not usable'
      err_stat = FAILURE
      CALL Display_Message(PROCEDURE_NAME, err_msg, err_stat); RETURN
    END IF
    IF ( .NOT. CRTM_Atmosphere_Associated(atm_TL) ) THEN
      err_msg = 'Input tangent-linear atmosphere object is not usable'
      err_stat = FAILURE
      CALL Display_Message(PROCEDURE_NAME, err_msg, err_stat); RETURN
    END IF


    ! Create the output object
    n_layers = Atm_TL%n_Layers
    n_clouds = Atm_TL%n_Clouds
!   CALL self_TL%Create(n_layers, Error_Message = err_msg)           
    CALL self_TL%Create(n_layers, n_clouds, Error_Message = err_msg) 
    IF ( .NOT. self_TL%Is_Usable() ) THEN
      err_stat = FAILURE
      CALL Display_Message(PROCEDURE_NAME, err_msg, err_stat); RETURN
    END IF


    ! Set the object quantities
    self_TL%Overlap        = cc_FWD%Overlap
    self_TL%Cloud_Fraction = atm_TL%Cloud_Fraction(1:atm_TL%n_Layers)


    ! Compute the cloud cover
    SELECT CASE (self_TL%Overlap)
      CASE (MAXIMUM_OVERLAP_ID); CALL Compute_Maximum_Overlap_TL()
      CASE (RANDOM_OVERLAP_ID) ; CALL Compute_Random_Overlap_TL()
      CASE (MAXRAN_OVERLAP_ID) ; CALL Compute_MaxRan_Overlap_TL()
      CASE (AVERAGE_OVERLAP_ID); CALL Compute_Average_Overlap_TL()
      CASE (OVERCAST_OVERLAP_ID);CALL Compute_Overcast_Overlap_TL()
    END SELECT

    ! Add TL of cloud scaling here!
    ! Partition all hydrometeors into cloudy column
    IF (cc_FWD%Total_Cloud_Cover > MIN_COVERAGE_THRESHOLD) then
       DO n = 1, n_clouds 
          atm_TL%Cloud(n)%Water_Content(1:n_layers) = &
                 atm_TL%Cloud(n)%Water_Content(1:n_layers) / cc_FWD%Total_Cloud_Cover &
               - self_TL%Total_Cloud_Cover * cc_FWD%iVar%wc(n,1:n_layers) / (cc_FWD%Total_Cloud_Cover**2)
       END DO
    END IF

  CONTAINS

    SUBROUTINE Compute_Maximum_Overlap_TL()
      INTEGER :: k
      self_TL%Cloud_Cover(1) = self_TL%Cloud_Fraction(1)
      DO k = 2, n_layers
        IF ( cc_FWD%Cloud_Fraction(k) > cc_FWD%Cloud_Cover(k-1) ) THEN
          self_TL%Cloud_Cover(k) = self_TL%Cloud_Fraction(k)
        ELSE
          self_TL%Cloud_Cover(k) = self_TL%Cloud_Cover(k-1)
        END IF
      END DO
      self_TL%Total_Cloud_Cover = self_TL%Cloud_Cover(n_layers)
    END SUBROUTINE Compute_Maximum_Overlap_TL

!   SUBROUTINE Compute_Random_Overlap_TL()
!     INTEGER  :: k
!     REAL(fp) :: prod_TL
!     prod_TL = ZERO
!     DO k = 1, n_layers
!       prod_TL = (ONE - cc_FWD%Cloud_Fraction(k))*prod_TL - &
!                 cc_FWD%iVar%prod(k-1)*self_TL%Cloud_Fraction(k)
!       self_TL%Cloud_Cover(k) = -prod_TL
!     END DO
!     self_TL%Total_Cloud_Cover = self_TL%Cloud_Cover(n_layers)
!   END SUBROUTINE Compute_Random_Overlap_TL
    SUBROUTINE Compute_Random_Overlap_TL()
      INTEGER  :: k
      REAL(fp) :: prod_TL(0:n_layers)
      prod_TL    = ZERO
      prod_TL(0) = ZERO
      DO k = 1, n_layers
        if (cc_FWD%Cloud_Fraction(k) > MIN_COVERAGE_THRESHOLD) then    
           prod_TL(k) = (ONE - cc_FWD%Cloud_Fraction(k))*prod_TL(k-1) - & 
                         cc_FWD%iVar%prod(k-1)*self_TL%Cloud_Fraction(k)      
        else
           prod_TL(k) = prod_TL(k-1) 
        endif
        self_TL%Cloud_Cover(k) = -prod_TL(k)
      END DO
      self_TL%Total_Cloud_Cover = self_TL%Cloud_Cover(n_layers)
    END SUBROUTINE Compute_Random_Overlap_TL

!   SUBROUTINE Compute_MaxRan_Overlap_TL()
!     INTEGER  :: k
!     REAL(fp) :: prod_TL, denom
!     
!     ! Give the variables shorter names
!     ASSOCIATE( prod  => cc_FWD%iVar%prod      , &
!                cf    => cc_FWD%Cloud_Fraction , &
!                cf_TL => self_TL%Cloud_Fraction, &
!                cc_TL => self_TL%Cloud_Cover     )
!                
!       ! The cloud cover profile
!       prod_TL  = -cf_TL(1)
!       cc_TL(1) = -prod_TL   ! == self_TL%Cloud_Fraction(1)
!       DO k = 2, n_layers
!         IF ( cf(k) > cf(k-1) ) THEN
!    !>>orig
!           denom = ONE/(ONE - cf(k-1))
!           prod_TL = ((ONE - cf(k)) * denom * prod_TL) - &
!                     (prod(k-1) * denom                    * cf_TL(k)  ) + &
!                     (prod(k-1) * (ONE - cf(k)) * denom**2 * cf_TL(k-1))
!    !<<orig
!    !>>test
!     !      prod_TL = ( ((ONE - cf(k)) / (ONE - cf(k-1)))                   * prod_TL    ) - &
!     !                ( (prod(k-1) / (ONE - cf(k-1)))                       * cf_TL(k)   ) + &
!     !                ( ((prod(k-1) * (ONE - cf(k))) / (ONE - cf(k-1))**2)  * cf_TL(k-1) )
!    !<<test
!        END IF
!         cc_TL(k) = -prod_TL
!       END DO
!     
!     END ASSOCIATE
!     
!     ! Extract out the tangent-linear total cloud cover
!     self_TL%Total_Cloud_Cover = self_TL%Cloud_Cover(n_layers)
!
!   END SUBROUTINE Compute_MaxRan_Overlap_TL
    SUBROUTINE Compute_MaxRan_Overlap_TL()
      INTEGER  :: k
      REAL(fp) :: prod_TL
      REAL(fp) :: maxcov_TL

      prod_TL                = -self_TL%Cloud_Fraction(1)
      self_TL%Cloud_Cover(1) = -prod_TL
      DO k = 2, n_layers
         IF ((cc_FWD%Cloud_Fraction(k-1) > cc_FWD%Cloud_Fraction(k))) THEN
            maxcov_TL = -self_TL%Cloud_Fraction(k-1)
         ELSE IF ((cc_FWD%Cloud_Fraction(k-1) < cc_FWD%Cloud_Fraction(k))) THEN
            maxcov_TL = -self_TL%Cloud_Fraction(k)
         ELSE IF ((CC_FWD%Cloud_Fraction(k-1) == cc_FWD%Cloud_Fraction(k))) THEN
            maxcov_TL = -self_TL%Cloud_Fraction(k)
         ENDIF
            prod_TL = prod_TL * cc_FWD%iVar%maxcov(k) / (one - cc_FWD%Cloud_Fraction(k-1)) +  &
              & self_TL%Cloud_Fraction(k-1) * cc_FWD%iVar%prod(k-1) * cc_FWD%iVar%maxcov(k) / &
              & (one - cc_FWD%Cloud_Fraction(k-1)) ** 2 + &
              & maxcov_TL * cc_FWD%iVar%prod(k-1) / (one -  cc_FWD%Cloud_Fraction(k-1))
         self_TL%Cloud_Cover(k) = -prod_TL
      ENDDO
      self_TL%Total_Cloud_Cover = self_TL%Cloud_Cover(n_layers)
    END SUBROUTINE Compute_MaxRan_Overlap_TL

    SUBROUTINE Compute_Average_Overlap_TL()
      INTEGER  :: k, n
      REAL(fp) :: lwc_TL(n_Layers), wc_sum_TL(0:n_Layers), cwc_sum_TL(0:n_Layers)
      REAL(fp) :: denom

      ! Give the variables shorter names
      ASSOCIATE( lwc      => cc_FWD%iVar%lwc       , &
                 wc_sum   => cc_FWD%iVar%wc_sum    , &
                 cwc_sum  => cc_FWD%iVar%cwc_sum   , &
                 cf       => cc_FWD%Cloud_Fraction , &
                 cc       => cc_FWD%Cloud_Cover    , &
              !  cloud    => atm%Cloud             , & !orig 
                 cloud    => cc_FWD%iVar%wc        , & 
                 cf_TL    => self_TL%Cloud_Fraction, &
                 cc_TL    => self_TL%Cloud_Cover   , &
                 cloud_TL => atm_TL%Cloud            )
                 
        ! The total layer water content
        lwc_TL = ZERO
        DO n = 1, atm_TL%n_Clouds
      !   WHERE (cloud(n)%Water_Content(1:n_layers) > WATER_CONTENT_THRESHOLD ) &  !orig
          WHERE (cloud(n,1:n_layers) > WATER_CONTENT_THRESHOLD )                & 
            lwc_TL = lwc_TL + cloud_TL(n)%Water_Content(1:n_layers)
        END DO

        ! The cloud cover profile
        wc_sum_TL(0)  = ZERO
        cwc_sum_TL(0) = ZERO
        DO k = 1, n_layers
           wc_sum_TL(k) =  wc_sum_TL(k-1) +  lwc_TL(k)
          cwc_sum_TL(k) = cwc_sum_TL(k-1) + (cf(k) * lwc_TL(k)) + (lwc(k) * cf_TL(k))
          IF ( wc_sum(k) > ZERO ) THEN
            denom = ONE/wc_sum(k)
            cc_TL(k) = (denom                 * cwc_sum_TL(k)) - &
                       (denom**2 * cwc_sum(k) * wc_sum_TL(k) )
          END IF
        END DO
      
      END ASSOCIATE
      
      ! Extract out the tangent-linear total cloud cover
      self_TL%Total_Cloud_Cover = self_TL%Cloud_Cover(n_layers)
      
    END SUBROUTINE Compute_Average_Overlap_TL

    SUBROUTINE Compute_Overcast_Overlap_TL()

      self_TL%Total_Cloud_Cover = ZERO 

    END SUBROUTINE Compute_Overcast_Overlap_TL

  END FUNCTION Compute_CloudCover_TL


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Compute_CloudCover_AD
!
! PURPOSE:
!   Function method to compute the adjoint cloud cover profile for supplied
!   forward model results and populate the Atmosphere adjoint object
!   with the results.
!
! CALLING SEQUENCE:
!   err_stat = cc_obj_AD%Compute_CloudCover_AD( &
!                cc_FWD       , &
!                atmosphere   , &
!                atmosphere_AD  )
!
! OBJECTS:
!   cc_obj_AD:      The adjoint cloud cover object. This object contains
!                   data on input, but is zeroed out upon output.
!                   UNITS:      N/A
!                   CLASS:      CRTM_CloudCover_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!   cc_FWD:         The forward model cloud cover object.
!                   UNITS:      N/A
!                   CLASS:      CRTM_CloudCover_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN)
!
!   atmosphere:     Atmopshere object containing the layer cloud fraction
!                   profile, and the actual cloud profiles for when cloud
!                   water content averaging of the cloud cover is selected.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Atmosphere_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!   atmosphere_AD:  The adjoint atmosphere object containing the results of the
!                   adjoint calculation. The adjoint cloud fraction profile
!                   will be modified on output. For the weighted average cloud
!                   clover method, the adjoint cloud water amounts will also be
!                   modified.
!                   UNITS:      N/A
!                   TYPE:       CRTM_Atmosphere_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!   err_stat:       The return value is an integer defining the error status.
!                   The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the computation was successful
!                        == FAILURE, an unrecoverable error occurred.
!                   UNITS:      N/A
!                   TYPE:       INTEGER
!                   DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Compute_CloudCover_AD( &
    self_AD, &  ! Input, but modified on output
    cc_FWD , &  ! Input
    atm    , &  ! Input
    atm_AD ) &  ! Output, but contains information on input
  RESULT(err_stat)
    ! Arguments
    CLASS(CRTM_CloudCover_type), INTENT(IN OUT) :: self_AD
    CLASS(CRTM_CloudCover_type), INTENT(IN)     :: cc_FWD
    TYPE(CRTM_Atmosphere_type) , INTENT(IN)     :: atm
    TYPE(CRTM_Atmosphere_type) , INTENT(IN OUT) :: atm_AD
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: PROCEDURE_NAME = 'CRTM_CloudCover_Define::Compute_CloudCover_AD'
    REAL(fp),     PARAMETER :: MIN_COVERAGE_THRESHOLD = 1.0e-06_fp                           
    REAL(fp),     PARAMETER :: MAX_COVERAGE_THRESHOLD = ONE - MIN_COVERAGE_THRESHOLD

    ! Local variables
    CHARACTER(ML) :: err_msg
    INTEGER :: n_layers
    INTEGER :: n_clouds
    INTEGER :: n      
    REAL(fp):: sum_wc

    ! Check inputs
    err_stat = SUCCESS
    IF ( .NOT. self_AD%Is_Usable() ) THEN
      err_msg = 'In/output adjoint cloud cover object is not usable'
      err_stat = FAILURE
      CALL Display_Message(PROCEDURE_NAME, err_msg, err_stat); RETURN
    END IF
    IF ( .NOT. cc_FWD%Is_Usable( Include_iVar=.TRUE. ) ) THEN
      err_msg = 'Input forward cloud cover object is not usable'
      err_stat = FAILURE
      CALL Display_Message(PROCEDURE_NAME, err_msg, err_stat); RETURN
    END IF
    IF ( .NOT. CRTM_Atmosphere_Associated(atm) ) THEN
      err_msg = 'Input atmosphere object is not usable'
      err_stat = FAILURE
      CALL Display_Message(PROCEDURE_NAME, err_msg, err_stat); RETURN
    END IF
    IF ( .NOT. CRTM_Atmosphere_Associated(atm_AD) ) THEN
      err_msg = 'In/output adjoint atmosphere object is not usable'
      err_stat = FAILURE
      CALL Display_Message(PROCEDURE_NAME, err_msg, err_stat); RETURN
    END IF


    ! Set the object quantities
    n_layers = atm_AD%n_Layers
    n_clouds = atm_AD%n_Clouds
    self_AD%Overlap = cc_FWD%Overlap

    ! Add AD of cloud scaling here!
    ! Partition all hydrometeors into cloudy column
     IF (cc_FWD%Total_Cloud_Cover > MIN_COVERAGE_THRESHOLD) then

       sum_wc = ZERO
       DO n = 1, n_clouds
          sum_wc = sum_wc  &
                 + SUM(atm_AD%Cloud(n)%Water_Content(1:n_layers) * cc_FWD%iVar%wc(n,1:n_layers))
       ENDDO
       sum_wc = sum_wc / (cc_FWD%Total_Cloud_Cover**2)
!>>test
!      sum_wc = ZERO
!      DO n = 1, n_clouds
!         sum_wc = sum_wc  &
!                + SUM( (atm_AD%Cloud(n)%Water_Content(1:n_layers) / cc_FWD%Total_Cloud_Cover) &
!                     * (cc_FWD%iVar%wc(n,1:n_layers) / cc_FWD%Total_Cloud_Cover) )                                     
!      ENDDO
!<<test

       self_AD%Total_Cloud_Cover = self_AD%Total_Cloud_Cover - sum_wc

       DO n = 1, n_clouds
         atm_AD%Cloud(n)%Water_Content(1:n_layers) = atm_AD%Cloud(n)%Water_Content(1:n_layers) &
                                          / cc_FWD%Total_Cloud_Cover
       ENDDO

     END IF

    ! Compute the cloud cover
    SELECT CASE (self_AD%Overlap)
      CASE (MAXIMUM_OVERLAP_ID); CALL Compute_Maximum_Overlap_AD()
      CASE (RANDOM_OVERLAP_ID) ; CALL Compute_Random_Overlap_AD()
      CASE (MAXRAN_OVERLAP_ID) ; CALL Compute_MaxRan_Overlap_AD()
      CASE (AVERAGE_OVERLAP_ID); CALL Compute_Average_Overlap_AD()
      CASE (OVERCAST_OVERLAP_ID);CALL Compute_Overcast_Overlap_AD()
    END SELECT

    ! Transfer the cloud fraction output
    atm_AD%Cloud_Fraction(1:n_Layers) = atm_AD%Cloud_Fraction(1:n_Layers) + self_AD%Cloud_Fraction
    self_AD%Cloud_Fraction = ZERO

  CONTAINS

    SUBROUTINE Compute_Maximum_Overlap_AD()
      INTEGER :: k
      self_AD%Cloud_Cover(n_layers) = self_AD%Cloud_Cover(n_layers) + self_AD%Total_Cloud_Cover
      self_AD%Total_Cloud_Cover = ZERO
      DO k = n_layers, 2, -1
        IF ( cc_FWD%Cloud_Fraction(k) > cc_FWD%Cloud_Cover(k-1) ) THEN
          self_AD%Cloud_Fraction(k) = self_AD%Cloud_Fraction(k) + self_AD%Cloud_Cover(k)
        ELSE
          self_AD%Cloud_Cover(k-1) = self_AD%Cloud_Cover(k-1) + self_AD%Cloud_Cover(k)
        END IF
        self_AD%Cloud_Cover(k) = ZERO
      END DO
      self_AD%Cloud_Fraction(1) = self_AD%Cloud_Fraction(1) + self_AD%Cloud_Cover(1)
      self_AD%Cloud_Cover(1) = ZERO
    END SUBROUTINE Compute_Maximum_Overlap_AD


!   SUBROUTINE Compute_Random_Overlap_AD()
!     INTEGER  :: k
!     REAL(fp) :: prod_AD
!     prod_AD = ZERO
!     self_AD%Cloud_Cover(n_layers) = self_AD%Cloud_Cover(n_layers) + self_AD%Total_Cloud_Cover
!     self_AD%Total_Cloud_Cover = ZERO
!     DO k = n_layers, 1, -1
!       prod_AD = prod_AD - self_AD%Cloud_Cover(k)
!       self_AD%Cloud_Cover(k) = ZERO
!       self_AD%Cloud_Fraction(k) = self_AD%Cloud_Fraction(k) - (cc_FWD%iVar%prod(k-1)*prod_AD)
!       prod_AD = (ONE - cc_FWD%Cloud_Fraction(k))*prod_AD
!     END DO
!     prod_AD = ZERO
!   END SUBROUTINE Compute_Random_Overlap_AD
    SUBROUTINE Compute_Random_Overlap_AD()
      INTEGER  :: k
      REAL(fp) :: prod_AD(0:n_layers)
      prod_AD           = ZERO
      prod_AD(n_layers) = ZERO
      self_AD%Cloud_Cover(n_layers) = self_AD%Cloud_Cover(n_layers) + self_AD%Total_Cloud_Cover
      self_AD%Total_Cloud_Cover = ZERO
      DO k = n_layers, 1, -1
        prod_AD(k) = prod_AD(k) - self_AD%Cloud_Cover(k)
        self_AD%Cloud_Cover(k) = ZERO
        if (cc_FWD%Cloud_Fraction(k) > MIN_COVERAGE_THRESHOLD) then    
           self_AD%Cloud_Fraction(k) = self_AD%Cloud_Fraction(k) - (cc_FWD%iVar%prod(k-1)*prod_AD(k))
           prod_AD(k-1) = prod_AD(k-1)+(ONE - cc_FWD%Cloud_Fraction(k))*prod_AD(k)
           prod_AD(k) = ZERO
        else
           prod_AD(k-1) = prod_AD(k-1)+ prod_AD(k)
           prod_AD(k) = ZERO 
        endif
      END DO
      prod_AD(0) = ZERO
    END SUBROUTINE Compute_Random_Overlap_AD

!   SUBROUTINE Compute_MaxRan_Overlap_AD()
!     INTEGER  :: k
!     REAL(fp) :: prod_AD, denom
!     prod_AD = ZERO
!     self_AD%Cloud_Cover(n_layers) = self_AD%Cloud_Cover(n_layers) + self_AD%Total_Cloud_Cover
!     self_AD%Total_Cloud_Cover = ZERO
!     DO k = n_layers, 2, -1
!       prod_AD = prod_AD - self_AD%Cloud_Cover(k)
!       self_AD%Cloud_Cover(k) = ZERO
!       IF ( cc_FWD%Cloud_Fraction(k) > cc_FWD%Cloud_Fraction(k-1) ) THEN
!         denom = ONE/(ONE - cc_FWD%Cloud_Fraction(k-1))
!         self_AD%Cloud_Fraction(k-1) = self_AD%Cloud_Fraction(k-1) + &
!                                       (cc_FWD%iVar%prod(k-1) * (ONE - cc_FWD%Cloud_Fraction(k)) * denom**2 * prod_AD)
!         self_AD%Cloud_Fraction(k) = self_AD%Cloud_Fraction(k) - &
!                                     (cc_FWD%iVar%prod(k-1) * denom * prod_AD)
!         prod_AD = (ONE - cc_FWD%Cloud_Fraction(k)) * denom * prod_AD
!       END IF
!     END DO
!     prod_AD = prod_AD - self_AD%Cloud_Cover(1)
!     self_AD%Cloud_Cover(1) = ZERO
!     self_AD%Cloud_Fraction(1) = self_AD%Cloud_Fraction(1) - prod_AD
!     prod_AD = ZERO
!    END SUBROUTINE Compute_MaxRan_Overlap_AD
    SUBROUTINE Compute_MaxRan_Overlap_AD()
      INTEGER  :: k
      REAL(fp) :: prod_AD
      REAL(fp) :: maxcov_AD
      prod_AD   = ZERO
      maxcov_AD = ZERO
      self_AD%Cloud_Cover(n_layers) = self_AD%Cloud_Cover(n_layers) + self_AD%Total_Cloud_Cover                                
      self_AD%Total_Cloud_Cover = ZERO
      DO k = n_layers, 2,  - 1
        prod_AD = prod_AD - self_AD%Cloud_Cover(k)
        self_AD%Cloud_Cover(k) = ZERO
        self_AD%Cloud_Fraction(k-1) = self_AD%Cloud_Fraction(k-1) +      &
                   & prod_AD * cc_FWD%iVar%prod(k-1) * cc_FWD%iVar%maxcov(k) / (one - cc_FWD%Cloud_Fraction(k-1)) ** 2
        maxcov_AD                   =      &
                   & maxcov_AD + prod_AD * cc_FWD%iVar%prod(k-1) / (one - cc_FWD%Cloud_Fraction(k-1))
        prod_AD                     =      &
                   & prod_AD  * cc_FWD%iVar%maxcov(k) / (one - cc_FWD%Cloud_Fraction(k-1))
        IF ((cc_FWD%Cloud_Fraction(k-1) > cc_FWD%Cloud_Fraction(k))) THEN
          self_AD%Cloud_Fraction(k-1) = self_AD%Cloud_Fraction(k-1) - maxcov_AD
          maxcov_AD     = zero
        ELSE IF ((cc_FWD%Cloud_Fraction(k-1) < cc_FWD%Cloud_Fraction(k))) THEN
          self_AD%Cloud_Fraction(k) = self_AD%Cloud_Fraction(k) - maxcov_AD
          maxcov_AD     = zero
        ELSE IF ((cc_FWD%Cloud_Fraction(k-1) == cc_FWD%Cloud_Fraction(k))) THEN
          self_AD%Cloud_Fraction(k) = self_AD%Cloud_Fraction(k) - maxcov_AD
          maxcov_AD     = ZERO
        ENDIF
      ENDDO
      prod_AD = prod_AD - maxcov_AD
      self_AD%Cloud_Cover(1) = ZERO
      self_AD%Cloud_Fraction(1) = self_AD%Cloud_Fraction(1) - prod_AD
      prod_AD = ZERO
    END SUBROUTINE Compute_MaxRan_Overlap_AD

    SUBROUTINE Compute_Average_Overlap_AD()
      INTEGER  :: k, n
      REAL(fp) :: lwc_AD(n_Layers), wc_sum_AD(0:n_Layers), cwc_sum_AD(0:n_Layers)
      REAL(fp) :: denom

      ! Initialise local adjoint variables
      lwc_AD     = ZERO
      wc_sum_AD  = ZERO
      cwc_sum_AD = ZERO

      ! Adjoint of the total cloud cover
      self_AD%Cloud_Cover(n_layers) = self_AD%Cloud_Cover(n_layers) + self_AD%Total_Cloud_Cover
      self_AD%Total_Cloud_Cover     = ZERO
      ! Adjoint of the cloud cover profile
      ASSOCIATE( lwc      => cc_FWD%iVar%lwc       , &
                 wc_sum   => cc_FWD%iVar%wc_sum    , &
                 cwc_sum  => cc_FWD%iVar%cwc_sum   , &
                 cf       => cc_FWD%Cloud_Fraction , &
              !  cloud    => atm%Cloud             , &   !orig
                 cloud    => cc_FWD%iVar%wc        , &  
                 cf_AD    => self_AD%Cloud_Fraction, &
                 cc_AD    => self_AD%Cloud_Cover   , &
                 cloud_AD => atm_AD%Cloud            )
                 
        DO k = n_layers, 1, -1
          IF ( wc_sum(k) > ZERO ) THEN
            denom = ONE/wc_sum(k)
            wc_sum_AD(k)  = wc_sum_AD(k)  - (denom**2 * cwc_sum(k) * cc_AD(k))
            cwc_sum_AD(k) = cwc_sum_AD(k) + (denom                 * cc_AD(k))
            cc_AD(k)      = ZERO
          END IF

          cf_AD(k)        = cf_AD(k)        + (lwc(k) * cwc_sum_AD(k))
          lwc_AD(k)       = lwc_AD(k)       + (cf(k)  * cwc_sum_AD(k))
          cwc_sum_AD(k-1) = cwc_sum_AD(k-1) + cwc_sum_AD(k)
          cwc_sum_AD(k)   = ZERO

          lwc_AD(k)      = lwc_AD(k)      + wc_sum_AD(k)
          wc_sum_AD(k-1) = wc_sum_AD(k-1) + wc_sum_AD(k)
          wc_sum_AD(k)   = ZERO
        END DO
        wc_sum_AD(0)  = ZERO
        cwc_sum_AD(0) = ZERO
      

        ! Adjoint of the total layer water content
        DO n = 1, SIZE(cloud_AD)
       !  WHERE (cloud(n)%Water_Content(1:n_layers) > WATER_CONTENT_THRESHOLD ) &   !orig
          WHERE (cloud(n,1:n_layers) > WATER_CONTENT_THRESHOLD ) &           
            cloud_AD(n)%Water_Content(1:n_layers) = cloud_AD(n)%Water_Content(1:n_layers) + lwc_AD
        END DO
        lwc_AD = ZERO

      END ASSOCIATE

    END SUBROUTINE Compute_Average_Overlap_AD

    SUBROUTINE Compute_Overcast_Overlap_AD()

       self_AD%Total_Cloud_Cover = ZERO 

    END SUBROUTINE Compute_Overcast_Overlap_AD

  END FUNCTION Compute_CloudCover_AD


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Is_Usable
!
! PURPOSE:
!   Elemental function method to test the status of CloudCover objects to
!   determien if they are usable.
!
! CALLING SEQUENCE:
!   status = cc_obj%Is_Usable( Include_iVar = Include_iVar )
!
! OBJECTS:
!   cc_obj:        Cloud cover object which is to have its usability tested.
!                  UNITS:      N/A
!                  CLASS:      CRTM_CloudCover_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!   Include_iVar:  Set this optional logical flag to alos check the status
!                  of the intermediate variable sub-object.
!                  IF .FALSE. - the subobject is NOT tested [DEFAULT]
!                     .TRUE.  - the subobject is tested
!                  UNITS:      N/A
!                  TYPE:       CHARACTER(*)
!                  DIMENSION:  Conformable with object.
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!   status:        The return value is a logical value indicating the
!                  usable status of the object.
!                    .TRUE.  - if the object is usable.
!                    .FALSE. - if the object is NOT usable.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Same as object
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Is_Usable( self, Include_iVar ) RESULT( status )
    CLASS(CRTM_CloudCover_type), INTENT(IN) :: self
    LOGICAL,          OPTIONAL , INTENT(IN) :: Include_iVar
    LOGICAL :: status
    status = self%Is_Allocated
    IF ( PRESENT(Include_iVar) ) THEN
      IF ( Include_iVar ) status = status .AND. self%iVar%Is_Usable()
    END IF
  END FUNCTION Is_Usable

  ELEMENTAL FUNCTION iVar_Is_Usable( self ) RESULT( status )
    CLASS(iVar_type), INTENT(IN) :: self
    LOGICAL :: status
    status = self%Is_Allocated
  END FUNCTION iVar_Is_Usable


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Destroy
!
! PURPOSE:
!   Elemental subroutine method to re-initialize CloudCover objects.
!
! CALLING SEQUENCE:
!   CALL cc_obj%Destroy()
!
! OBJECTS:
!   cc_obj:  Re-initialized cloud cover object(s).
!            UNITS:      N/A
!            CLASS:      CRTM_CloudCover_type
!            DIMENSION:  Scalar or any rank
!            ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Destroy( self )
    CLASS(CRTM_CloudCover_type), INTENT(INOUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE Destroy

  ELEMENTAL SUBROUTINE iVar_Destroy( self )
    CLASS(iVar_type), INTENT(INOUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE iVar_Destroy

!  SUBROUTINE Cleanup(self)
!    TYPE(CRTM_CloudCover_type) :: self
!    CALL self%Destroy()
!  END SUBROUTINE Cleanup


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Create
!
! PURPOSE:
!   Elemental subroutine method to create instances of CloudCover objects.
!
! CALLING SEQUENCE:
!   CALL cc_obj%Create( n_Layers, &
!                       n_Clouds, & 
!                       Forward       = Forward, &
!                       Error_Message = Error_Message )
!
! OBJECTS:
!   cc_obj:         Cloud cover object
!                   UNITS:      N/A
!                   CLASS:      CRTM_CloudCover_type
!                   DIMENSION:  Scalar or any rank
!                   ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!   n_Layers:       Number of layers for which there is cloud data.
!                   Must be > 0.
!                   UNITS:      N/A
!                   TYPE:       INTEGER
!                   DIMENSION:  Conformable with object.
!                   ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!   Forward:        Set this optional logical flag to allocate the sub-object to
!                   hold the intermediate forward model results.
!                   IF .FALSE. - the subobject is NOT allocated [DEFAULT]
!                      .TRUE.  - the subobject is allocated
!                   UNITS:      N/A
!                   TYPE:       CHARACTER(*)
!                   DIMENSION:  Conformable with object.
!                   ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!   Error_Message:  If an error occurred creating the object, this
!                   argument will contain error information.
!                   UNITS:      N/A
!                   TYPE:       CHARACTER(*)
!                   DIMENSION:  Conformable with object.
!                   ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Create( &
    self         , &
    n_Layers     , &
    n_Clouds     , & 
    Forward      , &
    Error_Message  )
    ! Arguments
    CLASS(CRTM_CloudCover_type), INTENT(INOUT) :: self
    INTEGER                    , INTENT(IN)  :: n_Layers
    INTEGER                    , INTENT(IN)  :: n_Clouds  
    LOGICAL,           OPTIONAL, INTENT(IN)  :: Forward
    CHARACTER(*),      OPTIONAL, INTENT(OUT) :: Error_Message
    ! Local variables
    CHARACTER(ML) :: alloc_msg
    INTEGER :: alloc_stat
    LOGICAL :: allocate_ivar


    ! Check input
    IF ( n_Layers < 1 ) THEN
      IF ( PRESENT(Error_Message) ) Error_Message = 'Invalid dimension inputs'
      RETURN
    END IF
    allocate_ivar = .FALSE.
    IF ( PRESENT(Forward) ) allocate_ivar = Forward


    ! Intermediate variable object
    IF ( allocate_ivar ) THEN
!     CALL self%iVar%Create(n_Layers, Error_Message = Error_Message)                  
      CALL self%iVar%Create(n_Layers, n_Clouds, Error_Message = Error_Message)       
      IF ( .NOT. self%iVar%Is_Usable() ) RETURN
    END IF


    ! Main object
    ! ...Perform the allocations
    ALLOCATE( self%Cloud_Fraction( n_Layers ), &
              self%Cloud_Cover( n_Layers ), &
              STAT = alloc_stat )
             !STAT = alloc_stat, ERRMSG = alloc_msg )
    IF ( alloc_stat /= 0 ) THEN
      IF ( PRESENT(Error_Message) ) Error_Message = alloc_msg
      RETURN
    END IF
    ! ...Initialise
    self%n_Layers = n_Layers
    self%Cloud_Fraction    = ZERO
    self%Cloud_Cover       = ZERO
    ! ...Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE Create


  ELEMENTAL SUBROUTINE iVar_Create( &
    self         , &
    n_Layers     , &
    n_Clouds     , &  
    Error_Message  )
    ! Arguments
    CLASS(iVar_type)      , INTENT(INOUT) :: self
    INTEGER               , INTENT(IN)  :: n_Layers
    INTEGER               , INTENT(IN)  :: n_Clouds 
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Error_Message
    ! Local variables
    CHARACTER(ML) :: alloc_msg
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Layers < 1 ) THEN
      IF ( PRESENT(Error_Message) ) Error_Message = 'iVar: Invalid dimension input'
      RETURN
    END IF

    ! Perform the allocations
    ALLOCATE( self%prod( 0:n_Layers ), &
              self%lwc( 1:n_Layers ), &
              self%wc_sum( 0:n_Layers ), &
              self%wc( 1:n_Clouds, 1:n_Layers ), & 
              self%maxcov( 1:n_Layers ), & 
              self%cwc_sum( 0:n_Layers ), &
              STAT = alloc_stat )
             !STAT = alloc_stat, ERRMSG = alloc_msg )
    IF ( alloc_stat /= 0 ) THEN
      IF ( PRESENT(Error_Message) ) Error_Message = 'iVar: '//TRIM(alloc_msg)
      RETURN
    END IF
    
    ! Initialise
    self%n_Layers = n_Layers
    self%n_Clouds = n_Clouds  
    self%prod    = ZERO
    self%lwc     = ZERO
    self%wc      = ZERO 
    self%maxcov  = ZERO
    self%wc_sum  = ZERO
    self%cwc_sum = ZERO
    
    ! Set allocation indicator
    self%Is_Allocated = .TRUE.
    
  END SUBROUTINE iVar_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Inspect
!
! PURPOSE:
!   Subroutine method to display the contents of a CloudCover object.
!
! CALLING SEQUENCE:
!   CALL cc_obj%Inspect( Hires=hires, Unit=unit, Verbose=Verbose )
!
! OBJECTS:
!   cc_obj:  Cloud cover object
!            UNITS:      N/A
!            CLASS:      CRTM_CloudCover_type
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!   Hires:   Set this logical argument to output object contents with
!            more significant digits.
!            If == .FALSE., output format is 'es13.6' [DEFAULT].
!               == .TRUE.,  output format is 'es22.15'
!            If not specified, default is .FALSE.
!            UNITS:      N/A
!            TYPE:       LOGICAL
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   Unit:    Unit number for an already open file to which the output
!            will be written.
!            If the argument is specified and the file unit is not
!            connected, the output goes to stdout.
!            UNITS:      N/A
!            TYPE:       INTEGER
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   Verbose: Set this logical argument to output the intermediate variable
!            sub-object contents if they are available.
!            If == .FALSE., the intermediate variables are NOT output [DEFAULT].
!               == .TRUE.,  the intermediate variables are output if available
!            If not specified, default is .FALSE.
!            UNITS:      N/A
!            TYPE:       LOGICAL
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Inspect( self, Hires, Unit, Verbose )
    ! Arguments
    CLASS(CRTM_CloudCover_type), INTENT(IN) :: self
    LOGICAL,           OPTIONAL, INTENT(IN) :: Hires
    INTEGER,           OPTIONAL, INTENT(IN) :: Unit
    LOGICAL,           OPTIONAL, INTENT(IN) :: Verbose
    ! Local variables
    INTEGER :: fid
    CHARACTER(20) :: fmtstring
    LOGICAL :: verbose_inspect

    ! Setup
    fmtstring = 'es13.6'
    IF ( PRESENT(Hires) ) THEN
      IF ( Hires ) fmtstring = 'es22.15'
    END IF
    fid = OUTPUT_UNIT
    IF ( PRESENT(Unit) ) THEN
      IF ( File_Open(Unit) ) fid = Unit
    END IF
    verbose_inspect = .FALSE.
    IF ( PRESENT(Verbose) ) verbose_inspect = Verbose

    ! The inspection output
    WRITE(fid,'(1x,"CLOUDCOVER OBJECT")')
    WRITE(fid,'(3x,"n_Layers          :",1x,i0)') self%n_Layers
    WRITE(fid,'(3x,"Overlap           :",1x,a )') self%Overlap_Name()
    WRITE(fid,'(3x,"Total cloud cover :",1x,'//TRIM(fmtstring)//')') self%Total_Cloud_Cover
    IF ( .NOT. self%Is_Usable() ) RETURN
    WRITE(fid,'(3x,"Cloud_Fraction:")')
    WRITE(fid,'(5(1x,'//TRIM(fmtstring)//',:))') self%Cloud_Fraction
    WRITE(fid,'(3x,"Cloud_Cover:")')
    WRITE(fid,'(5(1x,'//TRIM(fmtstring)//',:))') self%Cloud_Cover

    ! The intermediate variable sub-object
    IF ( verbose_inspect ) CALL self%iVar%Inspect()

  END SUBROUTINE Inspect

  
  SUBROUTINE iVar_Inspect(self, Hires, Unit )
    ! Arguments
    CLASS(iVar_Type) , INTENT(IN) :: self
    LOGICAL, OPTIONAL, INTENT(IN) :: Hires
    INTEGER, OPTIONAL, INTENT(IN) :: Unit
    ! Local variables
    INTEGER :: fid
    CHARACTER(20) :: fmtstring

    ! Setup
    fmtstring = 'es13.6'
    IF ( PRESENT(Hires) ) THEN
      IF ( Hires ) fmtstring = 'es22.15'
    END IF
    fid = OUTPUT_UNIT
    IF ( PRESENT(Unit) ) THEN
      IF ( File_Open(Unit) ) fid = Unit
    END IF
    
    ! The inspection output
    WRITE(fid,'(3x,"CLOUDCOVER iVar SUB-OBJECT")')
    WRITE(fid,'(5x,"n_Layers :",1x,i0)') self%n_Layers
    WRITE(fid,'(7x,"Layer-to-layer product:")')
    WRITE(fid,'(5(1x,'//TRIM(fmtstring)//',:))') self%prod
    WRITE(fid,'(7x,"Layer water content for all clouds:")')
    WRITE(fid,'(5(1x,'//TRIM(fmtstring)//',:))') self%lwc
    WRITE(fid,'(7x,"Cumulative layer water content for all clouds:")')
    WRITE(fid,'(5(1x,'//TRIM(fmtstring)//',:))') self%wc_sum
    WRITE(fid,'(7x,"Cumulative cloud fraction weighted layer water content for all clouds:")')
    WRITE(fid,'(5(1x,'//TRIM(fmtstring)//',:))') self%cwc_sum

  END SUBROUTINE iVar_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Set_To_Zero
!
! PURPOSE:
!   Elemental subroutine method to zero out the data arrays in a
!   CloudCover object.
!
! CALLING SEQUENCE:
!   CALL cc_obj%Set_To_Zero()
!
! OBJECTS:
!   cc_obj:  Cloud cover object
!            UNITS:      N/A
!            CLASS:      CRTM_CloudCover_type
!            DIMENSION:  Scalar or any rank
!            ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!   - The dimension components of the object are *NOT* set to zero.
!   - The overlap methodology identifier component is *NOT* reset.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Set_To_Zero( self )
    CLASS(CRTM_CloudCover_type), INTENT(IN OUT) :: self
    ! Do nothing if structure is unused
    IF ( .NOT. self%Is_Usable() ) RETURN
    ! Only zero out the data
    self%Total_Cloud_Cover = ZERO
    self%Cloud_Fraction    = ZERO
    self%Cloud_Cover       = ZERO
    ! The intermediate variable sub-object
    CALL self%iVar%Set_To_Zero()
    
  END SUBROUTINE Set_To_Zero


  ELEMENTAL SUBROUTINE iVar_Set_To_Zero( self )
    CLASS(iVar_type), INTENT(IN OUT) :: self
    ! Do nothing if structure is unused
    IF ( .NOT. self%Is_Usable() ) RETURN
    ! Only zero out the data
    self%prod    = ZERO
    self%lwc     = ZERO
    self%wc_sum  = ZERO
    self%cwc_sum = ZERO
  END SUBROUTINE iVar_Set_To_Zero



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                            ## OPERATOR METHODS ##                            ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   ==
!
! PURPOSE:
!   Operator method to test the equality of two CloudCover objects.
!
! CALLING SEQUENCE:
!   IF ( x == y ) THEN
!     ...
!   END IF
!
! OBJECTS:
!   x, y:  Two cloud cover object to be compared.
!          UNITS:      N/A
!          TYPE:       CRTM_CloudCover_type
!          DIMENSION:  Scalar or any rank
!          ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Equal( x, y ) RESULT( is_equal )
    CLASS(CRTM_CloudCover_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.

    ! Check the object association status
    IF ( x%Is_Usable() .NEQV. y%Is_Usable() ) RETURN

    ! Check contents
    ! ...Dimensions
    IF ( x%n_Layers /= y%n_Layers ) RETURN
    ! ...Scalars
    IF ( .NOT. ((x%Overlap              ==     y%Overlap          ) .AND. &
                (x%Total_Cloud_Cover .EqualTo. y%Total_Cloud_Cover)) ) RETURN
    ! ...Arrays
    IF ( x%Is_Usable() .AND. y%Is_Usable() ) THEN
      IF ( .NOT. (ALL(x%Cloud_Fraction .EqualTo. y%Cloud_Fraction) .AND. &
                  ALL(x%Cloud_Cover    .EqualTo. y%Cloud_Cover   )) ) RETURN
      ! Intermediate variable subobject
      IF ( .NOT. (x%iVar == y%iVar) ) RETURN
    END IF


    ! If we get here, then...
    is_equal = .TRUE.

  END FUNCTION Equal


  ELEMENTAL FUNCTION iVar_Equal( x, y ) RESULT( is_equal )
    CLASS(iVar_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.

    ! Check the object association status
    IF ( x%Is_Usable() .NEQV. y%Is_Usable() ) RETURN

    ! Check contents
    ! ...Dimensions
    IF ( x%n_Layers /= y%n_Layers ) RETURN
    ! ...Arrays
    IF ( x%Is_Usable() .AND. y%Is_Usable() ) THEN
      IF ( .NOT. (ALL(x%prod    .EqualTo. y%prod   ) .AND. &
                  ALL(x%lwc     .EqualTo. y%lwc    ) .AND. &
                  ALL(x%wc_sum  .EqualTo. y%wc_sum ) .AND. &
                  ALL(x%cwc_sum .EqualTo. y%cwc_sum)) ) RETURN
    END IF

    ! If we get here, then...
    is_equal = .TRUE.

  END FUNCTION iVar_Equal


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   /=
!
! PURPOSE:
!   Operator method to test the inequality of two CloudCover objects.
!
! CALLING SEQUENCE:
!   IF ( x /= y ) THEN
!     ...
!   END IF
!
! OBJECTS:
!   x, y:  Two cloud cover objects to be compared.
!          UNITS:      N/A
!          TYPE:       CRTM_CloudCover_type
!          DIMENSION:  Scalar or any rank
!          ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION NotEqual( x, y ) RESULT( not_equal )
    CLASS(CRTM_CloudCover_type), INTENT(IN) :: x, y
    LOGICAL :: not_equal
    not_equal = .NOT. (x == y)
  END FUNCTION NotEqual

  ELEMENTAL FUNCTION iVar_NotEqual( x, y ) RESULT( not_equal )
    CLASS(iVar_type), INTENT(IN) :: x, y
    LOGICAL :: not_equal
    not_equal = .NOT. (x == y)
  END FUNCTION iVar_NotEqual


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   .Compare.
!
! PURPOSE:
!   Operator method to compare two CloudCover objects.
!
!   This procedure performs similarly to the == operator, but is non-elemental
!   to allow for informational output when a difference is found between the
!   two objects being compared.
!
!   Mostly used for debugging.
!
! CALLING SEQUENCE:
!   IF ( x .Compare. y ) THEN
!     ...
!   END IF
!
! OBJECTS:
!   x, y:      The cloud cover objects to compare.
!              UNITS:      N/A
!              CLASS:      CRTM_CloudCover_type
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Compare_( x, y ) RESULT( is_equal )
    CLASS(CRTM_CloudCover_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_CloudCover_Define::Compare'
    ! Local variable
    CHARACTER(ML) :: msg

     ! Set up
    is_equal = .TRUE.

    ! Check the object association status
    IF ( x%Is_Usable() .NEQV. y%Is_Usable() ) THEN
      msg = 'Object association statuses are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE)
      is_equal = .FALSE.
    END IF

    ! Check contents
    ! ...Dimensions
    IF ( x%n_Layers /= y%n_Layers ) THEN
      msg = 'Object dimensions are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE)
      is_equal = .FALSE.
    END IF
    ! Scalars
    IF ( x%Overlap /= y%Overlap ) THEN
      msg = 'Object overlap assumptions are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE)
      is_equal = .FALSE.
    END IF
    IF ( .NOT. (x%Total_Cloud_Cover .EqualTo. y%Total_Cloud_Cover) ) THEN
      msg = 'Object Total_Cloud_Cover values are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE)
      is_equal = .FALSE.
    END IF
    ! ...Arrays and sub-objects
    IF ( x%Is_Usable() .AND. y%Is_Usable() ) THEN
      IF ( .NOT. ALL(x%Cloud_Fraction .EqualTo. y%Cloud_Fraction) ) THEN
        msg = 'Object Cloud_Fraction data are different'
        CALL Display_Message(ROUTINE_NAME, msg, FAILURE)
        is_equal = .FALSE.
      END IF
      IF ( .NOT. ALL(x%Cloud_Cover .EqualTo. y%Cloud_Cover) ) THEN
        msg = 'Object Cloud_Cover data are different'
        CALL Display_Message(ROUTINE_NAME, msg, FAILURE)
        is_equal = .FALSE.
      END IF
      IF ( .NOT. (x%iVar .Compare. y%iVar) ) THEN
        msg = 'Object iVar subobject data are different'
        CALL Display_Message(ROUTINE_NAME, msg, FAILURE)
        is_equal = .FALSE.
      END IF
    END IF

  END FUNCTION Compare_


  FUNCTION iVar_Compare( x, y ) RESULT( is_equal )
    CLASS(iVar_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_CloudCover_Define::iVar_Compare'
    ! Local variable
    CHARACTER(ML) :: msg

     ! Set up
    is_equal = .TRUE.

    ! Check the object association status
    IF ( x%Is_Usable() .NEQV. y%Is_Usable() ) THEN
      msg = 'Object association statuses are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE)
      is_equal = .FALSE.
    END IF

    ! Check contents
    ! ...Dimensions
    IF ( x%n_Layers /= y%n_Layers ) THEN
      msg = 'Object dimensions are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE)
      is_equal = .FALSE.
    END IF
    ! ...Arrays
    IF ( x%Is_Usable() .AND. y%Is_Usable() ) THEN
      IF ( .NOT. ALL(x%prod .EqualTo. y%prod) ) THEN
        msg = 'Object prod data are different'
        CALL Display_Message(ROUTINE_NAME, msg, FAILURE)
        is_equal = .FALSE.
      END IF
      IF ( .NOT. ALL(x%lwc .EqualTo. y%lwc) ) THEN
        msg = 'Object lwc data are different'
        CALL Display_Message(ROUTINE_NAME, msg, FAILURE)
        is_equal = .FALSE.
      END IF
      IF ( .NOT. ALL(x%wc_sum .EqualTo. y%wc_sum) ) THEN
        msg = 'Object wc_sum data are different'
        CALL Display_Message(ROUTINE_NAME, msg, FAILURE)
        is_equal = .FALSE.
      END IF
      IF ( .NOT. ALL(x%cwc_sum .EqualTo. y%cwc_sum) ) THEN
        msg = 'Object cwc_sum data are different'
        CALL Display_Message(ROUTINE_NAME, msg, FAILURE)
        is_equal = .FALSE.
      END IF
    END IF

  END FUNCTION iVar_Compare
  
END MODULE CRTM_CloudCover_Define
