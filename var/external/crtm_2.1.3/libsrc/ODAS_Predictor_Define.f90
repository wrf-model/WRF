!
! ODAS_Predictor_Define
!
! Module defining the Predictor object for the Optical Depth in
! Absorber Space (ODAS) algorithm and containing routines to
! manipulate it.
!
! CREATION HISTORY:
!       Written by:    Paul van Delst, 22-Dec-2006
!                      paul.vandelst@noaa.gov
!
!       Modifed by:    Yong Han, 25-June-2008
!                      yong.han@noaa.gov
!

MODULE ODAS_Predictor_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds, ONLY: fp
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: ODAS_Predictor_type
!  ! Operators
!  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: ODAS_Predictor_Associated
  PUBLIC :: ODAS_Predictor_Destroy
  PUBLIC :: ODAS_Predictor_Create
  PUBLIC :: ODAS_Predictor_Inspect
  PUBLIC :: ODAS_Predictor_Zero
!  PUBLIC :: ODAS_Predictor_ValidRelease
!  PUBLIC :: ODAS_Predictor_Info
!  PUBLIC :: ODAS_Predictor_DefineVersion
!  PUBLIC :: ODAS_Predictor_InquireFile
!  PUBLIC :: ODAS_Predictor_ReadFile
!  PUBLIC :: ODAS_Predictor_WriteFile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
!  INTERFACE OPERATOR(==)
!    MODULE PROCEDURE ODAS_Predictor_Equal
!  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: ODAS_Predictor_Define.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Release and version
  INTEGER, PARAMETER :: ODAS_PREDICTOR_RELEASE = 3  ! This determines structure and file formats.
  INTEGER, PARAMETER :: ODAS_PREDICTOR_VERSION = 1  ! This is just the default data version.
  ! Close status for write errors
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! -----------------------
  ! Derived type definition
  ! -----------------------
  !:tdoc+:
  TYPE :: ODAS_Predictor_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Release and version information
    INTEGER :: Release = ODAS_PREDICTOR_RELEASE
    INTEGER :: Version = ODAS_PREDICTOR_VERSION
    ! Dimension variables
    INTEGER :: n_Layers     = 0  ! K
    INTEGER :: n_Predictors = 0  ! I
    INTEGER :: n_Absorbers  = 0  ! J
    INTEGER :: n_Orders     = 0  ! IO
    ! Scalars
    REAL(fp) :: Secant_Sensor_Zenith = ZERO
    ! Arrays
    REAL(fp), ALLOCATABLE :: A(:,:)     ! 0:K x J, Integrated absorber
    REAL(fp), ALLOCATABLE :: dA(:,:)    !   K x J, Integrated absorber level difference
    REAL(fp), ALLOCATABLE :: aveA(:,:)  !   K x J, Integrated absorber layer average
    REAL(fp), ALLOCATABLE :: Ap(:,:,:)  !   K x IO x J, Power of absorber level
    REAL(fp), ALLOCATABLE :: X(:,:)     !   K x I, Predictors
  END TYPE ODAS_Predictor_type
  !:tdoc-:


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ODAS_Predictor_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the ODAS_Predictor structure.
!
! CALLING SEQUENCE:
!       Status = ODAS_Predictor_Associated( ODAS_Predictor )
!
! OBJECTS:
!       ODAS_Predictor:
!         Structure which is to have its member's
!         status tested.
!         UNITS:      N/A
!         TYPE:       ODAS_Predictor_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:
!         The return value is a logical value indicating the
!         status of the allocated members.
!          .TRUE.  - if the ODAS_Predictor object has been allocated.
!          .FALSE. - if the ODAS_Predictor object has NOT been allocated.
!         UNITS:      N/A
!         TYPE:       LOGICAL
!         DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION ODAS_Predictor_Associated( self ) RESULT( Status )
    TYPE(ODAS_Predictor_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION ODAS_Predictor_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ODAS_Predictor_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize ODAS_Predictor objects.
!
! CALLING SEQUENCE:
!       CALL ODAS_Predictor_Destroy( ODAS_Predictor )
!
! OBJECTS:
!       ODAS_Predictor:
!         Re-initialized ODAS_Predictor structure.
!         UNITS:      N/A
!         TYPE:       ODAS_Predictor_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE ODAS_Predictor_Destroy( self )
    TYPE(ODAS_Predictor_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE ODAS_Predictor_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ODAS_Predictor_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of an ODAS_Predictor object.
!
! CALLING SEQUENCE:
!       CALL ODAS_Predictor_Create( &
!              ODAS_Predictor, &
!              n_Layers      , &
!              n_Predictors  , &
!              n_Absorbers   , &
!              n_Orders        )
!
! OBJECTS:
!       ODAS_Predictor:
!         ODAS_Predictor object structure.
!         UNITS:      N/A
!         TYPE:       ODAS_Predictor_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Layers:
!         Number of atmospheric layers.
!         Must be > 0.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Conformable with the ODAS_Predictor object
!         ATTRIBUTES: INTENT(IN)
!
!       n_Predictors:
!         Number of absorption predictors.
!         Must be > 0.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Conformable with the ODAS_Predictor object
!         ATTRIBUTES: INTENT(IN)
!
!       n_Absorbers:
!         Number of atmospheric absorbers.
!         Must be > 0.
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Conformable with the ODAS_Predictor object
!         ATTRIBUTES: INTENT(IN)
!
!       n_Orders:
!         The polynormial function order for all absorbers
!         Must be > 0
!         UNITS:      N/A
!         TYPE:       INTEGER
!         DIMENSION:  Conformable with the ODAS_Predictor object
!         ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE ODAS_Predictor_Create( &
    self         , &  ! Output
    n_Layers     , &  ! Input
    n_Predictors , &  ! Input
    n_Absorbers  , &  ! Input
    n_Orders       )  ! Input
    ! Arguments
    TYPE(ODAS_Predictor_type), INTENT(OUT) :: self
    INTEGER,                   INTENT(IN)  :: n_Layers
    INTEGER,                   INTENT(IN)  :: n_Predictors
    INTEGER,                   INTENT(IN)  :: n_Absorbers
    INTEGER,                   INTENT(IN)  :: n_Orders
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Layers     < 1 .OR. &
         n_Predictors < 1 .OR. &
         n_Absorbers  < 1 .OR. &
         n_Orders     < 1 ) RETURN


    ! Perform the allocation
    ALLOCATE( self%A(0:n_Layers,n_Absorbers), &
              self%dA(n_Layers,n_Absorbers), &
              self%aveA(n_Layers,n_Absorbers), &
              self%Ap(n_Layers,n_Orders,n_Absorbers), &
              self%X(n_Layers,n_Predictors), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise dimensions
    self%n_Layers     = n_Layers
    self%n_Predictors = n_Predictors
    self%n_Absorbers  = n_Absorbers
    self%n_Orders     = n_Orders


    ! Set allocation indicator
    self%Is_Allocated = .TRUE.


    ! Initialise array data
    CALL ODAS_Predictor_Zero(self)

  END SUBROUTINE ODAS_Predictor_Create



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ODAS_Predictor_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a ODAS_Predictor object to stdout.
!
! CALLING SEQUENCE:
!       CALL ODAS_Predictor_Inspect( ODAS_Predictor )
!
! OBJECTS:
!       ODAS_Predictor:
!         ODAS_Predictor object to display.
!         UNITS:      N/A
!         TYPE:       ODAS_Predictor_type
!         DIMENSION:  Scalar
!         ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE ODAS_Predictor_Inspect(self)
    TYPE(ODAS_Predictor_type), INTENT(IN) :: self
    CHARACTER(*), PARAMETER :: RFMT='es13.6'
    INTEGER :: i, j
    WRITE(*,'(1x,"ODAS_Predictor OBJECT")')
    ! Release/version info
    WRITE(*,'(3x,"Release.Version :",1x,i0,".",i0)') self%Release, self%Version
    ! Dimensions
    WRITE(*,'(3x,"n_Layers     :",1x,i0)') self%n_Layers
    WRITE(*,'(3x,"n_Predictors :",1x,i0)') self%n_Predictors
    WRITE(*,'(3x,"n_Absorbers  :",1x,i0)') self%n_Absorbers
    WRITE(*,'(3x,"n_Orders     :",1x,i0)') self%n_Orders
    ! ODAS scalar data
    WRITE(*,'(3x,"ODAS scalar data :")')
    WRITE(*,'(5x,"Secant_Sensor_Zenith :",'//RFMT//')') self%Secant_Sensor_Zenith
    ! ODAS array data
    IF ( .NOT. ODAS_Predictor_Associated(self) ) RETURN
    WRITE(*,'(3x,"ODAS data arrays :")')
    WRITE(*,'(5x,"Integrated Absorber :")')
    DO j = 1, self%n_Absorbers
      WRITE(*,'(7x,"Absorber#: ",i0)') j
      WRITE(*,'(5(1x,'//RFMT//',:))') self%A(:,j)
    END DO
    WRITE(*,'(5x,"Integrated Absorber Level Difference:")')
    DO j = 1, self%n_Absorbers
      WRITE(*,'(7x,"Absorber#: ",i0)') j
      WRITE(*,'(5(1x,'//RFMT//',:))') self%dA(:,j)
    END DO
    WRITE(*,'(5x,"Integrated Absorber Layer Average:")')
    DO j = 1, self%n_Absorbers
      WRITE(*,'(7x,"Absorber#: ",i0)') j
      WRITE(*,'(5(1x,'//RFMT//',:))') self%aveA(:,j)
    END DO
    WRITE(*,'(5x,"Exponent Power of Absorber Level :")')
    DO j = 1, self%n_Absorbers
      DO i = 1, self%n_Orders
        WRITE(*,'(7x,"Absorber#: ",i0,"; Order#: ",i0)') j, i
        WRITE(*,'(5(1x,'//RFMT//',:))') self%Ap(:,i,j)
      END DO
      WRITE(*,*)
    END DO
    DO i = 1, self%n_Predictors
      WRITE(*,'(7x,"Predictor#: ",i0)') i
      WRITE(*,'(5(1x,'//RFMT//',:))') self%X(:,i)
    END DO
  END SUBROUTINE ODAS_Predictor_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ODAS_Predictor_Zero
!
! PURPOSE:
!       Elemental subroutine to zero-out an instance of an ODAS predictor object.
!
! CALLING SEQUENCE:
!       CALL ODAS_Predictor_Zero( ODAS_Predictor )
!
! OUTPUTS:
!       ODAS_Predictor:
!         ODAS_Predictor object structure.
!         UNITS:      N/A
!         TYPE:       ODAS_Predictor_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE ODAS_Predictor_Zero( self )
    TYPE(ODAS_Predictor_type), INTENT(IN OUT) :: self
    IF ( .NOT. ODAS_Predictor_Associated(self) ) RETURN
    self%Secant_Sensor_Zenith = ZERO
    self%A                    = ZERO
    self%dA                   = ZERO
    self%aveA                 = ZERO
    self%Ap                   = ZERO
    self%X                    = ZERO
  END SUBROUTINE ODAS_Predictor_Zero

END MODULE ODAS_Predictor_Define
