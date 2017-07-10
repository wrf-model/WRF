!
! CloudCoeff_Define
!
! Module defining the CloudCoeff data structure and containing routines to 
! manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!

MODULE CloudCoeff_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,            ONLY: Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: CloudCoeff_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: CloudCoeff_Associated
  PUBLIC :: CloudCoeff_Destroy
  PUBLIC :: CloudCoeff_Create
  PUBLIC :: CloudCoeff_Inspect
  PUBLIC :: CloudCoeff_ValidRelease
  PUBLIC :: CloudCoeff_Info
  PUBLIC :: CloudCoeff_DefineVersion
  

  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CloudCoeff_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CloudCoeff_Define.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'
  ! CloudCoeff init values
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: CLOUDCOEFF_RELEASE = 3  ! This determines structure and file formats.
  INTEGER, PARAMETER :: CLOUDCOEFF_VERSION = 1  ! This is just the data version for the release.
  ! Meggage string length
  INTEGER, PARAMETER :: ML = 256

  ! Number of stream angle definitions in the data set
  INTEGER, PARAMETER :: DEF_N_STREAM_SETS = 5
  INTEGER, PARAMETER :: DEF_N_STREAMS(DEF_N_STREAM_SETS)       = [2, 4, 6, 8, 16]
  ! ...This defines the offset in the "n_Legendre_Terms"
  ! ...dimension of the phase coefficient arrays for the
  ! ...various stream angle sets.
  INTEGER, PARAMETER :: DEF_LEGENDRE_OFFSET(DEF_N_STREAM_SETS) = [0, 0, 5, 12, 21]


  ! --------------------------------
  ! CloudCoeff data type definition, 
  !   MW:   Microwave
  !   IR:   Infrared
  !   Reff: Effective radius
  !   ke:   Extinction coefficient
  !   w:    Single scatter albedo
  !   g:    Asymmetry parameter
  !   L:    Liquid phase
  !   S:    Solid phase
  ! --------------------------------
  !:tdoc+:
  TYPE :: CloudCoeff_type
    ! Release and version information
    INTEGER(Long) :: Release = CLOUDCOEFF_RELEASE
    INTEGER(Long) :: Version = CLOUDCOEFF_VERSION
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Dataset parameter definitions (eventually stored in the datafile)
    INTEGER :: n_Stream_Sets                      = DEF_N_STREAM_SETS
    INTEGER :: n_Streams(DEF_N_STREAM_SETS)       = DEF_N_STREAMS
    INTEGER :: Legendre_Offset(DEF_N_STREAM_SETS) = DEF_LEGENDRE_OFFSET
    ! Array dimensions
    INTEGER(Long) :: n_MW_Frequencies   = 0   ! I1 dimension 
    INTEGER(Long) :: n_MW_Radii         = 0   ! I2 dimension
    INTEGER(Long) :: n_IR_Frequencies   = 0   ! I3 dimension
    INTEGER(Long) :: n_IR_Radii         = 0   ! I4 dimension
    INTEGER(Long) :: n_Temperatures     = 0   ! I5 dimension
    INTEGER(Long) :: n_Densities        = 0   ! I6 dimension
    INTEGER(Long) :: Max_Legendre_Terms = 0   ! I7 dimension
    INTEGER(Long) :: n_Legendre_Terms   = 0   
    INTEGER(Long) :: Max_Phase_Elements = 0   ! I8 dimension
    INTEGER(Long) :: n_Phase_Elements   = 0   
    ! LUT dimension vectors
    REAL(Double), ALLOCATABLE :: Frequency_MW(:)  ! I1
    REAL(Double), ALLOCATABLE :: Frequency_IR(:)  ! I3
    REAL(Double), ALLOCATABLE :: Reff_MW(:)       ! I2 
    REAL(Double), ALLOCATABLE :: Reff_IR(:)       ! I4
    REAL(Double), ALLOCATABLE :: Temperature(:)   ! I5
    REAL(Double), ALLOCATABLE :: Density(:)       ! I6
    ! Microwave data for liquid phase clouds
    REAL(Double), ALLOCATABLE :: ke_L_MW(:,:,:)          ! I1 x I2 x I5
    REAL(Double), ALLOCATABLE :: w_L_MW(:,:,:)           ! I1 x I2 x I5
    REAL(Double), ALLOCATABLE :: g_L_MW(:,:,:)           ! I1 x I2 x I5
    REAL(Double), ALLOCATABLE :: pcoeff_L_MW(:,:,:,:,:)  ! I1 x I2 x I5 x I7 x I8
    ! Microwave data for solid phase clouds
    REAL(Double), ALLOCATABLE :: ke_S_MW(:,:,:)          ! I1 x I2 x I6
    REAL(Double), ALLOCATABLE :: w_S_MW(:,:,:)           ! I1 x I2 x I6
    REAL(Double), ALLOCATABLE :: g_S_MW(:,:,:)           ! I1 x I2 x I6
    REAL(Double), ALLOCATABLE :: pcoeff_S_MW(:,:,:,:,:)  ! I1 x I2 x I6 x I7 x I8
    ! Infrared data. Note that the 0'th element in the I6 dimension
    ! of these data correspond to the liquid phase component. The
    ! remaining elements in this dimension are for the solid phase
    ! component
    REAL(Double), ALLOCATABLE :: ke_IR(:,:,:)        ! I3 x I4 x 0:I6
    REAL(Double), ALLOCATABLE :: w_IR(:,:,:)         ! I3 x I4 x 0:I6
    REAL(Double), ALLOCATABLE :: g_IR(:,:,:)         ! I3 x I4 x 0:I6
    REAL(Double), ALLOCATABLE :: pcoeff_IR(:,:,:,:)  ! I3 x I4 x 0:I6 x I7
  END TYPE CloudCoeff_type
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
!       CloudCoeff_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a CloudCoeff object.
!
! CALLING SEQUENCE:
!       Status = CloudCoeff_Associated( CloudCoeff )
!
! OBJECTS:
!       CloudCoeff:  CloudCoeff object which is to have its member's
!                    status tested.
!                    UNITS:      N/A
!                    TYPE:       TYPE(CloudCoeff_type)
!                    DIMENSION:  Scalar or any rank
!                    ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:      The return value is a logical value indicating the
!                    status of the CloudCoeff members.
!                    .TRUE.  - if ANY of the CloudCoeff allocatable or
!                              pointer members are in use.
!                    .FALSE. - if ALL of the CloudCoeff allocatable or
!                              pointer members are not in use.
!                    UNITS:      N/A
!                    TYPE:       LOGICAL
!                    DIMENSION:  Same as input CloudCoeff argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CloudCoeff_Associated( CloudCoeff ) RESULT( Status )
    TYPE(CloudCoeff_type), INTENT(IN) :: CloudCoeff
    LOGICAL :: Status
    Status = CloudCoeff%Is_Allocated
  END FUNCTION CloudCoeff_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CloudCoeff_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize CloudCoeff objects.
!
! CALLING SEQUENCE:
!       CALL CloudCoeff_Destroy( CloudCoeff )
!
! OBJECTS:
!       CloudCoeff:   Re-initialized CloudCoeff object.
!                     UNITS:      N/A
!                     TYPE:       TYPE(CloudCoeff_type)
!                     DIMENSION:  Scalar OR any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CloudCoeff_Destroy( CloudCoeff )
    TYPE(CloudCoeff_type), INTENT(OUT) :: CloudCoeff
    CloudCoeff%Is_Allocated = .FALSE.
    CloudCoeff%n_MW_Frequencies   = 0
    CloudCoeff%n_MW_Radii         = 0
    CloudCoeff%n_IR_Frequencies   = 0
    CloudCoeff%n_IR_Radii         = 0
    CloudCoeff%n_Temperatures     = 0
    CloudCoeff%n_Densities        = 0
    CloudCoeff%Max_Legendre_Terms = 0
    CloudCoeff%n_Legendre_Terms   = 0
    CloudCoeff%Max_Phase_Elements = 0
    CloudCoeff%n_Phase_Elements   = 0
  END SUBROUTINE CloudCoeff_Destroy
  

!--------------------------------------------------------------------------------
!
! NAME:
!       CloudCoeff_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of a CloudCoeff object.
!
! CALLING SEQUENCE:
!       CALL CloudCoeff_Create( CloudCoeff      , &
!                               n_MW_Frequencies, &
!                               n_MW_Radii      , &
!                               n_IR_Frequencies, &
!                               n_IR_Radii      , &
!                               n_Temperatures  , &
!                               n_Densities     , &
!                               n_Legendre_Terms, &
!                               n_Phase_Elements  )
!
! OBJECTS:
!       CloudCoeff:        CloudCoeff object.
!                          UNITS:      N/A
!                          TYPE:       TYPE(CloudCoeff_type)
!                          DIMENSION:  Scalar or any rank
!                          ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_MW_Frequencies:  The number of microwave frequencies in
!                          the look-up table (LUT) 
!                          The "I1" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_MW_Radii:        The number of discrete effective radii 
!                          for MW scatterers in the LUT.
!                          The "I2" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_IR_Frequencies:  The number of infrared frequencies in
!                          the LUT 
!                          The "I3" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_IR_Radii:        The number of discrete effective radii 
!                          for IR scatterers in the LUT.
!                          The "I4" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Temperatures:    The number of discrete layer temperatures
!                          in the LUT. 
!                          The "I5" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Densities:       The number of fixed densities for snow, graupel,
!                          and hail/ice in the LUT. 
!                          The "I6" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Legendre_Terms:  The maximum number of Legendre polynomial
!                          terms in the LUT.
!                          The "I7" dimension. Can be = 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Phase_Elements:  The maximum number of phase elements in the LUT.
!                          The "I8" dimension. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE CloudCoeff_Create( &
    CloudCoeff      , &
    n_MW_Frequencies, &
    n_MW_Radii      , &
    n_IR_Frequencies, &
    n_IR_Radii      , &
    n_Temperatures  , &
    n_Densities     , &
    n_Legendre_Terms, &
    n_Phase_Elements  )
    ! Arguments
    TYPE(CloudCoeff_type) , INTENT(OUT) :: CloudCoeff
    INTEGER,                INTENT(IN)  :: n_MW_Frequencies
    INTEGER,                INTENT(IN)  :: n_MW_Radii
    INTEGER,                INTENT(IN)  :: n_IR_Frequencies
    INTEGER,                INTENT(IN)  :: n_IR_Radii
    INTEGER,                INTENT(IN)  :: n_Temperatures
    INTEGER,                INTENT(IN)  :: n_Densities
    INTEGER,                INTENT(IN)  :: n_Legendre_Terms
    INTEGER,                INTENT(IN)  :: n_Phase_Elements
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CloudCoeff_Create'
    ! Local variables
    INTEGER :: alloc_stat(4)

    ! Check input
    IF ( n_MW_Frequencies < 1 .OR. &
         n_MW_Radii       < 1 .OR. &
         n_IR_Frequencies < 1 .OR. &
         n_IR_Radii       < 1 .OR. &
         n_Temperatures   < 1 .OR. &
         n_Densities      < 1 .OR. &
         n_Legendre_Terms < 0 .OR. &
         n_Phase_Elements < 1      ) RETURN


    ! Perform the allocation. The allocations were
    ! split across several calls for clarity only.
    ! ...Allocate the dimension vectors
    ALLOCATE( CloudCoeff%Frequency_MW(n_MW_Frequencies), &
              CloudCoeff%Frequency_IR(n_IR_Frequencies), &
              CloudCoeff%Reff_MW(n_MW_Radii), &
              CloudCoeff%Reff_IR(n_IR_Radii), &
              CloudCoeff%Temperature(n_Temperatures), &
              CloudCoeff%Density(n_Densities), &
              STAT = alloc_stat(1) )
    ! ...Allocate the microwave liquid phase arrays
    ALLOCATE( CloudCoeff%ke_L_MW(n_MW_Frequencies, n_MW_Radii, n_Temperatures), &
              CloudCoeff%w_L_MW(n_MW_Frequencies , n_MW_Radii, n_Temperatures), &
              CloudCoeff%g_L_MW(n_MW_Frequencies , n_MW_Radii, n_Temperatures), &
              CloudCoeff%pcoeff_L_MW(n_MW_Frequencies  , &
                                     n_MW_Radii        , &
                                     n_Temperatures    , &
                                     0:n_Legendre_Terms, &
                                     n_Phase_Elements    ), &
              STAT = alloc_stat(2) )
    ! ...Allocate the microwave solid phase arrays
    ALLOCATE( CloudCoeff%ke_S_MW(n_MW_Frequencies, n_MW_Radii, n_Densities), &
              CloudCoeff%w_S_MW(n_MW_Frequencies , n_MW_Radii, n_Densities), &
              CloudCoeff%g_S_MW(n_MW_Frequencies , n_MW_Radii, n_Densities), &
              CloudCoeff%pcoeff_S_MW(n_MW_Frequencies  , &
                                     n_MW_Radii        , &
                                     n_Densities       , &
                                     0:n_Legendre_Terms, &
                                     n_Phase_Elements    ), &
              STAT = alloc_stat(3) )
    ! ...Allocate the infrared arrays
    ALLOCATE( CloudCoeff%ke_IR(n_IR_Frequencies, n_IR_Radii, 0:n_Densities), &
              CloudCoeff%w_IR(n_IR_Frequencies , n_IR_Radii, 0:n_Densities), &
              CloudCoeff%g_IR(n_IR_Frequencies , n_IR_Radii, 0:n_Densities), &
              CloudCoeff%pcoeff_IR(n_IR_Frequencies  , &
                                   n_IR_Radii        , &
                                   0:n_Densities     , &
                                   0:n_Legendre_Terms  ), &
              STAT = alloc_stat(4) )
    IF ( ANY(alloc_stat /= 0) ) RETURN


    ! Initialise
    ! ...Dimensions
    CloudCoeff%n_MW_Frequencies   = n_MW_Frequencies
    CloudCoeff%n_MW_Radii         = n_MW_Radii
    CloudCoeff%n_IR_Frequencies   = n_IR_Frequencies
    CloudCoeff%n_IR_Radii         = n_IR_Radii
    CloudCoeff%n_Temperatures     = n_Temperatures
    CloudCoeff%n_Densities        = n_Densities
    CloudCoeff%Max_Legendre_Terms = n_Legendre_Terms
    CloudCoeff%n_Legendre_Terms   = n_Legendre_Terms 
    CloudCoeff%Max_Phase_Elements = n_Phase_Elements
    CloudCoeff%n_Phase_Elements   = n_Phase_Elements 
    ! ...Arrays
    CloudCoeff%Frequency_MW = ZERO
    CloudCoeff%Frequency_IR = ZERO
    CloudCoeff%Reff_MW      = ZERO
    CloudCoeff%Reff_IR      = ZERO
    CloudCoeff%Temperature  = ZERO
    CloudCoeff%Density      = ZERO
    
    CloudCoeff%ke_L_MW      = ZERO
    CloudCoeff%w_L_MW       = ZERO
    CloudCoeff%g_L_MW       = ZERO
    CloudCoeff%pcoeff_L_MW  = ZERO
    
    CloudCoeff%ke_S_MW      = ZERO
    CloudCoeff%w_S_MW       = ZERO
    CloudCoeff%g_S_MW       = ZERO
    CloudCoeff%pcoeff_S_MW  = ZERO
    
    CloudCoeff%ke_IR        = ZERO
    CloudCoeff%w_IR         = ZERO
    CloudCoeff%g_IR         = ZERO
    CloudCoeff%pcoeff_IR    = ZERO


    ! Set allocationindicator
    CloudCoeff%Is_Allocated = .TRUE.

  END SUBROUTINE CloudCoeff_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CloudCoeff_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a CloudCoeff object to stdout.
!
! CALLING SEQUENCE:
!       CALL CloudCoeff_Inspect( CloudCoeff )
!
! INPUTS:
!       CloudCoeff:    CloudCoeff object to display.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CloudCoeff_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CloudCoeff_Inspect( CloudCoeff, Pause )
    ! Arguments
    TYPE(CloudCoeff_type), INTENT(IN) :: CloudCoeff
    LOGICAL,     OPTIONAL, INTENT(IN) :: Pause
    ! Variables
    INTEGER :: i, j, k, l, m
    INTEGER :: kidx
    LOGICAL :: wait
    
    wait = .FALSE.
    IF ( PRESENT(Pause) ) wait = Pause
    
    WRITE(*,'(1x,"CloudCoeff OBJECT")')
    ! Dimensions
    WRITE(*,'(3x,"n_MW_Frequencies :",1x,i0)') CloudCoeff%n_MW_Frequencies
    WRITE(*,'(3x,"n_MW_Radii       :",1x,i0)') CloudCoeff%n_MW_Radii
    WRITE(*,'(3x,"n_IR_Frequencies :",1x,i0)') CloudCoeff%n_IR_Frequencies
    WRITE(*,'(3x,"n_IR_Radii       :",1x,i0)') CloudCoeff%n_IR_Radii
    WRITE(*,'(3x,"n_Temperatures   :",1x,i0)') CloudCoeff%n_Temperatures
    WRITE(*,'(3x,"n_Densities      :",1x,i0)') CloudCoeff%n_Densities
    WRITE(*,'(3x,"n_Legendre_Terms :",1x,i0)') CloudCoeff%n_Legendre_Terms
    WRITE(*,'(3x,"n_Phase_Elements :",1x,i0)') CloudCoeff%n_Phase_Elements
    IF ( .NOT. CloudCoeff_Associated(CloudCoeff) ) RETURN
    ! Dimensional vectors
    WRITE(*,'(/3x,"Dimensional vectors...")')
    WRITE(*,'(5x,"CloudCoeff Frequency_MW:")')
    WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%Frequency_MW
    WRITE(*,'(5x,"CloudCoeff Frequency_IR:")') 
    WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%Frequency_IR
    WRITE(*,'(5x,"CloudCoeff Reff_MW     :")') 
    WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%Reff_MW     
    WRITE(*,'(5x,"CloudCoeff Reff_IR     :")') 
    WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%Reff_IR     
    WRITE(*,'(5x,"CloudCoeff Temperature :")') 
    WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%Temperature 
    WRITE(*,'(5x,"CloudCoeff Density     :")') 
    WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%Density     

    ! Microwave data
    WRITE(*,'(/3x,"Microwave data...")')
    
    ! ...Liquid phase data
    IF ( wait ) THEN
      WRITE(*,'(/5x,"Press <ENTER> to view the microwave liquid phase mass extinction coefficients")')
      READ(*,*) 
    END IF
    DO j = 1, CloudCoeff%n_Temperatures
      WRITE(*,'(5x,"Microwave liquid phase mass extinction coefficients:")') 
      WRITE(*,'(7x,"Temperature     : ",es13.6)') CloudCoeff%Temperature(j)
      DO i = 1, CloudCoeff%n_MW_Radii
        WRITE(*,'(7x,"Effective radius: ",es13.6)') CloudCoeff%Reff_MW(i)
        WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%ke_L_MW(:,i,j)     
      END DO
    END DO
    
    IF ( wait ) THEN
      WRITE(*,'(/5x,"Press <ENTER> to view the microwave liquid phase single scatter albedo")')
      READ(*,*) 
    END IF
    DO j = 1, CloudCoeff%n_Temperatures
      WRITE(*,'(5x,"Microwave liquid phase single scatter albedo:")') 
      WRITE(*,'(7x,"Temperature     : ",es13.6)') CloudCoeff%Temperature(j)
      DO i = 1, CloudCoeff%n_MW_Radii
        WRITE(*,'(7x,"Effective radius: ",es13.6)') CloudCoeff%Reff_MW(i)
        WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%w_L_MW(:,i,j)     
      END DO
    END DO

    IF ( wait ) THEN
      WRITE(*,'(/5x,"Press <ENTER> to view the microwave liquid phase asymmetry parameter")')
      READ(*,*) 
    END IF
    DO j = 1, CloudCoeff%n_Temperatures
      WRITE(*,'(5x,"Microwave liquid phase asymmetry parameter:")') 
      WRITE(*,'(7x,"Temperature     : ",es13.6)') CloudCoeff%Temperature(j)
      DO i = 1, CloudCoeff%n_MW_Radii
        WRITE(*,'(7x,"Effective radius: ",es13.6)') CloudCoeff%Reff_MW(i)
        WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%g_L_MW(:,i,j)     
      END DO
    END DO

    DO m = 1, CloudCoeff%n_Stream_Sets
      IF ( wait ) THEN
        WRITE(*,'(/5x,"Press <ENTER> to view the ",i0,"-stream microwave liquid ",&
                     &"phase phase coefficients")') CloudCoeff%n_Streams(m)
        READ(*,*) 
      END IF
      WRITE(*,'(5x,i0,"-stream microwave liquid phase phase coefficients:")') CloudCoeff%n_Streams(m)
      DO l = 1, CloudCoeff%n_Phase_Elements
        WRITE(*,'(7x,"Phase element: ",i0)') l
        DO k = 1, CloudCoeff%n_Streams(m)
          WRITE(*,'(7x,"Legendre term: ",i0)') k
          kidx = k + CloudCoeff%Legendre_Offset(m)
          DO j = 1, CloudCoeff%n_Temperatures
            WRITE(*,'(7x,"Temperature     : ",es13.6)') CloudCoeff%Temperature(j)
            DO i = 1, CloudCoeff%n_MW_Radii
              WRITE(*,'(7x,"Effective radius: ",es13.6)') CloudCoeff%Reff_MW(i)
              WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%pcoeff_L_MW(:,i,j,kidx,l)
            END DO
          END DO
        END DO
      END DO
    END DO

    ! ...Solid phase data
    IF ( wait ) THEN
      WRITE(*,'(/5x,"Press <ENTER> to view the microwave solid phase mass extinction coefficients")')
      READ(*,*) 
    END IF
    DO j = 1, CloudCoeff%n_Densities
      WRITE(*,'(5x,"Microwave solid phase mass extinction coefficients:")') 
      WRITE(*,'(7x,"Density         : ",es13.6)') CloudCoeff%Density(j)
      DO i = 1, CloudCoeff%n_MW_Radii
        WRITE(*,'(7x,"Effective radius: ",es13.6)') CloudCoeff%Reff_MW(i)
        WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%ke_S_MW(:,i,j)     
      END DO
    END DO
    
    IF ( wait ) THEN
      WRITE(*,'(/5x,"Press <ENTER> to view the microwave solid phase single scatter albedo")')
      READ(*,*) 
    END IF
    DO j = 1, CloudCoeff%n_Densities
      WRITE(*,'(5x,"Microwave solid phase single scatter albedo:")') 
      WRITE(*,'(7x,"Density         : ",es13.6)') CloudCoeff%Density(j)
      DO i = 1, CloudCoeff%n_MW_Radii
        WRITE(*,'(7x,"Effective radius: ",es13.6)') CloudCoeff%Reff_MW(i)
        WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%w_S_MW(:,i,j)     
      END DO
    END DO
    
    IF ( wait ) THEN
      WRITE(*,'(/5x,"Press <ENTER> to view the microwave solid phase asymmetry parameter")')
      READ(*,*) 
    END IF
    DO j = 1, CloudCoeff%n_Densities
      WRITE(*,'(5x,"Microwave solid phase asymmetry parameter:")') 
      WRITE(*,'(7x,"Density         : ",es13.6)') CloudCoeff%Density(j)
      DO i = 1, CloudCoeff%n_MW_Radii
        WRITE(*,'(7x,"Effective radius: ",es13.6)') CloudCoeff%Reff_MW(i)
        WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%g_S_MW(:,i,j)     
      END DO
    END DO
    
    DO m = 1, CloudCoeff%n_Stream_Sets
      IF ( wait ) THEN
        WRITE(*,'(/5x,"Press <ENTER> to view the ",i0,"-stream microwave solid ",&
                     &"phase phase coefficients")') CloudCoeff%n_Streams(m)
        READ(*,*) 
      END IF
      WRITE(*,'(5x,i0,"-stream microwave solid phase phase coefficients:")') CloudCoeff%n_Streams(m)
      DO l = 1, CloudCoeff%n_Phase_Elements
        WRITE(*,'(7x,"Phase element: ",i0)') l
        DO k = 1, CloudCoeff%n_Streams(m)
          WRITE(*,'(7x,"Legendre term: ",i0)') k
          kidx = k + CloudCoeff%Legendre_Offset(m)
          DO j = 1, CloudCoeff%n_Densities
            WRITE(*,'(7x,"Density         : ",es13.6)') CloudCoeff%Density(j)
            DO i = 1, CloudCoeff%n_MW_Radii
              WRITE(*,'(7x,"Effective radius: ",es13.6)') CloudCoeff%Reff_MW(i)
              WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%pcoeff_S_MW(:,i,j,kidx,l)
            END DO
          END DO
        END DO
      END DO
    END DO


    ! Infrared data
    WRITE(*,'(/3x,"Infrared data...")')
    
    ! ...Liquid phase data
    IF ( wait ) THEN
      WRITE(*,'(/5x,"Press <ENTER> to view the infrared liquid phase mass extinction coefficients")')
      READ(*,*) 
    END IF
    WRITE(*,'(5x,"Infrared liquid phase mass extinction coefficients:")') 
    DO i = 1, CloudCoeff%n_IR_Radii
      WRITE(*,'(7x,"Effective radius: ",es13.6)') CloudCoeff%Reff_IR(i)
      WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%ke_IR(:,i,0)     
    END DO
    
    IF ( wait ) THEN
      WRITE(*,'(/5x,"Press <ENTER> to view the infrared liquid phase single scatter albedo")')
      READ(*,*) 
    END IF
    WRITE(*,'(5x,"Infrared liquid phase single scatter albedo:")') 
    DO i = 1, CloudCoeff%n_IR_Radii
      WRITE(*,'(7x,"Effective radius: ",es13.6)') CloudCoeff%Reff_IR(i)
      WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%w_IR(:,i,0)     
    END DO

    IF ( wait ) THEN
      WRITE(*,'(/5x,"Press <ENTER> to view the infrared liquid phase asymmetry parameter")')
      READ(*,*) 
    END IF
    WRITE(*,'(5x,"Infrared liquid phase asymmetry parameter:")') 
    DO i = 1, CloudCoeff%n_IR_Radii
      WRITE(*,'(7x,"Effective radius: ",es13.6)') CloudCoeff%Reff_IR(i)
      WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%g_IR(:,i,0)     
    END DO

    DO m = 1, CloudCoeff%n_Stream_Sets
      IF ( wait ) THEN
        WRITE(*,'(/5x,"Press <ENTER> to view the ",i0,"-stream infrared liquid ",&
                     &"phase phase coefficients")') CloudCoeff%n_Streams(m)
        READ(*,*) 
      END IF
      WRITE(*,'(5x,i0,"-stream infrared liquid phase phase coefficients:")') CloudCoeff%n_Streams(m)
      DO k = 1, CloudCoeff%n_Streams(m)
        WRITE(*,'(7x,"Legendre term: ",i0)') k
        kidx = k + CloudCoeff%Legendre_Offset(m)
        DO i = 1, CloudCoeff%n_IR_Radii
          WRITE(*,'(7x,"Effective radius: ",es13.6)') CloudCoeff%Reff_IR(i)
          WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%pcoeff_IR(:,i,0,kidx)
        END DO
      END DO
    END DO

    ! ...Solid phase data
    IF ( wait ) THEN
      WRITE(*,'(/5x,"Press <ENTER> to view the infrared solid phase mass extinction coefficients")')
      READ(*,*) 
    END IF
    DO j = 1, CloudCoeff%n_Densities
      WRITE(*,'(5x,"Infrared solid phase mass extinction coefficients:")') 
      WRITE(*,'(7x,"Density         : ",es13.6)') CloudCoeff%Density(j)
      DO i = 1, CloudCoeff%n_IR_Radii
        WRITE(*,'(7x,"Effective radius: ",es13.6)') CloudCoeff%Reff_IR(i)
        WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%ke_IR(:,i,j)     
      END DO
    END DO
    
    IF ( wait ) THEN
      WRITE(*,'(/5x,"Press <ENTER> to view the infrared solid phase single scatter albedo")')
      READ(*,*) 
    END IF
    DO j = 1, CloudCoeff%n_Densities
      WRITE(*,'(5x,"Infrared solid phase single scatter albedo:")') 
      WRITE(*,'(7x,"Density         : ",es13.6)') CloudCoeff%Density(j)
      DO i = 1, CloudCoeff%n_IR_Radii
        WRITE(*,'(7x,"Effective radius: ",es13.6)') CloudCoeff%Reff_IR(i)
        WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%w_IR(:,i,j)     
      END DO
    END DO
    
    IF ( wait ) THEN
      WRITE(*,'(/5x,"Press <ENTER> to view the infrared solid phase asymmetry parameter")')
      READ(*,*) 
    END IF
    DO j = 1, CloudCoeff%n_Densities
      WRITE(*,'(5x,"Infrared solid phase asymmetry parameter:")') 
      WRITE(*,'(7x,"Density         : ",es13.6)') CloudCoeff%Density(j)
      DO i = 1, CloudCoeff%n_IR_Radii
        WRITE(*,'(7x,"Effective radius: ",es13.6)') CloudCoeff%Reff_IR(i)
        WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%g_IR(:,i,j)     
      END DO
    END DO
    
    DO m = 1, CloudCoeff%n_Stream_Sets
      IF ( wait ) THEN
        WRITE(*,'(/5x,"Press <ENTER> to view the ",i0,"-stream infrared solid ",&
                     &"phase phase coefficients")') CloudCoeff%n_Streams(m)
        READ(*,*) 
      END IF
      WRITE(*,'(5x,i0,"-stream infrared solid phase phase coefficients:")') CloudCoeff%n_Streams(m)
      DO k = 1, CloudCoeff%n_Streams(m)
        WRITE(*,'(7x,"Legendre term: ",i0)') k
        kidx = k + CloudCoeff%Legendre_Offset(m)
        DO j = 1, CloudCoeff%n_Densities
          WRITE(*,'(7x,"Density         : ",es13.6)') CloudCoeff%Density(j)
          DO i = 1, CloudCoeff%n_IR_Radii
            WRITE(*,'(7x,"Effective radius: ",es13.6)') CloudCoeff%Reff_IR(i)
            WRITE(*,'(5(1x,es13.6,:))') CloudCoeff%pcoeff_IR(:,i,j,kidx)
          END DO
        END DO
      END DO
    END DO

  END SUBROUTINE CloudCoeff_Inspect


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CloudCoeff_ValidRelease
!
! PURPOSE:
!       Function to check the CloudCoeff Release value.
!
! CALLING SEQUENCE:
!       IsValid = CloudCoeff_ValidRelease( CloudCoeff )
!
! INPUTS:
!       CloudCoeff:    CloudCoeff object for which the Release component
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CloudCoeff_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       IsValid:       Logical value defining the release validity.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION CloudCoeff_ValidRelease( CloudCoeff ) RESULT( IsValid )
    ! Arguments
    TYPE(CloudCoeff_type), INTENT(IN) :: CloudCoeff
    ! Function result
    LOGICAL :: IsValid
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CloudCoeff_ValidRelease'
    ! Local variables
    CHARACTER(ML) :: msg

    ! Set up
    IsValid = .TRUE.


    ! Check release is not too old
    IF ( CloudCoeff%Release < CloudCoeff_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A CloudCoeff data update is needed. ", &
                  &"CloudCoeff release is ",i0, &
                  &". Valid release is ",i0,"." )' ) &
                  CloudCoeff%Release, CLOUDCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF


    ! Check release is not too new
    IF ( CloudCoeff%Release > CloudCoeff_RELEASE ) THEN
      IsValid = .FALSE.
      WRITE( msg,'("A CloudCoeff software update is needed. ", &
                  &"CloudCoeff release is ",i0, &
                  &". Valid release is ",i0,"." )' ) &
                  CloudCoeff%Release, CLOUDCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF

  END FUNCTION CloudCoeff_ValidRelease


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CloudCoeff_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about a CloudCoeff object.
!
! CALLING SEQUENCE:
!       CALL CloudCoeff_Info( CloudCoeff, Info )
!
! INPUTS:
!       CloudCoeff:    CloudCoeff object about which info is required.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CloudCoeff_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the passed CloudCoeff object.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CloudCoeff_Info( CloudCoeff, Info )
    ! Arguments
    TYPE(CloudCoeff_type), INTENT(IN)  :: CloudCoeff
    CHARACTER(*),       INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: Long_String

    ! Write the required data to the local string
    WRITE( Long_String, &
           '( a,1x,"CloudCoeff RELEASE.VERSION: ", i2, ".", i2.2, 2x, &
           &"N_FREQUENCIES(MW)=",i4,2x,&
           &"N_FREQUENCIES(IR)=",i4,2x,&
           &"N_RADII(MW)=",i2,2x,&
           &"N_RADII(IR)=",i2,2x,&
           &"N_TEMPERATURES=",i2,2x,&
           &"N_DENSITIES=",i2,2x,&
           &"N_LEGENDRE_TERMS=",i2,2x,&
           &"N_PHASE_ELEMENTS=",i2 )' ) &
           ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
           CloudCoeff%Release, CloudCoeff%Version, &
           CloudCoeff%n_MW_Frequencies, &
           CloudCoeff%n_IR_Frequencies, &
           CloudCoeff%n_MW_Radii      , &
           CloudCoeff%n_IR_Radii      , &
           CloudCoeff%n_Temperatures  , &
           CloudCoeff%n_Densities     , &
           CloudCoeff%n_Legendre_Terms, &
           CloudCoeff%n_Phase_Elements
                       
    ! Trim the output based on the
    ! dummy argument string length
    Info = Long_String(1:MIN(LEN(Info), LEN_TRIM(Long_String)))

  END SUBROUTINE CloudCoeff_Info
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CloudCoeff_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CloudCoeff_DefineVersion( Id )
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

  SUBROUTINE CloudCoeff_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CloudCoeff_DefineVersion




!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       CloudCoeff_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two CloudCoeff objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = CloudCoeff_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two CloudCoeff objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       TYPE(CloudCoeff_type)
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION CloudCoeff_Equal( x, y ) RESULT( is_equal )
    TYPE(CloudCoeff_type), INTENT(IN)  :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
    
    ! Check the object association status
    IF ( (.NOT. CloudCoeff_Associated(x)) .OR. &
         (.NOT. CloudCoeff_Associated(y))      ) RETURN

    ! Check contents
    ! ...Dimensions
    IF ( (x%n_MW_Frequencies /= y%n_MW_Frequencies) .OR. &
         (x%n_IR_Frequencies /= y%n_IR_Frequencies) .OR. &
         (x%n_MW_Radii       /= y%n_MW_Radii      ) .OR. &
         (x%n_IR_Radii       /= y%n_IR_Radii      ) .OR. &
         (x%n_Temperatures   /= y%n_Temperatures  ) .OR. &
         (x%n_Densities      /= y%n_Densities     ) .OR. &
         (x%n_Legendre_Terms /= y%n_Legendre_Terms) .OR. &
         (x%n_Phase_Elements /= y%n_Phase_Elements) ) RETURN
    ! ...Data
    IF ( ALL(x%Frequency_MW .EqualTo. y%Frequency_MW ) .AND. &
         ALL(x%Frequency_IR .EqualTo. y%Frequency_IR ) .AND. &
         ALL(x%Reff_MW      .EqualTo. y%Reff_MW      ) .AND. &
         ALL(x%Reff_IR      .EqualTo. y%Reff_IR      ) .AND. &
         ALL(x%Temperature  .EqualTo. y%Temperature  ) .AND. &
         ALL(x%Density      .EqualTo. y%Density      ) .AND. &
         ALL(x%ke_L_MW      .EqualTo. y%ke_L_MW      ) .AND. &
         ALL(x%w_L_MW       .EqualTo. y%w_L_MW       ) .AND. &
         ALL(x%g_L_MW       .EqualTo. y%g_L_MW       ) .AND. &
         ALL(x%pcoeff_L_MW  .EqualTo. y%pcoeff_L_MW  ) .AND. &
         ALL(x%ke_S_MW      .EqualTo. y%ke_S_MW      ) .AND. &
         ALL(x%w_S_MW       .EqualTo. y%w_S_MW       ) .AND. &
         ALL(x%g_S_MW       .EqualTo. y%g_S_MW       ) .AND. &
         ALL(x%pcoeff_S_MW  .EqualTo. y%pcoeff_S_MW  ) .AND. &
         ALL(x%ke_IR        .EqualTo. y%ke_IR        ) .AND. &
         ALL(x%w_IR         .EqualTo. y%w_IR         ) .AND. &
         ALL(x%g_IR         .EqualTo. y%g_IR         ) .AND. &
         ALL(x%pcoeff_IR    .EqualTo. y%pcoeff_IR    )       ) &
      is_equal = .TRUE.

  END FUNCTION CloudCoeff_Equal
  
END MODULE CloudCoeff_Define
