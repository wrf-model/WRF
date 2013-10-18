!
! Subset_Define
!
! Module containing the subset type definition and routines
! to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 26-May-2011
!                       paul.vandelst@noaa.gov
!

MODULE Subset_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler, ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Sort_Utility   , ONLY: InsertionSort
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: Subset_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Procedures
  PUBLIC :: Subset_Associated
  PUBLIC :: Subset_Destroy
  PUBLIC :: Subset_Create
  PUBLIC :: Subset_Inspect
  PUBLIC :: Subset_DefineVersion
  PUBLIC :: Subset_SetValue
  PUBLIC :: Subset_GetValue
  PUBLIC :: Subset_Generate


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE Subset_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module Parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: Subset_Define.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'


  ! -----------------------------------
  ! Channel subset data type definition
  ! -----------------------------------
  TYPE :: Subset_type
    PRIVATE
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Dimensions
    INTEGER :: n_Values = 0
    ! Subset inforamtion
    INTEGER, ALLOCATABLE :: Number(:)
    INTEGER, ALLOCATABLE :: Index(:) 
  END TYPE Subset_type


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
!       Subset_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the Subset structure.
!
! CALLING SEQUENCE:
!       Status = Subset_Associated( Subset )
!
! OBJECTS:
!       Subset:     Structure which is to have its member's
!                   status tested.
!                   UNITS:      N/A
!                   TYPE:       Subset_type
!                   DIMENSION:  Scalar or any rank
!                   ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:     The return value is a logical value indicating the
!                   status of the Subset members.
!                    .TRUE.  - if ANY of the Subset allocatable members
!                              are in use.
!                    .FALSE. - if ALL of the Subset allocatable members
!                              are not in use.
!                   UNITS:      N/A
!                   TYPE:       LOGICAL
!                   DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Subset_Associated( Subset ) RESULT( Status )
    TYPE(Subset_type), INTENT(IN) :: Subset
    LOGICAL :: Status
    Status = Subset%Is_Allocated
  END FUNCTION Subset_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Subset_Destroy
! 
! PURPOSE:
!       Elemental subroutine to re-initialize Subset objects.
!
! CALLING SEQUENCE:
!       CALL Subset_Destroy( Subset )
!
! OBJECTS:
!       Subset:      Re-initialized Subset structure.
!                     UNITS:      N/A
!                     TYPE:       Subset_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Subset_Destroy( Subset )
    TYPE(Subset_type), INTENT(OUT) :: Subset
    Subset%Is_Allocated = .FALSE.
    Subset%n_Values     = 0
  END SUBROUTINE Subset_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Subset_Create
! 
! PURPOSE:
!       Elemental subroutine to create an instance of an Subset object.
!
! CALLING SEQUENCE:
!       CALL Subset_Create( Subset  , &
!                           n_Values  )         
!
! OBJECTS:
!       Subset:             Subset object structure.
!                           UNITS:      N/A
!                           TYPE:       Subset_type
!                           DIMENSION:  Scalar or any rank
!                           ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Values:           Number of values in the subset.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Subset_Create( &
    Subset  , &  ! Output
    n_Values  )  ! Input
    ! Arguments
    TYPE(Subset_type), INTENT(OUT) :: Subset
    INTEGER          , INTENT(IN)  :: n_Values
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Values < 1 ) RETURN
    
    ! Perform the allocation
    ALLOCATE( Subset%Number( n_Values ), &
              Subset%Index( n_Values ),  &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    Subset%n_Values = n_Values
    ! ...Arrays
    Subset%Number = 0
    Subset%Index  = 0


    ! Set allocation indicator
    Subset%Is_Allocated = .TRUE.

  END SUBROUTINE Subset_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Subset_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a Subset object to stdout.
!
! CALLING SEQUENCE:
!       CALL Subset_Inspect( Subset )
!
! OBJECTS:
!       Subset:      Subset object to display.
!                      UNITS:      N/A
!                      TYPE:       Subset_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Subset_Inspect( Subset )
    TYPE(Subset_type), INTENT(IN) :: Subset
    WRITE(*,'(1x,"Subset OBJECT")')
    ! Dimensions
    WRITE(*,'(3x,"n_Values:",1x,i0)') Subset%n_Values
    IF ( .NOT. Subset_Associated(Subset) ) RETURN
    ! Subset info
    WRITE(*,'(3x,"Number  :")')
    WRITE(*,'(10(1x,i5,:))') Subset%Number
    WRITE(*,'(3x,"Index   :")')
    WRITE(*,'(10(1x,i5,:))') Subset%Index
          
  END SUBROUTINE Subset_Inspect
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Subset_DefineVersion
!
! PURPOSE:
!       Subroutine to return the version information for the
!       definition module(s).
!
! CALLING SEQUENCE:
!       CALL Subset_DefineVersion( Id )
!
! OUTPUTS:
!       Id:     Character string containing the version Id information for
!               this module.
!               UNITS:      N/A
!               TYPE:       CHARACTER(*)
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Subset_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE Subset_DefineVersion


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Subset_SetValue
!
! PURPOSE:
!       Subroutine to set the contents of a Subset object.
!
! CALLING SEQUENCE:
!       CALL Subset_SetValue( Subset, Number=Number, Index=Index )
!
! OBJECTS:
!       Subset:      Subset object for which values are to be set.
!                    UNITS:      N/A
!                    TYPE:       Subset_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Number:      Integer array to which the Number component of the Subset
!                    object is to be set. The size of the input must match 
!                    the allocated size of the component, otherwise all the
!                    component number values are set to zero.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Rank-1
!                    ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Index:       Integer array to which the Index component of the Subset
!                    object is to be set. The size of the input must match 
!                    the allocated size of the component, otherwise all the
!                    component index values are set to zero.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Rank-1
!                    ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Subset_SetValue( &
    Subset  , &  ! Input
    Number  , &  ! Optional input
    Index     )  ! Optional input
    ! Arguments
    TYPE(Subset_type), INTENT(IN OUT) :: Subset
    INTEGER, OPTIONAL, INTENT(IN)     :: Number(:)
    INTEGER, OPTIONAL, INTENT(IN)     :: Index(:)
    
    IF ( .NOT. Subset_Associated(Subset) ) RETURN
    
    IF ( PRESENT(Number) ) THEN
      IF ( SIZE(Number) == Subset%n_Values ) THEN
        Subset%Number = Number
      ELSE
        Subset%Number = 0
      END IF
    END IF
    
    IF ( PRESENT(Index) ) THEN
      IF ( SIZE(Index) == Subset%n_Values ) THEN
        Subset%Index = Index
      ELSE
        Subset%Index = 0
      END IF
    END IF
    
  END SUBROUTINE Subset_SetValue
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Subset_GetValue
!
! PURPOSE:
!       Subroutine to get and return the contents of a Subset object.
!
! CALLING SEQUENCE:
!       CALL Subset_GetValue( Subset, n_Values=n_Values, Number=Number, Index=Index )
!
! OBJECTS:
!       Subset:      Subset object from which values are to be retrieved.
!                    UNITS:      N/A
!                    TYPE:       Subset_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Values:    The dimension of the components of the Subset object.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Number:      Integer array to which the values of the Number
!                    component of the Subset object are to be assigned.
!                    The actual argument must be defined as allocatable.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Rank-1
!                    ATTRIBUTES: INTENT(OUT), OPTIONAL, ALLOCATABLE
!
!       Index:       Integer array to which the values of the Index
!                    component of the Subset object are to be assigned.
!                    The actual argument must be defined as allocatable.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Rank-1
!                    ATTRIBUTES: INTENT(OUT), OPTIONAL, ALLOCATABLE
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Subset_GetValue( &
    Subset  , &  ! Input
    n_Values, &  ! Optional output
    Number  , &  ! Optional output
    Index     )  ! Optional output
    ! Arguments
    TYPE(Subset_type),              INTENT(IN)  :: Subset
    INTEGER,              OPTIONAL, INTENT(OUT) :: n_Values
    INTEGER, ALLOCATABLE, OPTIONAL, INTENT(OUT) :: Number(:)
    INTEGER, ALLOCATABLE, OPTIONAL, INTENT(OUT) :: Index(:)
    ! Local variables
    INTEGER :: n
    
    n = Subset%n_Values
    IF ( PRESENT(n_Values) ) n_Values = n
    
    IF ( PRESENT(Number) ) THEN
      ALLOCATE(Number(n))
      Number = Subset%Number
    END IF
    
    IF ( PRESENT(Index) ) THEN
      ALLOCATE(Index(n))
      Index = Subset%Index
    END IF

  END SUBROUTINE Subset_GetValue
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Subset_Generate
!
! PURPOSE:
!       Subroutine to generate the subset indexing and return
!       it in a Subset object.
!
! CALLING SEQUENCE:
!       CALL Subset_Generate( Subset, List, Subset_List )
!
! OBJECTS:
!       Subset:      Subset object to hold the generated subset index
!                    information
!                    UNITS:      N/A
!                    TYPE:       Subset_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       List:        Array of values from which a subset is to be extracted.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Rank-1
!                    ATTRIBUTES: INTENT(IN)
!                    
!       Subset_List: Array of values defining the subset.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Rank-1
!                    ATTRIBUTES: INTENT(IN)
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Subset_Generate( &
    Subset     , &  ! Output
    List       , &  ! Input
    Subset_List  )  ! Input
    ! Arguments
    TYPE(Subset_type), INTENT(OUT) :: Subset
    INTEGER          , INTENT(IN)  :: List(:)
    INTEGER          , INTENT(IN)  :: Subset_List(:)
    ! Local variables
    INTEGER :: sorted_list(SIZE(List))
    INTEGER :: sorted_subset_list(SIZE(Subset_List))
    INTEGER :: i, n_list
    INTEGER :: n_subset_list
    INTEGER :: n_elements
    INTEGER :: isubset, iextract

    ! Set up
    ! ...No list data?
    n_list        = SIZE(List)
    n_subset_list = SIZE(Subset_List)
    IF ( n_list < 1 .OR. n_subset_list < 1 ) RETURN


    ! Sort the lists
    sorted_list = List
    CALL InsertionSort( sorted_list )
    sorted_subset_list = Subset_List
    CALL InsertionSort( sorted_subset_list )
    
    
    ! Count the elements to subset
    n_elements = COUNT( sorted_subset_list >= sorted_list(1) .AND. &
                        sorted_subset_list <= sorted_list(n_list)  )
    IF ( n_elements == 0 ) RETURN


    ! Allocate the Subset structure
    CALL Subset_Create( Subset, n_elements )
    IF ( .NOT. Subset_Associated( Subset ) ) RETURN


    ! Define the start points for the search
    ! ...Determine the starting index in the SUBSET list array
    isubset = MINLOC( sorted_subset_list - sorted_list(1), &
                      MASK = ( (sorted_subset_list - sorted_list(1)) >= 0 ), &
                      DIM  = 1 )
    ! ...Set the starting index in the output. This is always 1.
    iextract = 1


    ! Loop over the number of MAIN list elements
    List_Loop: DO i = 1, n_list
      IF ( sorted_list(i) == sorted_subset_list(isubset) ) THEN  ! Is the list element in the subset?
        Subset%Index(  iextract ) = i                              ! Save the index...
        Subset%Number( iextract ) = sorted_list(i)                 ! ...and number
        iextract = iextract + 1                                    ! Increment the extract...
        isubset  = isubset  + 1                                    ! ...and subset indices
        IF ( isubset > n_subset_list ) EXIT List_Loop              ! Exit loop if last element found
      END IF
    END DO List_Loop

  END SUBROUTINE Subset_Generate



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
!       Subset_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two Subset objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = Subset_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two Subset objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       Subset_type
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

  ELEMENTAL FUNCTION Subset_Equal( x, y ) RESULT( is_equal )
    TYPE(Subset_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
   
    ! Check the object association status
    IF ( (.NOT. Subset_Associated(x)) .OR. &
         (.NOT. Subset_Associated(y))      ) RETURN

    ! Check contents
    ! ...Dimensions
    IF ( x%n_Values /= y%n_Values ) RETURN
    ! ...Arrays
    IF ( ALL(x%Number == y%Number ) .AND. &
         ALL(x%Index  == y%Index  ) ) &
      is_equal = .TRUE.

  END FUNCTION Subset_Equal

END MODULE Subset_Define
