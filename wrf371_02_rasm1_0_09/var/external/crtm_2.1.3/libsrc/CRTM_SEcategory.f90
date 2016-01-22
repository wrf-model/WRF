!
! CRTM_SEcategory
!
! Module to compute the surface optical properties using
! a surface type category look-up-table.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 26-Aug-2011
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_SEcategory

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds        , ONLY: fp
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters   , ONLY: ZERO, ONE
  USE CRTM_Interpolation, ONLY: NPTS        , &
                                LPoly_type  , &
                                find_index  , &
                                interp_1D   , &
                                interp_1D_TL, &
                                interp_1D_AD, &
                                Clear_LPoly , &
                                LPoly       , &
                                LPoly_TL    , &
                                LPoly_AD
  USE SEcategory_Define , ONLY: SEcategory_type
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Data types
  PUBLIC :: iVar_type
  ! Science routines
  PUBLIC :: SEcategory_Emissivity


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_SEcategory.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! --------------------------------------
  ! Structure definitions to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    ! The interpolating polynomials
    TYPE(LPoly_type) :: xlp
    ! The LUT interpolation indices
    INTEGER :: i1, i2
    ! The LUT interpolation boundary check
    LOGICAL :: x_outbound
    ! The interpolation input
    REAL(fp) :: x_int
    ! The data to be interpolated
    REAL(fp) :: x(NPTS)
  END TYPE iVar_type


CONTAINS


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       SEcategory_Emissivity
!
! PURPOSE:
!       Function to compute surface emissivities from an emissivity or
!       reflectance LUT as a function of surface type category.
!
! CALLING SEQUENCE:
!       Error_Status = SEcategory_Emissivity( &
!                        SEcategory , & 
!                        Frequency   , & 
!                        Surface_Type, & 
!                        Emissivity  , & 
!                        iVar          ) 
!
! INPUTS:
!       SEcategory:      Emissivity/reflectivity LUT.
!                        UNITS:      N/A
!                        TYPE:       SEcategory_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Frequency:       Spectral frequency at which an emissivity is required.
!                        UNITS:      Inverse centimetres (cm^-1)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Surface_Type:    Index into the surface type dimension of the LUT indicating
!                        the surface type for which an emissivity is required.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Emissivity:      Surface emissivity for the specified surface type
!                        interpolated to the requested frequency.
!                        UNITS:      N/A
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
!       iVar:            Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the module containing this procedure.
!                        UNITS:      N/A
!                        TYPE:       iVar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was successful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION SEcategory_Emissivity( &
    SEcategory  , &
    Frequency   , &
    Surface_Type, &
    Emissivity  , &
    iVar        ) &
  RESULT( err_stat )
    ! Arguments
    TYPE(SEcategory_type), INTENT(IN)  :: SEcategory
    REAL(fp)             , INTENT(IN)  :: Frequency
    INTEGER              , INTENT(IN)  :: Surface_Type
    REAL(fp)             , INTENT(OUT) :: Emissivity
    TYPE(iVar_type)      , INTENT(OUT) :: iVar
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'SEcategory_Emissivity'
    ! Local variables
    CHARACTER(ML) :: msg
    REAL(fp) :: reflectance

    ! Setup
    err_stat = SUCCESS
    ! ...Check surface type valid range
    IF ( Surface_Type < 1 .OR. &
         Surface_Type > SEcategory%n_Surface_Types ) THEN
      Emissivity = ZERO
      err_stat = FAILURE
      msg = 'Invalid surface type index specified'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF
    ! ...Check surface type valid for classification
    IF ( .NOT. SEcategory%Surface_Type_IsValid(Surface_Type) ) THEN
      Emissivity = ZERO
      err_stat = FAILURE
      msg = 'Invalid surface type index specified for '//&
            TRIM(SEcategory%Classification_Name)//' classification'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF
    
    
    ! Find the frequency indices for interpolation
    iVar%x_int = MAX(MIN(SEcategory%Frequency(SEcategory%n_Frequencies),&
                         Frequency), &
                     SEcategory%Frequency(1))
    CALL find_index(SEcategory%Frequency, iVar%x_int, iVar%i1, iVar%i2, iVar%x_outbound)
    iVar%x = SEcategory%Frequency(iVar%i1:iVar%i2)


    ! Calculate the interpolating polynomial
    CALL LPoly( iVar%x, iVar%x_int, &  ! Input
                iVar%xlp            )  ! Output


    ! Perform Interpolation
    CALL interp_1D( SEcategory%Reflectance(iVar%i1:iVar%i2, Surface_Type), iVar%xlp, reflectance )
    Emissivity = ONE - reflectance

  END FUNCTION SEcategory_Emissivity
  
END MODULE CRTM_SEcategory
