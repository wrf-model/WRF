!
! CRTM_Atmosphere
!
! Module for adding layers to the CRTM atmosphere structure as required.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 29-Oct-2007
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_Atmosphere

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds            , ONLY: fp
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE CRTM_Parameters       , ONLY: ZERO, ONE, POINT_5, SET, &
                                    TOA_PRESSURE           , &
                                    MINIMUM_ABSORBER_AMOUNT
  USE CRTM_Atmosphere_Define, ONLY: CRTM_Atmosphere_type    , &
                                    OPERATOR(==), &
                                    OPERATOR(+), &
                                    CRTM_Atmosphere_Associated, &
                                    CRTM_Atmosphere_Create, &
                                    CRTM_Atmosphere_AddLayerCopy, &
                                    CRTM_Atmosphere_Zero
  USE CRTM_Model_Profiles   , ONLY: MODEL_LEVEL_PRESSURE, & 
                                    CRTM_Get_Model_Profile
  ! ...Internal variable definition module
  USE iAtm_Define,            ONLY: iAtm_type      , &
                                    iAtm_Associated, &
                                    iAtm_Create    , &
                                    iAtm_Destroy
                                    
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Module procedures
  PUBLIC :: CRTM_Atmosphere_AddLayers
  PUBLIC :: CRTM_Atmosphere_AddLayers_TL
  PUBLIC :: CRTM_Atmosphere_AddLayers_AD
  ! iAtm entities
  ! ...Structure
  PUBLIC :: iAtm_type      
  ! ...Procedures
  PUBLIC :: iAtm_Associated
  PUBLIC :: iAtm_Create
  PUBLIC :: iAtm_Destroy


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Atmosphere.f90 6543 2010-02-01 20:32:30Z paul.vandelst@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


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
!       CRTM_Atmosphere_AddLayers
!
! PURPOSE:
!       Function to copy an atmosphere structure and adding extra layers from
!       climatology as required to supplement the upper atmosphere profile data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Atmosphere_AddLayers( Atm_In , &  ! Input
!                                                 Atm_Out  )  ! Output
!
! INPUTS:
!       Atm_In:          Atmosphere structure that is to be supplemented
!                        or copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Atm_Out:         Copy of the input atmosphere structure with extra upper
!                        atmosphere layers added as required.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the operation was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Atmosphere_AddLayers( &
    Atm_In , &  ! Input
    Atm_Out) &  ! Output
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm_In
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_Out
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_AddLayers'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: i, j, k, n
    TYPE(iAtm_type) :: iAtm


    ! Set up
    err_stat = SUCCESS


    ! If extra layers are NOT needed,
    ! then simply copy the structure
    IF ( Atm_In%Level_Pressure(0) <= TOA_PRESSURE) THEN
      Atm_Out = Atm_In
      IF ( .NOT. CRTM_Atmosphere_Associated( Atm_Out ) ) THEN
        err_stat = FAILURE
        msg = 'Error assigning Atmosphere structure with NO extra layers'
        CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      END IF
      RETURN
    END IF
    
    
    ! Determine the number of extra layers required
    n = Extra_Layers( Atm_In )
    IF ( n < 1 ) THEN
      err_stat = FAILURE
      msg = 'Error determining extra layer count'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF
    
    
    ! Allocate the internal variable structure
    CALL iAtm_Create( iAtm, n, Atm_In%n_Absorbers )
    IF ( .NOT. iAtm_Associated( iAtm ) ) THEN
      err_stat = FAILURE
      msg = 'Error allocating iAtm internal structure'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF


    ! Get the extra layer profiles
    CALL CRTM_Get_Model_Profile( iAtm%pl, iAtm%tl, iAtm%al, Model=Atm_In%Climatology )


    ! First interpolate the extra levels to the user top pressure
    ! replacing the model data at that array index
    CALL Interp_LPoly( Atm_In%Level_Pressure(0), iAtm%pl(n-1:n), iAtm%ilpoly )
    iAtm%plint_save = Atm_In%Level_Pressure(0)
    iAtm%pln_save   = iAtm%pl(n)
    iAtm%pl(n)      = iAtm%plint_save
    CALL Interp_Linear( iAtm%ilpoly, iAtm%tl(n-1:n), iAtm%tlint_save )
    iAtm%tln_save = iAtm%tl(n)
    iAtm%tl(n)    = iAtm%tlint_save
    DO j = 1, Atm_In%n_Absorbers
      CALL Interp_Linear( iAtm%ilpoly, iAtm%al(n-1:n,j), iAtm%alint_save(j) )
      iAtm%aln_save(j) = iAtm%al(n,j)
      iAtm%al(n,j)     = iAtm%alint_save(j)
    END DO
    
    ! Now compute the model profile layer averages
    DO k = 1, n
      CALL Layer_P(iAtm%pl(k-1:k), iAtm%p(k))
      CALL Layer_X(iAtm%tl(k-1:k), iAtm%t(k))
    END DO
    DO j = 1, Atm_In%n_Absorbers
      DO k = 1, n
        CALL Layer_X(iAtm%al(k-1:k,j), iAtm%a(k,j))
      END DO
    END DO
    
    
    ! Now, extrapolate user layer profile to get the "layer 0" value and
    ! use it to shift the model profile to avoid a discontinuity at p(n)
    CALL Interp_LPoly( iAtm%p(n), Atm_In%Pressure(1:2), iAtm%elpoly )
    CALL Shift_Profile( iAtm%elpoly, Atm_In%Temperature(1:2), iAtm%t )
    DO j = 1, Atm_In%n_Absorbers
      CALL Shift_Profile( iAtm%elpoly, Atm_In%Absorber(1:2,j), iAtm%a(:,j) )
    END DO


    ! Make sure the absorber amounts are not negative.
    ! (Is a further, more physical, check needed here?)
    iAtm%a_save = iAtm%a
    WHERE (iAtm%a_save < ZERO) iAtm%a = MINIMUM_ABSORBER_AMOUNT
    

    ! Copy over the atmosphere structure with extra layers
    atm_out = CRTM_Atmosphere_AddLayerCopy( Atm_In, n )
    IF ( .NOT. CRTM_Atmosphere_Associated( atm_out ) ) THEN
      err_stat = FAILURE
      msg = 'Error copying Atmosphere structure with extra layers'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF


    ! Slot the added layers into the output atmosphere structure
    ! Note: Cloud and Aerosol assignments not really needed (the
    !       zeroing is handled by the structure allocation) since
    !       at TOA, typically, there are not any clouds and/or
    !       aerosols.
    ! ...Profile
    Atm_Out%Level_Pressure(0:n) = iAtm%pl
    Atm_Out%Pressure(1:n)       = iAtm%p
    Atm_Out%Temperature(1:n)    = iAtm%t
    DO j = 1, Atm_Out%n_Absorbers
      Atm_Out%Absorber(1:n,j)   = iAtm%a(:,j)
    END DO
    ! ...Clouds
    IF ( Atm_In%n_Clouds > 0 ) THEN
      DO i = 1, Atm_In%n_Clouds
        Atm_Out%Cloud(i)%Effective_Radius(1:n)   = ZERO
        Atm_Out%Cloud(i)%Effective_Variance(1:n) = ZERO
        Atm_Out%Cloud(i)%Water_Content(1:n)      = ZERO
      END DO
    END IF
    ! ...Aerosols
    IF ( Atm_In%n_Aerosols > 0 ) THEN
      DO i = 1, Atm_In%n_Aerosols
        Atm_Out%Aerosol(i)%Effective_Radius(1:n) = ZERO
        Atm_Out%Aerosol(i)%Concentration(1:n)    = ZERO
      END DO
    END IF


    ! Clean up
    CALL iAtm_Destroy( iAtm )
    
  END FUNCTION CRTM_Atmosphere_AddLayers


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_AddLayers_TL
!
! PURPOSE:
!       Function to copy a tangent-linear atmosphere structure and add extra
!       layers as required to supplement the upper atmosphere profile data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Atmosphere_AddLayers_TL( Atm_In    , &  ! FWD Input
!                                                    Atm_In_TL , &  ! TL  Input
!                                                    Atm_Out_TL  )  ! TL  Output
!
! INPUTS:
!       Atm_In:          Forward model atmosphere structure.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atm_In_TL:       Tangent-linear model atmosphere structure that is
!                        to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Atm_Out_TL:      Copy of the input tangent-linear atmosphere structure
!                        with extra upper atmosphere layers added as required.
!                        Note that the tangent-linear values of the added layers
!                        is *always* zero.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the operation was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Atmosphere_AddLayers_TL( &
    Atm_In    , &  ! FWD Input
    Atm_In_TL , &  ! TL  Input
    Atm_Out_TL) &  ! TL  Output
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm_In
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm_In_TL
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_Out_TL
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_AddLayers_TL'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: n


    ! Set up
    err_stat = SUCCESS


    ! If extra layers are NOT needed,
    ! then simply copy the structure
    IF ( Atm_In%Level_Pressure(0) <= TOA_PRESSURE) THEN
      Atm_Out_TL = Atm_In_TL
      IF ( .NOT. CRTM_Atmosphere_Associated( Atm_Out_TL ) ) THEN
        err_stat = FAILURE
        msg = 'Error assigning Atmosphere structure with NO extra layers'
        CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      END IF
      RETURN
    END IF

    
    ! Determine how many extra layers are needed
    n = Extra_Layers( Atm_In )
    IF ( n < 1 ) THEN
      err_stat = FAILURE
      msg = 'Error determining extra layer count'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF


    ! Copy over the atmosphere structure with extra layers
    ! (which will be zero by definition)
    atm_out_TL = CRTM_Atmosphere_AddLayerCopy( Atm_In_TL, n )
    IF ( .NOT. CRTM_Atmosphere_Associated( atm_out_TL ) ) THEN
      err_stat = FAILURE
      msg = 'Error copying Atmosphere structure with extra layers'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF
    
  END FUNCTION CRTM_Atmosphere_AddLayers_TL
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Atmosphere_AddLayers_AD
!
! PURPOSE:
!       Function to copy back an adjoint atmosphere structure removing added
!       extra layers as were required to supplement the upper atmosphere
!       profile data.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Atmosphere_AddLayers_AD( Atm_In    , &  ! FWD Input
!                                                    Atm_Out_AD, &  ! AD  Input
!                                                    Atm_In_AD   )  ! AD  Output
!
! INPUTS:
!       Atm_In:          Forward model atmosphere structure.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Atm_Out_AD:      Adjoint atmosphere structure that contains the added
!                        extra layers.
!                        ** SET TO ZERO ON EXIT ** 
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUTS:
!       Atm_In_AD:       Adjoint atmosphere structure at the original, user
!                        specified layering.
!                        ** MUST HAVE VALUE ON ENTRY **
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the operation was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Atmosphere_AddLayers_AD( &
    Atm_In    , &  ! FWD Input
    Atm_Out_AD, &  ! AD  Input
    Atm_In_AD ) &  ! AD  Output
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN)     :: Atm_In
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_Out_AD
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atm_In_AD
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Atmosphere_AddLayers_AD'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: i, j, n, no, nt


    ! Set up
    err_stat = SUCCESS


    ! If extra layers are NOT needed, then simply perform
    ! the adjoint sum. Remember the TL form is
    !   Atm_Out_TL = Atm_In_TL
    ! so the adjoint form is
    !   Atm_In_AD  = Atm_In_AD + Atm_Out_AD
    !   Atm_Out_AD = ZERO
    IF ( Atm_In%Level_Pressure(0) <= TOA_PRESSURE) THEN
      Atm_In_AD = Atm_In_AD + Atm_Out_AD
      CALL CRTM_Atmosphere_Zero( Atm_Out_AD )
      RETURN
    END IF
    
    
    ! Determine how many extra layers have been used
    n = Extra_Layers( Atm_In )
    IF ( n < 1 ) THEN
      err_stat = FAILURE
      msg = 'Error determining extra layer count'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF


    ! Perform the adjoint summations
    ! This is the adjoint equivalent of the TL Assign_Atmosphere
    no = Atm_In_AD%n_Layers
    nt = n + no
    ! ...Aerosols
    IF ( Atm_In_AD%n_Aerosols > 0 ) THEN
      DO i = 1, Atm_In_AD%n_Aerosols
        Atm_In_AD%Aerosol(i)%Concentration(1:no) = Atm_In_AD%Aerosol(i)%Concentration(1:no) + &
                                                   Atm_Out_AD%Aerosol(i)%Concentration(n+1:nt)
        Atm_In_AD%Aerosol(i)%Effective_Radius(1:no) = Atm_In_AD%Aerosol(i)%Effective_Radius(1:no) + &
                                                      Atm_Out_AD%Aerosol(i)%Effective_Radius(n+1:nt)
        Atm_In_AD%Aerosol(i)%Type = Atm_Out_AD%Aerosol(i)%Type
      END DO
    END IF
    ! ...Clouds    
    IF ( Atm_In_AD%n_Clouds > 0 ) THEN
      DO i = 1, Atm_In_AD%n_Clouds
        Atm_In_AD%Cloud(i)%Water_Content(1:no) = Atm_In_AD%Cloud(i)%Water_Content(1:no) + &
                                                 Atm_Out_AD%Cloud(i)%Water_Content(n+1:nt)
        Atm_In_AD%Cloud(i)%Effective_Variance(1:no) = Atm_In_AD%Cloud(i)%Effective_Variance(1:no) + &
                                                      Atm_Out_AD%Cloud(i)%Effective_Variance(n+1:nt)
        Atm_In_AD%Cloud(i)%Effective_Radius(1:no) = Atm_In_AD%Cloud(i)%Effective_Radius(1:no) + &
                                                    Atm_Out_AD%Cloud(i)%Effective_Radius(n+1:nt)
        Atm_In_AD%Cloud(i)%Type = Atm_Out_AD%Cloud(i)%Type
      END DO
    END IF
    ! ...Absorber data
    DO j = 1, Atm_In_AD%n_Absorbers
      Atm_In_AD%Absorber(1:no,j) = Atm_In_AD%Absorber(1:no,j) + Atm_Out_AD%Absorber(n+1:nt,j)
    END DO
    ! ...Temperature data
    Atm_In_AD%Temperature(1:no) = Atm_In_AD%Temperature(1:no) + Atm_Out_AD%Temperature(n+1:nt)
    ! ...Pressure data
    Atm_In_AD%Pressure(1:no)       = Atm_In_AD%Pressure(1:no) + Atm_Out_AD%Pressure(n+1:nt)
    Atm_In_AD%Level_Pressure(0:no) = Atm_In_AD%Level_Pressure(0:no) + Atm_Out_AD%Level_Pressure(n:nt)


    ! Zero the output atmosphere structure
    CALL CRTM_Atmosphere_Zero( Atm_Out_AD )

  END FUNCTION CRTM_Atmosphere_AddLayers_AD


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! Subprogram to determine the number of extra layers required
  FUNCTION Extra_Layers( Atm ) RESULT( n )
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: Atm
    INTEGER :: n
    DO n = 1, SIZE(MODEL_LEVEL_PRESSURE)
      IF ( MODEL_LEVEL_PRESSURE(n) >= Atm%Level_Pressure(0) ) RETURN
    END DO
    n = 0
  END FUNCTION Extra_Layers


  ! Subprogram to compute the average layer pressure
  SUBROUTINE Layer_P( p, p_layer )
    REAL(fp), INTENT(IN)  :: p(2)     ! Input
    REAL(fp), INTENT(OUT) :: p_layer  ! Output
    p_layer = (p(2)-p(1))/LOG(p(2)/p(1))
  END SUBROUTINE Layer_P


  ! Subprogram to compute the average layer amount of X
  SUBROUTINE Layer_X( x, x_layer )
    REAL(fp), INTENT(IN)  :: x(2)
    REAL(fp), INTENT(OUT) :: x_layer
    x_layer = POINT_5*(x(1)+x(2))
  END SUBROUTINE Layer_X
  

  ! Subprogram to compute the interpolating polynomial linear in log(p)
  SUBROUTINE Interp_LPoly( p_int, p, lpoly )
    REAL(fp), INTENT(IN)  :: p_int
    REAL(fp), INTENT(IN)  :: p(2)
    REAL(fp), INTENT(OUT) :: lpoly
    lpoly = (LOG(p_int)-LOG(p(1))) / (LOG(p(2))-LOG(p(1)))
  END SUBROUTINE Interp_LPoly


  ! Subprogram to perform linear interpolation
  SUBROUTINE Interp_Linear( lpoly, x, x_int )
    REAL(fp), INTENT(IN)  :: lpoly
    REAL(fp), INTENT(IN)  :: x(2)
    REAL(fp), INTENT(OUT) :: x_int
    x_int = (x(2)-x(1))*lpoly + x(1)
  END SUBROUTINE Interp_Linear


  ! Subprogram to shifted the added profile layers
  SUBROUTINE Shift_Profile( lpoly, x_toa, x_shifted )
    REAL(fp), INTENT(IN)     :: lpoly
    REAL(fp), INTENT(IN)     :: x_toa(2)
    REAL(fp), INTENT(IN OUT) :: x_shifted(:)
    INTEGER :: n
    REAL(fp) :: x_int, dx
    n = SIZE(x_shifted)
    CALL Interp_Linear( lpoly, x_toa, x_int )
    dx = x_int - x_shifted(n)
    x_shifted = x_shifted + dx
  END SUBROUTINE Shift_Profile
    
END MODULE CRTM_Atmosphere
