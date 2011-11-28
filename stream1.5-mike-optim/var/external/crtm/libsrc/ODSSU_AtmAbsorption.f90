!
! ODSSU_AtmAbsorption
!
! Module containing routines to compute the optical depth profile for SSUs
!
!
! CREATION HISTORY:
!       Based on CRTM_AtmAbsorption_ssu     by:  Quanhua Liu, JCSDA,      Dec  1, 2007
!       Rewritten for CRTMv2.0              by:  Yong Han, NOAA/NESDIS,   Oct  6, 2009
!       Revised                             by:  Paul van Delst, , JCSDA, Oct 26, 2009
!       Revised                             by:  Quanhua Liu, JCSDA,      Oct 29, 2009
!       Revised                             by:  Yong Chen, JCSDA,        Nov  9, 2009
!

MODULE ODSSU_AtmAbsorption

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                ONLY: fp
  USE Message_Handler,           ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE CRTM_Parameters,           ONLY: ZERO, ONE 
  USE CRTM_GeometryInfo_Define,  ONLY: CRTM_GeometryInfo_type 
  USE CRTM_AtmScatter_Define,    ONLY: CRTM_AtmAbsorption_type => CRTM_AtmScatter_type
  USE ODAS_AtmAbsorption,        ONLY: ODAS_AAVariables_type         => AAVariables_type,         &
                                       ODAS_Compute_AtmAbsorption    => Compute_AtmAbsorption,    &
                                       ODAS_Compute_AtmAbsorption_TL => Compute_AtmAbsorption_TL, &
                                       ODAS_Compute_AtmAbsorption_AD => Compute_AtmAbsorption_AD
  USE ODPS_AtmAbsorption,        ONLY: ODPS_AAVariables_type         ,  &
                                       ODPS_Compute_AtmAbsorption    ,  &
                                       ODPS_Compute_AtmAbsorption_TL ,  &
                                       ODPS_Compute_AtmAbsorption_AD 
  USE ODAS_Predictor,            ONLY: ODAS_Predictor_type        =>Predictor_type
  USE ODPS_Predictor,            ONLY: ODPS_Predictor_type        =>Predictor_type
  USE ODSSU_TauCoeff,            ONLY: TC

  USE SSU_Input_Define,   ONLY: SSU_Input_type, &
                                SSU_Input_GetValue, &
                                SSU_Input_CellPressureIsSet
  USE Search_Utility,     ONLY: Bisection_Search

  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! CRTM_AtmAbsorption structure data type
  ! in the CRTM_AtmAbsorption_Define module
  PUBLIC :: AAVariables_type
  PUBLIC :: ODSSU_Compute_AAV 
  PUBLIC :: Compute_AtmAbsorption
  PUBLIC :: Compute_AtmAbsorption_TL
  PUBLIC :: Compute_AtmAbsorption_AD

  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE Compute_AtmAbsorption
    MODULE PROCEDURE Compute_ODAS_AtmAbsorption
    MODULE PROCEDURE Compute_ODPS_AtmAbsorption
  END INTERFACE Compute_AtmAbsorption

  INTERFACE Compute_AtmAbsorption_TL
    MODULE PROCEDURE Compute_ODAS_AtmAbsorption_TL
    MODULE PROCEDURE Compute_ODPS_AtmAbsorption_TL
  END INTERFACE Compute_AtmAbsorption_TL

  INTERFACE Compute_AtmAbsorption_AD
    MODULE PROCEDURE Compute_ODAS_AtmAbsorption_AD
    MODULE PROCEDURE Compute_ODPS_AtmAbsorption_AD
  END INTERFACE Compute_AtmAbsorption_AD
   
  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: ODSSU_AtmAbsorption.f90 6156 2009-12-31 17:11:15Z yong.chen@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  
  
  ! ------------------------------------------
  ! Structure definition to hold forward model
  ! variables across FWD, TL, and AD calls
  ! ------------------------------------------
  TYPE :: AAVariables_type
    PRIVATE
    TYPE(ODAS_AAVariables_type) :: ODAS(2)
    REAL(fp) :: Weight(2) = ZERO
    REAL(fp) :: CO2_Cell = ZERO
    INTEGER  :: Index_low = 1
  END TYPE AAVariables_type


  
CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       ODSSU_Compute_AAV 
!
! PURPOSE:
!       Subroutine to calculate Internal variable output    
!
! CALLING SEQUENCE:
!       CALL ODSSU_Compute_AAV ( SSU_Input    , &  ! Inputq
!                                SensorIndex  , &  ! Input                       
!                                ChannelIndex , &  ! Input                           
!                                AAVariables    )  ! Internal variable output        
!
! INPUT ARGUMENTS:
!       SSU_Input:       Structure containing the view geometry
!                        information.
!                        UNITS:      N/A
!                        TYPE:       SSU_Input_type 
!                        DIMENSION:  Same as input Atmosphere structure
!                        ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       AAVariables:     Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_AtmAbsorption module.
!                        UNITS:      N/A
!                        TYPE:       TYPE(AAVariables_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
!------------------------------------------------------------------------------
  SUBROUTINE ODSSU_Compute_AAV( SSU_Input    , &  ! Input                      
                                SensorIndex  , &  ! Input                      
                                ChannelIndex , &  ! Input                      
                                AAV            )  ! Internal variable output   
    ! Arguments
    TYPE(SSU_Input_type)         , INTENT(IN)     :: SSU_Input
    INTEGER                      , INTENT(IN)     :: SensorIndex
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(AAVariables_type)       , INTENT(OUT)    :: AAV
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ODSSU_Compute_AAV'
    ! Variables
    CHARACTER(ML) :: msg
    REAL(fp) :: Time, Cell_Pressure

    ! Compute the CO2 cell pressure
    IF( SSU_Input_CellPressureIsSet(SSU_Input) ) THEN
      ! Use cell pressure data
      CALL SSU_Input_GetValue( SSU_Input, &
                               Channel = ChannelIndex, &
                               Cell_Pressure = Cell_Pressure )
      AAV%CO2_Cell = Cell_Pressure
      AAV%Index_low = Bisection_Search( TC(SensorIndex)%TC_CellPressure(:,ChannelIndex), AAV%CO2_Cell )
    ELSE
      ! Use mission time data
      CALL SSU_Input_GetValue( SSU_Input, Time=Time )
      IF( Time < TC(SensorIndex)%Ref_Time(1) )THEN
        Time = TC(SensorIndex)%Ref_Time(1)
        WRITE( msg,'("Invalid time. Reset to ",f8.2)' ) Time
        CALL Display_Message( ROUTINE_NAME, TRIM(msg), WARNING )
      END IF
      ! obtain CO2 cell pressure for Time
      CALL get_CO2_Cell_p( SensorIndex, ChannelIndex, Time, AAV%CO2_Cell )
      ! get index
      AAV%Index_low = Bisection_Search( TC(SensorIndex)%TC_CellPressure(:,ChannelIndex), AAV%CO2_Cell )
    END IF

    ! Compute the interpolation weights
    AAV%Weight(1) = (AAV%CO2_Cell - TC(SensorIndex)%TC_CellPressure(AAV%Index_low,ChannelIndex))/ &
                      (TC(SensorIndex)%TC_CellPressure(AAV%Index_low+1,ChannelIndex) - &
                       TC(SensorIndex)%TC_CellPressure(AAV%Index_low  ,ChannelIndex)   )
    AAV%Weight(2) = ONE - AAV%Weight(1)

  END SUBROUTINE ODSSU_Compute_AAV

!------------------------------------------------------------------------------
!
! NAME:
!       Compute_AtmAbsorption
!
! PURPOSE:
!       Subroutine to calculate the layer optical depths due to gaseous
!       absorption for a given sensor and channel and atmospheric profile.
!
! CALLING SEQUENCE:
!       CALL Compute_AtmAbsorption( SensorIndex  , &  ! Input
!                                   ChannelIndex , &  ! Input                        
!                                   Predictor    , &  ! Input                        
!                                   AtmAbsorption, &  ! Output                       
!                                   AAVariables    )  ! Internal variable in/output     
!
! INPUT ARGUMENTS:
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Predictor:       Structure containing the integrated absorber and
!                        predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       AAVariables:     Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_AtmAbsorption module.
!                        UNITS:      N/A
!                        TYPE:       TYPE(AAVariables_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!        AtmAbsorption:  Structure containing computed optical depth
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       AAVariables:     Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_AtmAbsorption module.
!                        UNITS:      N/A
!                        TYPE:       TYPE(AAVariables_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! COMMENTS:
!       Note the INTENT on the structure arguments are IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  SUBROUTINE Compute_ODAS_AtmAbsorption( SensorIndex  , &  ! Input
                                         ChannelIndex , &  ! Input                    
                                         Predictor    , &  ! Input                    
                                         AtmAbsorption, &  ! Output                   
                                         AAV            )  ! Internal variable output 
    ! Arguments
    INTEGER                      , INTENT(IN)     :: SensorIndex
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(ODAS_Predictor_type)    , INTENT(IN OUT) :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption
    TYPE(AAVariables_type)       , INTENT(IN OUT) :: AAV
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_AtmAbsorption (ODSSU_ODAS)'
    ! Variables
    REAL(fp) :: Optical_Depth( AtmAbsorption%n_Layers )
 
    ! Compute the optical depths
    ! ...At cell pressure 1
    CALL ODAS_Compute_AtmAbsorption(TC(SensorIndex)%ODAS(AAV%Index_low), &   
                                    ChannelIndex                       , &   
                                    Predictor                          , &   
                                    AtmAbsorption                      , &   
                                    AAV%ODAS(1) )                            
    Optical_Depth = AtmAbsorption%Optical_Depth                              
    ! ...At cell pressure 2                                                  
    CALL ODAS_Compute_AtmAbsorption(TC(SensorIndex)%ODAS(AAV%Index_low+1), &    
                                    ChannelIndex                         , &     
                                    Predictor                            , &     
                                    AtmAbsorption                        , &  
                                    AAV%ODAS(2) )                            
    ! ...Weighted average                                                    
    AtmAbsorption%Optical_Depth = AAV%Weight(1)*AtmAbsorption%Optical_Depth + &
                                  AAV%Weight(2)*Optical_Depth
                                  
  END SUBROUTINE Compute_ODAS_AtmAbsorption

  SUBROUTINE Compute_ODPS_AtmAbsorption( SensorIndex  , &  ! Input
                                         ChannelIndex , &  ! Input                    
                                         Predictor    , &  ! Input                    
                                         AtmAbsorption, &  ! Output                   
                                         AAV            )  ! Internal variable In/output 
    ! Arguments
    INTEGER                      , INTENT(IN)     :: SensorIndex
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(ODPS_Predictor_type)    , INTENT(IN OUT) :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption
    TYPE(AAVariables_type)       , INTENT(IN OUT) :: AAV
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_AtmAbsorption (ODSSU_ODPS)'
    ! Variables
    REAL(fp) :: Optical_Depth( AtmAbsorption%n_Layers )
 
    ! Compute the optical depths
    ! ...At cell pressure 1
    CALL ODPS_Compute_AtmAbsorption(TC(SensorIndex)%ODPS(AAV%Index_low), &     
                                    ChannelIndex                       , &     
                                    Predictor                          , &     
                                    AtmAbsorption )                            
    Optical_Depth = AtmAbsorption%Optical_Depth                                
    ! ...At cell pressure 2                                                    
    CALL ODPS_Compute_AtmAbsorption(TC(SensorIndex)%ODPS(AAV%Index_low+1), &    
                                    ChannelIndex                         , &     
                                    Predictor                            , &     
                                    AtmAbsorption )                            
    ! ...Weighted average
    AtmAbsorption%Optical_Depth = AAV%Weight(1)*AtmAbsorption%Optical_Depth + &
                                  AAV%Weight(2)*Optical_Depth
                                  
  END SUBROUTINE Compute_ODPS_AtmAbsorption

!---------------------------------------------------
! TL routine corresponding to Compute_AtmAbsorption
!---------------------------------------------------

  SUBROUTINE Compute_ODAS_AtmAbsorption_TL( SensorIndex     , &  
                                            ChannelIndex    , &  
                                            Predictor       , &  
                                            Predictor_TL    , &  
                                            AtmAbsorption_TL, &  
                                            AAV               )  
    ! Arguments
    INTEGER                      , INTENT(IN)     :: SensorIndex
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(ODAS_Predictor_type)    , INTENT(IN)     :: Predictor
    TYPE(ODAS_Predictor_type)    , INTENT(IN)     :: Predictor_TL
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption_TL
    TYPE(AAVariables_type)       , INTENT(IN)     :: AAV
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_AtmAbsorption_TL (ODSSU_ODAS)'
    ! Variables
    REAL(fp) :: Optical_Depth_TL(AtmAbsorption_TL%n_Layers)

    CALL ODAS_Compute_AtmAbsorption_TL(TC(SensorIndex)%ODAS(AAV%Index_low), &   
                                       ChannelIndex                       , &     
                                       Predictor                          , &     
                                       Predictor_TL                       , &     
                                       AtmAbsorption_TL                   , &     
                                       AAV%ODAS(1) )                             
    Optical_Depth_TL = AtmAbsorption_TL%Optical_Depth                           
    CALL ODAS_Compute_AtmAbsorption_TL(TC(SensorIndex)%ODAS(AAV%Index_low+1), & 
                                       ChannelIndex                         , &   
                                       Predictor                            , & 
                                       Predictor_TL                         , & 
                                       AtmAbsorption_TL                     , &   
                                       AAV%ODAS(2) )                            
    AtmAbsorption_TL%Optical_Depth = AAV%Weight(1)*AtmAbsorption_TL%Optical_Depth + &
                                     AAV%Weight(2)*Optical_Depth_TL
                                  
  END SUBROUTINE Compute_ODAS_AtmAbsorption_TL

  SUBROUTINE Compute_ODPS_AtmAbsorption_TL(SensorIndex     , &  
                                           ChannelIndex    , &  
                                           Predictor       , &  
                                           Predictor_TL    , &  
                                           AtmAbsorption_TL, &  
                                           AAV               )  
    ! Arguments
    INTEGER                      , INTENT(IN)     :: SensorIndex
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(ODPS_Predictor_type)    , INTENT(IN)     :: Predictor
    TYPE(ODPS_Predictor_type)    , INTENT(IN OUT) :: Predictor_TL
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption_TL
    TYPE(AAVariables_type)       , INTENT(IN)     :: AAV
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_AtmAbsorption_TL (ODSSU_ODPS)'
    ! Variables
    REAL(fp) :: Optical_Depth_TL(AtmAbsorption_TL%n_Layers)

    CALL ODPS_Compute_AtmAbsorption_TL(TC(SensorIndex)%ODPS(AAV%Index_low), &         
                                       ChannelIndex                       , &         
                                       Predictor                          , &         
                                       Predictor_TL                       , &         
                                       AtmAbsorption_TL )                             
    Optical_Depth_TL = AtmAbsorption_TL%Optical_Depth                                 
    CALL ODPS_Compute_AtmAbsorption_TL(TC(SensorIndex)%ODPS(AAV%Index_low+1), &       
                                       ChannelIndex                         , &         
                                       Predictor                            , &         
                                       Predictor_TL                         , &         
                                       AtmAbsorption_TL )                             
    AtmAbsorption_TL%Optical_Depth = AAV%Weight(1)*AtmAbsorption_TL%Optical_Depth + & 
                                    AAV%Weight(2)*Optical_Depth_TL                    
                                  
  END SUBROUTINE Compute_ODPS_AtmAbsorption_TL

!---------------------------------------------------
! AD routine corresponding to Compute_AtmAbsorption
!---------------------------------------------------

  SUBROUTINE Compute_ODAS_AtmAbsorption_AD(SensorIndex     , &  
                                           ChannelIndex    , &   
                                           Predictor       , &   
                                           AtmAbsorption_AD, &   
                                           Predictor_AD    , &   
                                           AAV               )   
    ! Arguments
    INTEGER,                       INTENT(IN)     :: SensorIndex
    INTEGER,                       INTENT(IN)     :: ChannelIndex
    TYPE(ODAS_Predictor_type),     INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption_AD
    TYPE(ODAS_Predictor_type),     INTENT(IN OUT) :: Predictor_AD
    TYPE(AAVariables_type)       , INTENT(IN)     :: AAV
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_AtmAbsorption_AD (ODSSU_ODAS)'
    ! Variables
    REAL(fp) :: Optical_Depth_AD( AtmAbsorption_AD%n_Layers)

    Optical_Depth_AD               = AAV%Weight(2)*AtmAbsorption_AD%Optical_Depth
    AtmAbsorption_AD%Optical_Depth = AAV%Weight(1)*AtmAbsorption_AD%Optical_Depth
    CALL ODAS_Compute_AtmAbsorption_AD(TC(SensorIndex)%ODAS(AAV%Index_low+1), &        
                                       ChannelIndex                         , &        
                                       Predictor                            , &        
                                       AtmAbsorption_AD                     , &        
                                       Predictor_AD                         , &        
                                       AAV%ODAS(2) )                                   
    AtmAbsorption_AD%Optical_Depth = AtmAbsorption_AD%Optical_Depth + Optical_Depth_AD 
    CALL ODAS_Compute_AtmAbsorption_AD(TC(SensorIndex)%ODAS(AAV%Index_low), &          
                                       ChannelIndex                       , &          
                                       Predictor                          , &          
                                       AtmAbsorption_AD                   , &          
                                       Predictor_AD                       , &          
                                       AAV%ODAS(1) )                                   
 
  END SUBROUTINE Compute_ODAS_AtmAbsorption_AD

  SUBROUTINE Compute_ODPS_AtmAbsorption_AD(SensorIndex     , &  
                                           ChannelIndex    , &   
                                           Predictor       , &   
                                           AtmAbsorption_AD, &   
                                           Predictor_AD    , &   
                                           AAV               )   
    ! Arguments
    INTEGER,                       INTENT(IN)     :: SensorIndex
    INTEGER,                       INTENT(IN)     :: ChannelIndex
    TYPE(ODPS_Predictor_type),     INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption_AD
    TYPE(ODPS_Predictor_type),     INTENT(IN OUT) :: Predictor_AD
    TYPE(AAVariables_type)       , INTENT(IN)     :: AAV
    ! Parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_AtmAbsorption_AD (ODSSU_ODPS)'
    ! Variables
    REAL(fp) :: Optical_Depth_AD( AtmAbsorption_AD%n_Layers)

    Optical_Depth_AD               = AAV%Weight(2)*AtmAbsorption_AD%Optical_Depth
    AtmAbsorption_AD%Optical_Depth = AAV%Weight(1)*AtmAbsorption_AD%Optical_Depth
    CALL ODPS_Compute_AtmAbsorption_AD(TC(SensorIndex)%ODPS(AAV%Index_low+1), &        
                                       ChannelIndex                         , &        
                                       Predictor                            , &        
                                       AtmAbsorption_AD                     , &        
                                       Predictor_AD  )                             
    AtmAbsorption_AD%Optical_Depth = AtmAbsorption_AD%Optical_Depth + Optical_Depth_AD 
    CALL ODPS_Compute_AtmAbsorption_AD(TC(SensorIndex)%ODPS(AAV%Index_low), &          
                                       ChannelIndex                       , &          
                                       Predictor                          , &          
                                       AtmAbsorption_AD                   , &          
                                       Predictor_AD  )                             
 
  END SUBROUTINE Compute_ODPS_AtmAbsorption_AD
!
!
    SUBROUTINE get_CO2_Cell_p(SensorIndex,ChannelIndex,u,y0)
! -------------------------------------------------------------------
!  Using an sensor "SensorIndex" and time "u" to find CO2 cell pressure "y0".
! -------------------------------------------------------------------
       INTEGER, INTENT( IN ) :: SensorIndex, ChannelIndex
       REAL(fp), INTENT( IN ) :: u
       REAL(fp), INTENT( OUT ) :: y0 
       INTEGER :: n, jLower, jUpper, jMiddle, indx

       n = SIZE(TC(SensorIndex)%Ref_Time)
       jLower = 1
       jUpper = n

       if(u.ge.TC(SensorIndex)%Ref_Time(n)) then
         y0 = TC(SensorIndex)%Ref_CellPressure(n,ChannelIndex)
       return
       else if(u.le.TC(SensorIndex)%Ref_Time(1)) then
         y0 = TC(SensorIndex)%Ref_CellPressure(1,ChannelIndex) 
       return
       endif

       indx = Bisection_Search( TC(SensorIndex)%Ref_Time, u )
      
       y0 = TC(SensorIndex)%Ref_CellPressure(indx,ChannelIndex) + &
          (TC(SensorIndex)%Ref_CellPressure(indx+1,ChannelIndex)- &
          TC(SensorIndex)%Ref_CellPressure(indx,ChannelIndex))/  &
          (TC(SensorIndex)%Ref_Time(indx+1)-TC(SensorIndex)%Ref_Time(indx))* &
          (u-TC(SensorIndex)%Ref_Time(indx))
       RETURN
    END SUBROUTINE get_CO2_Cell_p
!
  
END MODULE ODSSU_AtmAbsorption

