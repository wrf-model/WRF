!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_AtmOptics
!
! PURPOSE:
!       Module containing routines to compute single scattering albedo, and delta correction.
!
! CATEGORY:
!       CRTM : AtmOptics
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_AtmOptics
!
! MODULES:
!       Type_Kinds:                 Module containing data type kind definitions.
!
!       CRTM_Parameters:            Module of parameter definitions for the CRTM.
!                                   USEs: TYPE_KINDS module
!
!       CRTM_AtmScatter_Define:     Module defining the CRTM AtmScatter structure
!                                   and containing routines to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
! CONTAINS:
!       PUBLIC subprograms
!       ------------------
!         CRTM_Combine_AtmOptics:      Function to combine the optical properties
!                                      from AtmAbsorption, CloudScatter, and
!                                      AerosolScatter calculations.
!
!         CRTM_Combine_AtmOptics_TL:   Function to combine the tangent-linear
!                                      optical properties from AtmAbsorption,
!                                      CloudScatter, and AerosolScatter calculations.
!
!         CRTM_Combine_AtmOptics_AD:   Function to compute the adjoint of the
!                                      combined optical properties from AtmAbsorption,
!                                      CloudScatter, and AerosolScatter calculations.
!
!       PRIVATE subprograms
!       -------------------
!       
!         None.
!
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov  
!                       Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                       08-Jun-2005
!
!  Copyright (C) 2005 Yong Han, Quanhua Liu, Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!--------------------------------------------------------------------------------

MODULE CRTM_AtmOptics


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds
  USE Message_Handler

  ! -- CRTM modules
  USE CRTM_Parameters
  USE CRTM_AtmScatter_Define,    ONLY: CRTM_AtmScatter_type


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! --------------------
  ! Default visibilities
  ! --------------------

  ! -- Everything private by default
  PRIVATE

  ! -- The AtmOptics combination routines                               
  PUBLIC :: CRTM_Combine_AtmOptics
  PUBLIC :: CRTM_Combine_AtmOptics_TL
  PUBLIC :: CRTM_Combine_AtmOptics_AD


  ! -------------------------                                           
  ! PRIVATE Module parameters                                           
  ! -------------------------                                           

  ! -- RCS Id for the module                                            
  CHARACTER( * ),  PARAMETER, PRIVATE :: MODULE_RCS_ID = &              
  '$Id: CRTM_AtmOptics.f90 5959 2009-12-07 14:07:01Z paul.vandelst@noaa.gov $'        


  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------

  TYPE, PUBLIC :: CRTM_AOVariables_type
    PRIVATE

    REAL( fp_kind ), DIMENSION( MAX_N_LAYERS ) :: Optical_Depth = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_LAYERS ) :: bs            = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_LAYERS ) :: w             = ZERO

  END TYPE CRTM_AOVariables_type

CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!  *** USERS INSERT PRIVATE SUBPROGRAMS HERE ***


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Combine_AtmOptics
!
! PURPOSE:
!       Subroutine to combine the optical properties from AtmAbsorption,
!       CloudScatter, and AerosolScatter calculations.
!
! CATEGORY:
!       CRTM : RT Solution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Combine_AtmOptics( AtmOptics,      &  ! In/Output
!                                    AOVariables     )  ! Internal variable output
!
! IN/ OUTPUT ARGUMENTS:
!
!       AtmOptics:      Structure containing the combined atmospheric optical
!                       parameters
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmScatter_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
!       AOVariables:    Structure containing internal variables required for
!                       subsequent tangent-linear or adjoint model calls.
!                       The contents of this structure are NOT accessible
!                       outside of the CRTM_AtmOptics module.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AOVariables_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       There is no argument checking or structure allocation performed in
!       this subroutine.
!
! COMMENTS:
!       Note the INTENT on the output AtmOptics argument is IN OUT rather than
!       just OUT. This is necessary because the argument MUST be defined upon
!       input. To prevent memory leaks, and in this case errors in accessing
!       unallocated memory, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Combine_AtmOptics( AtmOptics,      &  ! Output
                                     AOV             )  ! Internal variable output


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs/Outputs
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN OUT ) :: AtmOptics

    ! -- Internal variable output
    TYPE( CRTM_AOVariables_type ),   INTENT( OUT )    :: AOV


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Combine_AtmOptics'


    ! ---------------
    ! Local variables
    ! ---------------


    INTEGER :: i, k, l


    !#--------------------------------------------------------------------------#
    !#                         -- NO SCATTERING CASE --                         #
    !#--------------------------------------------------------------------------#

    IF( AtmOptics%n_Legendre_Terms == 0 ) THEN
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- BEGIN MAIN LAYER LOOP --                      #
    !#--------------------------------------------------------------------------#

    Layer_Loop: DO k = 1, AtmOptics%n_Layers

      ! -- Save the unmodified optical parameters
      AOV%Optical_Depth(k) = AtmOptics%Optical_Depth(k)
      AOV%bs(k) = AtmOptics%Single_Scatter_Albedo(k)

      ! ------------------------------------------------------
      ! Only proceed if the total optical depth is significant
      ! ------------------------------------------------------

      Significant_Scattering: IF( AOV%bs(k) > BS_THRESHOLD ) THEN

        AOV%w(k) = AtmOptics%Single_Scatter_Albedo(k) / AtmOptics%Optical_Depth(k)
        
        DO i = 1, AtmOptics%n_Phase_Elements
          DO l = 1, AtmOptics%n_Legendre_Terms
             AtmOptics%Phase_Coefficient(l,i,k) = AtmOptics%Phase_Coefficient(l,i,k)/AtmOptics%Single_Scatter_Albedo(k)
          END DO
        ! Normalization requirement for energy conservation
             AtmOptics%Phase_Coefficient(0,i,k) = POINT_5
        END DO           
        
        AtmOptics%Delta_Truncation(k) = AtmOptics%Phase_Coefficient(AtmOptics%n_Legendre_Terms,1,k)        

          ! -----------------------------------------------------
          ! Redfine the total optical depth and single scattering
          ! albedo for the delta-function adjustment
          ! -----------------------------------------------------

          AtmOptics%Optical_Depth(k) = ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) )) * &
                                       AtmOptics%Optical_Depth(k)

          AtmOptics%Single_Scatter_Albedo(k) = ( ONE - AtmOptics%Delta_Truncation(k) ) * AOV%w(k)   / &
                                               ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) )

        END IF Significant_Scattering

    END DO Layer_Loop

  END SUBROUTINE CRTM_Combine_AtmOptics



!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Combine_AtmOptics_TL
!
! PURPOSE:
!       Subroutine to compute the tangent-linear form of the optical properties
!       from AtmAbsorption, CloudScatter, and AerosolScatter calculations.
!
! CATEGORY:
!       CRTM : AtmOptics
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Combine_AtmOptics_TL( AtmOptics,         &  ! FWD Input
!                                       AtmOptics_TL,      &  ! TL Output
!                                       AOVariables        )  ! Internal variable input
! INPUT ARGUMENTS:
!
!
!       AtmOptics:         Structure containing the combined atmospheric optical
!                          parameters
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!
!       AOVariables:       Structure containing internal forward model variables
!                          required for subsequent tangent-linear or adjoint model
!                          calls. The contents of this structure are NOT accessible
!                          outside of the CRTM_AtmOptics module.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AOVariables_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       AtmOptics_TL:      Structure containing the tangent linear combined
!                          atmospheric optical parameters
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTUPT ARGUMENTS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       There is no argument checking or structure allocation performed in
!       this subroutine.
!
! COMMENTS:
!       Note the INTENT on the output AtmOptics_TL argument is IN OUT rather than
!       just OUT. This is necessary because the argument MUST be defined upon
!       input. To prevent memory leaks, and in this case errors in accessing
!       unallocated memory, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Combine_AtmOptics_TL( AtmOptics,         &  ! FWD Input
                                        AtmOptics_TL,      &  ! TL Output
                                        AOV                )  ! Internal variable input



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#
            
    ! ---------
    ! Arguments
    ! ---------

    ! -- FWD Inputs
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN )     :: AtmOptics

    ! -- Outputs
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN OUT ) :: AtmOptics_TL

    ! -- Internal variable input
    TYPE( CRTM_AOVariables_type ),   INTENT( IN )     :: AOV


    ! ----------------
    ! Local parameters
    ! ----------------
                               
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Combine_AtmOptics_TL'

                                           
    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, k, l
    REAL( fp_kind ) :: w_TL

    !#--------------------------------------------------------------------------#
    !#                         -- BEGIN MAIN LAYER LOOP --                      #
    !#--------------------------------------------------------------------------#

    Layer_Loop: DO k = 1, AtmOptics%n_Layers

      ! ------------------------------------------------------
      ! Only proceed if the total optical depth is significant
      ! ------------------------------------------------------

      Significant_Scattering: IF( AOV%bs(k) > BS_THRESHOLD ) THEN

        w_TL = ( AtmOptics_TL%Single_Scatter_Albedo(k) / AOV%Optical_Depth(k) ) - &
                 ( AtmOptics_TL%Optical_Depth(k) * AOV%w(k) / AOV%Optical_Depth(k) )
                        
        DO i = 1, AtmOptics%n_Phase_Elements
          DO l = 1, AtmOptics%n_Legendre_Terms
             AtmOptics_TL%Phase_Coefficient(l,i,k) = ( AtmOptics_TL%Phase_Coefficient(l,i,k) &
                - AtmOptics%Phase_Coefficient(l,i,k)*AtmOptics_TL%Single_Scatter_Albedo(k) )/AOV%bs(k)             
          END DO
        ! Normalization requirement for energy conservation
             AtmOptics_TL%Phase_Coefficient(0,i,k) = ZERO
        END DO           
   
        AtmOptics_TL%Delta_Truncation(k) = AtmOptics_TL%Phase_Coefficient(AtmOptics%n_Legendre_Terms,1,k)        


          ! ----------------------------------------------------------
          ! Redefine the tangent-linear total optical depth and
          ! single scattering albedo for the delta-function adjustment
          !
          ! The expressions below are ordered to make the adjoint
          ! form easy to determine from the TL form.
          ! ----------------------------------------------------------

          ! -- The optical depth
          ! --
          ! -- tau = ( 1 - d.w ) . tau
          ! --
          ! -- so,
          ! --
          ! -- tau_TL = ( 1 - d.w ).tau_TL - d.tau.w_TL - w.tau.d_TL
          ! --
          ! -- Note that the optical depth from the AOV structure is
          ! -- used on the RHS of these expressions.

          AtmOptics_TL%Optical_Depth(k) = &
            ( ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) ) * AtmOptics_TL%Optical_Depth(k) ) - &
            ( AtmOptics%Delta_Truncation(k) * AOV%Optical_Depth(k) * w_TL ) - &
            ( AOV%w(k) * AOV%Optical_Depth(k) * AtmOptics_TL%Delta_Truncation(k) )


          ! -- The single scatter albedo, SSA
          ! --
          ! --        (1 - d).w
          ! -- SSA = -----------
          ! --         1 - d.w
          ! --
          ! -- so,
          ! --
          ! --           ( 1 - d + SSA.d )              ( SSA - 1 ).w
          ! -- SSA_TL = ------------------- . w_TL  +  --------------- . d_TL
          ! --               1 - d.w                       1 - d.w

          AtmOptics_TL%Single_Scatter_Albedo(k) = &
            ( ( ( ONE - AtmOptics%Delta_Truncation(k) + &
                  ( AtmOptics%Single_Scatter_Albedo(k)*AtmOptics%Delta_Truncation(k) ) ) * w_TL ) + &
              ( ( AtmOptics%Single_Scatter_Albedo(k) - ONE ) * AOV%w(k) * AtmOptics_TL%Delta_Truncation(k) ) ) / &
            ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) )

        END IF Significant_Scattering


    END DO Layer_Loop
                                      
  END SUBROUTINE CRTM_Combine_AtmOptics_TL



!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Combine_AtmOptics_AD
!
! PURPOSE:
!       Subroutine to compute the adjoint form of the optical properties
!       from AtmAbsorption, CloudScatter, and AerosolScatter calculations.
!
! CATEGORY:
!       CRTM : AtmOptics
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Combine_AtmOptics_AD( AtmOptics,         &  ! FWD Input
!                                       AtmOptics_AD,      &  ! AD Input
!                                       RTVariables        )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       AtmOptics:         Structure containing the combined atmospheric optical
!                          parameters
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AOVariables:       Structure containing internal forward model variables
!                          required for subsequent tangent-linear or adjoint model
!                          calls. The contents of this structure are NOT accessible
!                          outside of the CRTM_AtmOptics module.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AOVariables_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! IN/OUTPUT
!       AtmOptics_AD:      Structure containing the combined adjoint atmospheric
!                          optical parameters.
!                          NOTE: The components of this structures are all zeroed
!                                upon exit from this routine.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
!
! OPTIONAL OUTUPT ARGUMENTS:
!       None.
!
! SIDE EFFECTS:
!       The input AtmOptics_AD structure components are zeroed upon exit.
!
! RESTRICTIONS:
!       There is no argument checking or structure allocation performed in
!       this subroutine.
!
! COMMENTS:
!       Note the INTENT on the output adjoint arguments is IN OUT rather than
!       just OUT. This is necessary because the arguments MUST be defined upon
!       input. To prevent memory leaks, and in this case errors in accessing
!       unallocated memory, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Combine_AtmOptics_AD( AtmOptics,         &  ! FWD Input
                                        AtmOptics_AD,      &  ! AD Input
                                        AOV                )  ! Internal variable input

    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#
            
    ! ---------
    ! Arguments
    ! ---------

    ! -- FWD Inputs

    TYPE( CRTM_AtmScatter_type ),    INTENT( IN )     :: AtmOptics

    ! -- AD Inputs
    TYPE( CRTM_AtmScatter_type ),    INTENT( IN OUT ) :: AtmOptics_AD


    ! -- Internal variable input
    TYPE( CRTM_AOVariables_type ),   INTENT( IN )     :: AOV


    ! ----------------
    ! Local parameters
    ! ----------------
                               
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Combine_AtmOptics_AD'

                                           
    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, k, l
    REAL( fp_kind ) :: w_AD
    REAL( fp_kind ) :: bs_AD

    !#--------------------------------------------------------------------------#
    !#                          -- NO SCATTERING CASE --                        #
    !#--------------------------------------------------------------------------#

    IF( AtmOptics%n_Legendre_Terms == 0 ) RETURN


    !#--------------------------------------------------------------------------#
    !#                         -- BEGIN MAIN LAYER LOOP --                      #
    !#--------------------------------------------------------------------------#
                                            
    Layer_Loop: DO k = AtmOptics%n_Layers, 1, -1


      ! -----------------------------------------------------
      ! Initialise local, layer independent adjoint variables
      ! -----------------------------------------------------

      w_AD          = ZERO
                    
      ! ------------------------------------------------------
      ! Only proceed if the scattering is significant
      ! ------------------------------------------------------
               
                                           
        Significant_Scattering: IF( AOV%bs(k) > BS_THRESHOLD) THEN


          ! ---------------------------------------------------
          ! Compute the adjoint total optical depth and single
          ! scattering albedo for the delta function adjustment
          ! ---------------------------------------------------

          ! -- The tangent-linear single scatter albedo, SSA_TL
          ! --
          ! --             ( 1 - d + SSA.d )              ( SSA - 1 ).w
          ! --   SSA_TL = ------------------- . w_TL  +  --------------- . d_TL
          ! --                  1 - d.w                      1 - d.w
          ! --
          ! -- so,
          ! --                   ( SSA - 1 ).w
          ! --   d_AD = d_AD + ---------------- . SSA_AD
          ! --                      1 - d.w
          ! --
          ! --                  ( 1 - d + SSA.d ) 
          ! --   w_AD = w_AD + ------------------- . SSA_AD
          ! --                       1 - d.w     
          ! --
          ! --   SSA_AD = 0

          AtmOptics_AD%Delta_Truncation(k) = AtmOptics_AD%Delta_Truncation(k) + &
            ( ( AtmOptics%Single_Scatter_Albedo(k) - ONE ) * AOV%w(k) * AtmOptics_AD%Single_Scatter_Albedo(k) / &
              ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) ) )

          w_AD = w_AD + ( ( ONE - AtmOptics%Delta_Truncation(k) + &
                            ( AtmOptics%Single_Scatter_Albedo(k)*AtmOptics%Delta_Truncation(k) ) ) * &
                          AtmOptics_AD%Single_Scatter_Albedo(k) / &
                          ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) ) )

          AtmOptics_AD%Single_Scatter_Albedo(k) = ZERO


          ! -- The tangent-linear optical depth, tau_TL
          ! --
          ! --   tau_TL = ( 1 - d.w ).tau_TL - d.tau.w_TL - w.tau.d_TL
          ! --
          ! -- so,
          ! --
          ! --   d_AD = d_AD - w.tau.tau_AD
          ! --
          ! --   w_AD = w_AD - d.tau.tau_AD
          ! --
          ! --   tau_AD = ( 1 - d.w ).tau_AD
          ! --
          ! -- Note that the optical depth from the AOV structure is
          ! -- used on the RHS of the above expressions.

          AtmOptics_AD%Delta_Truncation(k) = AtmOptics_AD%Delta_Truncation(k) - &
            ( AOV%w(k)                    * &  ! w
              AOV%Optical_Depth(k)        * &  ! tau
              AtmOptics_AD%Optical_Depth(k) )  ! tau_AD

          w_AD = w_AD - ( AtmOptics%Delta_Truncation(k) * &  ! d
                          AOV%Optical_Depth(k)          * &  ! tau
                          AtmOptics_AD%Optical_Depth(k)   )  ! tau_AD

          AtmOptics_AD%Optical_Depth(k) = ( ONE - ( AtmOptics%Delta_Truncation(k) * AOV%w(k) ) ) * &
                                          AtmOptics_AD%Optical_Depth(k)


            ! -- Delta truncation adjoint
            L = AtmOptics%n_Legendre_Terms
            AtmOptics_AD%Phase_Coefficient(L,1,k) = AtmOptics_AD%Phase_Coefficient(L,1,k) + &
                                                    AtmOptics_AD%Delta_Truncation(k)
 
            AtmOptics_AD%Delta_Truncation(k) = ZERO

            DO i = 1, AtmOptics%n_Phase_Elements
        ! Normalization requirement for energy conservation
              AtmOptics_AD%Phase_Coefficient(0,i,k) = ZERO
              DO l = 1, AtmOptics%n_Legendre_Terms
                 AtmOptics_AD%Single_Scatter_Albedo(k) = AtmOptics_AD%Single_Scatter_Albedo(k)  &
                   - AtmOptics%Phase_Coefficient(l,i,k)*AtmOptics_AD%Phase_Coefficient(l,i,k)/AOV%bs(k) 
                 AtmOptics_AD%Phase_Coefficient(l,i,k) = ( AtmOptics_AD%Phase_Coefficient(l,i,k)/AOV%bs(k) )
              END DO
            END DO           

            AtmOptics_AD%Single_Scatter_Albedo(k) = AtmOptics_AD%Single_Scatter_Albedo(k)  &
                   + w_AD/ AOV%Optical_Depth(k)
            
            AtmOptics_AD%Optical_Depth(k) = AtmOptics_AD%Optical_Depth(k) - w_AD*AOV%w(k) / AOV%Optical_Depth(k)


        END IF Significant_Scattering
 

    END DO Layer_Loop


  END SUBROUTINE CRTM_Combine_AtmOptics_AD

END MODULE CRTM_AtmOptics


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_AtmOptics.f90 5959 2009-12-07 14:07:01Z paul.vandelst@noaa.gov $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 5959 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_AtmOptics.f90,v $
! Revision 1.14  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.13  2006/02/07 15:06:51  paulv
! - Added no scattering case check for when the number of Legendre terms is
!   zero. All components simply assign the required output and return.
!
! Revision 1.12  2005/10/18 12:08:51  paulv
! - Added initialisation of output adjoint structure members to the AD routine.
!   Strictly this should be done in the calling code, but doing it here
!   minimises code duplication (i.e. adjoint and K-matrix module.)
!
! Revision 1.11  2005/10/12 21:32:56  paulv
! - Corrected bug in the TL and AD single scatter albedo codes.
!   The tangent-linear form was written as
!
!                ( 1 - d + SSA )              ( SSA - 1 ).w
!      SSA_TL = ----------------- . w_TL  +  --------------- . d_TL
!                    1 - d.w                    1 - d.w
!
!   but it should have been --,
!                             |
!               ( 1 - d + SSA.d )              ( SSA - 1 ).w
!      SSA_TL = ------------------- . w_TL  +  --------------- . d_TL
!                     1 - d.w                      1 - d.w
!
!   The relevant adjoint line was changed from
!
!                     ( 1 - d + SSA )
!      w_AD = w_AD + ----------------- . SSA_AD
!                         1 - d.w
!
!   to -----------------------------,
!                                   |
!                     ( 1 - d + SSA.d )
!      w_AD = w_AD + ------------------- . SSA_AD
!                          1 - d.w
!
! Revision 1.10  2005/10/12 17:20:19  paulv
! - Corrected bug in name of internal AOV variable type definition.
! - Added various missing local variable declarations.
! - Corrected bug in continuation character specification.
! - Corrected bs reference to AOV%bs(k).
! - Corrected references to Phase_Coefficient in various AtmScatter
!   functions.
!
! Revision 1.9  2005/10/06 22:06:55  paulv
! - Updated documentation so it was in sync with the code.
! - Modified order of TL and AD arguments to be consistent with FWD model.
! - Added explanatory comments.
! - Added internal variable structure definition (AOVariables_type) for
!   forward variables that are required in the TL and AD calcs.
! - Added internal variable argument to FWD (output) and TL, AD (input)
!   routines.
! - Modifed FWD, TL, and AD Routines to use internal variable structure.
!   All temporary arrays for this purpose have been removed.
!
! Revision 1.8  2005/10/03 15:35:03  qliu
! -- Revised for efficiency, change AtmOptics as IN OUT.
!
! Revision 1.5  2005/08/17 21:15:09  qliu
! -- Deleted "USE CRTM_CloudScatter, ONLY : HGphase".
!
! Revision 1.4  2005/08/16 18:38:18  qliu
! - Converted from DOS to Unix file format.
!
! Revision 1.3  2005/08/16 16:30:35  qliu
! - Added header documentation for TL and AD routines.
!
! Revision 1.2  2005/08/04 20:31:31  paulv
! - Updated the forward routine, CRTM_Combine_AtmOptics. Untested. TL and AD
!   forms are unchanged.
!
! Revision 1.1  2005/07/15 16:36:25  paulv
! Initial checkin. Incomplete.
!
!
!

