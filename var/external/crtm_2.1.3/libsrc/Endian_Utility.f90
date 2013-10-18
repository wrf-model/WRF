!--------------------------------------------------------------------------------
!M+
! NAME:
!       Endian_Utility
!
! PURPOSE:
!       Module containing functions to byte-swap intrinsic data types.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Endian_Utility
!
! MODULES:
!       Type_Kinds:   Module to hold specification kinds for variable
!                     declaration.
!
! CONTAINS:
!       Big_Endian:   Logical function that returns .TRUE. if platform
!                     is big endian.
!
!       Swap_Endian:  Function that byte-swaps input arguments.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!         Written by:   Paul van Delst, CIMSS/SSEC, 17-Mar-2000
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000 Paul van Delst
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

MODULE Endian_Utility


  ! ----------
  ! Module use
  ! ----------
 
  USE Type_Kinds

 
  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Big_Endian
  PUBLIC :: Swap_Endian

          
  ! ------------------
  ! Overload interface
  ! ------------------

  INTERFACE Swap_Endian
    MODULE PROCEDURE Swap_Short_Integer
    MODULE PROCEDURE Swap_Long_Integer
    MODULE PROCEDURE Swap_LLong_Integer
    MODULE PROCEDURE Swap_Single_Float
    MODULE PROCEDURE Swap_Double_Float
    MODULE PROCEDURE Swap_Single_Complex
    MODULE PROCEDURE Swap_Double_Complex
  END INTERFACE Swap_Endian


CONTAINS


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
!       Big_Endian
!
! PURPOSE:
!       Function to determine if current platform is big-endian.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Result = Big_Endian()
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Result:    The return value is a logical value indicating whether
!                  the current platform is big-endian or not
!                  .TRUE.  - it is a big-endian platform.
!                  .FALSE. - it is NOT a big-endian platform.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!       Uses the Fortran90/95 intrinsics TRANSFER and IACHAR to test
!       if a 2-byte integer (value 1) retains that value when
!       transferred to a single-byte character representation. If
!       it does, the platform is little-endian. If not, it is big-
!       endian. This method was suggested by Clive Page, University
!       of Leicester, UK.
!
! EXAMPLE:
!       USE Endian_Utility
!         .....
!       WRITE( *, '( 5x, "Platform is " )', ADVANCE = 'NO' )
!       IF ( Big_Endian() ) THEN
!         WRITE( *, '( "big-endian." )' )
!       ELSE
!         WRITE( *, '( "litle-endian." )' )
!       END IF
!
!S-
!--------------------------------------------------------------------------------

  FUNCTION Big_Endian()


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER( Short ) :: Source = 1_Short


    ! ------------
    ! The function
    ! ------------

    LOGICAL :: Big_Endian


    ! ----------
    ! Intrinsics
    ! ----------

    INTRINSIC TRANSFER, ICHAR


    ! ----------------------------------
    ! Initialise result to little-endian
    ! ----------------------------------

    Big_Endian = .FALSE.


    ! ------------------------------------------------------------
    ! Test for "endian-ness".
    !
    ! TRANSFER( source, 'a' ) returns a result with the physical
    !   representation of the number 1, i.e. an integer, but
    !   interpreted as a character (the type of 'a' - a character,
    !   not the value, is what is important).
    !
    ! IACHAR returns the position of a character in the ASCII
    !   collating sequence associated with the kind type parameter
    !   of the character.
    ! ------------------------------------------------------------

    IF ( IACHAR( TRANSFER( Source, 'a' ) ) == 0 ) Big_Endian = .TRUE.

  END FUNCTION Big_Endian





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Swap_Endian
!
! PURPOSE:
!       Function to byte-swap input data.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Result = Swap_Endian( Input )
!
! INPUT ARGUMENTS:
!       Input:       Data object to be byte swapped.
!                    UNITS:      N/A
!                    TYPE:       Any of the following:
!                                  INTEGER( Short )
!                                  INTEGER( Long  )  [ == default integer]
!                                  INTEGER( LLong )
!                                  REAL( Single )   [ == default real]
!                                  REAL( Double )
!                                  COMPLEX( Single )
!                                  COMPLEX( Double )
!                    DIMENSION:  Scalar, or any allowed rank array.
!                    ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Result:      The return value is the byte swapped value
!                    UNITS:      N/A
!                    TYPE:       Same as Input
!                    DIMENSION:  Same as Input
!
! CALLS:
!       None.
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The TRANSFER intrinsic is used to rearrange the bytes by accessing
!       the data with a subscript triplet having a negative stride.
!
!       This method can be slow, not because of the TRANSFER function itself,
!       but the negative stride of the array access. It depends on the 
!       quality of implementation.
!
!       The byte-swap for the complex data types are only slightly
!       different in that each half of the total number representation
!       in bytes is swapped rather than the whole thing (i.e. real and
!       imaginary are swapped separately).
!
!S-
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Swap_Short_Integer ( Input ) RESULT ( Output )


    ! -------------------
    ! Argument and result
    ! -------------------

    INTEGER( Short ), INTENT( IN ) :: Input
    INTEGER( Short )               :: Output


    ! ----------------
    ! Local parameters
    ! ----------------

    INTEGER, PARAMETER :: N = n_Bytes_Short


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER( Byte ), DIMENSION( N ) :: Byte_Equivalent


    ! ------------------------------------------------
    ! Byte swap the data. The extra step in the middle
    ! is necessary for those compilers that can't
    ! handle a negative strided input to TRANSFER
    ! ------------------------------------------------

    Byte_Equivalent = TRANSFER( Input, Byte_Equivalent )
    Byte_Equivalent = Byte_Equivalent( N:1:-1 )
    Output          = TRANSFER( Byte_Equivalent, Output )

  END FUNCTION Swap_Short_Integer


  ELEMENTAL FUNCTION Swap_Long_Integer ( Input ) RESULT ( Output )


    ! -------------------
    ! Argument and result
    ! -------------------

    INTEGER( Long ), INTENT( IN ) :: Input
    INTEGER( Long )               :: Output


    ! ----------------
    ! Local parameters
    ! ----------------

    INTEGER, PARAMETER :: N = n_Bytes_Long


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER( Byte ), DIMENSION( N ) :: Byte_Equivalent


    ! ------------------------------------------------
    ! Byte swap the data. The extra step in the middle
    ! is necessary for those compilers that can't
    ! handle a negative strided input to TRANSFER
    ! ------------------------------------------------

    Byte_Equivalent = TRANSFER( Input, Byte_Equivalent )
    Byte_Equivalent = Byte_Equivalent( N:1:-1 )
    Output          = TRANSFER( Byte_Equivalent, Output )

  END FUNCTION Swap_Long_Integer


  ELEMENTAL FUNCTION Swap_LLong_Integer ( Input ) RESULT ( Output )


    ! -------------------
    ! Argument and result
    ! -------------------

    INTEGER( LLong ), INTENT( IN ) :: Input
    INTEGER( LLong )               :: Output


    ! ----------------
    ! Local parameters
    ! ----------------

    INTEGER, PARAMETER :: N = n_Bytes_LLong


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER( Byte ), DIMENSION( N ) :: Byte_Equivalent


    ! ------------------------------------------------
    ! Byte swap the data. The extra step in the middle
    ! is necessary for those compilers that can't
    ! handle a negative strided input to TRANSFER
    ! ------------------------------------------------

    Byte_Equivalent = TRANSFER( Input, Byte_Equivalent )
    Byte_Equivalent = Byte_Equivalent( N:1:-1 )
    Output          = TRANSFER( Byte_Equivalent, Output )

  END FUNCTION Swap_LLong_Integer


  ELEMENTAL FUNCTION Swap_Single_Float ( Input ) RESULT ( Output )


    ! -------------------
    ! Argument and result
    ! -------------------

    REAL( Single ), INTENT( IN ) :: Input
    REAL( Single )               :: Output


    ! ----------------
    ! Local parameters
    ! ----------------

    INTEGER, PARAMETER :: N = n_Bytes_Single


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER( Byte ), DIMENSION( N ) :: Byte_Equivalent


    ! ------------------------------------------------
    ! Byte swap the data. The extra step in the middle
    ! is necessary for those compilers that can't
    ! handle a negative strided input to TRANSFER
    ! ------------------------------------------------

    Byte_Equivalent = TRANSFER( Input, Byte_Equivalent )
    Byte_Equivalent = Byte_Equivalent( N:1:-1 )
    Output          = TRANSFER( Byte_Equivalent, Output )

  END FUNCTION Swap_Single_Float


  ELEMENTAL FUNCTION Swap_Double_Float ( Input ) RESULT ( Output )


    ! -------------------
    ! Argument and result
    ! -------------------

    REAL( Double ), INTENT( IN ) :: Input
    REAL( Double )               :: Output


    ! ----------------
    ! Local parameters
    ! ----------------

    INTEGER, PARAMETER :: N = n_Bytes_Double


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER( Byte ), DIMENSION( N ) :: Byte_Equivalent


    ! ------------------------------------------------
    ! Byte swap the data. The extra step in the middle
    ! is necessary for those compilers that can't
    ! handle a negative strided input to TRANSFER
    ! ------------------------------------------------

    Byte_Equivalent = TRANSFER( Input, Byte_Equivalent )
    Byte_Equivalent = Byte_Equivalent( N:1:-1 )
    Output          = TRANSFER( Byte_Equivalent, Output )

  END FUNCTION Swap_Double_Float


  ELEMENTAL FUNCTION Swap_Single_Complex ( Input ) RESULT ( Output )


    ! -------------------
    ! Argument and result
    ! -------------------

    COMPLEX( Single ), INTENT( IN ) :: Input
    COMPLEX( Single )               :: Output


    ! ------------------
    ! Byte-swap the data
    ! ------------------

    Output = CMPLX( Swap_Endian( REAL( Input,          Single ) ), &   ! Real
                    Swap_Endian( REAL( AIMAG( Input ), Single ) ), &   ! Imaginary
                    Single )

  END FUNCTION Swap_Single_Complex


  ELEMENTAL FUNCTION Swap_Double_Complex ( Input ) RESULT ( Output )


    ! -------------------
    ! Argument and result
    ! -------------------

    COMPLEX( Double ), INTENT( IN ) :: Input
    COMPLEX( Double )               :: Output


    ! ------------------
    ! Byte-swap the data
    ! ------------------

    Output = CMPLX( Swap_Endian( REAL( Input,          Double ) ), &   ! Real
                    Swap_Endian( REAL( AIMAG( Input ), Double ) ), &   ! Imaginary
                    Double )


  END FUNCTION Swap_Double_Complex

END MODULE Endian_Utility



!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Endian_Utility.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $
!
! $Date: 2004/12/01 19:35:15 $
!
! $Revision: 29405 $
!
! $State: Exp $
!
! $Name:  $
!
! $Log: Endian_Utility.f90,v $
! Revision 2.3  2004/12/01 19:35:15  paulv
! - Documentation errors corrected.
!
! Revision 2.2  2004/08/17 14:36:24  paulv
! - Changed the comment header for the actual byte-swapping code.
! - Note the log message for the last update is incorrect - the negative
!   stride must be removed from the input argument to TRANSFER to prevent
!   run-time crashes when versions of the xlf 8.1 compiler are used.
!
! Revision 2.1  2004/08/16 16:04:22  paulv
! - Due to an IBM xlf compiler bug I changed
!     Output = TRANSFER( Byte_Equivalent( N:1:-1 ), Output )
!   to
!     Byte_Equivalent = Byte_Equivalent( N:1:-1 )
!     Output = TRANSFER( (Byte_Equivalent(N:1:-1), Output )
!   The negative stride triplet used in the input to TRANSFER was causing
!   a Trace/BPT fault on the IBM (crashed the debugger too!).
!
! Revision 2.0  2004/08/12 22:23:16  paulv
! - New version.
! - Swap_Endian functions are now ELEMENTAL and accept any rank array input.
!
! Revision 1.3  2004/07/01 17:44:25  paulv
! - Repository resync. Last modified Nov 8, 2001.
!
! Revision 1.2  2000/06/02 16:26:12  paulv
! Removed 8-byte integer swapping routines. This data type is not generally
! supported so the functionality was removed.
!
! Revision 1.1  2000/04/03 14:48:43  paulv
! Initial checked in version
!
!
