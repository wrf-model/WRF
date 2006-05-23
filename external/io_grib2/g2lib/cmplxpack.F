      subroutine cmplxpack(fld,ndpts,idrsnum,idrstmpl,cpack,lcpack)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    cmplxpack
!   PRGMMR: Gilbert          ORG: W/NP11    DATE: 2004-08-27
!
! ABSTRACT: This subroutine packs up a data field using a complex
!   packing algorithm as defined in the GRIB2 documention.  It
!   supports GRIB2 complex packing templates with or without
!   spatial differences (i.e. DRTs 5.2 and 5.3).
!   It also fills in GRIB2 Data Representation Template 5.2 or 5.3 
!   with the appropriate values.
!
! PROGRAM HISTORY LOG:
! 2004-08-27  Gilbert
!
! USAGE:    CALL cmplxpack(fld,ndpts,idrsnum,idrstmpl,cpack,lcpack)
!   INPUT ARGUMENT LIST:
!     fld()    - Contains the data values to pack
!     ndpts    - The number of data values in array fld()
!     idrsnum  - Data Representation Template number 5.N
!                Must equal 2 or 3.
!     idrstmpl - Contains the array of values for Data Representation
!                Template 5.2 or 5.3
!                (1) = Reference value - ignored on input
!                (2) = Binary Scale Factor
!                (3) = Decimal Scale Factor
!                    .
!                    .
!                (7) = Missing value management
!                (8) = Primary missing value
!                (9) = Secondary missing value
!                    .
!                    .
!               (17) = Order of Spatial Differencing  ( 1 or 2 )
!                    .
!                    .
!
!   OUTPUT ARGUMENT LIST: 
!     idrstmpl - Contains the array of values for Data Representation
!                Template 5.3
!                (1) = Reference value - set by compack routine.
!                (2) = Binary Scale Factor - unchanged from input
!                (3) = Decimal Scale Factor - unchanged from input
!                    .
!                    .
!     cpack    - The packed data field (character*1 array)
!     lcpack   - length of packed field cpack().
!
! REMARKS: None
!
! ATTRIBUTES:
!   LANGUAGE: XL Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      integer,intent(in) :: ndpts,idrsnum
      real,intent(in) :: fld(ndpts)
      character(len=1),intent(out) :: cpack(*)
      integer,intent(inout) :: idrstmpl(*)
      integer,intent(out) :: lcpack

      

      if ( idrstmpl(7) .eq. 0 ) then       ! No internal missing values
         call compack(fld,ndpts,idrsnum,idrstmpl,cpack,lcpack)
      elseif ( idrstmpl(7).eq.1 .OR. idrstmpl(7).eq.2) then
         call misspack(fld,ndpts,idrsnum,idrstmpl,cpack,lcpack)
      else
         print *,'cmplxpack: Don:t recognize Missing value option.'
         lcpack=-1
      endif

      return
      end
