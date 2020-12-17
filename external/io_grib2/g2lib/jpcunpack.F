      subroutine jpcunpack(cpack,len,idrstmpl,ndpts,fld)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    jpcunpack
!   PRGMMR: Gilbert          ORG: W/NP11    DATE: 2002-12-17
!
! ABSTRACT: This subroutine unpacks a data field that was packed into a 
!   JPEG2000 code stream
!   using info from the GRIB2 Data Representation Template 5.40 or 5.40000.
!
! PROGRAM HISTORY LOG:
! 2002-12-17  Gilbert
!
! USAGE:    CALL jpcunpack(cpack,len,idrstmpl,ndpts,fld)
!   INPUT ARGUMENT LIST:
!     cpack    - The packed data field (character*1 array)
!     len      - length of packed field cpack().
!     idrstmpl - Contains the array of values for Data Representation
!                Template 5.40 or 5.40000
!     ndpts    - The number of data values to unpack
!
!   OUTPUT ARGUMENT LIST:
!     fld()    - Contains the unpacked data values
!
! REMARKS: None
!
! ATTRIBUTES:
!   LANGUAGE: XL Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      character(len=1),intent(in) :: cpack(len)
      integer,intent(in) :: ndpts,len
      integer,intent(in) :: idrstmpl(*)
      real,intent(out) :: fld(ndpts)

      integer :: ifld(ndpts)
      integer(4) :: ieee
      real :: ref,bscale,dscale
      integer :: dec_jpeg2000

      ieee = idrstmpl(1)
      call rdieee(ieee,ref,1)
      bscale = 2.0**real(idrstmpl(2))
      dscale = 10.0**real(-idrstmpl(3))
      nbits = idrstmpl(4)
!
!  if nbits equals 0, we have a constant field where the reference value
!  is the data value at each gridpoint
!
      if (nbits.ne.0) then
!         call g2lib_gbytes(cpack,ifld,0,nbits,0,ndpts)
         iret=dec_jpeg2000(cpack,len,ifld)
         do j=1,ndpts
           fld(j)=((real(ifld(j))*bscale)+ref)*dscale
         enddo
      else
         do j=1,ndpts
           fld(j)=ref
         enddo
      endif


      return
      end
