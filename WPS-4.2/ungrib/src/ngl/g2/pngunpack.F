      subroutine pngunpack(cpack,len,idrstmpl,ndpts,fld)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    pngunpack
!   PRGMMR: Gilbert          ORG: W/NP11    DATE: 2000-06-21
!
! ABSTRACT: This subroutine unpacks a data field that was packed into a
!   PNG image format 
!   using info from the GRIB2 Data Representation Template 5.41 or 5.40010.
!
! PROGRAM HISTORY LOG:
! 2000-06-21  Gilbert
!
! USAGE:    CALL pngunpack(cpack,len,idrstmpl,ndpts,fld)
!   INPUT ARGUMENT LIST:
!     cpack    - The packed data field (character*1 array)
!     len      - length of packed field cpack().
!     idrstmpl - Contains the array of values for Data Representation
!                Template 5.41 or 5.40010
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
      character(len=1),allocatable :: ctemp(:)
      integer(4) :: ieee
      real :: ref,bscale,dscale
      integer :: dec_png,width,height
#ifdef USE_PNG

      ieee = idrstmpl(1)
      call rdieee(ieee,ref,1)
      bscale = 2.0**real(idrstmpl(2))
      dscale = 10.0**real(-idrstmpl(3))
      nbits = idrstmpl(4)
      itype = idrstmpl(5)
!
!  if nbits equals 0, we have a constant field where the reference value
!  is the data value at each gridpoint
!
      if (nbits.ne.0) then
         allocate(ctemp(ndpts*4))
         iret=dec_png(cpack,width,height,ctemp)
         call gbytes(ctemp,ifld,0,nbits,0,ndpts)
         deallocate(ctemp)
         do j=1,ndpts
           fld(j)=((real(ifld(j))*bscale)+ref)*dscale
         enddo
      else
         do j=1,ndpts
           fld(j)=ref
         enddo
      endif


#endif /* USE_PNG */
      return
      end
