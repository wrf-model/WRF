      subroutine gf_unpack6(cgrib,lcgrib,iofst,ngpts,ibmap,bmap,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    gf_unpack6 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-26
!
! ABSTRACT: This subroutine unpacks Section 6 (Bit-Map Section)
!   starting at octet 6 of that Section.  
!
! PROGRAM HISTORY LOG:
! 2000-05-26  Gilbert
! 2002-01-24  Gilbert  - Changed to dynamically allocate arrays
!                        and to pass pointers to those arrays through
!                        the argument list.
!
! USAGE:    CALL gf_unpack6(cgrib,lcgrib,iofst,ngpts,ibmap,bmap,ierr)
!   INPUT ARGUMENT LIST:
!     cgrib    - Character array that contains the GRIB2 message
!     lcgrib   - Length (in bytes) of GRIB message array cgrib.
!     iofst    - Bit offset of the beginning of Section 6.
!     ngpts    - Number of grid points specified in the bit-map
!
!   OUTPUT ARGUMENT LIST:      
!     iofst    - Bit offset at the end of Section 6, returned.
!     ibmap    - Bitmap indicator ( see Code Table 6.0 )
!                0 = bitmap applies and is included in Section 6.
!                1-253 = Predefined bitmap applies
!                254 = Previously defined bitmap applies to this field
!                255 = Bit map does not apply to this product.
!     bmap()   - Pointer to a logical*1 array containing decoded bitmap. 
!                ( if ibmap=0 )
!     ierr     - Error return code.
!                0 = no error
!                4 = Unrecognized pre-defined bit-map.
!                6 = memory allocation error
!
! REMARKS: None
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      character(len=1),intent(in) :: cgrib(lcgrib)
      integer,intent(in) :: lcgrib,ngpts
      integer,intent(inout) :: iofst
      integer,intent(out) :: ibmap
      integer,intent(out) :: ierr
      logical*1,pointer,dimension(:) :: bmap

      integer :: intbmap(ngpts)

      ierr=0
      nullify(bmap)

      iofst=iofst+32    ! skip Length of Section
      iofst=iofst+8     ! skip section number

      call g2lib_gbyte(cgrib,ibmap,iofst,8)    ! Get bit-map indicator
      iofst=iofst+8

      if (ibmap.eq.0) then               ! Unpack bitmap
         istat=0
         if (ngpts.gt.0) allocate(bmap(ngpts),stat=istat)
         if (istat.ne.0) then
            ierr=6
            nullify(bmap)
            return
         endif
         call g2lib_gbytes(cgrib,intbmap,iofst,1,0,ngpts)
         iofst=iofst+ngpts
         do j=1,ngpts
           bmap(j)=.true.
           if (intbmap(j).eq.0) bmap(j)=.false.
         enddo
!      elseif (ibmap.eq.254) then               ! Use previous bitmap
!        return
!      elseif (ibmap.eq.255) then               ! No bitmap in message
!        bmap(1:ngpts)=.true.
!      else
!        print *,'gf_unpack6: Predefined bitmap ',ibmap,' not recognized.'
!        ierr=4
      endif
      
      return    ! End of Section 6 processing
      end

