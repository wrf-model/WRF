      subroutine gf_unpack2(cgrib,lcgrib,iofst,lencsec2,csec2,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    gf_unpack2 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2002-04-09
!
! ABSTRACT: This subroutine unpacks Section 2 (Local Use Section)
!           as defined in GRIB Edition 2.
!
! PROGRAM HISTORY LOG:
! 2002-04-09  Gilbert
!
! USAGE:    CALL gf_unpack2(cgrib,lcgrib,iofst,lencsec2,csec2,ierr)
!   INPUT ARGUMENT LIST:
!     cgrib    - Character array containing Section 2 of the GRIB2 message
!     lcgrib   - Length (in bytes) of GRIB message array cgrib.
!     iofst    - Bit offset of the beginning of Section 2.
!
!   OUTPUT ARGUMENT LIST:      
!     iofst    - Bit offset at the end of Section 2, returned.
!     lencsec2 - Length (in octets) of Local Use data
!     csec2()  - Pointer to a character*1 array containing local use data
!     ierr     - Error return code.
!                0 = no error
!                2 = Array passed is not section 2
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
      integer,intent(in) :: lcgrib
      integer,intent(inout) :: iofst
      integer,intent(out) :: lencsec2
      integer,intent(out) :: ierr
      character(len=1),pointer,dimension(:) :: csec2

      ierr=0
      lencsec2=0
      nullify(csec2)

      call g2lib_gbyte(cgrib,lensec,iofst,32)        ! Get Length of Section
      iofst=iofst+32    
      lencsec2=lensec-5
      call g2lib_gbyte(cgrib,isecnum,iofst,8)         ! Get Section Number
      iofst=iofst+8     
      ipos=(iofst/8)+1

      if ( isecnum.ne.2 ) then
         ierr=6
         print *,'gf_unpack2: Not Section 2 data. '
         return
      endif

      allocate(csec2(lencsec2),stat=istat)
      if (istat.ne.0) then
         ierr=6
         nullify(csec2)
         return
      endif
      
      csec2(1:lencsec2)=cgrib(ipos:ipos+lencsec2-1)
      iofst=iofst+(lencsec2*8)

      return    ! End of Section 2 processing
      end

