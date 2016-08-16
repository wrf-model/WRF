      subroutine gf_unpack1(cgrib,lcgrib,iofst,ids,idslen,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    gf_unpack1 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-26
!
! ABSTRACT: This subroutine unpacks Section 1 (Identification Section)
!   starting at octet 6 of that Section.  
!
! PROGRAM HISTORY LOG:
! 2000-05-26  Gilbert
! 2002-01-24  Gilbert  - Changed to dynamically allocate arrays
!                        and to pass pointers to those arrays through
!                        the argument list.
!
! USAGE:    CALL gf_unpack1(cgrib,lcgrib,iofst,ids,idslen,ierr)
!   INPUT ARGUMENT LIST:
!     cgrib    - Character array containing Section 1 of the GRIB2 message
!     lcgrib   - Length (in bytes) of GRIB message array cgrib.
!     iofst    - Bit offset of the beginning of Section 1.
!
!   OUTPUT ARGUMENT LIST:      
!     iofst    - Bit offset at the end of Section 1, returned.
!     ids      - Pointer to integer array containing information read from 
!                Section 1, the Identification section.
!            ids(1)  = Identification of originating Centre
!                                 ( see Common Code Table C-1 )
!            ids(2)  = Identification of originating Sub-centre
!            ids(3)  = GRIB Master Tables Version Number
!                                 ( see Code Table 1.0 )
!            ids(4)  = GRIB Local Tables Version Number
!                                 ( see Code Table 1.1 )
!            ids(5)  = Significance of Reference Time (Code Table 1.2)
!            ids(6)  = Year ( 4 digits )
!            ids(7)  = Month
!            ids(8)  = Day
!            ids(9)  = Hour
!            ids(10)  = Minute
!            ids(11)  = Second
!            ids(12)  = Production status of processed data
!                                 ( see Code Table 1.3 )
!            ids(13)  = Type of processed data ( see Code Table 1.4 )
!     idslen   - Number of elements in ids().
!     ierr     - Error return code.
!                0 = no error
!                6 = memory allocation error
!
! REMARKS: 
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      character(len=1),intent(in) :: cgrib(lcgrib)
      integer,intent(in) :: lcgrib
      integer,intent(inout) :: iofst
      integer,pointer,dimension(:) :: ids
      integer,intent(out) :: ierr,idslen

      integer,dimension(:) :: mapid(13)

      data mapid /2,2,1,1,1,2,1,1,1,1,1,1,1/

      ierr=0
      idslen=13
      nullify(ids)

      call g2lib_gbyte(cgrib,lensec,iofst,32)        ! Get Length of Section
      iofst=iofst+32
      iofst=iofst+8     ! skip section number
      !
      !   Unpack each value into array ids from the
      !   the appropriate number of octets, which are specified in
      !   corresponding entries in array mapid.
      !
      istat=0
      allocate(ids(idslen),stat=istat)
      if (istat.ne.0) then
         ierr=6
         nullify(ids)
         return
      endif
      
      do i=1,idslen
        nbits=mapid(i)*8
        call g2lib_gbyte(cgrib,ids(i),iofst,nbits)
        iofst=iofst+nbits
      enddo
      
      return    ! End of Section 1 processing
      end
