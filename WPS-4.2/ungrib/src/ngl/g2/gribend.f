      subroutine gribend(cgrib,lcgrib,lengrib,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    gribend 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-02
!
! ABSTRACT: This subroutine finalizes a GRIB message after all grids
!   and fields have been added.  It adds the End Section ( "7777" )
!   to the end of the GRIB message and calculates the length and stores
!   it in the appropriate place in Section 0.
!   This routine is used with routines "gribcreate", "addlocal", "addgrid",
!   and "addfield" to create a complete GRIB2 message.  Subroutine
!   gribcreate must be called first to initialize a new GRIB2 message.
!
! PROGRAM HISTORY LOG:
! 2000-05-02  Gilbert
!
! USAGE:    CALL gribend(cgrib,lcgrib,lengrib,ierr)
!   INPUT ARGUMENT LIST:
!     cgrib    - Character array to contain the GRIB2 message
!     lcgrib   - Maximum length (bytes) of array cgrib.
!
!   OUTPUT ARGUMENT LIST:      
!     cgrib    - Character array to contain the GRIB2 message
!     lengrib  - Length of the final GRIB2 message in octets (bytes)
!     ierr     - Error return code.
!                0 = no error
!                1 = GRIB message was not initialized.  Need to call
!                    routine gribcreate first.
!                2 = GRIB message already complete.  
!                3 = Sum of Section byte counts doesn't add to total byte count.
!                4 = Previous Section was not 7.
!
! REMARKS: This routine is intended for use with routines "gribcreate", 
!          "addlocal", "addgrid", and "addfield" to create a complete 
!          GRIB2 message.
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      character(len=1),intent(inout) :: cgrib(lcgrib)
      integer,intent(in) :: lcgrib
      integer,intent(out) :: lengrib,ierr
      
      character(len=4),parameter :: grib='GRIB',c7777='7777'
      character(len=4):: ctemp
      integer iofst,ibeg,lencurr,len
 
      ierr=0
!
!  Check to see if beginning of GRIB message exists
!
      ctemp=cgrib(1)//cgrib(2)//cgrib(3)//cgrib(4)
      if ( ctemp.ne.grib ) then
        print *,'gribend: GRIB not found in given message.'
        ierr=1
        return
      endif
!
!  Get current length of GRIB message
!  
      call gbyte(cgrib,lencurr,96,32)
!
!  Check to see if GRIB message is already complete
!
!      ctemp=cgrib(lencurr-3)//cgrib(lencurr-2)//cgrib(lencurr-1)
!     &      //cgrib(lencurr)
!      if ( ctemp.eq.c7777 ) then
!        print *,'gribend: GRIB message already complete.'
!        ierr=2
!        return
!      endif
!
!  Loop through all current sections of the GRIB message to
!  find the last section number.
!
      len=16    ! Length of Section 0
      do 
      !    Get number and length of next section
        iofst=len*8
        call gbyte(cgrib,ilen,iofst,32)
        iofst=iofst+32
        call gbyte(cgrib,isecnum,iofst,8)
        len=len+ilen
      !    Exit loop if last section reached
        if ( len.eq.lencurr ) exit
      !    If byte count for each section doesn't match current
      !    total length, then there is a problem.
        if ( len.gt.lencurr ) then
          print *,'gribend: Section byte counts don''t add to total.'
          print *,'gribend: Sum of section byte counts = ',len
          print *,'gribend: Total byte count in Section 0 = ',lencurr
          ierr=3
          return
        endif
      enddo
!
!  Can only add End Section (Section 8) after Section 7.
!
      if ( isecnum.ne.7 ) then
        print *,'gribend: Section 8 can only be added after Section 7.'
        print *,'gribend: Section ',isecnum,' was the last found in',
     &          ' given GRIB message.'
        ierr=4
        return
      endif
!
!  Add Section 8  - End Section
!
      cgrib(lencurr+1:lencurr+4)=c7777

!
!  Update current byte total of message in Section 0
!
      lengrib=lencurr+4
      call sbyte(cgrib,lengrib,96,32)

      return
      end




