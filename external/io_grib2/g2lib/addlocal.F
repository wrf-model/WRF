      subroutine addlocal(cgrib,lcgrib,csec2,lcsec2,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    addlocal 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-01
!
! ABSTRACT: This subroutine adds a Local Use Section (Section 2) to 
!   a GRIB2 message.
!   This routine is used with routines "gribcreate", "addgrid", "addfield",
!   and "gribend" to create a complete GRIB2 message.  Subroutine
!   gribcreate must be called first to initialize a new GRIB2 message.
!
! PROGRAM HISTORY LOG:
! 2000-05-01  Gilbert
!
! USAGE:    CALL addlocal(cgrib,lcgrib,csec2,lcsec2,ierr)
!   INPUT ARGUMENT LIST:
!     cgrib    - Character array to contain the GRIB2 message
!     lcgrib   - Maximum length (bytes) of array cgrib.
!     csec2    - Character array containing information to be added to
!                Section 2.
!     lcsec2   - Number of bytes of character array csec2 to be added to
!                Section 2.
!
!   OUTPUT ARGUMENT LIST:      
!     cgrib    - Character array to contain the GRIB2 message
!     ierr     - Error return code.
!                0 = no error
!                1 = GRIB message was not initialized.  Need to call
!                    routine gribcreate first.
!                2 = GRIB message already complete.  Cannot add new section.
!                3 = Sum of Section byte counts doesn't add to total byte count.
!                4 = Previous Section was not 1 or 7.
!
! REMARKS: Note that the Local Use Section ( Section 2 ) can only follow
!          Section 1 or Section 7 in a GRIB2 message.
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      character(len=1),intent(inout) :: cgrib(lcgrib)
      character(len=1),intent(in) :: csec2(lcsec2)
      integer,intent(in) :: lcgrib,lcsec2
      integer,intent(out) :: ierr
      
      character(len=4),parameter :: grib='GRIB',c7777='7777'
      character(len=4):: ctemp
      integer,parameter :: two=2
      integer lensec2,iofst,ibeg,lencurr,len
 
      ierr=0
!
!  Check to see if beginning of GRIB message exists
!
      ctemp=cgrib(1)//cgrib(2)//cgrib(3)//cgrib(4)
      if ( ctemp.ne.grib ) then
        print *,'addlocal: GRIB not found in given message.'
        print *,'addlocal: Call to routine gribcreate required',
     &          ' to initialize GRIB messge.'
        ierr=1
        return
      endif
!
!  Get current length of GRIB message
!  
      call g2lib_gbyte(cgrib,lencurr,96,32)
!
!  Check to see if GRIB message is already complete
!  
      ctemp=cgrib(lencurr-3)//cgrib(lencurr-2)//cgrib(lencurr-1)
     &      //cgrib(lencurr)
      if ( ctemp.eq.c7777 ) then
        print *,'addlocal: GRIB message already complete.  Cannot',
     &          ' add new section.'
        ierr=2
        return
      endif
!
!  Loop through all current sections of the GRIB message to
!  find the last section number.
!
      len=16    ! length of Section 0
      do 
      !    Get section number and length of next section
        iofst=len*8
        call g2lib_gbyte(cgrib,ilen,iofst,32)
        iofst=iofst+32
        call g2lib_gbyte(cgrib,isecnum,iofst,8)
        len=len+ilen
      !    Exit loop if last section reached
        if ( len.eq.lencurr ) exit
      !    If byte count for each section doesn't match current
      !    total length, then there is a problem.
        if ( len.gt.lencurr ) then
          print *,'addlocal: Section byte counts don''t add to total.'
          print *,'addlocal: Sum of section byte counts = ',len
          print *,'addlocal: Total byte count in Section 0 = ',lencurr
          ierr=3
          return
        endif
      enddo
!
!  Section 2 can only be added after sections 1 and 7.
!
      if ( (isecnum.ne.1) .and. (isecnum.ne.7) ) then
        print *,'addlocal: Section 2 can only be added after Section',
     &          ' 1 or Section 7.'
        print *,'addlocal: Section ',isecnum,' was the last found in',
     &          ' given GRIB message.'
        ierr=4
        return
      endif
!
!  Add Section 2  - Local Use Section
!
      ibeg=lencurr*8        !   Calculate offset for beginning of section 2
      iofst=ibeg+32         !   leave space for length of section
      call g2lib_sbyte(cgrib,two,iofst,8)     ! Store section number ( 2 )
      istart=lencurr+5
      cgrib(istart+1:istart+lcsec2)=csec2(1:lcsec2)
      !
      !   Calculate length of section 2 and store it in octets
      !   1-4 of section 2.
      !
      lensec2=lcsec2+5      ! bytes
      call g2lib_sbyte(cgrib,lensec2,ibeg,32)

!
!  Update current byte total of message in Section 0
!
      call g2lib_sbyte(cgrib,lencurr+lensec2,96,32)

      return
      end

