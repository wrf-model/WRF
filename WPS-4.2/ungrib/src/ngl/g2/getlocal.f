      subroutine getlocal(cgrib,lcgrib,localnum,csec2,lcsec2,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    getlocal 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-25
!
! ABSTRACT: This subroutine returns the contents of Section 2 ( Local 
!   Use Section ) from a GRIB2 message.  Since there can be multiple
!   occurrences of Section 2 within a GRIB message, the calling routine
!   indicates which occurrence is being requested with the localnum argument.
!
! PROGRAM HISTORY LOG:
! 2000-05-25  Gilbert
!
! USAGE:    CALL getlocal(cgrib,lcgrib,localnum,csec2,lcsec2,ierr)
!   INPUT ARGUMENT LIST:
!     cgrib    - Character array that contains the GRIB2 message
!     lcgrib   - Length (in bytes) of GRIB message in array cgrib.
!     localnum - The nth occurrence of Section 2 requested.
!
!   OUTPUT ARGUMENT LIST:      
!     csec2    - Character array containing information read from 
!                Section 2.
!                The dimension of this array can be obtained in advance
!                from argument maxlocal, which is returned from subroutine 
!                gb_info.
!     lcsec2   - Number of bytes of character array csec2 read from
!                Section 2.
!     ierr     - Error return code.
!                0 = no error
!                1 = Beginning characters "GRIB" not found.
!                2 = GRIB message is not Edition 2.
!                3 = The section 2 request number was not positive.
!                4 = End string "7777" found, but not where expected.
!                5 = End string "7777" not found at end of message.
!                6 = GRIB message did not contain the requested number of
!                    Local Use Sections.
!
! REMARKS: Note that subroutine gb_info can be used to first determine
!          how many Local Use sections exist in a given GRIB message.
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      character(len=1),intent(in) :: cgrib(lcgrib)
      integer,intent(in) :: lcgrib,localnum
      character(len=1),intent(out) :: csec2(*)
      integer,intent(out) :: lcsec2,ierr
      
      character(len=4),parameter :: grib='GRIB',c7777='7777'
      character(len=4) :: ctemp
      integer :: listsec0(2)
      integer iofst,ibeg,istart,numlocal

      ierr=0
      numlocal=0
!
!  Check for valid request number
!  
      if (localnum.le.0) then
        print *,'getlocal: Request for local section must be positive.'
        ierr=3
        return
      endif
!
!  Check for beginning of GRIB message in the first 100 bytes
!
      istart=0
      do j=1,100
        ctemp=cgrib(j)//cgrib(j+1)//cgrib(j+2)//cgrib(j+3)
        if (ctemp.eq.grib ) then
          istart=j
          exit
        endif
      enddo
      if (istart.eq.0) then
        print *,'getlocal:  Beginning characters GRIB not found.'
        ierr=1
        return
      endif
!
!  Unpack Section 0 - Indicator Section 
!
      iofst=8*(istart+5)
      call gbyte(cgrib,listsec0(1),iofst,8)     ! Discipline
      iofst=iofst+8
      call gbyte(cgrib,listsec0(2),iofst,8)     ! GRIB edition number
      iofst=iofst+8
      iofst=iofst+32
      call gbyte(cgrib,lengrib,iofst,32)        ! Length of GRIB message
      iofst=iofst+32
      lensec0=16
      ipos=istart+lensec0
!
!  Currently handles only GRIB Edition 2.
!  
      if (listsec0(2).ne.2) then
        print *,'getlocal: can only decode GRIB edition 2.'
        ierr=2
        return
      endif
!
!  Loop through the remaining sections keeping track of the 
!  length of each.  Also check to see that if the current occurrence
!  of Section 2 is the same as the one requested.
!
      do
        !    Check to see if we are at end of GRIB message
        ctemp=cgrib(ipos)//cgrib(ipos+1)//cgrib(ipos+2)//cgrib(ipos+3)
        if (ctemp.eq.c7777 ) then
          ipos=ipos+4
          !    If end of GRIB message not where expected, issue error
          if (ipos.ne.(istart+lengrib)) then
            print *,'getlocal: "7777" found, but not where expected.'
            ierr=4
            return
          endif
          exit
        endif
        !     Get length of Section and Section number
        iofst=(ipos-1)*8
        call gbyte(cgrib,lensec,iofst,32)        ! Get Length of Section
        iofst=iofst+32
        call gbyte(cgrib,isecnum,iofst,8)         ! Get Section number
        iofst=iofst+8
        !   If found the requested occurrence of Section 2,
        !   return the section contents.
        if (isecnum.eq.2) then
          numlocal=numlocal+1
          if (numlocal.eq.localnum) then
            lcsec2=lensec-5
            csec2(1:lcsec2)=cgrib(ipos+5:ipos+lensec-1)
            return
          endif
        endif
        !   Check to see if we read pass the end of the GRIB
        !   message and missed the terminator string '7777'.
        ipos=ipos+lensec                 ! Update beginning of section pointer
        if (ipos.gt.(istart+lengrib)) then
          print *,'getlocal: "7777"  not found at end of GRIB message.'
          ierr=5
          return
        endif
        
      enddo

!
!  If exited from above loop, the end of the GRIB message was reached
!  before the requested occurrence of section 2 was found.
!
      print *,'getlocal: GRIB message contained ',numlocal,
     &        ' local sections.'
      print *,'getlocal: The request was for the ',localnum,
     &        ' occurrence.'
      ierr=6

      return
      end







