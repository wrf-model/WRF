      subroutine gb_info(cgrib,lcgrib,listsec0,listsec1,
     &                    numfields,numlocal,maxlocal,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    gb_info 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-25
!
! ABSTRACT: This subroutine searches through a GRIB2 message and
!   returns the number of gridded fields found in the message and
!   the number (and maximum size) of Local Use Sections.
!   Also various checks  are performed
!   to see if the message is a valid GRIB2 message.
!
! PROGRAM HISTORY LOG:
! 2000-05-25  Gilbert
!
! USAGE:    CALL gb_info(cgrib,lcgrib,listsec0,listsec1,
!     &                    numfields,numlocal,maxlocal,ierr)
!   INPUT ARGUMENT LIST:
!     cgrib    - Character array that contains the GRIB2 message
!     lcgrib   - Length (in bytes) of GRIB message in array cgrib.
!
!   OUTPUT ARGUMENT LIST:      
!     listsec0 - Contains information decoded from GRIB Indicator Section 0.
!                Must be dimensioned >= 2.
!                listsec0(1)=Discipline-GRIB Master Table Number
!                            (see Code Table 0.0)
!                listsec0(2)=GRIB Edition Number (currently 2)
!                listsec0(3)=Length of GRIB message
!     listsec1 - Contains information read from GRIB Identification Section 1.
!                Must be dimensioned >= 13.
!                listsec1(1)=Id of orginating centre (Common Code Table C-1)
!                listsec1(2)=Id of orginating sub-centre (local table)
!                listsec1(3)=GRIB Master Tables Version Number (Code Table 1.0)
!                listsec1(4)=GRIB Local Tables Version Number 
!                listsec1(5)=Significance of Reference Time (Code Table 1.1)
!                listsec1(6)=Reference Time - Year (4 digits)
!                listsec1(7)=Reference Time - Month
!                listsec1(8)=Reference Time - Day
!                listsec1(9)=Reference Time - Hour
!                listsec1(10)=Reference Time - Minute
!                listsec1(11)=Reference Time - Second
!                listsec1(12)=Production status of data (Code Table 1.2)
!                listsec1(13)=Type of processed data (Code Table 1.3)
!     numfields- The number of gridded fieldse found in the GRIB message.
!     numlocal - The number of Local Use Sections ( Section 2 ) found in 
!                the GRIB message.
!     maxlocal-  The size of the largest Local Use Section ( Section 2 ).
!                Can be used to ensure that the return array passed
!                to subroutine getlocal is dimensioned large enough.
!     ierr     - Error return code.
!                0 = no error
!                1 = Beginning characters "GRIB" not found.
!                2 = GRIB message is not Edition 2.
!                3 = Could not find Section 1, where expected.
!                4 = End string "7777" found, but not where expected.
!                5 = End string "7777" not found at end of message.
!                6 = Invalid section number found.
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
      integer,intent(out) :: listsec0(3),listsec1(13)
      integer,intent(out) :: numlocal,numfields,maxlocal,ierr
      
      character(len=4),parameter :: grib='GRIB',c7777='7777'
      character(len=4) :: ctemp
      integer,parameter :: zero=0,one=1
      integer,parameter :: mapsec1len=13
      integer,parameter :: 
     &        mapsec1(mapsec1len)=(/ 2,2,1,1,1,2,1,1,1,1,1,1,1 /)
      integer iofst,ibeg,istart

      ierr=0
      numlocal=0
      numfields=0
      maxlocal=0
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
        print *,'gb_info:  Beginning characters GRIB not found.'
        ierr=1
        return
      endif
!
!  Unpack Section 0 - Indicator Section 
!
      iofst=8*(istart+5)
      call g2lib_gbyte(cgrib,listsec0(1),iofst,8)     ! Discipline
      iofst=iofst+8
      call g2lib_gbyte(cgrib,listsec0(2),iofst,8)     ! GRIB edition number
      iofst=iofst+8
      iofst=iofst+32
      call g2lib_gbyte(cgrib,lengrib,iofst,32)        ! Length of GRIB message
      iofst=iofst+32
      listsec0(3)=lengrib
      lensec0=16
      ipos=istart+lensec0
!
!  Currently handles only GRIB Edition 2.
!  
      if (listsec0(2).ne.2) then
        print *,'gb_info: can only decode GRIB edition 2.'
        ierr=2
        return
      endif
!
!  Unpack Section 1 - Identification Section
!
      call g2lib_gbyte(cgrib,lensec1,iofst,32)        ! Length of Section 1
      iofst=iofst+32
      call g2lib_gbyte(cgrib,isecnum,iofst,8)         ! Section number ( 1 )
      iofst=iofst+8
      if (isecnum.ne.1) then
        print *,'gb_info: Could not find section 1.'
        ierr=3
        return
      endif
      !
      !   Unpack each input value in array listsec1 into the
      !   the appropriate number of octets, which are specified in
      !   corresponding entries in array mapsec1.
      !
      do i=1,mapsec1len
        nbits=mapsec1(i)*8
        call g2lib_gbyte(cgrib,listsec1(i),iofst,nbits)
        iofst=iofst+nbits
      enddo
      ipos=ipos+lensec1
!
!  Loop through the remaining sections to see if they are valid.
!  Also count the number of times Section 2
!  and Section 4 appear.
!
      do
        ctemp=cgrib(ipos)//cgrib(ipos+1)//cgrib(ipos+2)//cgrib(ipos+3)
        if (ctemp.eq.c7777 ) then
          ipos=ipos+4
          if (ipos.ne.(istart+lengrib)) then
            print *,'gb_info: "7777" found, but not where expected.'
            ierr=4
            return
          endif
          exit
        endif
        iofst=(ipos-1)*8
        call g2lib_gbyte(cgrib,lensec,iofst,32)        ! Get Length of Section
        iofst=iofst+32
        call g2lib_gbyte(cgrib,isecnum,iofst,8)         ! Get Section number
        iofst=iofst+8
        ipos=ipos+lensec                 ! Update beginning of section pointer
        if (ipos.gt.(istart+lengrib)) then
          print *,'gb_info: "7777"  not found at end of GRIB message.'
          ierr=5
          return
        endif
        if ( isecnum.ge.2.AND.isecnum.le.7 ) then
           if (isecnum.eq.2) then     ! Local Section 2
              !   increment counter for total number of local sections found
              numlocal=numlocal+1
              lenposs=lensec-5
              if ( lenposs.gt.maxlocal ) maxlocal=lenposs
           elseif (isecnum.eq.4) then
              !   increment counter for total number of fields found
              numfields=numfields+1
           endif
        else
           print *,'gb_info: Invalid section number found in GRIB',
     &             ' message: ',isecnum
           ierr=6
           return
        endif
        
      enddo

      return
      end

