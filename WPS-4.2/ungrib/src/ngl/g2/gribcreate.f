      subroutine gribcreate(cgrib,lcgrib,listsec0,listsec1,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    gribcreate 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-04-28
!
! ABSTRACT: This subroutine initializes a new GRIB2 message and packs
!   GRIB2 sections 0 (Indicator Section) and 1 (Identification Section).
!   This routine is used with routines "addlocal", "addgrid", "addfield",
!   and "gribend" to create a complete GRIB2 message.  Subroutine
!   gribcreate must be called first to initialize a new GRIB2 message.
!   Also, a call to gribend is required to complete GRIB2 message
!   after all fields have been added.
!
! PROGRAM HISTORY LOG:
! 2000-04-28  Gilbert
!
! USAGE:    CALL gribcreate(cgrib,lcgrib,listsec0,listsec1,ierr)
!   INPUT ARGUMENT LIST:
!     cgrib    - Character array to contain the GRIB2 message
!     lcgrib   - Maximum length (bytes) of array cgrib.
!     listsec0 - Contains information needed for GRIB Indicator Section 0.
!                Must be dimensioned >= 2.
!                listsec0(1)=Discipline-GRIB Master Table Number
!                            (see Code Table 0.0)
!                listsec0(2)=GRIB Edition Number (currently 2)
!     listsec1 - Contains information needed for GRIB Identification Section 1.
!                Must be dimensioned >= 13.
!                listsec1(1)=Id of orginating centre (Common Code Table C-1)
!                listsec1(2)=Id of orginating sub-centre (local table)
!                listsec1(3)=GRIB Master Tables Version Number (Code Table 1.0)
!                listsec1(4)=GRIB Local Tables Version Number (Code Table 1.1)
!                listsec1(5)=Significance of Reference Time (Code Table 1.2)
!                listsec1(6)=Reference Time - Year (4 digits)
!                listsec1(7)=Reference Time - Month
!                listsec1(8)=Reference Time - Day
!                listsec1(9)=Reference Time - Hour
!                listsec1(10)=Reference Time - Minute
!                listsec1(11)=Reference Time - Second
!                listsec1(12)=Production status of data (Code Table 1.3)
!                listsec1(13)=Type of processed data (Code Table 1.4)
!
!   OUTPUT ARGUMENT LIST:      
!     cgrib    - Character array to contain the GRIB2 message
!     ierr     - Error return code.
!                0 = no error
!                1 = Tried to use for version other than GRIB Edition 2
!
! REMARKS: This routine is intended for use with routines "addlocal", 
!          "addgrid", "addfield", and "gribend" to create a complete 
!          GRIB2 message.
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      character(len=1),intent(inout) :: cgrib(lcgrib)
      integer,intent(in) :: listsec0(*),listsec1(*)
      integer,intent(in) :: lcgrib
      integer,intent(out) :: ierr
      
      character(len=4),parameter :: grib='GRIB'
      integer,parameter :: zero=0,one=1
      integer,parameter :: mapsec1len=13
      integer,parameter :: 
     &        mapsec1(mapsec1len)=(/ 2,2,1,1,1,2,1,1,1,1,1,1,1 /)
      integer lensec0,iofst,ibeg

      ierr=0
!
!  Currently handles only GRIB Edition 2.
!  
      if (listsec0(2).ne.2) then
        print *,'gribcreate: can only code GRIB edition 2.'
        ierr=1
        return
      endif
!
!  Pack Section 0 - Indicator Section 
!  ( except for total length of GRIB message )
!
!      cgrib=' '
      cgrib(1)=grib(1:1)                     ! Beginning of GRIB message
      cgrib(2)=grib(2:2)   
      cgrib(3)=grib(3:3)   
      cgrib(4)=grib(4:4)   
      call sbyte(cgrib,zero,32,16)           ! reserved for future use
      call sbyte(cgrib,listsec0(1),48,8)     ! Discipline
      call sbyte(cgrib,listsec0(2),56,8)     ! GRIB edition number
      lensec0=16      ! bytes (octets)
!
!  Pack Section 1 - Identification Section
!
      ibeg=lensec0*8        !   Calculate offset for beginning of section 1
      iofst=ibeg+32         !   leave space for length of section
      call sbyte(cgrib,one,iofst,8)     ! Store section number ( 1 )
      iofst=iofst+8
      !
      !   Pack up each input value in array listsec1 into the
      !   the appropriate number of octets, which are specified in
      !   corresponding entries in array mapsec1.
      !
      do i=1,mapsec1len
        nbits=mapsec1(i)*8
        call sbyte(cgrib,listsec1(i),iofst,nbits)
        iofst=iofst+nbits
      enddo
      !
      !   Calculate length of section 1 and store it in octets
      !   1-4 of section 1.
      !
      lensec1=(iofst-ibeg)/8
      call sbyte(cgrib,lensec1,ibeg,32)
!
!  Put current byte total of message into Section 0
!
      call sbyte(cgrib,zero,64,32)
      call sbyte(cgrib,lensec0+lensec1,96,32)

      return
      end
