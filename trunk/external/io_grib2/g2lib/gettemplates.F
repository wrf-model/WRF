      subroutine gettemplates(cgrib,lcgrib,ifldnum,igds,igdstmpl,
     &                    igdslen,ideflist,idefnum,ipdsnum,ipdstmpl,
     &                    ipdslen,coordlist,numcoord,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    gettemplates 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-26
!
! ABSTRACT: This subroutine returns the Grid Definition, and
!   Product Definition for a given data
!   field.  Since there can be multiple data fields packed into a GRIB2
!   message, the calling routine indicates which field is being requested
!   with the ifldnum argument.
!
! PROGRAM HISTORY LOG:
! 2000-05-26  Gilbert
!
! USAGE:    CALL gettemplates(cgrib,lcgrib,ifldnum,igds,igdstmpl,igdslen,
!    &                    ideflist,idefnum,ipdsnum,ipdstmpl,ipdslen,
!    &                    coordlist,numcoord,ierr)
!   INPUT ARGUMENT LIST:
!     cgrib    - Character array that contains the GRIB2 message
!     lcgrib   - Length (in bytes) of GRIB message array cgrib.
!     ifldnum  - Specifies which field in the GRIB2 message to return.
!
!   OUTPUT ARGUMENT LIST:      
!     igds     - Contains information read from the appropriate GRIB Grid 
!                Definition Section 3 for the field being returned.
!                Must be dimensioned >= 5.
!                igds(1)=Source of grid definition (see Code Table 3.0)
!                igds(2)=Number of grid points in the defined grid.
!                igds(3)=Number of octets needed for each 
!                            additional grid points definition.  
!                            Used to define number of
!                            points in each row ( or column ) for
!                            non-regular grids.  
!                            = 0, if using regular grid.
!                igds(4)=Interpretation of list for optional points 
!                            definition.  (Code Table 3.11)
!                igds(5)=Grid Definition Template Number (Code Table 3.1)
!     igdstmpl - Contains the data values for the specified Grid Definition
!                Template ( NN=igds(5) ).  Each element of this integer 
!                array contains an entry (in the order specified) of Grid
!                Defintion Template 3.NN
!                A safe dimension for this array can be obtained in advance
!                from maxvals(2), which is returned from subroutine gribinfo.
!     igdslen  - Number of elements in igdstmpl().  i.e. number of entries
!                in Grid Defintion Template 3.NN  ( NN=igds(5) ).
!     ideflist - (Used if igds(3) .ne. 0)  This array contains the
!                number of grid points contained in each row ( or column ).
!                (part of Section 3)
!                A safe dimension for this array can be obtained in advance
!                from maxvals(3), which is returned from subroutine gribinfo.
!     idefnum  - (Used if igds(3) .ne. 0)  The number of entries
!                in array ideflist.  i.e. number of rows ( or columns )
!                for which optional grid points are defined.
!     ipdsnum  - Product Definition Template Number ( see Code Table 4.0)
!     ipdstmpl - Contains the data values for the specified Product Definition
!                Template ( N=ipdsnum ).  Each element of this integer
!                array contains an entry (in the order specified) of Product
!                Defintion Template 4.N
!                A safe dimension for this array can be obtained in advance
!                from maxvals(4), which is returned from subroutine gribinfo.
!     ipdslen  - Number of elements in ipdstmpl().  i.e. number of entries
!                in Product Defintion Template 4.N  ( N=ipdsnum ).
!     coordlist- Array containg floating point values intended to document
!                the vertical discretisation associated to model data
!                on hybrid coordinate vertical levels.  (part of Section 4)
!                The dimension of this array can be obtained in advance
!                from maxvals(5), which is returned from subroutine gribinfo.
!     numcoord - number of values in array coordlist.
!     ierr     - Error return code.
!                0 = no error
!                1 = Beginning characters "GRIB" not found.
!                2 = GRIB message is not Edition 2.
!                3 = The data field request number was not positive.
!                4 = End string "7777" found, but not where expected.
!                6 = GRIB message did not contain the requested number of
!                    data fields.
!                7 = End string "7777" not found at end of message.
!               10 = Error unpacking Section 3.
!               11 = Error unpacking Section 4.
!
! REMARKS: Note that subroutine gribinfo can be used to first determine
!          how many data fields exist in the given GRIB message.
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      character(len=1),intent(in) :: cgrib(lcgrib)
      integer,intent(in) :: lcgrib,ifldnum
      integer,intent(out) :: igds(*),igdstmpl(*),ideflist(*)
      integer,intent(out) :: ipdsnum,ipdstmpl(*)
      integer,intent(out) :: idefnum,numcoord
      integer,intent(out) :: ierr
      real,intent(out) :: coordlist(*)
      
      character(len=4),parameter :: grib='GRIB',c7777='7777'
      character(len=4) :: ctemp
      integer:: listsec0(2)
      integer iofst,ibeg,istart
      logical have3,have4

      have3=.false.
      have4=.false.
      ierr=0
      numfld=0
!
!  Check for valid request number
!  
      if (ifldnum.le.0) then
        print *,'gettemplates: Request for field number must be ',
     &          'positive.'
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
        print *,'gettemplates:  Beginning characters GRIB not found.'
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
      lensec0=16
      ipos=istart+lensec0
!
!  Currently handles only GRIB Edition 2.
!  
      if (listsec0(2).ne.2) then
        print *,'gettemplates: can only decode GRIB edition 2.'
        ierr=2
        return
      endif
!
!  Loop through the remaining sections keeping track of the 
!  length of each.  Also keep the latest Grid Definition Section info.
!  Unpack the requested field number.
!
      do
        !    Check to see if we are at end of GRIB message
        ctemp=cgrib(ipos)//cgrib(ipos+1)//cgrib(ipos+2)//cgrib(ipos+3)
        if (ctemp.eq.c7777 ) then
          ipos=ipos+4
          !    If end of GRIB message not where expected, issue error
          if (ipos.ne.(istart+lengrib)) then
            print *,'gettemplates: "7777" found, but not where ',
     &              'expected.'
            ierr=4
            return
          endif
          exit
        endif
        !     Get length of Section and Section number
        iofst=(ipos-1)*8
        call g2lib_gbyte(cgrib,lensec,iofst,32)        ! Get Length of Section
        iofst=iofst+32
        call g2lib_gbyte(cgrib,isecnum,iofst,8)         ! Get Section number
        iofst=iofst+8
        !print *,' lensec= ',lensec,'    secnum= ',isecnum
        !
        !   If found Section 3, unpack the GDS info using the 
        !   appropriate template.  Save in case this is the latest
        !   grid before the requested field.
        !
        if (isecnum.eq.3) then
          iofst=iofst-40       ! reset offset to beginning of section
          call unpack3(cgrib,lcgrib,iofst,igds,igdstmpl,igdslen,
     &                 ideflist,idefnum,jerr)
          if (jerr.eq.0) then
            have3=.true.
          else
            ierr=10
            return
          endif
        endif
        !
        !   If found Section 4, check to see if this field is the
        !   one requested.
        !
        if (isecnum.eq.4) then
          numfld=numfld+1
          if (numfld.eq.ifldnum) then
            iofst=iofst-40       ! reset offset to beginning of section
            call unpack4(cgrib,lcgrib,iofst,ipdsnum,ipdstmpl,ipdslen,
     &                   coordlist,numcoord,jerr)
            if (jerr.eq.0) then
              have4=.true.
            else
              ierr=11
              return
            endif
          endif
        endif
        !
        !   Check to see if we read pass the end of the GRIB
        !   message and missed the terminator string '7777'.
        !
        ipos=ipos+lensec                 ! Update beginning of section pointer
        if (ipos.gt.(istart+lengrib)) then
          print *,'gettemplates: "7777"  not found at end of GRIB ',
     &            'message.'
          ierr=7
          return
        endif

        if (have3.and.have4) return
        
      enddo

!
!  If exited from above loop, the end of the GRIB message was reached
!  before the requested field was found.
!
      print *,'gettemplates: GRIB message contained ',numlocal,
     &        ' different fields.'
      print *,'gettemplates: The request was for the ',ifldnum,
     &        ' field.'
      ierr=6

      return
      end
