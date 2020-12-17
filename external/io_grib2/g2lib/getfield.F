      subroutine getfield(cgrib,lcgrib,ifldnum,igds,igdstmpl,igdslen,
     &                    ideflist,idefnum,ipdsnum,ipdstmpl,ipdslen,
     &                    coordlist,numcoord,ndpts,idrsnum,idrstmpl,
     &                    idrslen,ibmap,bmap,fld,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    getfield 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-26
!
! ABSTRACT: This subroutine returns the Grid Definition, Product Definition,
!   Bit-map ( if applicable ), and the unpacked data for a given data
!   field.  Since there can be multiple data fields packed into a GRIB2
!   message, the calling routine indicates which field is being requested
!   with the ifldnum argument.
!
! PROGRAM HISTORY LOG:
! 2000-05-26  Gilbert
!
! USAGE:    CALL getfield(cgrib,lcgrib,ifldnum,igds,igdstmpl,igdslen,
!    &                    ideflist,idefnum,ipdsnum,ipdstmpl,ipdslen,
!    &                    coordlist,numcoord,ndpts,idrsnum,idrstmpl,
!    &                    idrslen,ibmap,bmap,fld,ierr)
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
!     ndpts    - Number of data points unpacked and returned.
!     idrsnum  - Data Representation Template Number ( see Code Table 5.0)
!     idrstmpl - Contains the data values for the specified Data Representation
!                Template ( N=idrsnum ).  Each element of this integer
!                array contains an entry (in the order specified) of Product
!                Defintion Template 5.N
!                A safe dimension for this array can be obtained in advance
!                from maxvals(6), which is returned from subroutine gribinfo.
!     idrslen  - Number of elements in idrstmpl().  i.e. number of entries
!                in Data Representation Template 5.N  ( N=idrsnum ).
!     ibmap    - Bitmap indicator ( see Code Table 6.0 )
!                0 = bitmap applies and is included in Section 6.
!                1-253 = Predefined bitmap applies
!                254 = Previously defined bitmap applies to this field
!                255 = Bit map does not apply to this product.
!     bmap()   - Logical*1 array containing decoded bitmap. ( if ibmap=0 )
!                The dimension of this array can be obtained in advance
!                from maxvals(7), which is returned from subroutine gribinfo.
!     fld()    - Array of ndpts unpacked data points.
!                A safe dimension for this array can be obtained in advance
!                from maxvals(7), which is returned from subroutine gribinfo.
!     ierr     - Error return code.
!                0 = no error
!                1 = Beginning characters "GRIB" not found.
!                2 = GRIB message is not Edition 2.
!                3 = The data field request number was not positive.
!                4 = End string "7777" found, but not where expected.
!                6 = GRIB message did not contain the requested number of
!                    data fields.
!                7 = End string "7777" not found at end of message.
!                9 = Data Representation Template 5.NN not yet implemented.
!               10 = Error unpacking Section 3.
!               11 = Error unpacking Section 4.
!               12 = Error unpacking Section 5.
!               13 = Error unpacking Section 6.
!               14 = Error unpacking Section 7.
!
! REMARKS: Note that subroutine gribinfo can be used to first determine
!          how many data fields exist in a given GRIB message.
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
      integer,intent(out) :: idrsnum,idrstmpl(*)
      integer,intent(out) :: ndpts,ibmap,idefnum,numcoord
      integer,intent(out) :: ierr
      logical*1,intent(out) :: bmap(*)
      real,intent(out) :: fld(*),coordlist(*)
      
      character(len=4),parameter :: grib='GRIB',c7777='7777'
      character(len=4) :: ctemp
      integer:: listsec0(2)
      integer iofst,ibeg,istart
      integer(4) :: ieee
      logical have3,have4,have5,have6,have7

      have3=.false.
      have4=.false.
      have5=.false.
      have6=.false.
      have7=.false.
      ierr=0
      numfld=0
!
!  Check for valid request number
!  
      if (ifldnum.le.0) then
        print *,'getfield: Request for field number must be positive.'
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
        print *,'getfield:  Beginning characters GRIB not found.'
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
        print *,'getfield: can only decode GRIB edition 2.'
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
            print *,'getfield: "7777" found, but not where expected.'
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
        !   If found Section 5, check to see if this field is the
        !   one requested.
        !
        if ((isecnum.eq.5).and.(numfld.eq.ifldnum)) then
          iofst=iofst-40       ! reset offset to beginning of section
          call unpack5(cgrib,lcgrib,iofst,ndpts,idrsnum,idrstmpl,
     &                 idrslen,jerr)
          if (jerr.eq.0) then
            have5=.true.
          else
            ierr=12
            return
          endif
        endif
        !
        !   If found Section 6, Unpack bitmap.
        !   Save in case this is the latest
        !   bitmap before the requested field.
        !
        if (isecnum.eq.6) then
          iofst=iofst-40       ! reset offset to beginning of section
          call unpack6(cgrib,lcgrib,iofst,igds(2),ibmap,bmap,jerr)
          if (jerr.eq.0) then
            have6=.true.
          else
            ierr=13
            return
          endif
        endif
        !
        !   If found Section 7, check to see if this field is the
        !   one requested.
        !
        if ((isecnum.eq.7).and.(numfld.eq.ifldnum)) then
          if (idrsnum.eq.0) then
            call simunpack(cgrib(ipos+5),lensec-6,idrstmpl,ndpts,fld)
            have7=.true.
          elseif (idrsnum.eq.2.or.idrsnum.eq.3) then
            call comunpack(cgrib(ipos+5),lensec-6,lensec,idrsnum,
     &                     idrstmpl,ndpts,fld,ier)
            if ( ier .ne. 0 ) then
                ierr=14
                return
            endif
            have7=.true.
          elseif (idrsnum.eq.50) then
            call simunpack(cgrib(ipos+5),lensec-6,idrstmpl,ndpts-1,
     &                     fld(2))
            ieee=idrstmpl(5)
            call rdieee(ieee,fld(1),1)
            have7=.true.
          else
            print *,'getfield: Data Representation Template ',idrsnum,
     &              ' not yet implemented.'
            ierr=9
            return
          endif
        endif
        !
        !   Check to see if we read pass the end of the GRIB
        !   message and missed the terminator string '7777'.
        !
        ipos=ipos+lensec                 ! Update beginning of section pointer
        if (ipos.gt.(istart+lengrib)) then
          print *,'getfield: "7777"  not found at end of GRIB message.'
          ierr=7
          return
        endif

        if (have3.and.have4.and.have5.and.have6.and.have7) return
        
      enddo

!
!  If exited from above loop, the end of the GRIB message was reached
!  before the requested field was found.
!
      print *,'getfield: GRIB message contained ',numlocal,
     &        ' different fields.'
      print *,'getfield: The request was for the ',ifldnum,
     &        ' field.'
      ierr=6

      return
      end


      subroutine unpack3(cgrib,lcgrib,iofst,igds,igdstmpl,
     &                   mapgridlen,ideflist,idefnum,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    unpack3 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-26
!
! ABSTRACT: This subroutine unpacks Section 3 (Grid Definition Section)
!   starting at octet 6 of that Section.  
!
! PROGRAM HISTORY LOG:
! 2000-05-26  Gilbert
!
! USAGE:    CALL unpack3(cgrib,lcgrib,lensec,iofst,igds,igdstmpl,
!    &                   mapgridlen,ideflist,idefnum,ierr)
!   INPUT ARGUMENT LIST:
!     cgrib    - Character array that contains the GRIB2 message
!     lcgrib   - Length (in bytes) of GRIB message array cgrib.
!     iofst    - Bit offset of the beginning of Section 3.
!
!   OUTPUT ARGUMENT LIST:      
!     iofst    - Bit offset at the end of Section 3, returned.
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
!     mapgridlen- Number of elements in igdstmpl().  i.e. number of entries
!                in Grid Defintion Template 3.NN  ( NN=igds(5) ).
!     ideflist - (Used if igds(3) .ne. 0)  This array contains the
!                number of grid points contained in each row ( or column ).
!                (part of Section 3)
!     idefnum  - (Used if igds(3) .ne. 0)  The number of entries
!                in array ideflist.  i.e. number of rows ( or columns )
!                for which optional grid points are defined.
!     ierr     - Error return code.
!                0 = no error
!                5 = "GRIB" message contains an undefined Grid Definition
!                    Template.
!
! REMARKS: Uses Fortran 90 module gridtemplates.
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      use gridtemplates

      character(len=1),intent(in) :: cgrib(lcgrib)
      integer,intent(in) :: lcgrib
      integer,intent(inout) :: iofst
      integer,intent(out) :: igds(*),igdstmpl(*),ideflist(*)
      integer,intent(out) :: ierr,idefnum

      integer,allocatable :: mapgrid(:)
      integer :: mapgridlen,ibyttem
      logical needext

      ierr=0

      call g2lib_gbyte(cgrib,lensec,iofst,32)        ! Get Length of Section
      iofst=iofst+32
      iofst=iofst+8     ! skip section number

      call g2lib_gbyte(cgrib,igds(1),iofst,8)     ! Get source of Grid def.
      iofst=iofst+8
      call g2lib_gbyte(cgrib,igds(2),iofst,32)    ! Get number of grid pts.
      iofst=iofst+32
      call g2lib_gbyte(cgrib,igds(3),iofst,8)     ! Get num octets for opt. list
      iofst=iofst+8
      call g2lib_gbyte(cgrib,igds(4),iofst,8)     ! Get interpret. for opt. list
      iofst=iofst+8
      call g2lib_gbyte(cgrib,igds(5),iofst,16)    ! Get Grid Def Template num.
      iofst=iofst+16
      if (igds(1).eq.0) then
!      if (igds(1).eq.0.OR.igds(1).eq.255) then  ! FOR ECMWF TEST ONLY
        allocate(mapgrid(lensec))
        !   Get Grid Definition Template
        call getgridtemplate(igds(5),mapgridlen,mapgrid,needext,
     &                       iret)
        if (iret.ne.0) then
          ierr=5
          return
        endif
      else
!        igdstmpl=-1
        mapgridlen=0
        needext=.false.
      endif
      !
      !   Unpack each value into array igdstmpl from the
      !   the appropriate number of octets, which are specified in
      !   corresponding entries in array mapgrid.
      !
      ibyttem=0
      do i=1,mapgridlen
        nbits=iabs(mapgrid(i))*8
        if ( mapgrid(i).ge.0 ) then
          call g2lib_gbyte(cgrib,igdstmpl(i),iofst,nbits)
        else
          call g2lib_gbyte(cgrib,isign,iofst,1)
          call g2lib_gbyte(cgrib,igdstmpl(i),iofst+1,nbits-1)
          if (isign.eq.1) igdstmpl(i)=-igdstmpl(i)
        endif
        iofst=iofst+nbits
        ibyttem=ibyttem+iabs(mapgrid(i))
      enddo
      !
      !   Check to see if the Grid Definition Template needs to be
      !   extended.
      !   The number of values in a specific template may vary
      !   depending on data specified in the "static" part of the
      !   template.
      !
      if ( needext ) then
        call extgridtemplate(igds(5),igdstmpl,newmapgridlen,mapgrid)
        !   Unpack the rest of the Grid Definition Template
        do i=mapgridlen+1,newmapgridlen
          nbits=iabs(mapgrid(i))*8
          if ( mapgrid(i).ge.0 ) then
            call g2lib_gbyte(cgrib,igdstmpl(i),iofst,nbits)
          else
            call g2lib_gbyte(cgrib,isign,iofst,1)
            call g2lib_gbyte(cgrib,igdstmpl(i),iofst+1,nbits-1)
            if (isign.eq.1) igdstmpl(i)=-igdstmpl(i)
          endif
          iofst=iofst+nbits
          ibyttem=ibyttem+iabs(mapgrid(i))
        enddo
        mapgridlen=newmapgridlen
      endif
      !
      !   Unpack optional list of numbers defining number of points
      !   in each row or column, if included.  This is used for non regular
      !   grids.
      !
      if ( igds(3).ne.0 ) then
         nbits=igds(3)*8
         idefnum=(lensec-14-ibyttem)/igds(3)
         call g2lib_gbytes(cgrib,ideflist,iofst,nbits,0,idefnum)
         iofst=iofst+(nbits*idefnum)
      else
         idefnum=0
      endif
      if( allocated(mapgrid) ) deallocate(mapgrid)
      return    ! End of Section 3 processing
      end


      subroutine unpack4(cgrib,lcgrib,iofst,ipdsnum,ipdstmpl,mappdslen,
     &                   coordlist,numcoord,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    unpack4 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-26
!
! ABSTRACT: This subroutine unpacks Section 4 (Product Definition Section)
!   starting at octet 6 of that Section.  
!
! PROGRAM HISTORY LOG:
! 2000-05-26  Gilbert
!
! USAGE:    CALL unpack4(cgrib,lcgrib,iofst,ipdsnum,ipdstmpl,mappdslen,
!    &                   coordlist,numcoord,ierr)
!   INPUT ARGUMENT LIST:
!     cgrib    - Character array that contains the GRIB2 message
!     lcgrib   - Length (in bytes) of GRIB message array cgrib.
!     iofst    - Bit offset of the beginning of Section 4.
!
!   OUTPUT ARGUMENT LIST:      
!     iofst    - Bit offset of the end of Section 4, returned.
!     ipdsnum  - Product Definition Template Number ( see Code Table 4.0)
!     ipdstmpl - Contains the data values for the specified Product Definition
!                Template ( N=ipdsnum ).  Each element of this integer
!                array contains an entry (in the order specified) of Product
!                Defintion Template 4.N
!     mappdslen- Number of elements in ipdstmpl().  i.e. number of entries
!                in Product Defintion Template 4.N  ( N=ipdsnum ).
!     coordlist- Array containg floating point values intended to document
!                the vertical discretisation associated to model data
!                on hybrid coordinate vertical levels.  (part of Section 4)
!     numcoord - number of values in array coordlist.
!     ierr     - Error return code.
!                0 = no error
!                5 = "GRIB" message contains an undefined Product Definition
!                    Template.
!
! REMARKS: Uses Fortran 90 module pdstemplates.
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      use pdstemplates

      character(len=1),intent(in) :: cgrib(lcgrib)
      integer,intent(in) :: lcgrib
      integer,intent(inout) :: iofst
      real,intent(out) :: coordlist(*)
      integer,intent(out) :: ipdsnum,ipdstmpl(*)
      integer,intent(out) :: ierr,numcoord

      real(4),allocatable :: coordieee(:)
      integer,allocatable :: mappds(:)
      integer :: mappdslen
      logical needext

      ierr=0

      call g2lib_gbyte(cgrib,lensec,iofst,32)        ! Get Length of Section
      iofst=iofst+32
      iofst=iofst+8     ! skip section number
      allocate(mappds(lensec))

      call g2lib_gbyte(cgrib,numcoord,iofst,16)    ! Get num of coordinate values
      iofst=iofst+16
      call g2lib_gbyte(cgrib,ipdsnum,iofst,16)    ! Get Prod. Def Template num.
      iofst=iofst+16
      !   Get Product Definition Template
      call getpdstemplate(ipdsnum,mappdslen,mappds,needext,iret)
      if (iret.ne.0) then
        ierr=5
        return
      endif
      !
      !   Unpack each value into array ipdstmpl from the
      !   the appropriate number of octets, which are specified in
      !   corresponding entries in array mappds.
      !
      do i=1,mappdslen
        nbits=iabs(mappds(i))*8
        if ( mappds(i).ge.0 ) then
          call g2lib_gbyte(cgrib,ipdstmpl(i),iofst,nbits)
        else
          call g2lib_gbyte(cgrib,isign,iofst,1)
          call g2lib_gbyte(cgrib,ipdstmpl(i),iofst+1,nbits-1)
          if (isign.eq.1) ipdstmpl(i)=-ipdstmpl(i)
        endif
        iofst=iofst+nbits
      enddo
      !
      !   Check to see if the Product Definition Template needs to be
      !   extended.
      !   The number of values in a specific template may vary
      !   depending on data specified in the "static" part of the
      !   template.
      !
      if ( needext ) then
        call extpdstemplate(ipdsnum,ipdstmpl,newmappdslen,mappds)
        !   Unpack the rest of the Product Definition Template
        do i=mappdslen+1,newmappdslen
          nbits=iabs(mappds(i))*8
          if ( mappds(i).ge.0 ) then
            call g2lib_gbyte(cgrib,ipdstmpl(i),iofst,nbits)
          else
            call g2lib_gbyte(cgrib,isign,iofst,1)
            call g2lib_gbyte(cgrib,ipdstmpl(i),iofst+1,nbits-1)
            if (isign.eq.1) ipdstmpl(i)=-ipdstmpl(i)
          endif
          iofst=iofst+nbits
        enddo
        mappdslen=newmappdslen
      endif
      !
      !   Get Optional list of vertical coordinate values
      !   after the Product Definition Template, if necessary.
      !
      if ( numcoord .ne. 0 ) then
        allocate (coordieee(numcoord))
        call g2lib_gbytes(cgrib,coordieee,iofst,32,0,numcoord)
        call rdieee(coordieee,coordlist,numcoord)
        deallocate (coordieee)
        iofst=iofst+(32*numcoord)
      endif
      if( allocated(mappds) ) deallocate(mappds)
      return    ! End of Section 4 processing
      end


      subroutine unpack5(cgrib,lcgrib,iofst,ndpts,idrsnum,idrstmpl,
     &                   mapdrslen,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    unpack5 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-26
!
! ABSTRACT: This subroutine unpacks Section 5 (Data Representation Section)
!   starting at octet 6 of that Section.  
!
! PROGRAM HISTORY LOG:
! 2000-05-26  Gilbert
!
! USAGE:    CALL unpack5(cgrib,lcgrib,iofst,ndpts,idrsnum,idrstmpl,
!                        mapdrslen,ierr)
!   INPUT ARGUMENT LIST:
!     cgrib    - Character array that contains the GRIB2 message
!     lcgrib   - Length (in bytes) of GRIB message array cgrib.
!     iofst    - Bit offset of the beginning of Section 5.
!
!   OUTPUT ARGUMENT LIST:      
!     iofst    - Bit offset at the end of Section 5, returned.
!     ndpts    - Number of data points unpacked and returned.
!     idrsnum  - Data Representation Template Number ( see Code Table 5.0)
!     idrstmpl - Contains the data values for the specified Data Representation
!                Template ( N=idrsnum ).  Each element of this integer
!                array contains an entry (in the order specified) of Data
!                Representation Template 5.N
!     mapdrslen- Number of elements in idrstmpl().  i.e. number of entries
!                in Data Representation Template 5.N  ( N=idrsnum ).
!     ierr     - Error return code.
!                0 = no error
!                7 = "GRIB" message contains an undefined Data
!                    Representation Template.
!
! REMARKS: None
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      use drstemplates

      character(len=1),intent(in) :: cgrib(lcgrib)
      integer,intent(in) :: lcgrib
      integer,intent(inout) :: iofst
      integer,intent(out) :: ndpts,idrsnum,idrstmpl(*)
      integer,intent(out) :: ierr

C      integer,allocatable :: mapdrs(:)
      integer,allocatable :: mapdrs(:)
      integer :: mapdrslen
      logical needext

      ierr=0

      call g2lib_gbyte(cgrib,lensec,iofst,32)        ! Get Length of Section
      iofst=iofst+32
      iofst=iofst+8     ! skip section number
      allocate(mapdrs(lensec))

      call g2lib_gbyte(cgrib,ndpts,iofst,32)    ! Get num of data points
      iofst=iofst+32
      call g2lib_gbyte(cgrib,idrsnum,iofst,16)     ! Get Data Rep Template Num.
      iofst=iofst+16
      !   Gen Data Representation Template
      call getdrstemplate(idrsnum,mapdrslen,mapdrs,needext,iret)
      if (iret.ne.0) then
        ierr=7
        return
      endif
      !
      !   Unpack each value into array ipdstmpl from the
      !   the appropriate number of octets, which are specified in
      !   corresponding entries in array mappds.
      !
      do i=1,mapdrslen
        nbits=iabs(mapdrs(i))*8
        if ( mapdrs(i).ge.0 ) then
          call g2lib_gbyte(cgrib,idrstmpl(i),iofst,nbits)
        else
          call g2lib_gbyte(cgrib,isign,iofst,1)
          call g2lib_gbyte(cgrib,idrstmpl(i),iofst+1,nbits-1)
          if (isign.eq.1) idrstmpl(i)=-idrstmpl(i)
        endif
        iofst=iofst+nbits
      enddo
      !
      !   Check to see if the Data Representation Template needs to be
      !   extended.
      !   The number of values in a specific template may vary
      !   depending on data specified in the "static" part of the
      !   template.
      !
      if ( needext ) then
        call extdrstemplate(idrsnum,idrstmpl,newmapdrslen,mapdrs)
        !   Unpack the rest of the Data Representation Template
        do i=mapdrslen+1,newmapdrslen
          nbits=iabs(mapdrs(i))*8
          if ( mapdrs(i).ge.0 ) then
            call g2lib_gbyte(cgrib,idrstmpl(i),iofst,nbits)
          else
            call g2lib_gbyte(cgrib,isign,iofst,1)
            call g2lib_gbyte(cgrib,idrstmpl(i),iofst+1,nbits-1)
            if (isign.eq.1) idrstmpl(i)=-idrstmpl(i)
          endif
          iofst=iofst+nbits
        enddo
        mapdrslen=newmapdrslen
      endif
      if( allocated(mapdrs) ) deallocate(mapdrs)
      return    ! End of Section 5 processing
      end


      subroutine unpack6(cgrib,lcgrib,iofst,ngpts,ibmap,bmap,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    unpack6 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-26
!
! ABSTRACT: This subroutine unpacks Section 6 (Bit-Map Section)
!   starting at octet 6 of that Section.  
!
! PROGRAM HISTORY LOG:
! 2000-05-26  Gilbert
!
! USAGE:    CALL unpack6(cgrib,lcgrib,iofst,ngpts,ibmap,bmap,ierr)
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
!     bmap()   - Logical*1 array containing decoded bitmap. ( if ibmap=0 )
!     ierr     - Error return code.
!                0 = no error
!                4 = Unrecognized pre-defined bit-map.
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
      logical*1,intent(out) :: bmap(ngpts)

      integer :: intbmap(ngpts)

      ierr=0

      iofst=iofst+32    ! skip Length of Section
      iofst=iofst+8     ! skip section number

      call g2lib_gbyte(cgrib,ibmap,iofst,8)    ! Get bit-map indicator
      iofst=iofst+8

      if (ibmap.eq.0) then               ! Unpack bitmap
        call g2lib_gbytes(cgrib,intbmap,iofst,1,0,ngpts)
        iofst=iofst+ngpts
        do j=1,ngpts
          bmap(j)=.true.
          if (intbmap(j).eq.0) bmap(j)=.false.
        enddo
      elseif (ibmap.eq.254) then               ! Use previous bitmap
        return
      elseif (ibmap.eq.255) then               ! No bitmap in message
        bmap(1:ngpts)=.true.
      else
        print *,'unpack6: Predefined bitmap ',ibmap,' not recognized.'
        ierr=4
      endif
      
      return    ! End of Section 6 processing
      end

