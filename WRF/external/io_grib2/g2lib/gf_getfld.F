      subroutine gf_getfld(cgrib,lcgrib,ifldnum,unpack,expand,gfld,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    gf_getfld 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-26
!
! ABSTRACT: This subroutine returns the Grid Definition, Product Definition,
!   Bit-map ( if applicable ), and the unpacked data for a given data
!   field.  All of the information returned is stored in a derived
!   type variable, gfld.  Gfld is of type gribfield, which is defined
!   in module grib_mod, so users of this routine will need to include
!   the line "USE GRIB_MOD" in their calling routine.  Each component of the 
!   gribfield type is described in the OUTPUT ARGUMENT LIST section below.
!
!   Since there can be multiple data fields packed into a GRIB2
!   message, the calling routine indicates which field is being requested
!   with the ifldnum argument.
!
! PROGRAM HISTORY LOG:
! 2000-05-26  Gilbert
! 2002-01-24  Gilbert  - Changed to pass back derived type gribfield
!                        variable through argument list, instead of
!                        having many different arguments.
! 2004-05-20  Gilbert  - Added check to see if previous a bit-map is specified,
!                        but none was found.
!
! USAGE:    CALL gf_getfld(cgrib,lcgrib,ifldnum,unpack,expand,gfld,ierr)
!   INPUT ARGUMENT LIST:
!     cgrib    - Character array that contains the GRIB2 message
!     lcgrib   - Length (in bytes) of GRIB message array cgrib.
!     ifldnum  - Specifies which field in the GRIB2 message to return.
!     unpack   - Logical value indicating whether to unpack bitmap/data
!                .true. = unpack bitmap and data values
!                .false. = do not unpack bitmap and data values
!     expand   - Boolean value indicating whether the data points should be
!                expanded to the correspond grid, if a bit-map is present.
!                1 = if possible, expand data field to grid, inserting zero
!                    values at gridpoints that are bitmapped out.
!                    (SEE REMARKS2)
!                0 = do not expand data field, leaving it an array of
!                    consecutive data points for each "1" in the bitmap.
!                This argument is ignored if unpack == 0 OR if the
!                returned field does not contain a bit-map.
!
!   OUTPUT ARGUMENT LIST:      
!     gfld - derived type gribfield ( defined in module grib_mod )
!            ( NOTE: See Remarks Section )
!        gfld%version = GRIB edition number ( currently 2 )
!        gfld%discipline = Message Discipline ( see Code Table 0.0 )
!        gfld%idsect() = Contains the entries in the Identification
!                        Section ( Section 1 )
!                        This element is actually a pointer to an array
!                        that holds the data.
!            gfld%idsect(1)  = Identification of originating Centre 
!                                    ( see Common Code Table C-1 )
!                             7 - US National Weather Service
!            gfld%idsect(2)  = Identification of originating Sub-centre
!            gfld%idsect(3)  = GRIB Master Tables Version Number
!                                    ( see Code Table 1.0 )
!                             0 - Experimental
!                             1 - Initial operational version number
!            gfld%idsect(4)  = GRIB Local Tables Version Number
!                                    ( see Code Table 1.1 )
!                             0     - Local tables not used
!                             1-254 - Number of local tables version used
!            gfld%idsect(5)  = Significance of Reference Time (Code Table 1.2)
!                             0 - Analysis
!                             1 - Start of forecast
!                             2 - Verifying time of forecast
!                             3 - Observation time
!            gfld%idsect(6)  = Year ( 4 digits )
!            gfld%idsect(7)  = Month
!            gfld%idsect(8)  = Day
!            gfld%idsect(9)  = Hour
!            gfld%idsect(10)  = Minute
!            gfld%idsect(11)  = Second
!            gfld%idsect(12)  = Production status of processed data
!                                    ( see Code Table 1.3 )
!                              0 - Operational products
!                              1 - Operational test products
!                              2 - Research products
!                              3 - Re-analysis products
!            gfld%idsect(13)  = Type of processed data ( see Code Table 1.4 )
!                              0  - Analysis products
!                              1  - Forecast products
!                              2  - Analysis and forecast products
!                              3  - Control forecast products
!                              4  - Perturbed forecast products
!                              5  - Control and perturbed forecast products
!                              6  - Processed satellite observations
!                              7  - Processed radar observations
!        gfld%idsectlen = Number of elements in gfld%idsect().
!        gfld%local() = Pointer to character array containing contents
!                       of Local Section 2, if included
!        gfld%locallen = length of array gfld%local()
!        gfld%ifldnum = field number within GRIB message
!        gfld%griddef = Source of grid definition (see Code Table 3.0)
!                      0 - Specified in Code table 3.1
!                      1 - Predetermined grid Defined by originating centre
!        gfld%ngrdpts = Number of grid points in the defined grid.
!        gfld%numoct_opt = Number of octets needed for each 
!                          additional grid points definition.  
!                          Used to define number of
!                          points in each row ( or column ) for
!                          non-regular grids.  
!                          = 0, if using regular grid.
!        gfld%interp_opt = Interpretation of list for optional points 
!                          definition.  (Code Table 3.11)
!        gfld%igdtnum = Grid Definition Template Number (Code Table 3.1)
!        gfld%igdtmpl() = Contains the data values for the specified Grid 
!                         Definition Template ( NN=gfld%igdtnum ).  Each 
!                         element of this integer array contains an entry (in 
!                         the order specified) of Grid Defintion Template 3.NN
!                         This element is actually a pointer to an array
!                         that holds the data.
!        gfld%igdtlen = Number of elements in gfld%igdtmpl().  i.e. number of
!                       entries in Grid Defintion Template 3.NN  
!                       ( NN=gfld%igdtnum ).
!        gfld%list_opt() = (Used if gfld%numoct_opt .ne. 0)  This array 
!                          contains the number of grid points contained in 
!                          each row ( or column ).  (part of Section 3)
!                          This element is actually a pointer to an array
!                          that holds the data.  This pointer is nullified
!                          if gfld%numoct_opt=0.
!        gfld%num_opt = (Used if gfld%numoct_opt .ne. 0)  The number of entries
!                       in array ideflist.  i.e. number of rows ( or columns )
!                       for which optional grid points are defined.  This value
!                       is set to zero, if gfld%numoct_opt=0.
!        gfdl%ipdtnum = Product Definition Template Number (see Code Table 4.0)
!        gfld%ipdtmpl() = Contains the data values for the specified Product 
!                         Definition Template ( N=gfdl%ipdtnum ).  Each element
!                         of this integer array contains an entry (in the 
!                         order specified) of Product Defintion Template 4.N.
!                         This element is actually a pointer to an array
!                         that holds the data.
!        gfld%ipdtlen = Number of elements in gfld%ipdtmpl().  i.e. number of
!                       entries in Product Defintion Template 4.N  
!                       ( N=gfdl%ipdtnum ).
!        gfld%coord_list() = Real array containing floating point values 
!                            intended to document the vertical discretisation
!                            associated to model data on hybrid coordinate
!                            vertical levels.  (part of Section 4)
!                            This element is actually a pointer to an array
!                            that holds the data.
!        gfld%num_coord = number of values in array gfld%coord_list().
!        gfld%ndpts = Number of data points unpacked and returned.
!        gfld%idrtnum = Data Representation Template Number 
!                       ( see Code Table 5.0)
!        gfld%idrtmpl() = Contains the data values for the specified Data 
!                         Representation Template ( N=gfld%idrtnum ).  Each 
!                         element of this integer array contains an entry 
!                         (in the order specified) of Product Defintion 
!                         Template 5.N.
!                         This element is actually a pointer to an array
!                         that holds the data.
!        gfld%idrtlen = Number of elements in gfld%idrtmpl().  i.e. number 
!                       of entries in Data Representation Template 5.N 
!                       ( N=gfld%idrtnum ).
!        gfld%unpacked = logical value indicating whether the bitmap and
!                        data values were unpacked.  If false, 
!                        gfld%bmap and gfld%fld pointers are nullified.
!        gfld%expanded = Logical value indicating whether the data field
!                         was expanded to the grid in the case where a
!                         bit-map is present.  If true, the data points in
!                         gfld%fld match the grid points and zeros were
!                         inserted at grid points where data was bit-mapped
!                         out.  If false, the data values in gfld%fld were
!                         not expanded to the grid and are just a consecutive
!                         array of data points corresponding to each value of
!                         "1" in gfld%bmap.
!        gfld%ibmap = Bitmap indicator ( see Code Table 6.0 )
!                     0 = bitmap applies and is included in Section 6.
!                     1-253 = Predefined bitmap applies
!                     254 = Previously defined bitmap applies to this field
!                     255 = Bit map does not apply to this product.
!        gfld%bmap() = Logical*1 array containing decoded bitmap, 
!                      if ibmap=0 or ibap=254.  Otherwise nullified.
!                      This element is actually a pointer to an array
!                      that holds the data.
!        gfld%fld() = Array of gfld%ndpts unpacked data points.
!                     This element is actually a pointer to an array
!                     that holds the data.
!     ierr     - Error return code.
!                0 = no error
!                1 = Beginning characters "GRIB" not found.
!                2 = GRIB message is not Edition 2.
!                3 = The data field request number was not positive.
!                4 = End string "7777" found, but not where expected.
!                6 = GRIB message did not contain the requested number of
!                    data fields.
!                7 = End string "7777" not found at end of message.
!                8 = Unrecognized Section encountered.
!                9 = Data Representation Template 5.NN not yet implemented.
!               15 = Error unpacking Section 1.
!               16 = Error unpacking Section 2.
!               10 = Error unpacking Section 3.
!               11 = Error unpacking Section 4.
!               12 = Error unpacking Section 5.
!               13 = Error unpacking Section 6.
!               14 = Error unpacking Section 7.
!               17 = Previous bitmap specified, but none exists.
!
! REMARKS: Note that derived type gribfield contains pointers to many
!          arrays of data.  The memory for these arrays is allocated
!          when the values in the arrays are set, to help minimize
!          problems with array overloading.  Because of this users
!          are encouraged to free up this memory, when it is no longer
!          needed, by an explicit call to subroutine gf_free.
!          ( i.e.   CALL GF_FREE(GFLD) )
!
!          Subroutine gb_info can be used to first determine
!          how many data fields exist in a given GRIB message.
!
! REMARKS2: It may not always be possible to expand a bit-mapped data field.
!           If a pre-defined bit-map is used and not included in the GRIB2
!           message itself, this routine would not have the necessary
!           information to expand the data.  In this case, gfld%expanded would
!           would be set to 0 (false), regardless of the value of input
!           argument expand.
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$
      use grib_mod
    
      character(len=1),intent(in) :: cgrib(lcgrib)
      integer,intent(in) :: lcgrib,ifldnum
      logical,intent(in) :: unpack,expand
      type(gribfield),intent(out) :: gfld
      integer,intent(out) :: ierr
!      integer,intent(out) :: igds(*),igdstmpl(*),ideflist(*)
!      integer,intent(out) :: ipdsnum,ipdstmpl(*)
!      integer,intent(out) :: idrsnum,idrstmpl(*)
!      integer,intent(out) :: ndpts,ibmap,idefnum,numcoord
!      logical*1,intent(out) :: bmap(*)
!      real,intent(out) :: fld(*),coordlist(*)
      
      character(len=4),parameter :: grib='GRIB',c7777='7777'
      character(len=4) :: ctemp
      real,pointer,dimension(:) :: newfld
      integer:: listsec0(2),igds(5)
      integer iofst,ibeg,istart
      integer(4) :: ieee
      logical*1,pointer,dimension(:) :: bmpsave
      logical have3,have4,have5,have6,have7

      interface
         subroutine gf_unpack1(cgrib,lcgrib,iofst,ids,idslen,ierr)
            character(len=1),intent(in) :: cgrib(lcgrib)
            integer,intent(in) :: lcgrib
            integer,intent(inout) :: iofst
            integer,pointer,dimension(:) :: ids
            integer,intent(out) :: ierr,idslen
         end subroutine gf_unpack1
         subroutine gf_unpack2(cgrib,lcgrib,iofst,lencsec2,csec2,ierr)
            character(len=1),intent(in) :: cgrib(lcgrib)
            integer,intent(in) :: lcgrib
            integer,intent(inout) :: iofst
            integer,intent(out) :: lencsec2
            integer,intent(out) :: ierr
            character(len=1),pointer,dimension(:) :: csec2
         end subroutine gf_unpack2
         subroutine gf_unpack3(cgrib,lcgrib,iofst,igds,igdstmpl,
     &                         mapgridlen,ideflist,idefnum,ierr)
            character(len=1),intent(in) :: cgrib(lcgrib)
            integer,intent(in) :: lcgrib
            integer,intent(inout) :: iofst
            integer,pointer,dimension(:) :: igdstmpl,ideflist
            integer,intent(out) :: igds(5)
            integer,intent(out) :: ierr,idefnum
         end subroutine gf_unpack3
         subroutine gf_unpack4(cgrib,lcgrib,iofst,ipdsnum,ipdstmpl,
     &                      mappdslen,coordlist,numcoord,ierr)
            character(len=1),intent(in) :: cgrib(lcgrib)
            integer,intent(in) :: lcgrib
            integer,intent(inout) :: iofst
            real,pointer,dimension(:) :: coordlist
            integer,pointer,dimension(:) :: ipdstmpl
            integer,intent(out) :: ipdsnum
            integer,intent(out) :: ierr,numcoord
         end subroutine gf_unpack4
         subroutine gf_unpack5(cgrib,lcgrib,iofst,ndpts,idrsnum,
     &                         idrstmpl,mapdrslen,ierr)
            character(len=1),intent(in) :: cgrib(lcgrib)
            integer,intent(in) :: lcgrib
            integer,intent(inout) :: iofst
            integer,intent(out) :: ndpts,idrsnum
            integer,pointer,dimension(:) :: idrstmpl
            integer,intent(out) :: ierr
         end subroutine gf_unpack5
         subroutine gf_unpack6(cgrib,lcgrib,iofst,ngpts,ibmap,bmap,ierr)
            character(len=1),intent(in) :: cgrib(lcgrib)
            integer,intent(in) :: lcgrib,ngpts
            integer,intent(inout) :: iofst
            integer,intent(out) :: ibmap
            integer,intent(out) :: ierr
            logical*1,pointer,dimension(:) :: bmap
         end subroutine gf_unpack6
         subroutine gf_unpack7(cgrib,lcgrib,iofst,igdsnum,igdstmpl,
     &                         idrsnum,idrstmpl,ndpts,fld,ierr)
            character(len=1),intent(in) :: cgrib(lcgrib)
            integer,intent(in) :: lcgrib,ndpts,idrsnum,igdsnum
            integer,intent(inout) :: iofst
            integer,pointer,dimension(:) :: idrstmpl,igdstmpl
            integer,intent(out) :: ierr
            real,pointer,dimension(:) :: fld
         end subroutine gf_unpack7
      end interface

      have3=.false.
      have4=.false.
      have5=.false.
      have6=.false.
      have7=.false.
      ierr=0
      numfld=0
      gfld%locallen=0
      nullify(gfld%list_opt,gfld%igdtmpl,gfld%ipdtmpl)
      nullify(gfld%coord_list,gfld%idrtmpl,gfld%bmap,gfld%fld)
!
!  Check for valid request number
!  
      if (ifldnum.le.0) then
        print *,'gf_getfld: Request for field number must be positive.'
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
        print *,'gf_getfld:  Beginning characters GRIB not found.'
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
        print *,'gf_getfld: can only decode GRIB edition 2.'
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
            print *,'gf_getfld: "7777" found, but not where expected.'
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
        !  Check to see if section number is valid
        !
        if ( (isecnum.lt.1).OR.(isecnum.gt.7) ) then
          print *,'gf_getfld: Unrecognized Section Encountered=',isecnum     
          ierr=8
          return
        endif
        !
        !   If found Section 1, decode elements in Identification Section
        !
        if (isecnum.eq.1) then
          iofst=iofst-40       ! reset offset to beginning of section
          call gf_unpack1(cgrib,lcgrib,iofst,gfld%idsect,
     &                    gfld%idsectlen,jerr)
          if (jerr.ne.0) then
            ierr=15
            return
          endif
        endif
        !
        !   If found Section 2, Grab local section
        !   Save in case this is the latest one before the requested field.
        !
        if (isecnum.eq.2) then
          iofst=iofst-40       ! reset offset to beginning of section
          if (associated(gfld%local)) deallocate(gfld%local)
          call gf_unpack2(cgrib,lcgrib,iofst,gfld%locallen,
     &                    gfld%local,jerr)
          if (jerr.ne.0) then
            ierr=16
            return
          endif
        endif
        !
        !   If found Section 3, unpack the GDS info using the 
        !   appropriate template.  Save in case this is the latest
        !   grid before the requested field.
        !
        if (isecnum.eq.3) then
          iofst=iofst-40       ! reset offset to beginning of section
          if (associated(gfld%igdtmpl)) deallocate(gfld%igdtmpl)
          if (associated(gfld%list_opt)) deallocate(gfld%list_opt)
          call gf_unpack3(cgrib,lcgrib,iofst,igds,gfld%igdtmpl,
     &                 gfld%igdtlen,gfld%list_opt,gfld%num_opt,jerr)
          if (jerr.eq.0) then
            have3=.true.
            gfld%griddef=igds(1)
            gfld%ngrdpts=igds(2)
            gfld%numoct_opt=igds(3)
            gfld%interp_opt=igds(4)
            gfld%igdtnum=igds(5)
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
            gfld%discipline=listsec0(1)
            gfld%version=listsec0(2)
            gfld%ifldnum=ifldnum
            gfld%unpacked=unpack
            gfld%expanded=.false.
            iofst=iofst-40       ! reset offset to beginning of section
            call gf_unpack4(cgrib,lcgrib,iofst,gfld%ipdtnum,
     &                      gfld%ipdtmpl,gfld%ipdtlen,gfld%coord_list,
     &                      gfld%num_coord,jerr)
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
          call gf_unpack5(cgrib,lcgrib,iofst,gfld%ndpts,gfld%idrtnum,
     &                    gfld%idrtmpl,gfld%idrtlen,jerr)
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
          if (unpack) then   ! unpack bitmap
            iofst=iofst-40       ! reset offset to beginning of section
            bmpsave=>gfld%bmap      ! save pointer to previous bitmap
            call gf_unpack6(cgrib,lcgrib,iofst,gfld%ngrdpts,gfld%ibmap,
     &                   gfld%bmap,jerr)
            if (jerr.eq.0) then
              have6=.true.
              if (gfld%ibmap .eq. 254) then    ! use previously specified bitmap
                 if ( associated(bmpsave) ) then
                    gfld%bmap=>bmpsave
                 else
                    print *,'gf_getfld:  Previous bit-map specified,',
     &                       ' but none exists,'
                    ierr=17
                    return
                 endif
              else                             ! get rid of it
                 if ( associated(bmpsave) ) deallocate(bmpsave)
              endif
            else
              ierr=13
              return
            endif
          else    ! do not unpack bitmap
            call g2lib_gbyte(cgrib,gfld%ibmap,iofst,8)      ! Get BitMap Indicator
            have6=.true.
          endif
        endif
        !
        !   If found Section 7, check to see if this field is the
        !   one requested.
        !
        if ((isecnum.eq.7).and.(numfld.eq.ifldnum).and.unpack) then
          iofst=iofst-40       ! reset offset to beginning of section
          call gf_unpack7(cgrib,lcgrib,iofst,gfld%igdtnum,
     &                    gfld%igdtmpl,gfld%idrtnum,
     &                    gfld%idrtmpl,gfld%ndpts,
     &                    gfld%fld,jerr)
          if (jerr.eq.0) then
            have7=.true.
            !  If bitmap is used with this field, expand data field
            !  to grid, if possible.
            if ( gfld%ibmap .ne. 255 .AND. associated(gfld%bmap) ) then
               if ( expand ) then
                  allocate(newfld(gfld%ngrdpts))
                  !newfld(1:gfld%ngrdpts)=0.0
                  !newfld=unpack(gfld%fld,gfld%bmap,newfld)
                  n=1
                  do j=1,gfld%ngrdpts
                      if ( gfld%bmap(j) ) then
                        newfld(j)=gfld%fld(n)
                        n=n+1
                      else
                        newfld(j)=0.0
                      endif
                  enddo
                  deallocate(gfld%fld);
                  gfld%fld=>newfld;
                  gfld%expanded=.true.
               else 
                  gfld%expanded=.false.
               endif
            else 
               gfld%expanded=.true.
            endif
          else
            print *,'gf_getfld: return from gf_unpack7 = ',jerr
            ierr=14
            return
          endif
        endif
        !
        !   Check to see if we read pass the end of the GRIB
        !   message and missed the terminator string '7777'.
        !
        ipos=ipos+lensec                 ! Update beginning of section pointer
        if (ipos.gt.(istart+lengrib)) then
          print *,'gf_getfld: "7777"  not found at end of GRIB message.'
          ierr=7
          return
        endif
        !
        !  If unpacking requested, return when all sections have been
        !  processed
        !
        if (unpack.and.have3.and.have4.and.have5.and.have6.and.have7)
     &      return
        !
        !  If unpacking is not requested, return when sections 
        !  3 through 6 have been processed
        !
        if ((.NOT.unpack).and.have3.and.have4.and.have5.and.have6)
     &      return
        
      enddo

!
!  If exited from above loop, the end of the GRIB message was reached
!  before the requested field was found.
!
      print *,'gf_getfld: GRIB message contained ',numlocal,
     &        ' different fields.'
      print *,'gf_getfld: The request was for the ',ifldnum,
     &        ' field.'
      ierr=6

      return
      end

