      subroutine gf_unpack4(cgrib,lcgrib,iofst,ipdsnum,ipdstmpl,
     &                      mappdslen,coordlist,numcoord,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    gf_unpack4 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-26
!
! ABSTRACT: This subroutine unpacks Section 4 (Product Definition Section)
!   starting at octet 6 of that Section.  
!
! PROGRAM HISTORY LOG:
! 2000-05-26  Gilbert
! 2002-01-24  Gilbert  - Changed to dynamically allocate arrays
!                        and to pass pointers to those arrays through
!                        the argument list.
!
! USAGE:    CALL gf_unpack4(cgrib,lcgrib,iofst,ipdsnum,ipdstmpl,mappdslen,
!    &                   coordlist,numcoord,ierr)
!   INPUT ARGUMENT LIST:
!     cgrib    - Character array that contains the GRIB2 message
!     lcgrib   - Length (in bytes) of GRIB message array cgrib.
!     iofst    - Bit offset of the beginning of Section 4.
!
!   OUTPUT ARGUMENT LIST:      
!     iofst    - Bit offset of the end of Section 4, returned.
!     ipdsnum  - Product Definition Template Number ( see Code Table 4.0)
!     ipdstmpl - Pointer to integer array containing the data values for 
!                the specified Product Definition
!                Template ( N=ipdsnum ).  Each element of this integer
!                array contains an entry (in the order specified) of Product
!                Defintion Template 4.N
!     mappdslen- Number of elements in ipdstmpl().  i.e. number of entries
!                in Product Defintion Template 4.N  ( N=ipdsnum ).
!     coordlist- Pointer to real array containing floating point values 
!                intended to document
!                the vertical discretisation associated to model data
!                on hybrid coordinate vertical levels.  (part of Section 4)
!     numcoord - number of values in array coordlist.
!     ierr     - Error return code.
!                0 = no error
!                5 = "GRIB" message contains an undefined Product Definition
!                    Template.
!                6 = memory allocation error
!
! REMARKS: Uses Fortran 90 module pdstemplates and module re_alloc.
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      use pdstemplates
      use re_alloc        !  needed for subroutine realloc

      character(len=1),intent(in) :: cgrib(lcgrib)
      integer,intent(in) :: lcgrib
      integer,intent(inout) :: iofst
      real,pointer,dimension(:) :: coordlist
      integer,pointer,dimension(:) :: ipdstmpl
      integer,intent(out) :: ipdsnum
      integer,intent(out) :: ierr,numcoord

      real(4),allocatable :: coordieee(:)
      integer,allocatable :: mappds(:)
      integer :: mappdslen
      logical needext

      ierr=0
      nullify(ipdstmpl,coordlist)

      call gbyte(cgrib,lensec,iofst,32)        ! Get Length of Section
      iofst=iofst+32
      iofst=iofst+8     ! skip section number
      allocate(mappds(lensec))

      call gbyte(cgrib,numcoord,iofst,16)    ! Get num of coordinate values
      iofst=iofst+16
      call gbyte(cgrib,ipdsnum,iofst,16)    ! Get Prod. Def Template num.
      iofst=iofst+16
      !   Get Product Definition Template
      call getpdstemplate(ipdsnum,mappdslen,mappds,needext,iret)
      if (iret.ne.0) then
        ierr=5
        if( allocated(mappds) ) deallocate(mappds)
        return
      endif
      !
      !   Unpack each value into array ipdstmpl from the
      !   the appropriate number of octets, which are specified in
      !   corresponding entries in array mappds.
      !
      istat=0
      if (mappdslen.gt.0) allocate(ipdstmpl(mappdslen),stat=istat)
      if (istat.ne.0) then
         ierr=6
         nullify(ipdstmpl)
         if( allocated(mappds) ) deallocate(mappds)
         return
      endif
      do i=1,mappdslen
        nbits=iabs(mappds(i))*8
        if ( mappds(i).ge.0 ) then
          call gbyte(cgrib,ipdstmpl(i),iofst,nbits)
        else
          call gbyte(cgrib,isign,iofst,1)
          call gbyte(cgrib,ipdstmpl(i),iofst+1,nbits-1)
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
        call realloc(ipdstmpl,mappdslen,newmappdslen,istat)
        !   Unpack the rest of the Product Definition Template
        do i=mappdslen+1,newmappdslen
          nbits=iabs(mappds(i))*8
          if ( mappds(i).ge.0 ) then
            call gbyte(cgrib,ipdstmpl(i),iofst,nbits)
          else
            call gbyte(cgrib,isign,iofst,1)
            call gbyte(cgrib,ipdstmpl(i),iofst+1,nbits-1)
            if (isign.eq.1) ipdstmpl(i)=-ipdstmpl(i)
          endif
          iofst=iofst+nbits
        enddo
        mappdslen=newmappdslen
      endif
      if( allocated(mappds) ) deallocate(mappds)
      !
      !   Get Optional list of vertical coordinate values
      !   after the Product Definition Template, if necessary.
      !
      nullify(coordlist)
      if ( numcoord .ne. 0 ) then
         allocate (coordieee(numcoord),stat=istat1)
         allocate(coordlist(numcoord),stat=istat)
         if ((istat1+istat).ne.0) then
            ierr=6
            nullify(coordlist)
            if( allocated(coordieee) ) deallocate(coordieee)
            return
         endif
        call gbytes(cgrib,coordieee,iofst,32,0,numcoord)
        call rdieee(coordieee,coordlist,numcoord)
        deallocate (coordieee)
        iofst=iofst+(32*numcoord)
      endif
      
      return    ! End of Section 4 processing
      end

