      subroutine gf_unpack5(cgrib,lcgrib,iofst,ndpts,idrsnum,idrstmpl,
     &                   mapdrslen,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    gf_unpack5 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-26
!
! ABSTRACT: This subroutine unpacks Section 5 (Data Representation Section)
!   starting at octet 6 of that Section.  
!
! PROGRAM HISTORY LOG:
! 2000-05-26  Gilbert
! 2002-01-24  Gilbert  - Changed to dynamically allocate arrays
!                        and to pass pointers to those arrays through
!                        the argument list.
!
! USAGE:    CALL gf_unpack5(cgrib,lcgrib,iofst,ndpts,idrsnum,idrstmpl,
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
!     idrstmpl - Pointer to an integer array containing the data values for 
!                the specified Data Representation
!                Template ( N=idrsnum ).  Each element of this integer
!                array contains an entry (in the order specified) of Data
!                Representation Template 5.N
!     mapdrslen- Number of elements in idrstmpl().  i.e. number of entries
!                in Data Representation Template 5.N  ( N=idrsnum ).
!     ierr     - Error return code.
!                0 = no error
!                6 = memory allocation error
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
      use re_alloc        !  needed for subroutine realloc

      character(len=1),intent(in) :: cgrib(lcgrib)
      integer,intent(in) :: lcgrib
      integer,intent(inout) :: iofst
      integer,intent(out) :: ndpts,idrsnum
      integer,pointer,dimension(:) :: idrstmpl
      integer,intent(out) :: ierr

      integer,allocatable :: mapdrs(:)
      integer :: mapdrslen
      logical needext

      ierr=0
      nullify(idrstmpl)

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
        if( allocated(mapdrs) ) deallocate(mapdrs)
        return
      endif
      !
      !   Unpack each value into array ipdstmpl from the
      !   the appropriate number of octets, which are specified in
      !   corresponding entries in array mappds.
      !
      istat=0
      if (mapdrslen.gt.0) allocate(idrstmpl(mapdrslen),stat=istat)
      if (istat.ne.0) then
         ierr=6
         nullify(idrstmpl)
         if( allocated(mapdrs) ) deallocate(mapdrs)
         return
      endif
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
        call realloc(idrstmpl,mapdrslen,newmapdrslen,istat)
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

