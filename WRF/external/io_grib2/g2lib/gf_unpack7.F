      subroutine gf_unpack7(cgrib,lcgrib,iofst,igdsnum,igdstmpl,
     &                      idrsnum,idrstmpl,ndpts,fld,ierr)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    gf_unpack7 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2002-01-24
!
! ABSTRACT: This subroutine unpacks GRIB2 Section 7 (Data Section).
!
! PROGRAM HISTORY LOG:
! 2002-01-24  Gilbert
! 2002-12-17  Gilbert  - Added support for new templates using 
!                        PNG and JPEG2000 algorithms/templates.
! 2004-12-29  Gilbert  - Added check on comunpack return code.
!
! USAGE:    CALL gf_unpack7(cgrib,lcgrib,iofst,igdsnum,igdstmpl,
!    &                      idrsnum,idrstmpl,ndpts,fld,ierr)
!   INPUT ARGUMENT LIST:
!     cgrib    - Character array that contains the GRIB2 message
!     lcgrib   - Length (in bytes) of GRIB message array cgrib.
!     iofst    - Bit offset of the beginning of Section 7.
!     igdsnum  - Grid Definition Template Number ( see Code Table 3.0)
!                (Only required to unpack DRT 5.51)
!     igdstmpl - Pointer to an integer array containing the data values for
!                the specified Grid Definition
!                Template ( N=igdsnum ).  Each element of this integer
!                array contains an entry (in the order specified) of Grid
!                Definition Template 3.N
!                (Only required to unpack DRT 5.51)
!     idrsnum  - Data Representation Template Number ( see Code Table 5.0)
!     idrstmpl - Pointer to an integer array containing the data values for
!                the specified Data Representation
!                Template ( N=idrsnum ).  Each element of this integer
!                array contains an entry (in the order specified) of Data
!                Representation Template 5.N
!     ndpts    - Number of data points unpacked and returned.
!
!   OUTPUT ARGUMENT LIST:      
!     iofst    - Bit offset at the end of Section 7, returned.
!     fld()    - Pointer to a real array containing the unpacked data field.
!     ierr     - Error return code.
!                0 = no error
!                4 = Unrecognized Data Representation Template
!                5 = One of GDT 3.50 through 3.53 required to unpack DRT 5.51
!                6 = memory allocation error
!                7 = corrupt section 7.
!
! REMARKS: None
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      character(len=1),intent(in) :: cgrib(lcgrib)
      integer,intent(in) :: lcgrib,ndpts,igdsnum,idrsnum
      integer,intent(inout) :: iofst
      integer,pointer,dimension(:) :: igdstmpl,idrstmpl
      integer,intent(out) :: ierr
      real,pointer,dimension(:) :: fld


      ierr=0
      nullify(fld)

      call g2lib_gbyte(cgrib,lensec,iofst,32)        ! Get Length of Section
      iofst=iofst+32    
      iofst=iofst+8     ! skip section number

      ipos=(iofst/8)+1
      istat=0
      allocate(fld(ndpts),stat=istat)
      if (istat.ne.0) then
         ierr=6
         return
      endif

      if (idrsnum.eq.0) then
        call simunpack(cgrib(ipos),lensec-5,idrstmpl,ndpts,fld)
      elseif (idrsnum.eq.2.or.idrsnum.eq.3) then
        call comunpack(cgrib(ipos),lensec-5,lensec,idrsnum,idrstmpl,
     &                 ndpts,fld,ier)
        if ( ier .NE. 0 ) then
           ierr=7
           return
        endif
      elseif (idrsnum.eq.50) then      !  Spectral simple
        call simunpack(cgrib(ipos),lensec-5,idrstmpl,ndpts-1,
     &                 fld(2))
        ieee=idrstmpl(5)
        call rdieee(ieee,fld(1),1)
      elseif (idrsnum.eq.51) then      !  Spectral complex
        if (igdsnum.ge.50.AND.igdsnum.le.53) then
          call specunpack(cgrib(ipos),lensec-5,idrstmpl,ndpts,
     &                    igdstmpl(1),igdstmpl(2),igdstmpl(3),fld)
        else
          print *,'gf_unpack7: Cannot use GDT 3.',igdsnum,
     &            ' to unpack Data Section 5.51.'
          ierr=5
          nullify(fld)
          return
        endif
#ifdef USE_JPEG2000
      elseif (idrsnum.eq.40 .OR. idrsnum.eq.40000) then
        call jpcunpack(cgrib(ipos),lensec-5,idrstmpl,ndpts,fld)
#endif  /* USE_JPEG2000 */
#ifdef USE_PNG
      elseif (idrsnum.eq.41 .OR. idrsnum.eq.40010) then
        call pngunpack(cgrib(ipos),lensec-5,idrstmpl,ndpts,fld)
#endif  /* USE_PNG */
      else
        print *,'gf_unpack7: Data Representation Template ',idrsnum,
     &          ' not yet implemented.'
        ierr=4
        nullify(fld)
        return
      endif

      iofst=iofst+(8*lensec)
      
      return    ! End of Section 7 processing
      end

