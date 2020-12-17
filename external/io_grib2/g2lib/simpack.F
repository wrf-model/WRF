      subroutine simpack(fld,ndpts,idrstmpl,cpack,lcpack)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    simpack
!   PRGMMR: Gilbert          ORG: W/NP11    DATE: 2000-06-21
!
! ABSTRACT: This subroutine packs up a data field using a simple
!   packing algorithm as defined in the GRIB2 documention.  It
!   also fills in GRIB2 Data Representation Template 5.0 with the
!   appropriate values.
!
! PROGRAM HISTORY LOG:
! 2000-06-21  Gilbert
!
! USAGE:    CALL simpack(fld,ndpts,idrstmpl,cpack,lcpack)
!   INPUT ARGUMENT LIST:
!     fld()    - Contains the data values to pack
!     ndpts    - The number of data values in array fld()
!     idrstmpl - Contains the array of values for Data Representation
!                Template 5.0
!                (1) = Reference value - ignored on input
!                (2) = Binary Scale Factor
!                (3) = Decimal Scale Factor
!                (4) = Number of bits used to pack data, if value is
!                      > 0 and  <= 31.
!                      If this input value is 0 or outside above range
!                      then the num of bits is calculated based on given 
!                      data and scale factors.
!                (5) = Original field type - currently ignored on input
!                      Data values assumed to be reals.
!
!   OUTPUT ARGUMENT LIST: 
!     idrstmpl - Contains the array of values for Data Representation
!                Template 5.0
!                (1) = Reference value - set by simpack routine.
!                (2) = Binary Scale Factor - unchanged from input
!                (3) = Decimal Scale Factor - unchanged from input
!                (4) = Number of bits used to pack data, unchanged from 
!                      input if value is between 0 and 31.
!                      If this input value is 0 or outside above range
!                      then the num of bits is calculated based on given 
!                      data and scale factors.
!                (5) = Original field type - currently set = 0 on output.
!                      Data values assumed to be reals.
!     cpack    - The packed data field (character*1 array)
!     lcpack   - length of packed field cpack().
!
! REMARKS: None
!
! ATTRIBUTES:
!   LANGUAGE: XL Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      integer,intent(in) :: ndpts
      real,intent(in) :: fld(ndpts)
      character(len=1),intent(out) :: cpack(*)
      integer,intent(inout) :: idrstmpl(*)
      integer,intent(out) :: lcpack

      real(4) :: ref
      integer(4) :: iref
      integer :: ifld(ndpts)
      integer,parameter :: zero=0
      
      bscale=2.0**real(-idrstmpl(2))
      dscale=10.0**real(idrstmpl(3))
      if (idrstmpl(4).le.0.OR.idrstmpl(4).gt.31) then
         nbits=0
      else
         nbits=idrstmpl(4)
      endif
!
!  Find max and min values in the data
!
      rmax=fld(1)
      rmin=fld(1)
      do j=2,ndpts
        if (fld(j).gt.rmax) rmax=fld(j)
        if (fld(j).lt.rmin) rmin=fld(j)
      enddo
!
!  If max and min values are not equal, pack up field.
!  If they are equal, we have a constant field, and the reference
!  value (rmin) is the value for each point in the field and
!  set nbits to 0.
!
      if (rmin.ne.rmax) then
        !
        !  Determine which algorithm to use based on user-supplied 
        !  binary scale factor and number of bits.
        !
        if (nbits.eq.0.AND.idrstmpl(2).eq.0) then
           !
           !  No binary scaling and calculate minumum number of 
           !  bits in which the data will fit.
           !
           imin=nint(rmin*dscale)
           imax=nint(rmax*dscale)
           maxdif=imax-imin
           temp=alog(real(maxdif+1))/alog(2.0)
           nbits=ceiling(temp)
           rmin=real(imin)
           !   scale data
           do j=1,ndpts
             ifld(j)=nint(fld(j)*dscale)-imin
           enddo
        elseif (nbits.ne.0.AND.idrstmpl(2).eq.0) then
           !
           !  Use minimum number of bits specified by user and
           !  adjust binary scaling factor to accomodate data.
           !
           rmin=rmin*dscale
           rmax=rmax*dscale
           maxnum=(2**nbits)-1
           temp=alog(real(maxnum)/(rmax-rmin))/alog(2.0)
           idrstmpl(2)=ceiling(-1.0*temp)
           bscale=2.0**real(-idrstmpl(2))
           !   scale data
           do j=1,ndpts
             ifld(j)=nint(((fld(j)*dscale)-rmin)*bscale)
           enddo
        elseif (nbits.eq.0.AND.idrstmpl(2).ne.0) then
           !
           !  Use binary scaling factor and calculate minumum number of 
           !  bits in which the data will fit.
           !
           rmin=rmin*dscale
           rmax=rmax*dscale
           maxdif=nint((rmax-rmin)*bscale)
           temp=alog(real(maxdif+1))/alog(2.0)
           nbits=ceiling(temp)
           !   scale data
           do j=1,ndpts
             ifld(j)=nint(((fld(j)*dscale)-rmin)*bscale)
           enddo
        elseif (nbits.ne.0.AND.idrstmpl(2).ne.0) then
           !
           !  Use binary scaling factor and use minumum number of 
           !  bits specified by user.   Dangerous - may loose
           !  information if binary scale factor and nbits not set
           !  properly by user.
           !
           rmin=rmin*dscale
           !   scale data
           do j=1,ndpts
             ifld(j)=nint(((fld(j)*dscale)-rmin)*bscale)
           enddo
        endif
        !
        !  Pack data, Pad last octet with Zeros, if necessary,
        !  and calculate the length of the packed data in bytes
        !
        call g2lib_sbytes(cpack,ifld,0,nbits,0,ndpts)
        nbittot=nbits*ndpts
        left=8-mod(nbittot,8)
        if (left.ne.8) then
          call g2lib_sbyte(cpack,zero,nbittot,left)    ! Pad with zeros to fill Octet
          nbittot=nbittot+left
        endif
        lcpack=nbittot/8

      else
        nbits=0
        lcpack=0
      endif

!
!  Fill in ref value and number of bits in Template 5.0
!
      call mkieee(rmin,ref,1)   ! ensure reference value is IEEE format
!      call g2lib_gbyte(ref,idrstmpl(1),0,32)
      iref=transfer(ref,iref)
      idrstmpl(1)=iref
      idrstmpl(4)=nbits
      idrstmpl(5)=0         ! original data were reals

      return
      end
