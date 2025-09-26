      subroutine specunpack(cpack,len,idrstmpl,ndpts,JJ,KK,MM,fld)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    specunpack
!   PRGMMR: Gilbert          ORG: W/NP11    DATE: 2002-12-19
!
! ABSTRACT: This subroutine unpacks a spectral data field that was packed 
!   using the complex packing algorithm for spherical harmonic data as 
!   defined in the GRIB2 documention,
!   using info from the GRIB2 Data Representation Template 5.51.
!
! PROGRAM HISTORY LOG:
! 2002-12-19  Gilbert
!
! USAGE:    CALL specunpack(cpack,len,idrstmpl,ndpts,JJ,KK,MM,fld)
!   INPUT ARGUMENT LIST:
!     cpack    - The packed data field (character*1 array)
!     len      - length of packed field cpack().
!     idrstmpl - Contains the array of values for Data Representation
!                Template 5.51
!     ndpts    - The number of data values to unpack
!     JJ       - J - pentagonal resolution parameter
!     KK       - K - pentagonal resolution parameter
!     MM       - M - pentagonal resolution parameter
!
!   OUTPUT ARGUMENT LIST:
!     fld()    - Contains the unpacked data values
!
! REMARKS: None
!
! ATTRIBUTES:
!   LANGUAGE: XL Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      character(len=1),intent(in) :: cpack(len)
      integer,intent(in) :: ndpts,len,JJ,KK,MM
      integer,intent(in) :: idrstmpl(*)
      real,intent(out) :: fld(ndpts)

      integer :: ifld(ndpts),Ts
      integer(4) :: ieee
      real :: ref,bscale,dscale,unpk(ndpts)
      real,allocatable :: pscale(:)

      ieee = idrstmpl(1)
      call rdieee(ieee,ref,1)
      bscale = 2.0**real(idrstmpl(2))
      dscale = 10.0**real(-idrstmpl(3))
      nbits = idrstmpl(4)
      Js=idrstmpl(6)
      Ks=idrstmpl(7)
      Ms=idrstmpl(8)
      Ts=idrstmpl(9)

      if (idrstmpl(10).eq.1) then           ! unpacked floats are 32-bit IEEE
         !call gbytes(cpack,ifld,0,32,0,Ts)
         call rdieee(cpack,unpk,Ts)          ! read IEEE unpacked floats
         iofst=32*Ts
         call gbytes(cpack,ifld,iofst,nbits,0,ndpts-Ts)  ! unpack scaled data
!
!   Calculate Laplacian scaling factors for each possible wave number.
!
         allocate(pscale(JJ+MM))
         tscale=real(idrstmpl(5))*1E-6
         do n=Js,JJ+MM
            pscale(n)=real(n*(n+1))**(-tscale)
         enddo
!
!   Assemble spectral coeffs back to original order.
!
         inc=1
         incu=1
         incp=1
         do m=0,MM
            Nm=JJ      ! triangular or trapezoidal
            if ( KK .eq. JJ+MM ) Nm=JJ+m          ! rhombodial
            Ns=Js      ! triangular or trapezoidal
            if ( Ks .eq. Js+Ms ) Ns=Js+m          ! rhombodial
            do n=m,Nm
               if (n.le.Ns .AND. m.le.Ms) then    ! grab unpacked value
                  fld(inc)=unpk(incu)         ! real part
                  fld(inc+1)=unpk(incu+1)     ! imaginary part
                  inc=inc+2
                  incu=incu+2
               else                         ! Calc coeff from packed value
                  fld(inc)=((real(ifld(incp))*bscale)+ref)*
     &                      dscale*pscale(n)           ! real part
                  fld(inc+1)=((real(ifld(incp+1))*bscale)+ref)*
     &                      dscale*pscale(n)           ! imaginary part
                  inc=inc+2
                  incp=incp+2
               endif
            enddo
         enddo

         deallocate(pscale)

      else
         print *,'specunpack: Cannot handle 64 or 128-bit floats.'
         fld=0.0
         return
      endif

      return
      end
