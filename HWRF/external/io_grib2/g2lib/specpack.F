      subroutine specpack(fld,ndpts,JJ,KK,MM,idrstmpl,cpack,lcpack)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    specpack
!   PRGMMR: Gilbert          ORG: W/NP11    DATE: 2002-12-19
!
! ABSTRACT: This subroutine packs a spectral data field using the complex
!   packing algorithm for spherical harmonic data as 
!   defined in the GRIB2 Data Representation Template 5.51.
!
! PROGRAM HISTORY LOG:
! 2002-12-19  Gilbert
!
! USAGE:    CALL specpack(fld,ndpts,JJ,KK,MM,idrstmpl,cpack,lcpack)
!   INPUT ARGUMENT LIST:
!     fld()    - Contains the packed data values
!     ndpts    - The number of data values to pack
!     JJ       - J - pentagonal resolution parameter
!     KK       - K - pentagonal resolution parameter
!     MM       - M - pentagonal resolution parameter
!     idrstmpl - Contains the array of values for Data Representation
!                Template 5.51
!
!   OUTPUT ARGUMENT LIST:
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

      real,intent(in) :: fld(ndpts)
      integer,intent(in) :: ndpts,JJ,KK,MM
      integer,intent(inout) :: idrstmpl(*)
      character(len=1),intent(out) :: cpack(*)
      integer,intent(out) :: lcpack

      integer :: ifld(ndpts),Ts,tmplsim(5)
      real :: bscale,dscale,unpk(ndpts),tfld(ndpts)
      real,allocatable :: pscale(:)

      bscale = 2.0**real(-idrstmpl(2))
      dscale = 10.0**real(idrstmpl(3))
      nbits = idrstmpl(4)
      Js=idrstmpl(6)
      Ks=idrstmpl(7)
      Ms=idrstmpl(8)
      Ts=idrstmpl(9)

!
!   Calculate Laplacian scaling factors for each possible wave number.
!
      allocate(pscale(JJ+MM))
      tscale=real(idrstmpl(5))*1E-6
      do n=Js,JJ+MM
         pscale(n)=real(n*(n+1))**(tscale)
      enddo
!
!   Separate spectral coeffs into two lists; one to contain unpacked
!   values within the sub-spectrum Js, Ks, Ms, and the other with values 
!   outside of the sub-spectrum to be packed.
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
            if (n.le.Ns .AND. m.le.Ms) then    ! save unpacked value
               unpk(incu)=fld(inc)         ! real part
               unpk(incu+1)=fld(inc+1)     ! imaginary part
               inc=inc+2
               incu=incu+2
            else                         ! Save value to be packed and scale
                                         ! Laplacian scale factor
               tfld(incp)=fld(inc)*pscale(n)         ! real part
               tfld(incp+1)=fld(inc+1)*pscale(n)     ! imaginary part
               inc=inc+2
               incp=incp+2
            endif
         enddo
      enddo

      deallocate(pscale)

      incu=incu-1
      if (incu .ne. Ts) then
         print *,'specpack: Incorrect number of unpacked values ',
     &           'given:',Ts     
         print *,'specpack: Resetting idrstmpl(9) to ',incu
         Ts=incu
      endif
!
!  Add unpacked values to the packed data array in 32-bit IEEE format
!
      call mkieee(unpk,cpack,Ts)
      ipos=4*Ts
!
!  Scale and pack the rest of the coefficients
! 
      tmplsim(2)=idrstmpl(2)
      tmplsim(3)=idrstmpl(3)
      tmplsim(4)=idrstmpl(4)
      call simpack(tfld,ndpts-Ts,tmplsim,cpack(ipos+1),lcpack)
      lcpack=lcpack+ipos
!
!  Fill in Template 5.51
!
      idrstmpl(1)=tmplsim(1)
      idrstmpl(2)=tmplsim(2)
      idrstmpl(3)=tmplsim(3)
      idrstmpl(4)=tmplsim(4)
      idrstmpl(9)=Ts
      idrstmpl(10)=1         ! Unpacked spectral data is 32-bit IEEE

      return
      end
