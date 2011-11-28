      subroutine getpoly(csec3,lcsec3,jj,kk,mm)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    getpoly 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2002-12-11
!
! ABSTRACT: This subroutine returns the J, K, and M pentagonal resolution
!   parameters specified in a GRIB Grid Definition Section used
!   spherical harmonic coefficients using GDT 5.50 through 5.53
!
! PROGRAM HISTORY LOG:
! 2002-12-11  Gilbert
!
! USAGE:    CALL getpoly(csec3,lcsec3,jj,kk,mm)
!   INPUT ARGUMENT LIST:
!     csec3    - Character array that contains the packed GRIB2 GDS
!    lcsec3    - Length (in octets) of section 3
!
!   OUTPUT ARGUMENT LIST:      
!         JJ   = J - pentagonal resolution parameter
!         KK   = K - pentagonal resolution parameter
!         MM   = M - pentagonal resolution parameter
!
! REMARKS:  Returns JJ, KK, and MM set to zero, if grid template
!           not recognized.
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$
!      use grib_mod
    
      character(len=1),intent(in) :: csec3(*)
      integer,intent(in) :: lcsec3
      integer,intent(out) :: jj,kk,mm
      
      integer,pointer,dimension(:) :: igdstmpl,list_opt
      integer :: igds(5)
      integer iofst,igdtlen,num_opt,jerr

      interface
         subroutine gf_unpack3(cgrib,lcgrib,iofst,igds,igdstmpl,
     &                         mapgridlen,ideflist,idefnum,ierr)
            character(len=1),intent(in) :: cgrib(lcgrib)
            integer,intent(in) :: lcgrib
            integer,intent(inout) :: iofst
            integer,pointer,dimension(:) :: igdstmpl,ideflist
            integer,intent(out) :: igds(5)
            integer,intent(out) :: ierr,idefnum
         end subroutine gf_unpack3
      end interface

      nullify(igdstmpl,list_opt)
        !
      iofst=0       ! set offset to beginning of section
      call gf_unpack3(csec3,lcsec3,iofst,igds,igdstmpl,
     &                 igdtlen,list_opt,num_opt,jerr)
      if (jerr.eq.0) then
         selectcase( igds(5) )     !  Template number
           case (50:53)   ! Spherical harmonic coefficients
              jj=igdstmpl(1)
              kk=igdstmpl(2)
              mm=igdstmpl(3)
           case default
              jj=0
              kk=0
              mm=0
         end select
      else
         jj=0
         kk=0
         mm=0
      endif
        !
      if (associated(igdstmpl)) deallocate(igdstmpl)
      if (associated(list_opt)) deallocate(list_opt)

      return
      end
