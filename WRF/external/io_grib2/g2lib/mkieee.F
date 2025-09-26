      subroutine mkieee(a,rieee,num)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    mkieee 
!   PRGMMR: Gilbert         ORG: W/NP11    DATE: 2000-05-09
!
! ABSTRACT: This subroutine stores a list of real values in 
!   32-bit IEEE floating point format.
!
! PROGRAM HISTORY LOG:
! 2000-05-09  Gilbert
!
! USAGE:    CALL mkieee(a,rieee,num)
!   INPUT ARGUMENT LIST:
!     a        - Input array of floating point values.
!     num      - Number of floating point values to convert.
!
!   OUTPUT ARGUMENT LIST:      
!     rieee    - Output array of floating point values in 32-bit IEEE format.
!
! REMARKS: None
!
! ATTRIBUTES:
!   LANGUAGE: Fortran 90
!   MACHINE:  IBM SP
!
!$$$

      real,intent(in) :: a(num)
      real(4),intent(out) :: rieee(num)
      integer,intent(in) :: num

      integer(4) :: ieee 

      real,save :: two23
      real,save :: two126
      integer,save :: once=0

      if ( once .EQ. 0 ) then
         once=1
         two23=scale(1.0,23)
         two126=scale(1.0,126)
      endif

      alog2=alog(2.0)

      do j=1,num
        ieee=0

        if (a(j).eq.0.) then
          ieee=0
          rieee(j)=transfer(ieee,rieee(j))
!       write(6,fmt='(f20.10,5x,b32)') a,a
!       write(6,fmt='(f20.10,5x,b32)') rieee,rieee
          cycle
        endif
        
!
!  Set Sign bit (bit 31 - leftmost bit)
!
        if (a(j).lt.0.0) then
          ieee=ibset(ieee,31)
          atemp=abs(a(j))
        else
          ieee=ibclr(ieee,31)
          atemp=a(j)
        endif
!
!  Determine exponent n with base 2
!
        n=floor(alog(atemp)/alog2)
        iexp=n+127
        if (n.gt.127) iexp=255     ! overflow
        if (n.lt.-127) iexp=0
        !      set exponent bits ( bits 30-23 )
        call mvbits(iexp,0,8,ieee,23)
!
!  Determine Mantissa
! 
        if (iexp.ne.255) then
          if (iexp.ne.0) then
            atemp=(atemp/(2.0**n))-1.0
          else
            atemp=atemp*two126
          endif
          imant=nint(atemp*two23)
        else
          imant=0
        endif
        !      set mantissa bits ( bits 22-0 )
        call mvbits(imant,0,23,ieee,0)
!
!  Transfer IEEE bit string to real variable
!
        rieee(j)=transfer(ieee,rieee(j))
!       write(6,fmt='(f20.10,5x,b32)') a,a
!       write(6,fmt='(f20.10,5x,b32)') rieee,rieee

      enddo

      return
      end

