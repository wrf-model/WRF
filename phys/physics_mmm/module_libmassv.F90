!=================================================================================================================
 module module_libmassv

 implicit none


 interface vrec
    module procedure vrec_d
    module procedure vrec_s
 end interface

 interface vsqrt
    module procedure vsqrt_d
    module procedure vsqrt_s
 end interface

 integer, parameter, private :: R4KIND = selected_real_kind(6)
 integer, parameter, private :: R8KIND = selected_real_kind(12)

 contains


!=================================================================================================================
 subroutine vrec_d(y,x,n)
!=================================================================================================================
 integer,intent(in):: n
 real(kind=R8KIND),dimension(*),intent(in):: x
 real(kind=R8KIND),dimension(*),intent(out):: y

 integer:: j
!-----------------------------------------------------------------------------------------------------------------

 do j=1,n
    y(j)=real(1.0,kind=R8KIND)/x(j)
 enddo

 end subroutine vrec_d

!=================================================================================================================
 subroutine vrec_s(y,x,n)
!=================================================================================================================
 integer,intent(in):: n
 real(kind=R4KIND),dimension(*),intent(in):: x
 real(kind=R4KIND),dimension(*),intent(out):: y

 integer:: j
!-----------------------------------------------------------------------------------------------------------------

 do j=1,n
    y(j)=real(1.0,kind=R4KIND)/x(j)
 enddo

 end subroutine vrec_s

!=================================================================================================================
 subroutine vsqrt_d(y,x,n)
!=================================================================================================================
 integer,intent(in):: n
 real(kind=R8KIND),dimension(*),intent(in):: x
 real(kind=R8KIND),dimension(*),intent(out):: y

 integer:: j
!-----------------------------------------------------------------------------------------------------------------

 do j=1,n
    y(j)=sqrt(x(j))
 enddo

 end subroutine vsqrt_d

!=================================================================================================================
 subroutine vsqrt_s(y,x,n)
!=================================================================================================================

 integer,intent(in):: n
 real(kind=R4KIND),dimension(*),intent(in):: x
 real(kind=R4KIND),dimension(*),intent(out):: y

 integer:: j

!-----------------------------------------------------------------------------------------------------------------

 do j=1,n
    y(j)=sqrt(x(j))
 enddo

 end subroutine vsqrt_s

!=================================================================================================================
 end module module_libmassv
!=================================================================================================================
