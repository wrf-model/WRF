subroutine da_interp_2d_partial(fm2d, info, k, n1, n2, fo2d, mask2d)

   !-----------------------------------------------------------------------
   ! Interpolation option (1=linear)
   !                      (2=quadratic)
   !                      (3=masked average)
   !-----------------------------------------------------------------------

   implicit none

   real,            intent(in)  :: fm2d(ims:ime,jms:jme)   ! Input variable
   type(infa_type), intent(in)  :: info
   integer,         intent(in)  :: k                       ! level
   integer,         intent(in)  :: n1,n2                   ! Range of obs
   real,            intent(out) :: fo2d(n1:n2)             ! Output variable 
   integer, optional, intent(in):: mask2d(ims:ime,jms:jme) ! Field mask (1=valid, 0=invalid)
   
   if (trace_use_frequent) call da_trace_entry("da_interp_2d_partial")
   
   select case(interp_option)
   case (1) ;
      call da_interp_lin_2d_partial(fm2d, info, k, n1, n2, fo2d)
   case (2) ;
      call da_interp_quad_2d_partial(fm2d, info, k, n1, n2, fo2d)
   case (3) ;
      if (.not. present(mask2d)) call da_error(__FILE__,__LINE__,(/"da_interp_2d_partial: no mask given for masked interpolation"/))
      call da_interp_msk_avg_2d_partial(fm2d, info, k, n1, n2, fo2d, mask2d)
   case default;
      write(unit=message(1),fmt='(a,i4)') 'Interpolation option not supported: ', interp_option
      call da_warning(__FILE__,__LINE__,message(1:1))
   end select   
      
   if (trace_use_frequent) call da_trace_exit("da_interp_2d_partial")

end subroutine da_interp_2d_partial
