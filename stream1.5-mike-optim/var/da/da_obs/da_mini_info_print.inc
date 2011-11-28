subroutine da_mini_info_print(print_unit, oi_each, re_each, gr_each, k)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (each_level_type), intent(in) :: oi_each
   type (residual_type),   intent(in) :: re_each, gr_each
   integer,                intent(in) :: print_unit, k

   if (trace_use) call da_trace_entry("da_mini_info_print")

   write(unit=print_unit, fmt='(a,i3,8e12.4)') &
        '  oi  ', k, &
        oi_each%u%inv, oi_each%v%inv, &
        oi_each%t%inv, oi_each%p%inv, oi_each%q%inv

   write(unit=print_unit, fmt='(a,i3,8e12.4)') &
        '  re  ', k, &
        re_each%u, re_each%v, re_each%t, re_each%p, re_each%q

   write(unit=print_unit, fmt='(a,i3,8e12.4)') &
        ' grad ', k, &
        gr_each%u, gr_each%v, gr_each%t, gr_each%p, gr_each%q

   if (trace_use) call da_trace_exit("da_mini_info_print")

end subroutine da_mini_info_print


