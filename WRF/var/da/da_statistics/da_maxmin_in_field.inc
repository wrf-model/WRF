subroutine da_maxmin_in_field(field, max, min)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,                    intent(in)  :: field(its:ite,jts:jte,kts:kte)
   type(maxmin_field_type), intent(out) :: max(kts:kte)
   type(maxmin_field_type), intent(out) :: min(kts:kte)

   if (trace_use_dull) call da_trace_entry("da_maxmin_in_field")

   do k = kts, kte
      max(k)%value = -1.0e20
      min(k)%value =  1.0e20
      do j = jts, jte
         do i = its, ite
            if (field(i,j,k) > max(k)%value) then
               max(k)%value = field(i,j,k)
               max(k)%i     = i
               max(k)%j     = j
            end if
            if (field(i,j,k) < min(k)%value) then
               min(k)%value = field(i,j,k)
               min(k)%i     = i
               min(k)%j     = j
            end if
         end do
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_maxmin_in_field")

end subroutine da_maxmin_in_field


