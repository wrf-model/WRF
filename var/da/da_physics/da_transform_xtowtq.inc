subroutine da_transform_xtowtq (grid)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------------
 
   implicit none

   type (domain), intent(inout) :: grid

   integer :: i, j
   real    :: height

   if (trace_use) call da_trace_entry("da_transform_xtowtq")

   !------------------------------------------------------------------------
   ! [1.0] Calculate surface variable(1-m wind, 2-m moisture and temperature)
   !------------------------------------------------------------------------

   !------------------------------------------------------------------------
   ! [2.0] Calculate sfc variable perturbations at the cross point
   !------------------------------------------------------------------------

   do j=jts, jte
      do i=its, ite
         grid%xa%tgrn(i,j) = 0.0
         height = grid%xb%h(i,j,kts) - grid%xb%terr(i,j)                 
         if (height <= 0.0) then
            message(1) = "Negative height found"
            write(unit=message(2),FMT='(2I6,A,F10.2,A,F10.2)') &
               i,j,' ht = ',grid%xb%h(i,j,kts) ,' terr =  ',grid%xb%terr(i,j)
            call da_error(__FILE__,__LINE__,message(1:2))
         end if
         call da_sfc_wtq_lin(grid%xb%psfc(i,j), grid%xb%tgrn(i,j), &
            grid%xb%p(i,j,kts), grid%xb%t(i,j,kts), grid%xb%q(i,j,kts), &
            grid%xb%u(i,j,kts), grid%xb%v(i,j,kts), &
            grid%xb%regime(i,j), &
            grid%xa%psfc(i,j), grid%xa%tgrn(i,j), &
            grid%xa%p(i,j,kts), grid%xa%t(i,j,kts), grid%xa%q(i,j,kts), &
            grid%xa%u(i,j,kts), grid%xa%v(i,j,kts), &
            height      , grid%xb%rough(i,j), grid%xb%xland(i,j), grid%xb%ds, & ! Modified by Eric Chiang (AUG 2010)
            grid%xa%u10(i,j), grid%xa%v10(i,j), &
            grid%xa%t2(i,j),  grid%xa%q2(i,j))
      end do
   end do

   if (trace_use) call da_trace_exit("da_transform_xtowtq")

end subroutine da_transform_xtowtq
 

