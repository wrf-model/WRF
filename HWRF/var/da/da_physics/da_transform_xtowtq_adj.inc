subroutine da_transform_xtowtq_adj (grid)

   !--------------------------------------------------------------------------
   ! Purpose: TBD
   !--------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout)    :: grid

   integer :: i, j, is, js, ie, je
   real    :: height

   if (trace_use) call da_trace_entry("da_transform_xtowtq_adj")

   is = its
   js = jts

   ie = ite
   je = jte

   if (test_transforms) then
      is = its-1
      js = jts-1

      ie = ite+1
      je = jte+1

      if (is < ids) is = ids
      if (js < jds) js = jds

      if (ie > ide) ie = ide
      if (je > jde) je = jde
   end if

   ! Adjoint from Gridded 10-m wind and 2-m moisture and temperature
   ! to the model adjoint variables

   do j=js, je
      do i=is, ie
         grid%xa%tgrn(i,j)=0.0

         height = grid%xb%h(i,j,kts) - grid%xb%terr(i,j)                 
         if (height <= 0.0) then
            message(1) = "Negative height found"
            write(unit=message(2),FMT='(2I6,A,F10.2,A,F10.2)') &
               i,j,' ht = ',grid%xb%h(i,j,kts) ,' terr =  ',grid%xb%terr(i,j)
            call da_error(__FILE__,__LINE__,message(1:2))
         end if
         call da_sfc_wtq_adj(grid%xb%psfc(i,j), grid%xb%tgrn(i,j), &
            grid%xb%p(i,j,kts), grid%xb%t(i,j,kts), grid%xb%q(i,j,kts), &
            grid%xb%u(i,j,kts), grid%xb%v(i,j,kts), &
            grid%xb%regime(i,j),  &
            grid%xa%psfc(i,j), grid%xa%tgrn(i,j), &
            grid%xa%p(i,j,kts), grid%xa%t(i,j,kts), grid%xa%q(i,j,kts), &
            grid%xa%u(i,j,kts), grid%xa%v(i,j,kts), &
            height      , grid%xb%rough(i,j), grid%xb%xland(i,j), grid%xb%ds, & ! Modified by Eric Chiang (AUG 2010) 
            grid%xa%u10(i,j),grid%xa%v10(i,j), &
            grid%xa%t2 (i,j),grid%xa%q2 (i,j))

         grid%xa%tgrn(i,j)=0.0
      end do
   end do

   if (trace_use) call da_trace_exit("da_transform_xtowtq_adj")

end subroutine da_transform_xtowtq_adj


