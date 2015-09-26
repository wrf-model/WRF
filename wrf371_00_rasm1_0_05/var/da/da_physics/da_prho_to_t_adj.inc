subroutine da_prho_to_t_adj(grid) 

   !---------------------------------------------------------------------------
   !  Purpose: Adjoint of da_prho_to_t.
   !
   !  Method:  Standard adjoint coding.
   !
   !  Assumptions: 1) Model level stored top down.
   !---------------------------------------------------------------------------

   implicit none
   
   type (domain), intent(inout)  :: grid

   integer :: is, ie       ! 1st dim. end points.
   integer :: js, je       ! 2nd dim. end points.
   integer :: k            ! Loop counter.
   real    :: temp(its:ite,jts:jte) ! Temporary array.

   if (trace_use) call da_trace_entry("da_prho_to_t_adj")

   !---------------------------------------------------------------------------
   !  [1.0] initialise:
   !---------------------------------------------------------------------------

   is = its
   ie = ite
   js = jts
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

   !---------------------------------------------------------------------------
   ! [2.0] Calculate temperature increments:
   !---------------------------------------------------------------------------

   do k = kts, kte
      temp(is:ie,js:je) = grid%xa % t(is:ie,js:je,k) * grid%xb % t(is:ie,js:je,k)

      grid%xa % p(is:ie,js:je,k) = grid%xa % p(is:ie,js:je,k) &
         + temp(is:ie,js:je) / grid%xb % p(is:ie,js:je,k)
                         
      grid%xa % rho(is:ie,js:je,k) = grid%xa % rho(is:ie,js:je,k) &
         - temp(is:ie,js:je) / grid%xb % rho(is:ie,js:je,k)
   end do  

   if (trace_use) call da_trace_exit("da_prho_to_t_adj")
                             
end subroutine da_prho_to_t_adj


