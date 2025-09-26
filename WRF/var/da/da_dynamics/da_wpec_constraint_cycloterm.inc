subroutine da_wpec_constraint_cycloterm (rho, u, v, coefx, coefy, term_x, term_y)

   !---------------------------------------------------------------------------
   !  Purpose: Calculates cyclostrophic term in wpec constraint equation.
   !
   !  Method:  Term is rho (u.grad) u on a single level.
   !---------------------------------------------------------------------------

   implicit none
   
   real, intent(in)    :: rho(ims:ime,jms:jme)    ! Density.
   real, intent(in)    :: u(ims:ime,jms:jme)      ! u wind increment
   real, intent(in)    :: v(ims:ime,jms:jme)      ! v wind increment
   real, intent(in)    :: coefx(ims:ime,jms:jme)  ! Multiplicative const.
   real, intent(in)    :: coefy(ims:ime,jms:jme)
   real, intent(inout) :: term_x(ims:ime,jms:jme) ! x component of term.
   real, intent(inout) :: term_y(ims:ime,jms:jme) ! y component of term.

   integer :: i, j                         ! Loop counters.
   integer :: is, ie                       ! 1st dim. end points.
   integer :: js, je                       ! 2nd dim. end points.
   
   real    :: data(ims:ime,jms:jme)        ! Temporary storage.

   real    :: varb

   if (trace_use) call da_trace_entry("da_wpec_constraint_cycloterm")

   !---------------------------------------------------------------------------
   ! [1.0] Initialise:
   !---------------------------------------------------------------------------
   
   ! Computation to check for edge of domain:
   is = its; ie = ite; js = jts; je = jte
   if (.not. global .and. its==ids) is = ids+1
   if (.not. global .and. ite==ide) ie = ide-1
   if (jts==jds) js = jds+1
   if (jte==jde) je = jde-1
   
   !---------------------------------------------------------------------------
   ! [2.0] Calculate term_x = rho M (u.du/dx + v.du/dy):
   !---------------------------------------------------------------------------

   ! [2.1] Interior points:

   do j = js, je
      do i = is, ie
         data(i,j) = u(i,j) * coefx(i,j)*(u(i+1,j) - &
            u(i-1,j)) + v(i,j) * coefy(i,j)*(u(i,j+1) - &
            u(i,j-1))
      end do
   end do
   

   !  [2.7] Multiply by rho  and add to term_x:
      
   term_x(its:ite,jts:jte) = rho(its:ite,jts:jte)*data(its:ite,jts:jte) + term_x(its:ite,jts:jte)

   !---------------------------------------------------------------------------
   ! [3.0] Calculate term_y = rho M (u.dv/dx + v.dv/dy):
   !---------------------------------------------------------------------------

   ! [3.1] Interior points:

   do j = js, je
      do i = is, ie
         data(i,j) = u(i,j) * coefx(i,j)*(v(i+1,j) - v(i-1,j)) + &
                     v(i,j) * coefy(i,j)*(v(i,j+1) - v(i,j-1))
      end do
   end do
   

   ! [3.7] Multiply by rho and add to term_y

   term_y(its:ite,jts:jte)=rho(its:ite,jts:jte)* data(its:ite,jts:jte) + &
      term_y(its:ite,jts:jte)

   if (trace_use) call da_trace_exit("da_wpec_constraint_cycloterm")

end subroutine da_wpec_constraint_cycloterm


