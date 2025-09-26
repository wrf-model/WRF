subroutine da_wpec_constraint_geoterm (cori,rho, u, v, term_x, term_y)
 
   !---------------------------------------------------------------------------
   ! Purpose: calculates nonlinear geostrophic term in wpec constraint equation.
   !
   ! method:  term is k x rho f u on a single level.
   !---------------------------------------------------------------------------

   implicit none
   real,         intent(in)  :: cori(ims:ime,jms:jme)   ! Coriolis factor.
   real,         intent(in)  :: rho(ims:ime,jms:jme)    ! Density
   real,         intent(in)  :: u(:,:)       ! u wind comp. (dot pts)
   real,         intent(in)  :: v(:,:)       ! v wind comp. (dot pts)
   real,         intent(out) :: term_x(:,:)  ! x component of term.
   real,         intent(out) :: term_y(:,:)  ! y component of term.
   integer :: is, ie                       ! 1st dim. end points.
   integer :: js, je                       ! 2nd dim. end points.

   if (trace_use) call da_trace_entry("da_wpec_constraint_geoterm")

   !---------------------------------------------------------------------------
   !  [1.0] Initialise:
   !---------------------------------------------------------------------------

   ! Computation to check for edge of domain:
   is = its; ie = ite; js = jts; je = jte
   if (.not. global .and. its == ids) is = ids+1
   if (.not. global .and. ite == ide) ie = ide-1
   if (jts == jds) js = jds+1; if (jte == jde) je = jde-1

   !---------------------------------------------------------------------------
   !  [2.0] Calculate term_x = -f rho v~:
   !---------------------------------------------------------------------------

   term_x(is:ie, js:je) = term_x(is:ie, js:je)-rho(is:ie, js:je) &
      * v(is:ie, js:je) * cori(is:ie, js:je)

   !---------------------------------------------------------------------------
   !  [3.0] Calculate term_y = f rho u~:
   !---------------------------------------------------------------------------

   term_y(is:ie, js:je) = term_y(is:ie, js:je)+rho(is:ie, js:je) &
      * u(is:ie, js:je) * cori(is:ie, js:je)

   if (trace_use) call da_trace_exit("da_wpec_constraint_geoterm")

end subroutine da_wpec_constraint_geoterm


