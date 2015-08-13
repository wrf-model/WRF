subroutine da_balance_geoterm_nl (domain, k, u, v, term_x, term_y)
 
   !---------------------------------------------------------------------------
   ! Purpose: calculates nonlinear geostrophic term in balance equation.
   !
   ! method:  term is k x rho f u on a single level.
   !---------------------------------------------------------------------------

   implicit none

   type(domain), intent(in)  :: grid
   integer,      intent(in)  :: k            ! Model level.
   real,         intent(in)  :: u(:,:)       ! u wind comp. (dot pts)
   real,         intent(in)  :: v(:,:)       ! v wind comp. (dot pts)
   real,         intent(out) :: term_x(:,:)  ! x component of term.
   real,         intent(out) :: term_y(:,:)  ! y component of term.

   if (trace_use) call da_trace_entry("da_balance_geoterm_nl")

   !---------------------------------------------------------------------------
   !  [1.0] Initialise:
   !---------------------------------------------------------------------------

   term_x(:,:) = 0.0
   term_y(:,:) = 0.0

   !---------------------------------------------------------------------------
   !  [2.0] Calculate term_x = -f rho v~:
   !---------------------------------------------------------------------------

   term_x(its:ite, jts:jte) = -grid%xb%rho(its:ite, jts:jte, k) &
      * grid%xb%  v(its:ite, jts:jte, k) * grid%xb%cori(its:ite, jts:jte)

   !---------------------------------------------------------------------------
   !  [3.0] Calculate term_y = f rho u~:
   !---------------------------------------------------------------------------

   term_y(its:ite, jts:jte) = grid%xb%rho(its:ite, jts:jte, k) &
      * grid%xb%  u(its:ite, jts:jte, k) * grid%xb%cori(its:ite, jts:jte)

   if (trace_use) call da_trace_exit("da_balance_geoterm_nl")

end subroutine da_balance_geoterm_nl


