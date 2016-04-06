subroutine da_balance_equation_adj(grid, xbx, phi_b, u, v)

   !---------------------------------------------------------------------------
   ! Purpose: Adjoint of da_balance_equation
   !---------------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)               :: grid

   type (xbx_type),intent(in) :: xbx              ! Header & non-gridded vars.
   real, intent(in)    :: phi_b(ims:ime,jms:jme,kms:kme) ! Balanced mass increment.
   real, intent(inout) :: u(ims:ime,jms:jme,kms:kme) ! u wind comp. (dot pts)
   real, intent(inout) :: v(ims:ime,jms:jme,kms:kme) ! v wind comp. (dot pts)

   integer             :: i, j, k          ! Loop counters.
   integer             :: is, ie           ! 1st dim. end points.
   integer             :: js, je           ! 2nd dim. end points.

   real, dimension(ims:ime,jms:jme) :: coefx, &   ! Multiplicative coefficient.
                                       coefy, &   ! Multiplicative coefficient.
                                       term_x, &  ! Balance eqn x term
                                       term_y     ! Balance eqn y term

   real, dimension(ims:ime,jms:jme,kms:kme) :: del2phi_b  ! Del**2 phi_b/M**2
   real                       :: coeff1, coeff2   ! Multiplicative coefficient.

   if (trace_use) call da_trace_entry("da_balance_equation_adj")

   !---------------------------------------------------------------------------
   ! [1.0] Initialise:
   !---------------------------------------------------------------------------

   ! Computation to check for edge of domain:
   is = its-1; ie = ite+1; js = jts-1; je = jte+1
   if (.not. global .and. its == ids) is = ids+1
   if (.not. global .and. ite == ide) ie = ide-1
   if (jts == jds ) js = jds+1
   if (jte == jde ) je = jde-1

   if (fg_format == fg_format_kma_global) then
      coefx(is:ie,js:je) = grid%xb%coefx(is:ie,js:je)
      coefy(is:ie,js:je) = grid%xb%coefy(is:ie,js:je)
   else if (fg_format == fg_format_wrf_arw_global) then
      write (unit=message(1),fmt='(A,I3)') ' needs work for fg_format  = ',fg_format
      call da_error(__FILE__,__LINE__,message(1:1))
   else if (fg_format == fg_format_wrf_arw_regional) then
      coefx(is:ie,js:je) = grid%xb%coefz(is:ie,js:je)
      coefy(is:ie,js:je) = coefx(is:ie,js:je)
   else if (fg_format == fg_format_wrf_nmm_regional) then
      write (unit=message(1),fmt='(A,I3)') ' needs work for fg_format  = ',fg_format
      call da_error(__FILE__,__LINE__,message(1:1))
   else
      write (unit=message(1),fmt='(A,I3)') ' Wrong FG_FORMAT = ',fg_format
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   ! [1.1] Multiplicative coefficent for conversion RHS->Del**2 phi_b/M**2:

   del2phi_b(:,:,:) = 0.0

   !---------------------------------------------------------------------------
   ! [3.0] Solve Del**2 phi_b = RHS for phi_b:
   !---------------------------------------------------------------------------

   call da_solve_poissoneqn_fst_adj(grid,xbx, phi_b, del2phi_b)

   !---------------------------------------------------------------------------
   ! [2.0] Calculate RHS of balance equation in gridpt space:
   !---------------------------------------------------------------------------

   do k = kts, kte
      ! [2.4] Del**2 Phi_b boundary conditions (null as zero boundary conditions):

      ! [2.3] Take divergence to get Del**2 phi_b/M**2:

      term_x(ims:ime,jms:jme) = 0.0
      term_y(ims:ime,jms:jme) = 0.0

      do j = je, js, -1
         do i = ie, is, -1
            coeff1 = coefx(i,j) * del2phi_b(i,j,k)
            coeff2 = coefy(i,j) * del2phi_b(i,j,k)

            term_x(i+1,j) = term_x(i+1,j) - coeff1
            term_x(i-1,j) = term_x(i-1,j) + coeff1
            term_y(i,j+1) = term_y(i,j+1) - coeff2
            term_y(i,j-1) = term_y(i,j-1) + coeff2
         end do
      end do

      ! [2.2] Include cyclostrophic terms in balance eqn if requested:

      if (balance_type == balance_cyc .OR. balance_type == balance_geocyc ) then
         call da_balance_cycloterm_adj (grid%xb%rho(:,:,k),grid%xb%u(:,:,k),&
            grid%xb%v(:,:,k), u(:,:,k), v(:,:,k), grid%xb%coefx(:,:), grid%xb%coefy(:,:),&
            term_x(:,:), term_y(:,:))
      end if

      
      ! [2.1] Calculate geostrophic terms in balance eqn:
 
      if (balance_type == balance_geo .OR. balance_type == balance_geocyc ) then
         call da_balance_geoterm_adj (grid%xb%cori, grid%xb%rho(:,:,k), term_x, term_y, &
            u(:,:,k), v(:,:,k))
      end if
   end do

   if (trace_use) call da_trace_exit("da_balance_equation_adj")

end subroutine da_balance_equation_adj


