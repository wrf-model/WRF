subroutine da_balance_equation_lin(grid, xbx, u, v, phi_b)

   !---------------------------------------------------------------------------
   !  Purpose: Calculates balanced mass increment phi_b from wind increments.
   !
   !  Method:  Solve grad**2 Phi_b = - div[ k x rho f u + rho (u.grad ) u ] by
   !           1) Calculate RHS of balance equation in gridpt space.
   !           2) Solve Del**2 phi_b = RHS for phi_b using double (FCT).
   !
   !---------------------------------------------------------------------------

   implicit none

   type(domain),   intent(inout) :: grid
   type(xbx_type), intent(in)    :: xbx                            ! Header & non-gridded vars. 
   real,           intent(in)    :: u(ims:ime,jms:jme,kms:kme)     ! u wind comp.
   real,           intent(in)    :: v(ims:ime,jms:jme,kms:kme)     ! v wind comp.
   real,           intent(out)   :: phi_b(ims:ime,jms:jme,kms:kme) ! Balanced mass increment.

   integer :: i, j, k                 ! Loop counters.
   integer :: is, ie                  ! 1st dim. end points.
   integer :: js, je                  ! 2nd dim. end points.

   real    :: coefx(ims:ime,jms:jme)  ! Multiplicative coefficient.
   real    :: coefy(ims:ime,jms:jme)  ! Multiplicative coefficient.
   real    :: term_x(ims:ime,jms:jme) ! Balance eqn x term
   real    :: term_y(ims:ime,jms:jme) ! Balance eqn y term
   
   real    :: del2phi_b(ims:ime,jms:jme,kms:kme)  ! Del**2 phi_b/M**2

   if (trace_use) call da_trace_entry("da_balance_equation_lin")

   !---------------------------------------------------------------------------
   ! [1.0] Initialise iand set Multipilactive constants
   !---------------------------------------------------------------------------

   ! Computation to check for edge of domain:

   is = its; ie = ite; js = jts; je = jte
   if (.not.global .and. its == ids) is = ids+1
   if (.not.global .and. ite == ide) ie = ide-1
   if (jts == jds ) js = jds+1; if (jte == jde) je = jde-1

   if (fg_format == fg_format_kma_global) then
      coefx = grid%xb%coefx
      coefy = grid%xb%coefy
   else if( fg_format == fg_format_wrf_arw_regional) then
      coefx = grid%xb%coefz
      coefy = coefx   
   else if (fg_format == fg_format_wrf_arw_global) then
      write (unit=message(1),fmt='(A,I3)') ' needs work for fg_format_wrf_arw_global  = ',fg_format
      call da_error(__FILE__,__LINE__,message(1:1))
   else if (fg_format == fg_format_wrf_nmm_regional) then
      write (unit=message(1),fmt='(A,I3)') ' needs work for fg_format_wrf_nmm_regional = ',fg_format
      call da_error(__FILE__,__LINE__,message(1:1))
   else
      write(unit=message(1),fmt='(A,I3)') 'Wrong FG_FORMAT = ',fg_format
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   ! [1.1] Multiplicative coefficent for conversion RHS->Del**2 phi_b/M**2:

   do k = kts, kte
      term_x(ims:ime,jms:jme) = 0.0
      term_y(ims:ime,jms:jme) = 0.0

      !---------------------------------------------------------------------------
      ! [2.0] Calculate RHS of balance equation in gridpt space:
      !---------------------------------------------------------------------------

      ! [2.1] Include geostrophic terms in balance eqn if requested:

      if (balance_type == balance_geo .OR. balance_type == balance_geocyc ) then
         call da_balance_geoterm_lin (grid%xb % cori, grid%xb % rho(:,:,k), u(:,:,k), v(:,:,k), &
            term_x, term_y)
      end if
      
      ! [2.2] Include cyclostrophic terms in balance eqn if requested:

      if (balance_type == balance_cyc .OR. balance_type == balance_geocyc ) then
         call da_balance_cycloterm_lin (grid%xb % rho(:,:,k), grid%xb % u(:,:,k),   &
            grid%xb % v(:,:,k), u(:,:,k), v(:,:,k), grid%xb % coefx(:,:), grid%xb % coefy(:,:), &
            term_x(:,:), term_y(:,:))
      end if
      
      ! [2.3] Take divergence to get Del**2 phi_b/M**2:

      do j = js, je
         do i = is, ie
          del2phi_b(i,j,k) = -coefx(i,j)*( term_x(i+1,j) - term_x(i-1,j)) &
                             -coefy(i,j)*( term_y(i,j+1) - term_y(i,j-1)) 
         end do
      end do

      ! [2.4] Del**2 Phi_b  boundary conditions:

      if (.not. global .and. its == ids ) del2phi_b(its,js:je,k) = 0.0
      if (.not. global .and. ite == ide ) del2phi_b(ite,js:je,k) = 0.0
      if (jts == jds ) del2phi_b(is:ie,jts,k) = 0.0
      if (jte == jde ) del2phi_b(is:ie,jte,k) = 0.0

      if (.not. global .and. (its == ids .and. jts == jds) ) del2phi_b(its,jts,k) = 0.0
      if (.not. global .and. (its == ids .and. jte == jde) ) del2phi_b(its,jte,k) = 0.0
      if (.not. global .and. (ite == ide .and. jts == jds) ) del2phi_b(ite,jts,k) = 0.0
      if (.not. global .and. (ite == ide .and. jte == jde) ) del2phi_b(ite,jte,k) = 0.0     
   end do

   !------------------------------------------------------------------------------
   !  [3.0] Solve Del**2 phi_b = RHS for phi_b:
   !------------------------------------------------------------------------------

   call da_solve_poissoneqn_fst(grid, xbx, del2phi_b, phi_b) 

   if (trace_use) call da_trace_exit("da_balance_equation_lin")

end subroutine da_balance_equation_lin


