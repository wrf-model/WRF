subroutine da_wpec_constraint_lin(grid, xbx)

   !---------------------------------------------------------------------------
   !  Purpose: Calculates TLM of balance equation G(x)
   !---------------------------------------------------------------------------

   implicit none

   type(domain),   intent(inout) :: grid
   type(xbx_type), intent(in)    :: xbx                            ! Header & non-gridded vars.

   integer :: i, j, k                 ! Loop counters.
   integer :: is, ie                  ! 1st dim. end points.
   integer :: js, je                  ! 2nd dim. end points.

   real    :: coefx(ims:ime,jms:jme)  ! Multiplicative coefficient.
   real    :: coefy(ims:ime,jms:jme)  ! Multiplicative coefficient.
   real    :: term_x(ims:ime,jms:jme) ! Balance eqn x term
   real    :: term_y(ims:ime,jms:jme) ! Balance eqn y term
   real    :: phi_b_x(ims:ime,jms:jme) ! Balance eqn x term
   real    :: phi_b_y(ims:ime,jms:jme) ! Balance eqn y term
   

   if (trace_use) call da_trace_entry("da_wpec_constraint_lin")

   !---------------------------------------------------------------------------
   ! [1.0] Initialise i and set multiplicative constants
   !---------------------------------------------------------------------------

   is = its; ie = ite; js = jts; je = jte
   if (.not.global .and. its == ids) is = ids+1
   if (.not.global .and. ite == ide) ie = ide-1
   if (jts == jds ) js = jds+1; if (jte == jde) je = jde-1

   if (fg_format == fg_format_kma_global) then
      coefx = grid%xb%coefx
      coefy = grid%xb%coefy
   else if( fg_format == fg_format_wrf_arw_regional) then
      coefx = grid%xb%coefx
      coefy = grid%xb%coefy
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

   ! [1.1] 

   phi_b_x = 0.0
   phi_b_y = 0.0

 
   do k = kts, kte

      term_x = 0.0
      term_y = 0.0

      !---------------------------------------------------------------------------
      ! [2.0] Calculate RHS of balance equation in gridpoint space:
      !---------------------------------------------------------------------------

      ! [2.1] Include geostrophic terms in balance eqn if requested:

      if (balance_type == balance_geo .OR. balance_type == balance_geocyc ) then

         ! [2.1.1] Calculate term_x = -f rho v~:
         term_x = term_x - grid%xb%rho(:,:,k) * grid%xb%cori * grid%xa%v(:,:,k)

         ! [2.1.2] Calculate term_y = f rho u~:
         term_y = term_y + grid%xb%rho(:,:,k) * grid%xb%cori * grid%xa%u(:,:,k)

      end if
      
      ! [2.2] Include cyclostrophic terms in balance eqn if requested:

      if (balance_type == balance_cyc .OR. balance_type == balance_geocyc ) then

         do j = js, je
            do i = is, ie
               ! [2.2.1] Calculate term_x = rho M ( u'du/dx + v'du/dy + udu'/dx + vdu'/dy )
               term_x(i,j) = term_x(i,j) + grid%xb%rho(i,j,k) * &
                   ( coefx(i,j)*grid%xa%u(i,j,k) * ( grid%xb%u(i+1,j,k) - grid%xb%u(i-1,j,k)) + &
                     coefy(i,j)*grid%xa%v(i,j,k) * ( grid%xb%u(i,j+1,k) - grid%xb%u(i,j-1,k)) + &
                     coefx(i,j)*grid%xb%u(i,j,k) * ( grid%xa%u(i+1,j,k) - grid%xa%u(i-1,j,k)) + &
                     coefy(i,j)*grid%xb%v(i,j,k) * ( grid%xa%u(i,j+1,k) - grid%xa%u(i,j-1,k)))

               ! [2.2.2] Calculate term_y = rho M ( u'dv/dx + v'dv/dy + udv'/dx + vdv'/dy )
               term_y(i,j) = term_y(i,j) + grid%xb%rho(i,j,k) * &
                   ( coefx(i,j)*grid%xa%u(i,j,k) * ( grid%xb%v(i+1,j,k) - grid%xb%v(i-1,j,k)) + &
                     coefy(i,j)*grid%xa%v(i,j,k) * ( grid%xb%v(i,j+1,k) - grid%xb%v(i,j-1,k)) + &
                     coefx(i,j)*grid%xb%u(i,j,k) * ( grid%xa%v(i+1,j,k) - grid%xa%v(i-1,j,k)) + &
                     coefy(i,j)*grid%xb%v(i,j,k) * ( grid%xa%v(i,j+1,k) - grid%xa%v(i,j-1,k)))
            end do
         end do

      end if

      ! [2.3] Include phi_b terms in balance eqn
      do j = js, je
         do i = is, ie
            phi_b_x(i,j) = coefx(i,j)*(grid%xa%p(i+1,j,k)-grid%xa%p(i-1,j,k) + grid%xb % rho(i,j,k)*(grid%xa%geoh(i+1,j,k)-grid%xa%geoh(i-1,j,k)) )
            phi_b_y(i,j) = coefy(i,j)*(grid%xa%p(i,j+1,k)-grid%xa%p(i,j-1,k) + grid%xb % rho(i,j,k)*(grid%xa%geoh(i,j+1,k)-grid%xa%geoh(i,j-1,k)) )                 
         end do
      end do

   !------------------------------------------------------------------------------
   !  [3.0] Solve Grad_p for balance equation :
   !------------------------------------------------------------------------------
      do j = js, je
         do i = is, ie
            grid%xa%grad_p_x(i,j,k)=phi_b_x(i,j)+term_x(i,j)
            grid%xa%grad_p_y(i,j,k)=phi_b_y(i,j)+term_y(i,j)
         end do
      end do

   end do

   if (trace_use) call da_trace_exit("da_wpec_constraint_lin")

end subroutine da_wpec_constraint_lin


