subroutine da_wpec_constraint(grid, xbx)

   !---------------------------------------------------------------------------
   !  Purpose: Calculates balance equation G(x)
   !---------------------------------------------------------------------------

   implicit none

   type(domain),   intent(inout) :: grid
   type(xbx_type), intent(in)    :: xbx                            ! Header & non-gridded vars.

   real   :: u(ims:ime,jms:jme,kms:kme)     ! u wind comp.
   real   :: v(ims:ime,jms:jme,kms:kme)     ! v wind comp.
   real   :: phi_b(ims:ime,jms:jme,kms:kme) 

   integer :: i, j, k                 ! Loop counters.
   integer :: is, ie                  ! 1st dim. end points.
   integer :: js, je                  ! 2nd dim. end points.

   real    :: coefx(ims:ime,jms:jme)  ! Multiplicative coefficient.
   real    :: coefy(ims:ime,jms:jme)  ! Multiplicative coefficient.
   real    :: term_x(ims:ime,jms:jme) ! Balance eqn x term
   real    :: term_y(ims:ime,jms:jme) ! Balance eqn y term
   real    :: phi_b_x(ims:ime,jms:jme) ! Balance eqn x term
   real    :: phi_b_y(ims:ime,jms:jme) ! Balance eqn y term
   


   if (trace_use) call da_trace_entry("da_wpec_constraint")

   !---------------------------------------------------------------------------
   ! [1.0] Initialise i and set multiplicative constants
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

   phi_b  =   grid%xb%p

   do k = kts, kte

      term_x(ims:ime,jms:jme) = 0.0
      term_y(ims:ime,jms:jme) = 0.0
      phi_b_x(ims:ime,jms:jme) = 0.0
      phi_b_y(ims:ime,jms:jme) = 0.0

      !---------------------------------------------------------------------------
      ! [2.0] Calculate RHS of balance equation in gridpoint space:
      !---------------------------------------------------------------------------

      ! [2.1] Include geostrophic terms in balance eqn if requested:

      if (balance_type == balance_geo .OR. balance_type == balance_geocyc ) then
         call da_wpec_constraint_geoterm (grid%xb % cori, grid%xb % rho(:,:,k), grid%xb %u(:,:,k), grid%xb %v(:,:,k), &
            term_x, term_y)
      end if
      
      ! [2.2] Include cyclostrophic terms in balance eqn if requested:

      if (balance_type == balance_cyc .OR. balance_type == balance_geocyc ) then
         call da_wpec_constraint_cycloterm (grid%xb % rho(:,:,k), grid%xb % u(:,:,k),   &
            grid%xb % v(:,:,k), grid%xb % coefx(:,:), grid%xb % coefy(:,:), &
            term_x, term_y)
      end if

      ! [2.3] Include phi_b terms in balance eqn
      do j = js, je
         do i = is, ie
            phi_b_x(i,j) = coefx(i,j)*(phi_b(i+1,j,k)-phi_b(i-1,j,k)) 
            phi_b_y(i,j) = coefy(i,j)*(phi_b(i,j+1,k)-phi_b(i,j-1,k))                 
         end do
      end do

   !------------------------------------------------------------------------------
   !  [3.0] Solve Grad_p for balance eqn :
   !------------------------------------------------------------------------------
      do j = js, je
         do i = is, ie
            grid%xb%xb_p_x(i,j,k)=phi_b_x(i,j)+term_x(i,j)
            grid%xb%xb_p_y(i,j,k)=phi_b_y(i,j)+term_y(i,j)
         end do
      end do

    end do


   if (trace_use) call da_trace_exit("da_wpec_constraint")

end subroutine da_wpec_constraint


