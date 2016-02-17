subroutine da_wpec_constraint_adj(grid, xbx)

   !---------------------------------------------------------------------------
   ! Purpose: Calculates ADM of balance equation G(x)
   !---------------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)               :: grid

   type (xbx_type),intent(in) :: xbx              ! Header & non-gridded vars.

   real :: p(ims:ime,jms:jme,kms:kme) ! pressure increment.
   real :: geoh(ims:ime,jms:jme,kms:kme) ! geopotential height increment.
   real :: u(ims:ime,jms:jme,kms:kme) ! u wind comp. (dot pts)
   real :: v(ims:ime,jms:jme,kms:kme) ! v wind comp. (dot pts)

   integer             :: i, j, k          ! Loop counters.
   integer             :: is, ie           ! 1st dim. end points.
   integer             :: js, je           ! 2nd dim. end points.

   real, dimension(ims:ime,jms:jme) :: coefx, &   ! Multiplicative coefficient.
                                       coefy, &   ! Multiplicative coefficient.
                                       term_x, &  ! Balance eqn x term
                                       term_y     ! Balance eqn y term
   real    :: phi_b_x(ims:ime,jms:jme) ! Balance eqn x term
   real    :: phi_b_y(ims:ime,jms:jme) ! Balance eqn y term

   real                 :: data(ims:ime,jms:jme)        ! Work array.
   real                 :: var, uar

   if (trace_use) call da_trace_entry("da_wpec_constraint_adj")

   !---------------------------------------------------------------------------
   ! [1.0] Initialise:
   !---------------------------------------------------------------------------

   is = its; ie = ite; js = jts; je = jte
   if (.not. global .and. its == ids) is = ids+1
   if (.not. global .and. ite == ide) ie = ide-1
   if (jts == jds ) js = jds+1
   if (jte == jde ) je = jde-1

   if (fg_format == fg_format_kma_global) then
      coefx = grid%xb%coefx
      coefy = grid%xb%coefy
   else if (fg_format == fg_format_wrf_arw_regional) then
      coefx = grid%xb%coefx
      coefy = grid%xb%coefy
   else if (fg_format == fg_format_wrf_arw_global) then
      write (unit=message(1),fmt='(A,I3)') ' needs work for fg_format  = ',fg_format
      call da_error(__FILE__,__LINE__,message(1:1))
   else if (fg_format == fg_format_wrf_nmm_regional) then
      write (unit=message(1),fmt='(A,I3)') ' needs work for fg_format  = ',fg_format
      call da_error(__FILE__,__LINE__,message(1:1))
   else
      write (unit=message(1),fmt='(A,I3)') ' Wrong FG_FORMAT = ',fg_format
      call da_error(__FILE__,__LINE__,message(1:1))
   end if
   
   u       = 0.0
   v       = 0.0
   p       = 0.0
   geoh    = 0.0

   do k = kts, kte

      term_x = 0.0
      term_y = 0.0
      phi_b_x = 0.0
      phi_b_y = 0.0

      !---------------------------------------------------------------------------
      ! [2.0] Solve Grad_p for balance eqn
      !---------------------------------------------------------------------------

      phi_b_x = grid%xa%grad_p_x(:,:,k)
      phi_b_y = grid%xa%grad_p_y(:,:,k)
      term_x = grid%xa%grad_p_x(:,:,k)
      term_y = grid%xa%grad_p_y(:,:,k)

      !---------------------------------------------------------------------------
      ! [3.0] Calculate RHS of balance equation in gridpt space
      !---------------------------------------------------------------------------

      ! [3.1] Include phi_b terms in balance eqn

      do j = je, js, -1
         do i = ie, is, -1

            p(i+1,j,k) = p(i+1,j,k) + coefx(i,j) * phi_b_x(i,j)
            p(i-1,j,k) = p(i-1,j,k) - coefx(i,j) * phi_b_x(i,j)
            p(i,j+1,k) = p(i,j+1,k) + coefy(i,j) * phi_b_y(i,j)
            p(i,j-1,k) = p(i,j-1,k) - coefy(i,j) * phi_b_y(i,j)

            geoh(i+1,j,k) = geoh(i+1,j,k) + coefx(i,j) * phi_b_x(i,j) * grid%xb % rho(i,j,k)
            geoh(i-1,j,k) = geoh(i-1,j,k) - coefx(i,j) * phi_b_x(i,j) * grid%xb % rho(i,j,k)
            geoh(i,j+1,k) = geoh(i,j+1,k) + coefy(i,j) * phi_b_y(i,j) * grid%xb % rho(i,j,k)
            geoh(i,j-1,k) = geoh(i,j-1,k) - coefy(i,j) * phi_b_y(i,j) * grid%xb % rho(i,j,k)

         end do
      end do

      ! [3.2] Include cyclostrophic terms in balance eqn if requested:

      if (balance_type == balance_cyc .OR. balance_type == balance_geocyc ) then

         ! [3.2.1] Calculate adjoint of term_y = rho M ( u'dv/dx + v'dv/dy + udv'/dx + vdv'/dy ):
         data = grid%xb%rho(:,:,k) * term_y

         do j = je, js, -1
            do i = ie, is, -1
               uar = coefx(i,j) * grid%xb%u(i,j,k) * data(i,j)
               var = coefy(i,j) * grid%xb%v(i,j,k) * data(i,j)

                u(i,j,k) = u(i,j,k) + coefx(i,j)*data(i,j)*( grid%xb%v(i+1,j,k) - grid%xb%v(i-1,j,k) )
                v(i,j,k) = v(i,j,k) + coefy(i,j)*data(i,j)*( grid%xb%v(i,j+1,k) - grid%xb%v(i,j-1,k) )
                v(i+1,j,k) = v(i+1,j,k) + uar
                v(i-1,j,k) = v(i-1,j,k) - uar
                v(i,j+1,k) = v(i,j+1,k) + var
                v(i,j-1,k) = v(i,j-1,k) - var
            end do
         end do

         ! [3.2.2] Calculate adjoint of term_x = rho M ( u'du/dx + v'du/dy + udu'/dx + vdu'/dy ):
         data = grid%xb%rho(:,:,k) * term_x

         do j = je, js, -1
            do i = ie, is, -1
               uar = coefx(i,j) * grid%xb%u(i,j,k) * data(i,j)
               var = coefy(i,j) * grid%xb%v(i,j,k) * data(i,j)

                u(i,j,k) = u(i,j,k) + coefx(i,j)*( grid%xb%u(i+1,j,k) - grid%xb%u(i-1,j,k) ) * data(i,j)
                v(i,j,k) = v(i,j,k) + coefy(i,j)*( grid%xb%u(i,j+1,k) - grid%xb%u(i,j-1,k) ) * data(i,j)
                u(i+1,j,k) = u(i+1,j,k) + uar
                u(i-1,j,k) = u(i-1,j,k) - uar
                u(i,j+1,k) = u(i,j+1,k) + var
                u(i,j-1,k) = u(i,j-1,k) - var
            end do
         end do
      end if

      
      ! [3.3] Calculate geostrophic terms in balance eqn:
 
      if (balance_type == balance_geo .OR. balance_type == balance_geocyc ) then
         ! [3.3.1] Calculate term_y = f rho u~:
         u(:,:,k) = u(:,:,k) + grid%xb%rho(:,:,k) * grid%xb%cori * term_y

         ! [3.3.2] Calculate term_x = -f rho v~:
         v(:,:,k) = v(:,:,k) - grid%xb%rho(:,:,k) * grid%xb%cori * term_x

      end if

   end do

   !---------------------------------------------------------------------------
   ! [4.0] Store results in grid%xa structure
   !---------------------------------------------------------------------------


   grid%xa%u=u
   grid%xa%v=v
   grid%xa%p=p
   grid%xa%geoh=geoh


   if (trace_use) call da_trace_exit("da_wpec_constraint_adj")

end subroutine da_wpec_constraint_adj


