subroutine da_balance_cycloterm (xb, k, term_x, term_y)

   !---------------------------------------------------------------------------
   !  Purpose: Calculates cyclostrophic term in balance equation.
   !
   !  Method:  Term is rho (u.grad) u on a single level.
   !---------------------------------------------------------------------------

   implicit none
   
   type(xb_type), intent(in)    :: xb           ! First guess structure.
   integer,       intent(in)    :: k            ! Model level.
   real,          intent(inout) :: term_x(:,:)  ! x component of term.
   real,          intent(inout) :: term_y(:,:)  ! y component of term.

   integer :: i, j                         ! Loop counters.
   integer :: is, ie                       ! 1st dim. end points.
   integer :: js, je                       ! 2nd dim. end points.
   
   real    :: data(ims:ime,jms:jme)        ! Temporary storage.

   real    :: varb

   if (trace_use) call da_trace_entry("da_balance_cycloterm")

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
         data(i,j) = xb%u(i,j,k) * xb%coefx(i,j)*(xb%u(i+1,j,k) - &
            xb%u(i-1,j,k)) + xb%v(i,j,k) * xb%coefy(i,j)*(xb%u(i,j+1,k) - &
            xb%u(i,j-1,k))
      end do
   end do
   
   if (.NOT. global) then ! For global only interior points

      ! [2.2] Bottom boundaries:

      if (its==ids) then
         i = its

         do j = js, je 
            varb = -3.0*xb%u(i,j,k)+4.0*xb%u(i+1,j,k)-xb%u(i+2,j,k)

            data(i,j) = xb%u(i,j,k) * xb%coefx(i,j) * varb + &
               xb%v(i,j,k) * xb%coefy(i,j) * (xb%u(i,j+1,k) - xb%u(i,j-1,k))
         end do
      end if

      ! [2.3] Top boundaries:

      if (ite==ide) then
         i = ite

         do j = js, je
            varb = 3.0*xb%u(i,j,k)-4.0*xb%u(i-1,j,k)+xb%u(i-2,j,k)

            data(i,j) = xb%u(i,j,k) * xb%coefx(i,j) * varb + &
               xb%v(i,j,k) * xb%coefy(i,j) * (xb%u(i,j+1,k) - xb%u(i,j-1,k))
         end do
      end if

      ! [2.4] Left boundaries:

      if (jts==jds) then
         j = jts

         do i = is, ie
            varb = -3.0*xb%u(i,j,k)+4.0*xb%u(i,j+1,k)-xb%u(i,j+2,k)

            data(i,j) = xb%u(i,j,k) * xb%coefx(i,j) * (xb%u(i+1,j,k) - &
               xb%u(i-1,j,k)) + xb%v(i,j,k) * xb%coefy(i,j) * varb
         end do
      end if

      ! [2.5] Right boundaries:

      if (jte==jde) then
         j = jte

         do i = is, ie
            varb = 3.0*xb%u(i,j,k)-4.0*xb%u(i,j-1,k)+xb%u(i,j-2,k)

            data(i,j) = xb%u(i,j,k) * xb%coefx(i,j) * (xb%u(i+1,j,k) - &
               xb%u(i-1,j,k)) + xb%v(i,j,k) * xb%coefy(i,j) * varb
         end do
      end if

      ! [2.6] Corner points:

      if (its==ids .AND. jts==jds) then
         data(its,jts) = 0.5 * (data(its,jts+1) + data(its+1,jts))
      end if

      if (ite==ide .AND. jts==jds) then
         data(ite,jts) = 0.5 * (data(ite-1,jts) + data(ite,jts+1))
      end if

      if (its==ids .AND. jte==jde) then
         data(its,jde) = 0.5 * (data(its,jde-1) + data(its+1,jde))
      end if

      if (ite==ide .AND. jte==jde) then 
         data(ite,jte) = 0.5 * (data(ite-1,jte) + data(ite,jte-1))
      end if
   end if ! not global

   !  [2.7] Multiply by rho  and add to term_x:
      
   term_x(its:ite,jts:jte) = xb%rho(its:ite,jts:jte,k)*data(its:ite,jts:jte) + term_x(its:ite,jts:jte)

   !---------------------------------------------------------------------------
   ! [3.0] Calculate term_y = rho M (u.dv/dx + v.dv/dy):
   !---------------------------------------------------------------------------

   ! [3.1] Interior points:

   do j = js, je
      do i = is, ie
         data(i,j) = xb%u(i,j,k) * xb%coefx(i,j)*(xb%v(i+1,j,k) - xb%v(i-1,j,k)) + &
                     xb%v(i,j,k) * xb%coefy(i,j)*(xb%v(i,j+1,k) - xb%v(i,j-1,k))
      end do
   end do
   
   
   if (.NOT. global) then ! For global only interior points

      ! [3.2] Bottom boundaries:

      if (its==ids) then
         i = its

         do j = js, je
            varb = -3.0*xb%v(i,j,k)+4.0*xb%v(i+1,j,k)-xb%v(i+2,j,k)

            data(i,j) = xb%u(i,j,k) * xb%coefx(i,j)* varb + &
                        xb%v(i,j,k) * xb%coefy(i,j)* (xb%v(i,j+1,k) - xb%v(i,j-1,k))
         end do
      end if

      !  [3.3] Top boundaries:

      if (ite==ide) then
         i = ite

         do j = js, je
            varb = 3.0*xb%v(i,j,k)-4.0*xb%v(i-1,j,k)+xb%v(i-2,j,k)

            data(i,j) = xb%u(i,j,k) * xb%coefx(i,j)* varb + &
                        xb%v(i,j,k) * xb%coefy(i,j)* (xb%v(i,j+1,k) - xb%v(i,j-1,k))
         end do
      end if

      ! [3.4] Left boundaries:

      if (jts==jds) then
         j = jts

         do i = is, ie
            varb = -3.0*xb%v(i,j,k)+4.0*xb%v(i,j+1,k)-xb%v(i,j+2,k)

            data(i,j) = xb%u(i,j,k) * xb%coefx(i,j)* (xb%v(i+1,j,k) - xb%v(i-1,j,k)) + &
                        xb%v(i,j,k) * xb%coefy(i,j)* varb
         end do
      end if

      ! [3.5] Right boundaries:

      if (jte==jde) then
         j = jte

         do i = is, ie
            varb = 3.0*xb%v(i,j,k)-4.0*xb%v(i,j+1,k)+xb%v(i,j+2,k)

            data(i,j) = xb%u(i,j,k) * xb%coefx(i,j)* (xb%v(i+1,j,k) - xb%v(i-1,j,k)) + &
                        xb%v(i,j,k) * xb%coefy(i,j)* varb
         end do
      end if

      ! [3.6] Corner points:

      if (its==ids .AND. jts==jds) then
         data(its,jts) = 0.5 * (data(its,jts+1) + data(its+1,jts))
      end if

      if (ite==ide .AND. jts==jds) then
         data(ite,jts) = 0.5 * (data(ite-1,jts) + data(ite,jts+1))
      end if

      if (its==ids .AND. jte==jde) then
         data(its,jde) = 0.5 * (data(its,jde-1) + data(its+1,jde))
      end if

      if (ite==ide .AND. jte==jde) then 
         data(ite,jte) = 0.5 * (data(ite-1,jte) + data(ite,jte-1))
      end if
   end if ! not global

   ! [3.7] Multiply by rho and add to term_y

   term_y(its:ite,jts:jte)=xb%rho(its:ite,jts:jte,k)* data(its:ite,jts:jte) + &
      term_y(its:ite,jts:jte)

   if (trace_use) call da_trace_exit("da_balance_cycloterm")

end subroutine da_balance_cycloterm


