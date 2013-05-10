subroutine da_balance_cycloterm_lin( rho, ub, vb, u, v, coefx, coefy, term_x, term_y)
                                     
   !---------------------------------------------------------------------------
   ! Purpose: Calculates linearised cyclostrophic term in balance equation.
   !
   ! Method:  Term is rho (u.grad) u on a single level.
   !---------------------------------------------------------------------------

   implicit none
   
   real, intent(in)    :: rho(ims:ime,jms:jme)    ! Density.
   real, intent(in)    :: ub(ims:ime,jms:jme)     ! Background u wind
   real, intent(in)    :: vb(ims:ime,jms:jme)     ! Background u wind
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

   real    :: varb, var

   if (trace_use) call da_trace_entry("da_balance_cycloterm_lin")

   !---------------------------------------------------------------------------
   ! [1.0] Initialise:
   !---------------------------------------------------------------------------

   ! Computation to check for edge of domain:
   is = its; ie = ite; js = jts; je = jte
   if (.not. global .and. its == ids) is = ids+1
   if (.not. global .and. ite == ide) ie = ide-1
   if (jts == jds) js = jds+1; if (jte == jde) je = jde-1

   !---------------------------------------------------------------------------
   ! [2.0] Calculate term_x = rho M ( u'du/dx + v'du/dy + udu'/dx + vdu'/dy):
   !---------------------------------------------------------------------------

   ! [2.1] Interior points:

   do j = js, je
      do i = is, ie
         data(i,j) = coefx(i,j)*u(i,j) * ( ub(i+1,j) - ub(i-1,j)) + &
                     coefy(i,j)*v(i,j) * ( ub(i,j+1) - ub(i,j-1)) + &
                     coefx(i,j)*ub(i,j) * ( u(i+1,j) - u(i-1,j)) + &
                     coefy(i,j)*vb(i,j) * ( u(i,j+1) - u(i,j-1))
      end do
   end do

   if (.NOT. global) then ! For global only interior points needed  

      ! [2.2] Bottom boundaries:

      if (its == ids) then
         i = its

         do j = js, je 
            var  = -3.0*u (i,j)+4.0*u(i+1,j)-u(i+2,j)
            varb = -3.0*ub(i,j)+4.0*ub(i+1,j)-ub(i+2,j)

            data(i,j) = coefx(i,j)* u(i,j) * varb + &
                        coefy(i,j)* v(i,j) * ( ub(i,j+1) - ub(i,j-1)) + &
                        coefx(i,j)*ub(i,j) * var + &
                        coefy(i,j)*vb(i,j) * ( u(i,j+1) - u(i,j-1))
         end do
      end if

      ! [2.3] Top boundaries:

      if (ite == ide) then
         i = ite

         do j = js, je
            var  = 3.0*u (i,j)-4.0*u (i-1,j)+u (i-2,j)
            varb = 3.0*ub(i,j)-4.0*ub(i-1,j)+ub(i-2,j)

            data(i,j) = coefx(i,j)*u(i,j) * varb + &
                        coefy(i,j)*v(i,j) * ( ub(i,j+1) - ub(i,j-1)) + &
                        coefx(i,j)*ub(i,j) * var + &
                        coefy(i,j)*vb(i,j) * ( u(i,j+1) - u(i,j-1))
         end do
      end if

      ! [2.4] Left boundaries:

      if (jts == jds) then
         j = jts

         do i = is, ie
            var  = -3.0*u (i,j)+4.0*u (i,j+1)-u (i,j+2)
            varb = -3.0*ub(i,j)+4.0*ub(i,j+1)-ub(i,j+2)

            data(i,j) = coefx(i,j)*u(i,j) * ( ub(i+1,j) - ub(i-1,j)) + &
                        coefy(i,j)*v(i,j) * varb + &
                        coefx(i,j)*ub(i,j) * ( u(i+1,j) - u(i-1,j)) + &
                        coefy(i,j)*vb(i,j) * var
         end do
      end if

      ! [2.5] Right boundaries:

      if (jte == jde) then
         j = jte

         do i = is, ie
            var  = 3.0*u (i,j)-4.0*u (i,j-1)+u (i,j-2)
            varb = 3.0*ub(i,j)-4.0*ub(i,j-1)+ub(i,j-2)

            data(i,j) = coefx(i,j)*u(i,j) * ( ub(i+1,j) - ub(i-1,j)) + &
                        coefy(i,j)*v(i,j) * varb + &
                        coefx(i,j)*ub(i,j) * ( u(i+1,j) - u(i-1,j)) + &
                        coefy(i,j)*vb(i,j) * var
         end do
      end if

      ! [2.6] Corner points:

      if (its == ids .AND. jts == jds) then
         data(its,jts) = 0.5 * ( data(its,jts+1) + data(its+1,jts))
      end if

      if (ite == ide .AND. jts == jds) then
         data(ite,jts) = 0.5 * ( data(ite-1,jts) + data(ite,jts+1))
      end if

      if (its == ids .AND. jte == jde) then
         data(its,jde) = 0.5 * ( data(its,jde-1) + data(its+1,jde))
      end if

      if (ite == ide .AND. jte == jde) then 
         data(ite,jte) = 0.5 * ( data(ite-1,jte) + data(ite,jte-1))
      end if
   end if ! not global
      
   ! [2.7] Multiply by rho and add to term_x:

   term_x(its:ite,jts:jte) = rho(its:ite,jts:jte) * data(its:ite,jts:jte) + &
                             term_x(its:ite,jts:jte)

   !---------------------------------------------------------------------------
   ! [3.0] Calculate term_y = rho M ( u'dv/dx + v'dv/dy + udv'/dx + vdv'/dy):
   !---------------------------------------------------------------------------

   ! [3.1] Interior points:

   do j = js, je
      do i = is, ie
         data(i,j) = coefx(i,j)*u(i,j) * ( vb(i+1,j) - vb(i-1,j)) + &
                     coefy(i,j)*v(i,j) * ( vb(i,j+1) - vb(i,j-1)) + &
                     coefx(i,j)*ub(i,j) * ( v(i+1,j) - v(i-1,j)) + &
                     coefy(i,j)*vb(i,j) * ( v(i,j+1) - v(i,j-1))
      end do
   end do
   
   if (.NOT. global) then      ! For global only interior points needed  
      ! [3.2] Bottom boundaries:

      if (its == ids) then
         i = its

         do j = js, je
            var  = -3.0*v (i,j)+4.0*v (i+1,j)-v (i+2,j)
            varb = -3.0*vb(i,j)+4.0*vb(i+1,j)-vb(i+2,j)

            data(i,j) = coefx(i,j)*u(i,j) * varb + &
                        coefy(i,j)*v(i,j) * ( vb(i,j+1) - vb(i,j-1)) + &
                        coefx(i,j)*ub(i,j) * var + &
                        coefy(i,j)*vb(i,j) * ( v(i,j+1) - v(i,j-1))
         end do
      end if

      !  [3.3] Top boundaries:

      if (ite == ide) then
         i = ite

         do j = js, je
            var  = 3.0*v (i,j)-4.0*v (i-1,j)+v (i-2,j)
            varb = 3.0*vb(i,j)-4.0*vb(i-1,j)+vb(i-2,j)

            data(i,j) = coefx(i,j)*u(i,j) * varb + &
                        coefy(i,j)*v(i,j) * ( vb(i,j+1) - vb(i,j-1)) + &
                        coefx(i,j)*ub(i,j) * var + &
                        coefy(i,j)*vb(i,j) * ( v(i,j+1) - v(i,j-1))
         end do
      end if

      !  [3.4] Left boundaries:

      if (jts == jds) then
         j = jts

         do i = is, ie
            varb = -3.0*vb(i,j)+4.0*vb(i,j+1)-vb(i,j+2)
            var  = -3.0*v (i,j)+4.0*v (i,j+1)-v (i,j+2)

            data(i,j) = coefx(i,j)*u(i,j) * ( vb(i+1,j) - vb(i-1,j)) + &
                        coefy(i,j)*v(i,j) * varb + &
                        coefx(i,j)*ub(i,j) * ( v(i+1,j) - v(i-1,j)) + &
                        coefy(i,j)*vb(i,j) * var
         end do
      end if

      !  [3.5] Right boundaries:

      if (jte == jde) then
         j = jte

         do i = is, ie
            varb = 3.0*vb(i,j)-4.0*vb(i,j-1)+vb(i,j-2)
            var  = 3.0*v (i,j)-4.0*v (i,j-1)+v (i,j-2)

            data(i,j) = coefx(i,j)*u(i,j) * ( vb(i+1,j) - vb(i-1,j)) + &
                        coefy(i,j)*v(i,j) * varb + &
                        coefx(i,j)*ub(i,j) * ( v(i+1,j) - v(i-1,j)) + &
                        coefy(i,j)*vb(i,j) * var
         end do
      end if

      !  [3.6] Corner points:

      if (its == ids .AND. jts == jds) then
         data(its,jts) = 0.5 * ( data(its,jts+1) + data(its+1,jts))
      end if

      if (ite == ide .AND. jts == jds) then
         data(ite,jts) = 0.5 * ( data(ite-1,jts) + data(ite,jts+1))
      end if

      if (its == ids .AND. jte == jde) then
         data(its,jde) = 0.5 * ( data(its,jde-1) + data(its+1,jde))
      end if

      if (ite == ide .AND. jte == jde) then 
         data(ite,jte) = 0.5 * ( data(ite-1,jte) + data(ite,jte-1))
      end if
   end if ! not global

   ! [3.7] Multiply by  rho and add to term_y

   term_y(its:ite,jts:jte) = rho(its:ite,jts:jte) * data(its:ite,jts:jte) + term_y(its:ite,jts:jte)

   if (trace_use) call da_trace_exit("da_balance_cycloterm_lin")

end subroutine da_balance_cycloterm_lin


