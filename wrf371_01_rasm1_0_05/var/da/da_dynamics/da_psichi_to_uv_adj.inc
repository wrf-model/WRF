subroutine da_psichi_to_uv_adj(u, v, coefx, coefy, psi, chi)

   !---------------------------------------------------------------------------
   !  Purpose: Adjoint code of da_psichi_to_uv  
   !
   !  Method:  u = coefx * (-dpsi/dy + dchi/dx)
   !           v = coefy * ( dpsi/dx + dchi/dy)
   !
   !  Assumptions: Unstaggered grid.
   !               Lateral boundary conditions - dpsi/dn, dchi/dn = 0 (FCT)
   !               grid size may or may not be equal
   ! 
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !---------------------------------------------------------------------------

   implicit none
   
   real, intent(inout) :: u(ims:ime,jms:jme,kms:kme)   ! u wind comp.
   real, intent(inout) :: v(ims:ime,jms:jme,kms:kme)   ! v wind comp.
   real, intent(inout) :: psi(ims:ime,jms:jme,kms:kme) ! Stream function
   real, intent(inout) :: chi(ims:ime,jms:jme,kms:kme) ! Velocity potential
   real, intent(in)    :: coefx(ims:ime,jms:jme)       ! Multiplicative coeff.
   real, intent(in)    :: coefy(ims:ime,jms:jme)       ! Multiplicative coeff.
   
   integer :: i, j, k                      ! Loop counters.
   integer :: is, ie                       ! 1st dim. end points.
   integer :: js, je                       ! 2nd dim. end points.
#ifdef A2C
   real               :: psi1, psi2, chi1, chi2
#endif

   if (trace_use) call da_trace_entry("da_psichi_to_uv_adj")

   !---------------------------------------------------------------------------
   ! [5.0] For Global application, set Wast-Eest Periodic boundary
   !---------------------------------------------------------------------------

#ifdef A2C
   if (fg_format == fg_format_kma_global) then
#else
   if (global) then
#endif
      call da_set_boundary_3d(u) 
      call da_set_boundary_3d(v) 
   end if

   !---------------------------------------------------------------------------
   ! Computation to check for edge of domain:
   !---------------------------------------------------------------------------

   is = its-1
   ie = ite+1
   js = jts-1
   je = jte+1
   if (jts == jds) js = jds+1
   if (jte == jde) je = jde-1

#ifdef A2C
   if (.not. (fg_format == fg_format_kma_global) ) then
#else
   if (.not. global) then
#endif
      if (its == ids) is = ids+1
      if (ite == ide) ie = ide-1

      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( k ) 
      do k = kts, kte

         !---------------------------------------------------------------------
         ! [4.0] Corner points (assume average of surrounding points - poor?):
         !---------------------------------------------------------------------

         ! [4.1] Bottom-left point:

         if (its == ids .AND. jts == jds) then
            u(its+1,jts,k) = u(its+1,jts,k) + 0.5 * u(its,jts,k)
            u(its,jts+1,k) = u(its,jts+1,k) + 0.5 * u(its,jts,k)
            v(its+1,jts,k) = v(its+1,jts,k) + 0.5 * v(its,jts,k)
            v(its,jts+1,k) = v(its,jts+1,k) + 0.5 * v(its,jts,k)
         end if

        ! [4.2] Top-left point:

#ifdef A2C
         if (its == ids .AND. jte == jde) then
            u(its+1,jte,k) = u(its+1,jte,k) + 0.5 * u(its,jte,k)
            u(its,jte-1,k) = u(its,jte-1,k) + 0.5 * u(its,jte,k)
            v(its+1,jte,k) = v(its+1,jte,k) + 0.5 * v(its,jte,k)
            v(its,jte-1,k) = v(its,jte-1,k) + 0.5 * v(its,jte,k)
         end if
#else
         if (ite == ide .AND. jts == jds) then
            u(ite-1,jts,k) = u(ite-1,jts,k) + 0.5 * u(ite,jts,k)
            u(ite,jts+1,k) = u(ite,jts+1,k) + 0.5 * u(ite,jts,k)
            v(ite-1,jts,k) = v(ite-1,jts,k) + 0.5 * v(ite,jts,k)
            v(ite,jts+1,k) = v(ite,jts+1,k) + 0.5 * v(ite,jts,k)
         end if
#endif

         ! [4.3] Bottom-right point:

#ifdef A2C
         if (ite == ide .AND. jts == jds) then
            u(ite-1,jts,k) = u(ite-1,jts,k) + 0.5 * u(ite,jts,k)
            u(ite,jts+1,k) = u(ite,jts+1,k) + 0.5 * u(ite,jts,k)
            v(ite-1,jts,k) = v(ite-1,jts,k) + 0.5 * v(ite,jts,k)
            v(ite,jts+1,k) = v(ite,jts+1,k) + 0.5 * v(ite,jts,k)
         end if

#else
         if (its == ids .AND. jte == jde) then
            u(its+1,jte,k) = u(its+1,jte,k) + 0.5 * u(its,jte,k)
            u(its,jte-1,k) = u(its,jte-1,k) + 0.5 * u(its,jte,k)
            v(its+1,jte,k) = v(its+1,jte,k) + 0.5 * v(its,jte,k)
            v(its,jte-1,k) = v(its,jte-1,k) + 0.5 * v(its,jte,k)
         end if
#endif
         ! [4.4] Top-right point:

         if (ite == ide .AND. jte == jde) then
            u(ite-1,jte,k) = u(ite-1,jte,k) + 0.5 * u(ite,jte,k)
            u(ite,jte-1,k) = u(ite,jte-1,k) + 0.5 * u(ite,jte,k)
            v(ite-1,jte,k) = v(ite-1,jte,k) + 0.5 * v(ite,jte,k)
            v(ite,jte-1,k) = v(ite,jte-1,k) + 0.5 * v(ite,jte,k)
         end if
      end do
      !$OMP END PARALLEL DO
   end if


   ! [3.0] Compute u, v at domain boundaries:

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( i, j, k, message ) 
   do k = kts, kte
#ifdef A2C
   if (.not. (fg_format == fg_format_kma_global) ) then
#else
   if (.not. global) then
#endif
         ! [3.4] Northern boundaries:

         if (jte == jde) then
            j = jte

            do i = ie, is, -1
#ifdef A2C
              if ((fg_format==fg_format_wrf_arw_regional) .or. &
                  (fg_format==fg_format_wrf_arw_global  ) ) then
             u(i,j-1,k) = u(i,j-1,k) + 2.0*u(i,j,k)   
             v(i,j-1,k) = v(i,j-1,k) + 2.0*v(i,j,k)   
             u(i,j-2,k) = u(i,j-2,k) -     u(i,j,k)   
             v(i,j-2,k) = v(i,j-2,k) -     v(i,j,k)   

             else if (fg_format == fg_format_kma_global) then
#endif
               psi(i+1,j,k) = psi(i+1,j,k) + coefx(i,j) * v(i,j,k)
               psi(i-1,j,k) = psi(i-1,j,k) - coefx(i,j) * v(i,j,k)
               chi(i,j  ,k) = chi(i,j  ,k) + coefy(i,j) * v(i,j,k)
               chi(i,j-2,k) = chi(i,j-2,k) - coefy(i,j) * v(i,j,k)

               psi(i,j  ,k) = psi(i,j  ,k) - coefy(i,j) * u(i,j,k)
               psi(i,j-2,k) = psi(i,j-2,k) + coefy(i,j) * u(i,j,k)
               chi(i+1,j,k) = chi(i+1,j,k) + coefx(i,j) * u(i,j,k)
               chi(i-1,j,k) = chi(i-1,j,k) - coefx(i,j) * u(i,j,k)
#ifdef A2C
             else
                 write(unit=message(1),fmt='(A,I5)') &
                       "Wrong choice for fg_format = ",fg_format
                 call da_error(__FILE__,__LINE__,message(1:1))
             end if
#endif
            end do
         end if
         ! [3.3] Southern boundaries:

         if (jts == jds) then
            j = jts

            do i = ie, is, -1

#ifdef A2C
             if ((fg_format==fg_format_wrf_arw_regional) .or. &
                 (fg_format==fg_format_wrf_arw_global  ) ) then
             u(i,j+1,k) = u(i,j+1,k) + 2.0*u(i,j,k) 
             v(i,j+1,k) = v(i,j+1,k) + 2.0*v(i,j,k) 
             u(i,j+2,k) = u(i,j+2,k) -     u(i,j,k) 
             v(i,j+2,k) = v(i,j+2,k) -     v(i,j,k) 
        
             else if (fg_format == fg_format_kma_global) then
#endif

               psi(i+1,j,k) = psi(i+1,j,k) + coefx(i,j) * v(i,j,k)
               psi(i-1,j,k) = psi(i-1,j,k) - coefx(i,j) * v(i,j,k)
               chi(i,j+2,k) = chi(i,j+2,k) + coefy(i,j) * v(i,j,k)
               chi(i,j  ,k) = chi(i,j  ,k) - coefy(i,j) * v(i,j,k)

               psi(i,j+2,k) = psi(i,j+2,k) - coefy(i,j) * u(i,j,k)
               psi(i,j  ,k) = psi(i,j  ,k) + coefy(i,j) * u(i,j,k)
               chi(i+1,j,k) = chi(i+1,j,k) + coefx(i,j) * u(i,j,k)
               chi(i-1,j,k) = chi(i-1,j,k) - coefx(i,j) * u(i,j,k)
#ifdef A2C
             else
               write(unit=message(1),fmt='(A,I5)') &
                     "Wrong choice for fg_format = ",fg_format
               call da_error(__FILE__,__LINE__,message(1:1))
             end if
#endif
            end do
         end if

         ! [3.2] Eastern boundaries:

         if (ite == ide) then
            i = ite
            do j = je, js, -1
#ifdef A2C
             if ((fg_format==fg_format_wrf_arw_regional) .or. &
                 (fg_format==fg_format_wrf_arw_global  ) ) then
             u(i-1,j,k) = u(i-1,j,k) + 2.0*u(i,j,k) 
             v(i-1,j,k) = v(i-1,j,k) + 2.0*v(i,j,k) 
             u(i-2,j,k) = u(i-2,j,k) -     u(i,j,k) 
             v(i-2,j,k) = v(i-2,j,k) -     v(i,j,k) 
             else if (fg_format == fg_format_kma_global) then
#endif
               psi(i  ,j,k) = psi(i  ,j,k) + coefx(i,j) * v(i,j,k)
               psi(i-2,j,k) = psi(i-2,j,k) - coefx(i,j) * v(i,j,k)
               chi(i,j+1,k) = chi(i,j+1,k) + coefy(i,j) * v(i,j,k)
               chi(i,j-1,k) = chi(i,j-1,k) - coefy(i,j) * v(i,j,k)

               psi(i,j+1,k) = psi(i,j+1,k) - coefy(i,j) * u(i,j,k)
               psi(i,j-1,k) = psi(i,j-1,k) + coefy(i,j) * u(i,j,k)
               chi(i  ,j,k) = chi(i  ,j,k) + coefx(i,j) * u(i,j,k)
               chi(i-2,j,k) = chi(i-2,j,k) - coefx(i,j) * u(i,j,k)
#ifdef A2C
             else
               write(unit=message(1),fmt='(A,I5)') &
                     "Wrong choice for fg_format = ",fg_format
               call da_error(__FILE__,__LINE__,message(1:1))
             end if
#endif
            end do
         end if

         ! [3.1] Western boundaries:
         if (its == ids) then
            i = its

            do j = je, js, -1
#ifdef A2C
             if ((fg_format==fg_format_wrf_arw_regional) .or. &
                 (fg_format==fg_format_wrf_arw_global  ) ) then
             u(i+1,j,k) = u(i+1,j,k) + 2.0*u(i,j,k) 
             v(i+1,j,k) = v(i+1,j,k) + 2.0*v(i,j,k) 
             u(i+2,j,k) = u(i+2,j,k) -     u(i,j,k) 
             v(i+2,j,k) = v(i+2,j,k) -     v(i,j,k) 
             else if (fg_format == fg_format_kma_global) then
#endif
               psi(i+2,j,k) = psi(i+2,j,k) + coefx(i,j) * v(i,j,k)
               psi(i  ,j,k) = psi(i  ,j,k) - coefx(i,j) * v(i,j,k)
               chi(i,j+1,k) = chi(i,j+1,k) + coefy(i,j) * v(i,j,k)
               chi(i,j-1,k) = chi(i,j-1,k) - coefy(i,j) * v(i,j,k)

               psi(i,j+1,k) = psi(i,j+1,k) - coefy(i,j) * u(i,j,k)
               psi(i,j-1,k) = psi(i,j-1,k) + coefy(i,j) * u(i,j,k)
               chi(i+2,j,k) = chi(i+2,j,k) + coefx(i,j) * u(i,j,k)
               chi(i,  j,k) = chi(i,  j,k) - coefx(i,j) * u(i,j,k)
#ifdef A2C
             else
                 write(unit=message(1),fmt='(A,I5)') &
                       "Wrong choice for fg_format = ",fg_format
                 call da_error(__FILE__,__LINE__,message(1:1))
             end if
#endif
            end do
         end if
   end if   

      !------------------------------------------------------------------------
      ! [2.0] Compute u, v at interior points (2nd order central finite diffs):
      !------------------------------------------------------------------------
      do j = je, js, -1
         do i = ie, is, -1
#ifdef A2C
            if ((fg_format==fg_format_wrf_arw_regional) .or. &
                (fg_format==fg_format_wrf_arw_global  ) ) then

            psi1 = coefx(i,j)*v(i,j,k) 
            psi2 =-coefx(i,j)*v(i,j,k) 
            chi1 = coefy(i,j)*v(i,j,k)
            chi(i,j,k)   = chi(i,j,k) + 2.0*chi1 
            chi(i,j-1,k) = chi(i,j-1,k) - 2.0*chi1 
            psi(i-1,j-1,k) = psi(i-1,j-1,k) +0.5*psi2 
            psi(i-1,j  ,k) = psi(i-1,j  ,k) +0.5*psi2 
            psi(i+1,j-1,k) = psi(i+1,j-1,k) + 0.5*psi1 
            psi(i+1,j  ,k) = psi(i+1,j  ,k) + 0.5*psi1 

            psi1 = -coefy(i,j)*u(i,j,k)
            psi2 =  coefy(i,j)*u(i,j,k)
            chi1 =  coefx(i,j)*u(i,j,k)
            chi(i,j,k)    = chi(i,j,k)    + 2.0*chi1 
            chi(i-1,j,k)  = chi(i-1,j,k)  - 2.0*chi1 
            psi(i,j-1,k)  = psi(i,j-1,k)  + 0.5*psi2 
            psi(i-1,j-1,k)= psi(i-1,j-1,k)+ 0.5*psi2 
            psi(i,j+1,k)  = psi(i,j+1,k)  + 0.5*psi1 
            psi(i-1,j+1,k)= psi(i-1,j+1,k)+ 0.5*psi1 
          else if (fg_format == fg_format_kma_global) then
#endif
            psi(i+1,j,k) = psi(i+1,j,k) + coefx(i,j) * v(i,j,k)
            psi(i-1,j,k) = psi(i-1,j,k) - coefx(i,j) * v(i,j,k)
            chi(i,j+1,k) = chi(i,j+1,k) + coefy(i,j) * v(i,j,k)
            chi(i,j-1,k) = chi(i,j-1,k) - coefy(i,j) * v(i,j,k)

            psi(i,j+1,k) = psi(i,j+1,k) - coefy(i,j) * u(i,j,k)
            psi(i,j-1,k) = psi(i,j-1,k) + coefy(i,j) * u(i,j,k)
            chi(i+1,j,k) = chi(i+1,j,k) + coefx(i,j) * u(i,j,k)
            chi(i-1,j,k) = chi(i-1,j,k) - coefx(i,j) * u(i,j,k)
#ifdef A2C
          else
              write(unit=message(1),fmt='(A,I5)') &
                    "Wrong choice for fg_format = ",fg_format
              call da_error(__FILE__,__LINE__,message(1:1))
          end if
#endif
         end do
      end do
   end do    !  loop over levels
   !$OMP END PARALLEL DO

   !---------------------------------------------------------------------------
   ! [5.0] For Global application, set Wast-Eest Periodic boundary
   !---------------------------------------------------------------------------

#ifdef A2C
   if( fg_format == fg_format_kma_global ) then
#else
   if (global) then
#endif
       call da_set_boundary_3d(psi)
       call da_set_boundary_3d(chi)
   end if

   if (trace_use) call da_trace_exit("da_psichi_to_uv_adj")

end subroutine da_psichi_to_uv_adj
