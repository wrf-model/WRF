subroutine da_transform_xtogpsref_lin(grid)

   !---------------------------------------------------------------------
   ! Purpose: calculate gpsref increment from t, p, q increments
   !---------------------------------------------------------------------

   implicit none

   type (domain), intent(inout) :: grid
   
   integer :: i, j, k, ijk
   real    :: partone, parttwo, dividnd
   real    :: partone9,parttwo9,dividnd9

   if (trace_use_dull) call da_trace_entry("da_transform_xtogpsref_lin")

   ! 2.0 Loop for a tile

   do k = kts, kte
      do j = jts, jte
         do i = its, ite
            !-----calculate refractivity
            !     Note: p in Pascal, q its the specific humidity

            ! 2.1 Part one: hydrostatic part

! Do not consider the pressure perturbation (YRG, 10/08/2009):
!            partone  = 0.776*grid%xa%p(i,j,k)/grid%xb%t(i,j,k) &

            partone  = &
               - 0.776*grid%xb%p(i,j,k)*grid%xa%t (i,j,k)/ &
               (grid%xb%t(i,j,k)*grid%xb%t(i,j,k))

            partone9 = 0.776*grid%xb%p(i,j,k)/grid%xb%t(i,j,k)

            ! 2.2 Part two considering the moitsture:

            dividnd  = grid%xa%t (i,j,k)*(0.622+0.378*grid%xb%q(i,j,k)) &
                     + grid%xb%t(i,j,k)*       0.378*grid%xa%q(i,j,k)
            dividnd9 = grid%xb%t(i,j,k)*(0.622+0.378*grid%xb%q(i,j,k))
            parttwo  =     coeff*grid%xa%q(i,j,k)/dividnd9 &
                     -     coeff*grid%xb%q(i,j,k)*dividnd /(dividnd9*dividnd9)
            parttwo9 = 1.0+coeff*grid%xb%q(i,j,k)/dividnd9

            ! 2.3 Total refractivity

            grid%xa%ref(i,j,k)= partone9 * parttwo + partone  * parttwo9
         end do
      end do
   end do

   if ( use_gpsephobs ) then
#ifdef DM_PARALLEL
      ijk = (ide-ids+1)*(jde-jds+1)*(kde-kds+1)
   !  Collect xb component of ref into global buffer.
      call da_patch_to_global( grid, grid%xa%ref, global_xa_ref )
      call wrf_dm_bcast_real( global_xa_ref, ijk )
#else
      do k = kds, kde
         do j = jds, jde
            do i = ids, ide
               global_xa_ref(i,j,k) = grid%xa%ref(i,j,k)
            enddo
         enddo
      enddo
#endif
   endif

   if (trace_use_dull) call da_trace_exit("da_transform_xtogpsref_lin")

end subroutine da_transform_xtogpsref_lin


