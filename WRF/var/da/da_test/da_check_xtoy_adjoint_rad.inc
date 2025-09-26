subroutine da_check_xtoy_adjoint_rad(iv, y, adjtest_lhs, pertile_lhs)

   !------------------------------------------------------------------------------
   ! Purpose: Calculate innovation vector for radiance data.
   !------------------------------------------------------------------------------

   implicit none

   type(iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type(y_type),  intent(inout)  :: y             ! y = h (xa)
   real,          intent(inout)  :: adjtest_lhs, pertile_lhs   

   integer :: inst, n, k          ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_rad")

   do inst = 1, iv%num_inst                 ! loop for sensor
      if (iv%instid(inst)%num_rad < 1) cycle

      do n= 1, iv%instid(inst)%num_rad       ! loop for pixel
         ! if (iv%instid(inst)%rad(n)%loc%proc_domain_with_halo) then
         if (iv%instid(inst)%info%proc_domain(1,n)) then
            do k = 1, iv%instid(inst)%nchan
               ! if ( iv%instid(inst)%tb_qc(k,n) >= obs_qc_pointer ) &
               adjtest_lhs = adjtest_lhs + &
                 ( y%instid(inst)%tb(k,n)/iv%instid(inst)%tb_error(k,n) )**2
            end do
         end if

         do k=1, iv%instid(inst)%nchan
            ! if ( iv%instid(inst)%tb_qc(k,n) >= obs_qc_pointer ) &
            pertile_lhs = pertile_lhs + &
             ( y%instid(inst)%tb(k,n)/iv%instid(inst)%tb_error(k,n) )**2

            y%instid(inst)%tb(k,n) =  &
              y%instid(inst)%tb(k,n) / (iv%instid(inst)%tb_error(k,n))**2
         end do
         ! end if
      end do
   end do 

   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_rad")

end subroutine da_check_xtoy_adjoint_rad


