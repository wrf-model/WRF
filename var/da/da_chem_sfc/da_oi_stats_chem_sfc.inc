subroutine da_oi_stats_chem_sfc (stats_unit, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent (in)    :: stats_unit    ! Output unit for stats.
   type (iv_type), intent (inout) :: iv            ! iv

   type (stats_chem_sfc_type) :: stats
   integer                 :: nchem
   integer                 :: n, ichem
   real                    :: chem_inc, v_inc

   if (trace_use_dull) call da_trace_entry("da_oi_stats_chem_sfc")


   allocate(stats%maximum%chem(num_chemic_surf))
   allocate(stats%minimum%chem(num_chemic_surf))
   allocate(stats%average%chem(num_chemic_surf))
   allocate(stats%rms_err%chem(num_chemic_surf))

   do ichem=PARAM_FIRST_SCALAR, num_chemic_surf
    stats%maximum%chem(ichem) = maxmin_type (0.0, 0, 0)
    stats%minimum%chem(ichem) = maxmin_type (0.0, 0, 0)
    stats%average%chem(ichem) = 0.0
    stats%rms_err%chem(ichem) = 0.0
   end do

   do ichem=PARAM_FIRST_SCALAR, num_chemic_surf
   nchem=0

!   stats%maximum%chem(ichem) = maxmin_type (missing_r, 0, 0)
!   stats%minimum%chem(ichem) = maxmin_type (missing_r, 0, 0)
!   stats%average = residual_chem_sfc_type(0.0, 0.0, 0.0, 0.0, 0.0)
!   stats%rms_err = stats%average

   do n=1, iv%info(chemic_surf)%nlocal
      if (iv%info(chemic_surf)%proc_domain(1,n)) then

         call da_stats_calculate(iv%info(chemic_surf)%obs_global_index(n), &
            0, iv%chemic_surf(n)%chem(ichem)%qc, &
            iv%chemic_surf(n)%chem(ichem)%inv, nchem, &
            stats%minimum%chem(ichem), stats%maximum%chem(ichem), &
            stats%average%chem(ichem), stats%rms_err%chem(ichem))

      end if 
   end do

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int (nchem)

   call da_proc_stats_combine(stats%average%chem(ichem), stats%rms_err%chem(ichem), &
      stats%minimum%chem(ichem)%value, stats%maximum%chem(ichem)%value, &
      stats%minimum%chem(ichem)%n, stats%maximum%chem(ichem)%n, &
      stats%minimum%chem(ichem)%l, stats%maximum%chem(ichem)%l)

   if (rootproc) then
      if (nchem /= 0 ) then
         write(unit=stats_unit, fmt='(/a/)') ' Diagnostics of OI for chem'
         if (chemicda_opt == 1 .or. chemicda_opt == 2) then
           call da_print_stats_chem_sfc(stats_unit, nchem, stats)
         else if (chemicda_opt == 3 .or. chemicda_opt == 4 .or. chemicda_opt == 5) then
             call da_print_stats_chem_sfc(stats_unit, nchem, stats, ichem)   
         end if
      end if
   end if

   end do
    deallocate(stats%maximum%chem)
    deallocate(stats%minimum%chem)
    deallocate(stats%average%chem)
    deallocate(stats%rms_err%chem)                                                                                               

   if (trace_use_dull) call da_trace_exit("da_oi_stats_chem_sfc")

end subroutine da_oi_stats_chem_sfc


