
subroutine init_4dvar

   implicit none

   integer :: idum1, idum2, run_seconds ,rc
   TYPE(WRFU_TimeInterval) :: run_length
   integer :: itmp
   real    :: rtmp

   if (trace_use_dull) call da_trace_entry("init_4dvar")

   run_length = head_grid%stop_subtime - head_grid%start_subtime
   CALL WRFU_TimeIntervalGet( run_length, S=run_seconds, rc=rc )

   if ( run_seconds .gt. config_flags%interval_seconds ) then
      write(unit=message(1),fmt='(A)') 'In 4D-Var assimilation window, lateral boundary update is not allowed.'
      write(unit=message(2),fmt='(A)') '"run_seconds" must be less than or equal to "interval_seconds"'
      write(unit=message(3),fmt='(A,I0)')'run_seconds      = ',run_seconds
      write(unit=message(4),fmt='(A,I0)')'interval_seconds = ',config_flags%interval_seconds
      call da_error(__FILE__,__LINE__,message(1:4))
   end if

   ! Save the some physics options, as they will be changed in TL and AD model.
   call nl_get_mp_physics (head_grid%id, itmp) 
   original_mp_physics = itmp
   call nl_get_mp_physics_ad (head_grid%id, itmp)
   original_mp_physics_ad = itmp
   call nl_get_ra_lw_physics (head_grid%id, itmp)
   original_ra_lw_physics = itmp
   call nl_get_ra_sw_physics (head_grid%id, itmp)
   original_ra_sw_physics = itmp
   call nl_get_sf_sfclay_physics (head_grid%id, itmp)
   original_sf_sfclay_physics = itmp
   call nl_get_bl_pbl_physics (head_grid%id, itmp)
   original_bl_pbl_physics = itmp
   call nl_get_cu_physics (head_grid%id, itmp)
   original_cu_physics = itmp
   call nl_get_cudt (head_grid%id, rtmp)
   original_cudt = rtmp
   !call nl_get_mp_zero_out (head_grid%id, original_mp_zero_out)
   !call nl_get_sf_surface_physics (head_grid%id, original_sf_surface_physics)
   call nl_get_ifsnow (head_grid%id, itmp)
   original_ifsnow = itmp
   call nl_get_icloud (head_grid%id, itmp)
   original_icloud = itmp
   !call nl_get_isfflx (head_grid%id, original_isfflx)

   ! Initialize linked list for trajectory
   call xtraj_io_initialize

   ! Initialize linked list for adjoint forcing and tl. pertbation
   call adtl_initialize

   ! Initialize coefficiences for Jc DF
   if ( head_grid%jcdfi_use .OR. head_grid%jcdfi_diag == 1 ) call jcdfi_init_coef

   if (.not. associated(model_grid)) model_grid => head_grid
   model_config_flags = config_flags
   
   if (trace_use_dull) call da_trace_exit("init_4dvar")

end subroutine init_4dvar

subroutine clean_4dvar

   if (trace_use_dull) call da_trace_entry("clean_4dvar")

   ! Release linked list for trajectory
   call xtraj_io_initialize
   
   ! Release linked list for ad. forcing and tl. pertubation
   call adtl_initialize

   if (associated(model_grid)) nullify(model_grid)

   if (trace_use_dull) call da_trace_exit("clean_4dvar")

end subroutine clean_4dvar

subroutine input_nl_xtraj ( time )

   implicit none

   character*256, intent(in) :: time

   if (trace_use_dull) call da_trace_entry("input_nl_xtraj")

   call read_nl_xtraj ( time )

   if (trace_use_dull) call da_trace_entry("input_nl_xtraj")

end subroutine input_nl_xtraj

subroutine push_tl_pert (time )

   implicit none

   character*256, intent(in) :: time

   if (trace_use_dull) call da_trace_entry("push_tl_pert")

   call save_tl_pert ( time )

   if (trace_use_dull) call da_trace_entry("push_tl_pert")

end subroutine push_tl_pert

subroutine push_ad_forcing (time )

   implicit none
 
   character*256, intent(in) :: time

   if (trace_use_dull) call da_trace_entry("push_ad_forcing")

   call save_ad_forcing ( time )

   if (trace_use_dull) call da_trace_entry("push_ad_forcing")

end subroutine push_ad_forcing

subroutine pop_tl_pert (time )

   implicit none

   character*256, intent(in) :: time

   if (trace_use_dull) call da_trace_entry("pop_tl_pert")

   call read_tl_pert ( time )

   if (trace_use_dull) call da_trace_entry("pop_tl_pert")

end subroutine pop_tl_pert

subroutine pop_ad_forcing (time )

   implicit none

   character*256, intent(in) :: time

   if (trace_use_dull) call da_trace_entry("pop_ad_forcing")

   call read_ad_forcing ( time )

   if (trace_use_dull) call da_trace_entry("pop_ad_forcing")

end subroutine pop_ad_forcing

subroutine kj_swap (source, target, is, ie, js, je, ks, ke)

   implicit none

   integer, intent(in) :: is, ie, js, je, ks, ke
   real, dimension(is:ie,js:je,ks:ke), intent(in) :: source
   real, dimension(is:ie,ks:ke,js:je), intent(out) :: target

   integer :: i, j, k

   if (trace_use_dull) call da_trace_entry("kj_swap")

   do j = js, je
      do k = ks, ke
         do i = is, ie
            target(i,k,j) = source(i,j,k)
         enddo
      enddo
   enddo   

   if (trace_use_dull) call da_trace_entry("kj_swap")

end subroutine kj_swap

subroutine kj_swap_reverse (source, target, is, ie, js, je, ks, ke)

   implicit none

   integer, intent(in) :: is, ie, js, je, ks, ke
   real, dimension(is:ie,js:je,ks:ke), intent(out) :: target
   real, dimension(is:ie,ks:ke,js:je), intent(in) :: source

   integer :: i, j, k

   if (trace_use_dull) call da_trace_entry("kj_swap_reverse")

   do k = ks, ke
      do j = js, je
         do i = is, ie
            target(i,j,k) = source(i,k,j)
         enddo
      enddo
   enddo

   if (trace_use_dull) call da_trace_entry("kj_swap_reverse")

end subroutine kj_swap_reverse

subroutine upsidedown_ad_forcing

   implicit none

   integer :: nobwin

   if (trace_use_dull) call da_trace_entry("upsidedown_ad_forcing")
   
   ! In this method, we will swap linked list node objects (references to the data). 
   ! Swapping starts from the first node object and the first node object is swapped 
   ! with the last node object. Then, the second node object is swapped with the one 
   ! before the last nodes object.

   call swap_ad_forcing ( num_fgat_time/2 )

   if (trace_use_dull) call da_trace_entry("upsidedown_ad_forcing")

end subroutine upsidedown_ad_forcing
