subroutine da_get_innov_vector_pseudo(grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none
   
   type(domain),     intent(in)    :: grid        ! Background structure 
   type(y_type),     intent(inout) :: ob          ! Observation structure.
   type(iv_type),    intent(inout) :: iv          ! O-B structure.

   integer :: n        ! Loop counter.

   real, allocatable :: model_u(:,:)
   real, allocatable :: model_v(:,:)
   real, allocatable :: model_q(:,:)
   real, allocatable :: model_p(:,:)
   real, allocatable :: model_t(:,:)
   
   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_pseudo")


   allocate (model_u(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_v(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_q(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_p(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_t(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))

   call da_convert_zk (iv%info(pseudo))

#ifdef A2C
   call da_interp_lin_3d (grid%xb%u, iv%info(pseudo), model_u,'u')
   call da_interp_lin_3d (grid%xb%v, iv%info(pseudo), model_v,'v')
#else
   call da_interp_lin_3d (grid%xb%u, iv%info(pseudo), model_u)
   call da_interp_lin_3d (grid%xb%v, iv%info(pseudo), model_v)
#endif
   call da_interp_lin_3d (grid%xb%t, iv%info(pseudo), model_t)
   call da_interp_lin_3d (grid%xb%p, iv%info(pseudo), model_p)
   call da_interp_lin_3d (grid%xb%q, iv%info(pseudo), model_q)

   do n=iv%info(pseudo)%n1,iv%info(pseudo)%n2
      !---------------------------------------------------------------
      ! [3.0] Calculate observation O = B +(O-B):
      !---------------------------------------------------------------

      if (pseudo_var(1:1) == 'u' .or.  pseudo_var(1:1) == 'U') then
         ob % pseudo(n) % u = model_u(1,n) + iv % pseudo(n) % u % inv
      else if (pseudo_var(1:1) == 'v' .or.  pseudo_var(1:1) == 'V') then
         ob % pseudo(n) % v = model_v(1,n) + iv % pseudo(n) % v % inv
      else if (pseudo_var(1:1) == 't' .or.  pseudo_var(1:1) == 'T') then
         ob % pseudo(n) % t = model_t(1,n) + iv % pseudo(n) % t % inv
      else if (pseudo_var(1:1) == 'p' .or.  pseudo_var(1:1) == 'P') then
         ob % pseudo(n) % p = model_p(1,n) + iv % pseudo(n) % p % inv
      else if (pseudo_var(1:1) == 'q' .or.  pseudo_var(1:1) == 'Q') then
         ob % pseudo(n) % q = model_q(1,n) + iv % pseudo(n) % q % inv
      end if 
   end do

   deallocate (model_u)
   deallocate (model_v)
   deallocate (model_q)
   deallocate (model_p)
   deallocate (model_t)
   
   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_pseudo")

end subroutine da_get_innov_vector_pseudo


