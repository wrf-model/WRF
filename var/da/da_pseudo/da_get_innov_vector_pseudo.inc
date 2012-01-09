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
#ifdef CLOUD_CV
   real, allocatable :: model_qcw(:,:)
   real, allocatable :: model_qci(:,:)
   real, allocatable :: model_qrn(:,:)
   real, allocatable :: model_qsn(:,:)
#endif   
   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_pseudo")


   allocate (model_u(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_v(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_q(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_p(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_t(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
#ifdef CLOUD_CV
   allocate (model_qcw(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_qci(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_qrn(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_qsn(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
#endif

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
#ifdef CLOUD_CV
   call da_interp_lin_3d (grid%xb%qcw, iv%info(pseudo), model_qcw)
   call da_interp_lin_3d (grid%xb%qci, iv%info(pseudo), model_qci)
   call da_interp_lin_3d (grid%xb%qrn, iv%info(pseudo), model_qrn)
   call da_interp_lin_3d (grid%xb%qsn, iv%info(pseudo), model_qsn)
#endif
   do n=iv%info(pseudo)%n1,iv%info(pseudo)%n2
      !---------------------------------------------------------------
      ! [3.0] Calculate observation O = B +(O-B):
      !---------------------------------------------------------------

      select case(pseudo_var(1:1))
      case ('u', 'U')
         if (ob % pseudo(n) % u > missing_r) then
             iv % pseudo(n) % u % inv = ob%pseudo(n)%u - model_u(1,n)
         else
             ob % pseudo(n) % u = model_u(1,n) + iv % pseudo(n) % u % inv
	 endif   
      case ('v', 'V')
         if (ob % pseudo(n) % v > missing_r) then
             iv % pseudo(n) % v % inv = ob%pseudo(n)%v - model_v(1,n)
         else
             ob % pseudo(n) % v = model_v(1,n) + iv % pseudo(n) % v % inv
	 endif   
      case ('t', 'T')
         if (ob % pseudo(n) % t > missing_r) then
             iv % pseudo(n) % t % inv = ob%pseudo(n)%t - model_t(1,n)
         else
             ob % pseudo(n) % t = model_t(1,n) + iv % pseudo(n) % t % inv
	 endif   
      case ('p', 'P')
         if (ob % pseudo(n) % p > missing_r) then
             iv % pseudo(n) % p % inv = ob%pseudo(n)%p - model_p(1,n)
         else
             ob % pseudo(n) % p = model_p(1,n) + iv % pseudo(n) % p % inv
	 endif   
      case ('q', 'Q')
         if (ob % pseudo(n) % q > missing_r) then
             iv % pseudo(n) % q % inv = ob%pseudo(n)%q - model_q(1,n)
         else
             ob % pseudo(n) % q = model_q(1,n) + iv % pseudo(n) % q % inv
	 endif   
      end select 

#ifdef CLOUD_CV
      select case(pseudo_var(1:3))
      case ('qcw', 'QCW')
! here it is assumed that the inv is obtained from namelist
! set  ob % pseudo(n) % q to missing, which may be assigned a value by case ('q')
         ob % pseudo(n) % q = missing_r
         if (ob % pseudo(n) % q > missing_r) then
             iv % pseudo(n) % q % inv = ob%pseudo(n)%q - model_qcw(1,n)
         else
             ob % pseudo(n) % q = model_qcw(1,n) + iv % pseudo(n) % q % inv
         endif
      case ('qci', 'QCI')
         ob % pseudo(n) % q = missing_r
         if (ob % pseudo(n) % q > missing_r) then
             iv % pseudo(n) % q % inv = ob%pseudo(n)%q - model_qci(1,n)
         else
             ob % pseudo(n) % q = model_qci(1,n) + iv % pseudo(n) % q % inv
         endif
      case ('qrn', 'QRN')
         ob % pseudo(n) % q = missing_r
         if (ob % pseudo(n) % q > missing_r) then
             iv % pseudo(n) % q % inv = ob%pseudo(n)%q - model_qrn(1,n)
         else
             ob % pseudo(n) % q = model_qrn(1,n) + iv % pseudo(n) % q % inv
         endif
      case ('qsn', 'QSN')
         ob % pseudo(n) % q = missing_r
         if (ob % pseudo(n) % q > missing_r) then
             iv % pseudo(n) % q % inv = ob%pseudo(n)%q - model_qsn(1,n)
         else
             ob % pseudo(n) % q = model_qsn(1,n) + iv % pseudo(n) % q % inv
         endif
      end select
#endif
   end do

   deallocate (model_u)
   deallocate (model_v)
   deallocate (model_q)
   deallocate (model_p)
   deallocate (model_t)
#ifdef CLOUD_CV
   deallocate (model_qcw)
   deallocate (model_qci)
   deallocate (model_qrn)
   deallocate (model_qsn)
#endif   
   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_pseudo")

end subroutine da_get_innov_vector_pseudo


