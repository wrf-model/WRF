subroutine da_get_innov_vector_pseudo(it, grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it          ! External iteration
   type(domain),     intent(in)    :: grid        ! Background structure 
   type(y_type),     intent(inout) :: ob          ! Observation structure.
   type(iv_type),    intent(inout) :: iv          ! O-B structure.

   integer :: n        ! Loop counter.

   real, allocatable :: model_u(:,:)
   real, allocatable :: model_v(:,:)
   real, allocatable :: model_q(:,:)
   real, allocatable :: model_p(:,:)
   real, allocatable :: model_t(:,:)

   real, allocatable :: model_qcw(:,:)
   real, allocatable :: model_qci(:,:)
   real, allocatable :: model_qrn(:,:)
   real, allocatable :: model_qsn(:,:)
   real, allocatable :: model_qgr(:,:)

   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_pseudo")

   allocate (model_u(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_v(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_q(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_p(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_t(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))

   allocate (model_qcw(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_qci(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_qrn(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_qsn(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (model_qgr(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))

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

   call da_interp_lin_3d (grid%xb%qcw, iv%info(pseudo), model_qcw)
   call da_interp_lin_3d (grid%xb%qci, iv%info(pseudo), model_qci)
   call da_interp_lin_3d (grid%xb%qrn, iv%info(pseudo), model_qrn)
   call da_interp_lin_3d (grid%xb%qsn, iv%info(pseudo), model_qsn)
   call da_interp_lin_3d (grid%xb%qgr, iv%info(pseudo), model_qgr)

   do n=iv%info(pseudo)%n1,iv%info(pseudo)%n2
      !---------------------------------------------------------------
      ! [3.0] Calculate observation O = B +(O-B):
      !---------------------------------------------------------------

      ! inv is from namelist for the first outer-loop
      select case(pseudo_var(1:1))
      case ('u', 'U')
         if ( it > 1 ) then
             iv % pseudo(n) % u % inv = ob%pseudo(n)%u - model_u(1,n)
         else
             ob % pseudo(n) % u = model_u(1,n) + iv % pseudo(n) % u % inv
         end if
      case ('v', 'V')
         if ( it > 1 ) then
             iv % pseudo(n) % v % inv = ob%pseudo(n)%v - model_v(1,n)
         else
             ob % pseudo(n) % v = model_v(1,n) + iv % pseudo(n) % v % inv
         end if
      case ('t', 'T')
         if ( it > 1 ) then
             iv % pseudo(n) % t % inv = ob%pseudo(n)%t - model_t(1,n)
         else
             ob % pseudo(n) % t = model_t(1,n) + iv % pseudo(n) % t % inv
         end if
      case ('p', 'P')
         if ( it > 1 ) then
             iv % pseudo(n) % p % inv = ob%pseudo(n)%p - model_p(1,n)
         else
             ob % pseudo(n) % p = model_p(1,n) + iv % pseudo(n) % p % inv
         end if
      case ('q', 'Q')
         if ( len_trim(adjustl(pseudo_var)) == 1 ) then
            if ( it > 1 ) then
                iv % pseudo(n) % q % inv = ob%pseudo(n)%q - model_q(1,n)
            else
                ob % pseudo(n) % q = model_q(1,n) + iv % pseudo(n) % q % inv
            end if
         end if
      end select

      select case(pseudo_var(1:3))
      case ('qcw', 'QCW')
         if ( it > 1 ) then
             iv % pseudo(n) % q % inv = ob%pseudo(n)%q - model_qcw(1,n)
         else
             ob % pseudo(n) % q = model_qcw(1,n) + iv % pseudo(n) % q % inv
         endif
      case ('qci', 'QCI')
         if ( it > 1 ) then
             iv % pseudo(n) % q % inv = ob%pseudo(n)%q - model_qci(1,n)
         else
             ob % pseudo(n) % q = model_qci(1,n) + iv % pseudo(n) % q % inv
         endif
      case ('qrn', 'QRN')
         if ( it > 1 ) then
             iv % pseudo(n) % q % inv = ob%pseudo(n)%q - model_qrn(1,n)
         else
             ob % pseudo(n) % q = model_qrn(1,n) + iv % pseudo(n) % q % inv
         endif
      case ('qsn', 'QSN')
         if ( it > 1 ) then
             iv % pseudo(n) % q % inv = ob%pseudo(n)%q - model_qsn(1,n)
         else
             ob % pseudo(n) % q = model_qsn(1,n) + iv % pseudo(n) % q % inv
         endif
      case ('qgr', 'QGR')
         if ( it > 1 ) then
             iv % pseudo(n) % q % inv = ob%pseudo(n)%q - model_qgr(1,n)
         else
             ob % pseudo(n) % q = model_qgr(1,n) + iv % pseudo(n) % q % inv
         endif
      end select

   end do

   deallocate (model_u)
   deallocate (model_v)
   deallocate (model_q)
   deallocate (model_p)
   deallocate (model_t)

   deallocate (model_qcw)
   deallocate (model_qci)
   deallocate (model_qrn)
   deallocate (model_qsn)
   deallocate (model_qgr)

   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_pseudo")

end subroutine da_get_innov_vector_pseudo


