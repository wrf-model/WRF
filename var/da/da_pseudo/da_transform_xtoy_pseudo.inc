subroutine da_transform_xtoy_pseudo(grid, iv, y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   type (domain),  intent(inout) :: grid
   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   integer :: n        ! Loop counter.

   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)
   real, allocatable :: q(:,:)
   real, allocatable :: p(:,:)
   real, allocatable :: t(:,:)
!  working array for 5 hydrometeor variables
   real, allocatable :: qw(:,:)

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_pseudo")

   allocate (u(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (v(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (q(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (p(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (t(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))
   allocate (qw(1,iv%info(pseudo)%n1:iv%info(pseudo)%n2))

#ifdef A2C
   call da_interp_lin_3d(grid%xa%u, iv%info(pseudo), u,'u')
   call da_interp_lin_3d(grid%xa%v, iv%info(pseudo), v,'v')
#else
   call da_interp_lin_3d(grid%xa%u, iv%info(pseudo), u)
   call da_interp_lin_3d(grid%xa%v, iv%info(pseudo), v)
#endif
   call da_interp_lin_3d(grid%xa%q, iv%info(pseudo), q)
   call da_interp_lin_3d(grid%xa%p, iv%info(pseudo), p)
   call da_interp_lin_3d(grid%xa%t, iv%info(pseudo), t)

   select case(pseudo_var(1:3))
   case ('qcw', 'QCW')
        call da_interp_lin_3d(grid%xa%qcw, iv%info(pseudo), qw)
   case ('qci', 'QCI')
        call da_interp_lin_3d(grid%xa%qci, iv%info(pseudo), qw)
   case ('qrn', 'QRN')
        call da_interp_lin_3d(grid%xa%qrn, iv%info(pseudo), qw)
   case ('qsn', 'QSN')
        call da_interp_lin_3d(grid%xa%qsn, iv%info(pseudo), qw)
   case ('qgr', 'QGR')
        call da_interp_lin_3d(grid%xa%qgr, iv%info(pseudo), qw)
   end select

   do n=iv%info(pseudo)%n1,iv%info(pseudo)%n2
      y%pseudo(n)%u = u(1,n)
      y%pseudo(n)%v = v(1,n)
      y%pseudo(n)%p = p(1,n)
      y%pseudo(n)%t = t(1,n)
      if ( len_trim(adjustl(pseudo_var)) == 1 ) then
         y%pseudo(n)%q = q(1,n)
      else
         select case(pseudo_var(1:3))
         case ('qcw', 'QCW','qci', 'QCI','qrn', 'QRN','qsn', 'QSN', 'qgr', 'QGR')
            y%pseudo(n)%q = qw(1,n)
         end select
      end if
   end do

   deallocate (u)
   deallocate (v)
   deallocate (q)
   deallocate (p)
   deallocate (t)
   deallocate (qw)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_pseudo")

end subroutine da_transform_xtoy_pseudo


