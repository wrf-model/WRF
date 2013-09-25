subroutine da_transform_xtoy_div_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none
   type (domain),  intent(inout) :: grid
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)
   integer :: n        ! Loop counter.

!    real, allocatable :: u(:,:)
!    real, allocatable :: v(:,:)
!    real, allocatable :: q(:,:)
!    real, allocatable :: p(:,:)
!    real, allocatable :: t(:,:)
!    real :: xdiv(ims:ime,jms:jme,kms:kme)
   real, allocatable :: div(:,:)
! #ifdef CLOUD_CV
!    real, allocatable :: qw(:,:)
! #endif

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_div_adj")
  jo_grad_x%div(:,:,:) =0.
!    allocate (u(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
!    allocate (v(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
!    allocate (q(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
!    allocate (p(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
!    allocate (t(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
   allocate (div(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
! #ifdef CLOUD_CV
!    allocate (qw(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
! #endif

    do n=iv%info(idxdiv)%n1,iv%info(idxdiv)%n2
 !  do n=1,1
!       u(1,n) = jo_grad_y%pseudo(n)%u
!       v(1,n) = jo_grad_y%pseudo(n)%v
      div(1,n) = jo_grad_y%div(n)
! #ifdef CLOUD_CV
!    select case(pseudo_var(1:3))
!    case ('qcw', 'QCW','qci', 'QCI','qrn', 'QRN', 'qsn', 'QSN')
!         qw(1,n) = jo_grad_y%pseudo(n)%q
! !        jo_grad_y%pseudo(n)%q = 0.0
!    end select
! #endif
!       q(1,n) = jo_grad_y%pseudo(n)%q
!       p(1,n) = jo_grad_y%pseudo(n)%p
!       t(1,n) = jo_grad_y%pseudo(n)%t
   end do

! #ifdef A2C
!    call da_interp_lin_3d_adj(jo_grad_x%u, iv%info(idxdiv), u,'u')
!    call da_interp_lin_3d_adj(jo_grad_x%v, iv%info(idxdiv), v,'v')
! #else
!    call da_interp_lin_3d_adj(jo_grad_x%u, iv%info(idxdiv), u)
!    call da_interp_lin_3d_adj(jo_grad_x%v, iv%info(idxdiv), v)
   

! #endif
   call da_interp_lin_3d_adj(jo_grad_x%div, iv%info(idxdiv), div)
!    call da_interp_lin_3d_adj(jo_grad_x%q, iv%info(idxdiv), q)
!    call da_interp_lin_3d_adj(jo_grad_x%p, iv%info(idxdiv), p)
!    call da_interp_lin_3d_adj(jo_grad_x%t, iv%info(idxdiv), t)
   call da_uv_to_divergence_adj(grid, jo_grad_x%u, jo_grad_x%v, jo_grad_x%div)
! #ifdef CLOUD_CV
! !   print*,'pseudo observatin =',pseudo_var(1:3)
!    select case(pseudo_var(1:3))
!    case ('qcw', 'QCW')
!         call da_interp_lin_3d_adj(jo_grad_x%qcw, iv%info(idxdiv), qw)
!    case ('qci', 'QCI')
!         call da_interp_lin_3d_adj(jo_grad_x%qci, iv%info(idxdiv), qw)
!    case ('qrn', 'QRN')
!         call da_interp_lin_3d_adj(jo_grad_x%qrn, iv%info(idxdiv), qw)
!    case ('qsn', 'QSN')
!         call da_interp_lin_3d_adj(jo_grad_x%qsn, iv%info(idxdiv), qw)
!    end select
! #endif

!    deallocate (u)
!    deallocate (v)
!    deallocate (q)
!    deallocate (p)
!    deallocate (t)
   deallocate (div)

! #ifdef CLOUD_CV
!    deallocate (qw)
! #endif
   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_div_adj")

end subroutine da_transform_xtoy_div_adj


