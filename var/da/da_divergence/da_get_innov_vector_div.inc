subroutine da_get_innov_vector_div(grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none
   
   type(domain),     intent(inout) :: grid        ! Background structure 
   type(y_type),     intent(inout) :: ob          ! Observation structure.
   type(iv_type),    intent(inout) :: iv          ! O-B structure.
   integer :: n, ndiv       ! Loop counter.
!   real :: div_val=0.0000, div_err=0.01
!   real :: div_x=160.0, div_y=140.0, div_z=30.0
!    real, allocatable :: div_x(:,:), div_y(:,:), div_z(:,:)
!    real, allocatable :: model_u(:,:)
!    real, allocatable :: model_v(:,:)
!    real, allocatable :: model_q(:,:)
!    real, allocatable :: model_p(:,:)
!    real, allocatable :: model_t(:,:)
   real, allocatable :: model_div(:,:)
! #ifdef CLOUD_CV
!    real, allocatable :: model_qcw(:,:)
!    real, allocatable :: model_qci(:,:)
!    real, allocatable :: model_qrn(:,:)
!    real, allocatable :: model_qsn(:,:)
! #endif  
! 	real :: div_val=0.0001, div_err=0.0001
! 	real :: div_x=160.0, div_y=140.0, div_z=30.0 
!    if (trace_use_dull) call da_trace_entry("da_get_innov_vector_div")

!    allocate (model_u(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
!    allocate (model_v(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
!    allocate (model_q(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
!    allocate (model_p(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
!    allocate (model_t(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
   allocate (model_div(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
!    allocate (div_x(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
!    allocate (div_y(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
!    allocate (div_z(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
! #ifdef CLOUD_CV
!    allocate (model_qcw(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
!    allocate (model_qci(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
!    allocate (model_qrn(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
!    allocate (model_qsn(1,iv%info(idxdiv)%n1:iv%info(idxdiv)%n2))
! #endif

!	set pseudo divergence points
!	div_x(1,:) = (/80., 160., 240./)
!	div_y(1,:) = (/70., 140., 210./)
!	div_z(1,:) = (/10.,  20., 30./)


! if (trace_use_dull) call da_trace_entry("da_setup_div_obs")
! 
! !ndiv = ite*jte*kte
! 
! ! call da_allocate_observations(iv)
! 
! do n=1, iv%info(idxdiv)%nlocal
! 	iv% div(n) % inv = missing_r
! 
! 	iv% div(n) % qc  = missing_data
! 
! 	ob% div(n) = missing_r
! 
! 	!---------------------------------------------------------------
! 	! [1.0] Initialise components of innovation vector:
! 	!---------------------------------------------------------------
! 
! 	iv%info(idxdiv)%x(:,n)  = div_x
! 	iv%info(idxdiv)%y(:,n)  = div_y
! 	iv%info(idxdiv)%zk(:,n) = div_z
! 
! 	iv%info(idxdiv)%i(:,n) = int(div_x)
! 	iv%info(idxdiv)%j(:,n) = int(div_y)
! 	iv%info(idxdiv)%k(:,n) = int(div_z)
! 
! 	iv%info(idxdiv)%dx(:,n) = div_x-real(iv%info(idxdiv)%i(:,n))
! 	iv%info(idxdiv)%dy(:,n) = div_y-real(iv%info(idxdiv)%j(:,n))
! 	iv%info(idxdiv)%dxm(:,n)=1.0-iv%info(idxdiv)%dx(:,n)
! 	iv%info(idxdiv)%dym(:,n)=1.0-iv%info(idxdiv)%dy(:,n)
! 	iv%info(idxdiv)%levels(:) = 1
! 
! 	iv% div(n) % inv = div_val
! 	iv% div(n) % error = div_err
! 	iv% div(n) % qc = 0
! end do
! call da_convert_zk (iv%info(idxdiv))
! 
! if (trace_use_dull) call da_trace_exit("da_setup_div_obs")
! 	iv%div(:) % inv = div_val
! 	iv%div(:) % error = div_err
! 	iv%div(:) % qc = 0 
! 	iv%info(idxdiv)%levels(:) = 1
	 
!    n=0
!    do k=kts,kte
!       do j=jts,jte
! 		do i=its,ite
!         	n=n+1
!       		iv%info(idxdiv)%x(:,n)  = i
!       		iv%info(idxdiv)%y(:,n)  = j
!       		iv%info(idxdiv)%zk(:,n) = k
!       	end do
!       end do
!    end do
 
! 	iv%info(idxdiv)%i(:,:) = iv%info(idxdiv)%x(:,:)
! 	iv%info(idxdiv)%j(:,:) = iv%info(idxdiv)%y(:,:)
! 	iv%info(idxdiv)%k(:,:) = iv%info(idxdiv)%zk(:,:)

! 	iv%info(idxdiv)%dx(:,:) = 0. !i-real(iv%info(idxdiv)%i(:,n))
! 	iv%info(idxdiv)%dy(:,:) = 0. !j-real(iv%info(idxdiv)%j(:,n))
! 	iv%info(idxdiv)%dxm(:,:)=1.0 !-iv%info(idxdiv)%dx(:,n)
! 	iv%info(idxdiv)%dym(:,:)=1.0 !-iv%info(idxdiv)%dy(:,n)   
   
!   call da_convert_zk (iv%info(idxdiv))
   
   call da_uv_to_divergence(grid%xb, grid%xb%u, grid%xb%v, grid%xb%div)
! #ifdef A2C
!    call da_interp_lin_3d (grid%xb%u, iv%info(idxdiv), model_u,'u')
!    call da_interp_lin_3d (grid%xb%v, iv%info(idxdiv), model_v,'v')
! #else
!    call da_interp_lin_3d (grid%xb%u, iv%info(idxdiv), model_u)
!    call da_interp_lin_3d (grid%xb%v, iv%info(idxdiv), model_v)
! #endif
   call da_interp_lin_3d (grid%xb%div, iv%info(idxdiv), model_div)
!    call da_interp_lin_3d (grid%xb%t, iv%info(idxdiv), model_t)
!    call da_interp_lin_3d (grid%xb%p, iv%info(idxdiv), model_p)
!    call da_interp_lin_3d (grid%xb%q, iv%info(idxdiv), model_q)
! #ifdef CLOUD_CV
!    call da_interp_lin_3d (grid%xb%qcw, iv%info(idxdiv), model_qcw)
!    call da_interp_lin_3d (grid%xb%qci, iv%info(idxdiv), model_qci)
!    call da_interp_lin_3d (grid%xb%qrn, iv%info(idxdiv), model_qrn)
!    call da_interp_lin_3d (grid%xb%qsn, iv%info(idxdiv), model_qsn)
! #endif


   do n=iv%info(idxdiv)%n1,iv%info(idxdiv)%n2
!   do n=1,1
      !---------------------------------------------------------------
      ! [3.0] Calculate observation O = B +(O-B):
      !---------------------------------------------------------------
          
!        if (ob%div(n) > missing_r) then
!               iv%div(n)%inv = ob%div(n) - model_div(1,n)
!           else
             ob%div(n) = model_div(1,n) + iv%div(n)%inv
!        endif 

! #ifdef CLOUD_CV
!       select case(pseudo_var(1:3))
!       case ('qcw', 'QCW')
! ! here it is assumed that the inv is obtained from namelist
! ! set  ob % pseudo(n) % q to missing, which may be assigned a value by case ('q')
!          ob % pseudo(n) % q = missing_r
!          if (ob % pseudo(n) % q > missing_r) then
!              iv % pseudo(n) % q % inv = ob%pseudo(n)%q - model_qcw(1,n)
!          else
!              ob % pseudo(n) % q = model_qcw(1,n) + iv % pseudo(n) % q % inv
!          endif
!       case ('qci', 'QCI')
!          ob % pseudo(n) % q = missing_r
!          if (ob % pseudo(n) % q > missing_r) then
!              iv % pseudo(n) % q % inv = ob%pseudo(n)%q - model_qci(1,n)
!          else
!              ob % pseudo(n) % q = model_qci(1,n) + iv % pseudo(n) % q % inv
!          endif
!       case ('qrn', 'QRN')
!          ob % pseudo(n) % q = missing_r
!          if (ob % pseudo(n) % q > missing_r) then
!              iv % pseudo(n) % q % inv = ob%pseudo(n)%q - model_qrn(1,n)
!          else
!              ob % pseudo(n) % q = model_qrn(1,n) + iv % pseudo(n) % q % inv
!          endif
!       case ('qsn', 'QSN')
!          ob % pseudo(n) % q = missing_r
!          if (ob % pseudo(n) % q > missing_r) then
!              iv % pseudo(n) % q % inv = ob%pseudo(n)%q - model_qsn(1,n)
!          else
!              ob % pseudo(n) % q = model_qsn(1,n) + iv % pseudo(n) % q % inv
!          endif
!       end select
! #endif
   end do

!    deallocate (model_u)
!    deallocate (model_v)
!    deallocate (model_q)
!    deallocate (model_p)
!    deallocate (model_t)
   deallocate (model_div)
!    deallocate (div_x)
!    deallocate (div_y)
!    deallocate (div_z)
! #ifdef CLOUD_CV
!    deallocate (model_qcw)
!    deallocate (model_qci)
!    deallocate (model_qrn)
!    deallocate (model_qsn)
! #endif   
   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_div")

end subroutine da_get_innov_vector_div


