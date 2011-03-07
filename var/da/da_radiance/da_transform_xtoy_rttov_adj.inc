subroutine da_transform_xtoy_rttov_adj ( iv, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! Purpose: transform gradient from obs space to model grid space.
   !
   ! METHOD:  jo_grad_x = H^T jo_grad_y =  - H^T R^-1 ( d - H delta_x )
   !           1. input gradient in obs space and reference state of RTTOV
   !           2. call adjoint of RTM
   !           3. adjoint of interpolation from model grid to obs loc
   !---------------------------------------------------------------------------

   implicit none

   type (x_type), intent(inout)   :: jo_grad_x !
   type (y_type),  intent(in)     :: jo_grad_y ! H' delta_x
   type (iv_type), intent(in)     :: iv        ! O-B structure.

#ifdef RTTOV

   integer                        :: i, j, k,c,l  ! Index dimension.
   integer                        :: nlevels ! Number of obs levels.
   integer                        :: num_rad  ! Number of radiance obs
#ifdef DM_PARALLEL
   integer :: status(mpi_status_size) ! MPI status.
#endif

   real, allocatable              :: model_mr(:)

   integer            :: nchan, n

   type(con_vars_type),  allocatable :: con_vars(:), con_vars_ad(:)
   type(aux_vars_type),  allocatable :: aux_vars(:)

   ! evenly distributed versions
   type(con_vars_type),  allocatable  :: d_con_vars(:), d_con_vars_ad(:)
   type(aux_vars_type),  allocatable  :: d_aux_vars(:)
   real,    allocatable :: d_tb(:,:)
   integer :: d_num_rad, l_num_rad,e,s,len,nl
   real, allocatable :: temp_t(:,:),temp_q(:,:), temp_ps(:), rtemp(:,:)
   real, allocatable :: temp_tb(:,:)
   real, allocatable :: temp_t_jac(:,:,:), temp_q_jac(:,:,:), temp_ps_jac(:,:)
   integer, allocatable :: itemp(:,:)

   if (iv%num_inst < 1) return

   if (trace_use) call da_trace_entry("da_transform_xtoy_rttov_adj")

   do i = 1, iv%num_inst                 ! loop for sensor

      if (iv%instid(i)%num_rad_glo < 1) cycle

      num_rad  = iv%instid(i)%num_rad
      nlevels  = iv%instid(i)%nlevels
      nchan    = iv%instid(i)%nchan

      if (num_rad > 0) then

         allocate (model_mr(1:nlevels))

         allocate (con_vars(num_rad))
         allocate (con_vars_ad(num_rad))
         allocate (aux_vars(num_rad))

         !---------------------------------------------------------------
         ! [1.0] assign tb = R^-1 Re :
         !---------------------------------------------------------------

         !---------------------------------------------
         ! [2.0] get input of da_rttov_ad
         !---------------------------------------------

         do n = 1, num_rad
            con_vars(n) % nlevels = nlevels 

            allocate (con_vars(n)    % t(nlevels))
            allocate (con_vars(n)    % q(nlevels))
            allocate (con_vars_ad(n) % t(nlevels))
            allocate (con_vars_ad(n) % q(nlevels))

            con_vars_ad(n) % t(:) = 0.0
            con_vars_ad(n) % q(:) = 0.0
            con_vars_ad(n) % ps   = 0.0

            con_vars(n) % t(:) = iv%instid(i)%t(:,n)
            con_vars(n) % q(:) = iv%instid(i)%mr(:,n)
            con_vars(n) % ps   = iv%instid(i)%ps(n)

            if ( use_rttov_kmatrix ) then
               allocate (con_vars(n) % t_jac(nchan,nlevels))
               allocate (con_vars(n) % q_jac(nchan,nlevels))
               allocate (con_vars(n) % ps_jac(nchan))
               do k = 1, nlevels
                  con_vars(n) % t_jac(:,k) = iv%instid(i)%t_jacobian(:,k,n)
                  con_vars(n) % q_jac(:,k) = iv%instid(i)%q_jacobian(:,k,n)
               end do
               con_vars(n) % ps_jac(:)= iv%instid(i)%ps_jacobian(:,n)
            end if

            aux_vars(n) % t2m = iv%instid(i)%t2m(n)
            aux_vars(n) % q2m = iv%instid(i)%mr2m(n)
            aux_vars(n) % u10 = iv%instid(i)%u10(n)
            aux_vars(n) % v10 = iv%instid(i)%v10(n)

            aux_vars(n) % surftype = iv%instid(i)%surftype(n)
            aux_vars(n) % surft    = iv%instid(i)%ts(n)
            ! aux_vars(n) % fastem(:) = 0.0

            aux_vars(n) % satzen  = iv%instid(i)%satzen(n)
            aux_vars(n) % satazi  = iv%instid(i)%satazi(n)
    
            aux_vars(n) % solzen   = iv%instid(i)%solzen(n)
            aux_vars(n) % solazi   = iv%instid(i)%solazi(n)
            aux_vars(n) % elevation = iv%instid(i)%elevation(n)
            aux_vars(n) % rlat     = iv%instid(i)%info%lat(1,n)
 
         end do
      end if

      !-------------------------------------------------
      ! [2.9] Distribute evenly across processors
      !-------------------------------------------------

      d_num_rad=num_tovs_after(i,myproc+1)

      if (d_num_rad > 0) then
         allocate (d_con_vars(d_num_rad))
         allocate (d_con_vars_ad(d_num_rad))
         allocate (d_aux_vars(d_num_rad))
         allocate (d_tb(nchan,d_num_rad))
         d_tb(:,:) = 0.0
         d_con_vars(:) % nlevels = nlevels
         do n = 1, d_num_rad
            allocate (d_con_vars(n) % t(nlevels))
            allocate (d_con_vars(n) % q(nlevels))
            if ( use_rttov_kmatrix ) then
               allocate (d_con_vars(n) % t_jac(nchan,nlevels))
               allocate (d_con_vars(n) % q_jac(nchan,nlevels))
               allocate (d_con_vars(n) % ps_jac(nchan))
            end if
            allocate (d_con_vars_ad(n) % t(nlevels))
            allocate (d_con_vars_ad(n) % q(nlevels))
         end do

         ! Fill up with data that stays local

         l_num_rad=min(num_rad,d_num_rad)

         if (l_num_rad > 0) then
            do n = 1, l_num_rad
               d_con_vars(n) % t(:) = con_vars(n) % t(:) 
               d_con_vars(n) % q(:) = con_vars(n) % q(:)
               ! d_aux_vars(n) % fastem(:) = 0.0
               if ( use_rttov_kmatrix ) then
                  d_con_vars(n) % t_jac(:,:) = con_vars(n) % t_jac(:,:)
                  d_con_vars(n) % q_jac(:,:) = con_vars(n) % q_jac(:,:)
                  d_con_vars(n) % ps_jac(:)  = con_vars(n) % ps_jac(:)
               end if
            end do
            d_con_vars(1:l_num_rad) % nlevels  = con_vars(1:l_num_rad) % nlevels
            d_con_vars(1:l_num_rad) % ps       = con_vars(1:l_num_rad) % ps
            d_aux_vars(1:l_num_rad) % t2m      = aux_vars(1:l_num_rad) % t2m
            d_aux_vars(1:l_num_rad) % q2m      = aux_vars(1:l_num_rad) % q2m
            d_aux_vars(1:l_num_rad) % u10      = aux_vars(1:l_num_rad) % u10
            d_aux_vars(1:l_num_rad) % v10      = aux_vars(1:l_num_rad) % v10
            d_aux_vars(1:l_num_rad) % surftype = aux_vars(1:l_num_rad) % surftype
            d_aux_vars(1:l_num_rad) % surft    = aux_vars(1:l_num_rad) % surft
            d_aux_vars(1:l_num_rad) % satzen   = aux_vars(1:l_num_rad) % satzen
            d_aux_vars(1:l_num_rad) % satazi   = aux_vars(1:l_num_rad) % satazi

            d_aux_vars(1:l_num_rad) % solzen   = aux_vars(1:l_num_rad) % solzen
            d_aux_vars(1:l_num_rad) % solazi   = aux_vars(1:l_num_rad) % solazi
            d_aux_vars(1:l_num_rad) % elevation   = aux_vars(1:l_num_rad) % elevation
            d_aux_vars(1:l_num_rad) % rlat     = aux_vars(1:l_num_rad) % rlat
 
            d_tb(:,1:l_num_rad) = jo_grad_y%instid(i)%tb(:,1:l_num_rad)
        
         end if

         ! Get data from elsewhere
#ifdef DM_PARALLEL
         do c=1,tovs_copy_count(i)
            if (tovs_send_pe(i,c)==myproc) then
               s=tovs_send_start(i,c)
               len=tovs_send_count(i,c)
               e=s+len-1
               nl=len*nlevels
               allocate(temp_t(nlevels,len))
               do j=1,len
                  temp_t(:,j)=con_vars(s+j-1) % t(:)
               end do
               call mpi_send( temp_t,nl, true_mpi_real, tovs_recv_pe(i,c), &
                  c*11+1, comm, ierr)

               allocate(temp_q(nlevels,len))
               do j=1,len
                  temp_q(:,j)=con_vars(s+j-1) % q(:)
               end do
               call mpi_send (temp_q,nl, true_mpi_real, tovs_recv_pe(i,c), &
                  c*11+2, comm, ierr)
    
               allocate (temp_tb(nchan,len))
               temp_tb(:,:)=jo_grad_y%instid(i)%tb(:,s:e)
               call mpi_send (temp_tb,len*nchan,  &
                  true_mpi_real, tovs_recv_pe(i,c), c*11+3, comm, ierr)

               allocate (rtemp(len,12))
               rtemp(:,1)= con_vars(s:e) % ps
               rtemp(:,2)= aux_vars(s:e) % t2m
               rtemp(:,3)= aux_vars(s:e) % q2m
               rtemp(:,4)= aux_vars(s:e) % u10
               rtemp(:,5)= aux_vars(s:e) % v10
               rtemp(:,6)= aux_vars(s:e) % surft
               rtemp(:,7)= aux_vars(s:e) % satzen
               rtemp(:,8)= aux_vars(s:e) % satazi
               rtemp(:,9)= aux_vars(s:e) % solzen
               rtemp(:,10)= aux_vars(s:e) % solazi
               rtemp(:,11)= aux_vars(s:e) % elevation
               rtemp(:,12)= aux_vars(s:e) % rlat
               call mpi_send(rtemp,len*12, true_mpi_real, tovs_recv_pe(i,c), &
                  c*11+4, comm, ierr)

               allocate (itemp(len,2))
               itemp(:,1)= con_vars(s:e) % nlevels
               itemp(:,2)= aux_vars(s:e) % surftype
               call mpi_send(itemp,len*2, mpi_integer, tovs_recv_pe(i,c), &
                  c*11+5, comm, ierr)

               deallocate (temp_tb)
               deallocate (temp_t)
               deallocate (temp_q)
               deallocate (itemp)
               deallocate (rtemp)

               if ( use_rttov_kmatrix ) then
                  allocate(temp_t_jac(nchan,nlevels,len))
                  do k = 1, nlevels
                     do j=1,len
                        temp_t_jac(:,k,j)=con_vars(s+j-1) % t_jac(:,k)
                     end do
                  end do
                  call mpi_send( temp_t_jac, nl*nchan, true_mpi_real, tovs_recv_pe(i,c), &
                     c*11+6, comm, ierr)

                  allocate(temp_q_jac(nchan,nlevels,len))
                  do k = 1, nlevels
                     do j=1,len
                        temp_q_jac(:,:,j)=con_vars(s+j-1) % q_jac(:,:)
                     end do
                  end do
                  call mpi_send( temp_q_jac, nl*nchan, true_mpi_real, tovs_recv_pe(i,c), &
                     c*11+7, comm, ierr)

                  allocate(temp_ps_jac(nchan,len))
                  do j=1,len
                     temp_ps_jac(:,j)=con_vars(s+j-1) % ps_jac(:)
                  end do
                  call mpi_send( temp_ps_jac, len*nchan, true_mpi_real, tovs_recv_pe(i,c), &
                     c*11+8, comm, ierr)

                  deallocate (temp_t_jac)
                  deallocate (temp_q_jac)
                  deallocate (temp_ps_jac)
               end if

            end if

            if (tovs_recv_pe(i,c)==myproc) then
               s=tovs_recv_start(i,c)
               len=tovs_send_count(i,c)
               e=s+len-1
               nl=len*nlevels
               allocate(temp_t(nlevels,len))
               call mpi_recv(temp_t,nl, true_mpi_real, tovs_send_pe(i,c), &
                  c*11+1, comm, status, ierr)
               do j=1,len
                  d_con_vars(s+j-1) % t(:)=temp_t(:,j)
               end do

               allocate(temp_q(nlevels,len))
               call mpi_recv(temp_q,nl, true_mpi_real, tovs_send_pe(i,c), &
                  c*11+2, comm, status, ierr)
               do j=1,len
                  d_con_vars(s+j-1) % q(:)=temp_q(:,j)
               end do

               allocate (temp_tb(nchan,len))
               call mpi_recv(temp_tb,len*nchan, true_mpi_real, &
                  tovs_send_pe(i,c), c*11+3, comm, status, ierr)
               d_tb(:,s:e)=temp_tb(:,:)

               allocate (rtemp(len,12))
               call mpi_recv(rtemp,len*12, true_mpi_real, &
                  tovs_send_pe(i,c), c*11+4, comm, status, ierr)
               d_con_vars(s:e) % ps       = rtemp(:,1)
               d_aux_vars(s:e) % t2m      = rtemp(:,2)
               d_aux_vars(s:e) % q2m      = rtemp(:,3)
               d_aux_vars(s:e) % u10      = rtemp(:,4)
               d_aux_vars(s:e) % v10      = rtemp(:,5)
               d_aux_vars(s:e) % surft    = rtemp(:,6)
               d_aux_vars(s:e) % satzen   = rtemp(:,7)
               d_aux_vars(s:e) % satazi   = rtemp(:,8)
               d_aux_vars(s:e) % solzen   = rtemp(:,9)
               d_aux_vars(s:e) % solazi   = rtemp(:,10)
               d_aux_vars(s:e) % elevation= rtemp(:,11)
               d_aux_vars(s:e) % rlat     = rtemp(:,12)

               allocate (itemp(len,2))
               call mpi_recv(itemp,len*2, mpi_integer, &
                  tovs_send_pe(i,c), c*11+5, comm, status, ierr)
               d_con_vars(s:e) % nlevels  = itemp(:,1)
               d_aux_vars(s:e) % surftype = itemp(:,2)

               deallocate (temp_tb)
               deallocate (temp_t)
               deallocate (temp_q)
               deallocate (itemp)
               deallocate (rtemp)

               if ( use_rttov_kmatrix ) then
                  allocate(temp_t_jac(nchan,nlevels,len))
                  call mpi_recv(temp_t_jac, nl*nchan, true_mpi_real, tovs_send_pe(i,c), &
                     c*11+6, comm, status, ierr)
                  do k = 1, nlevels
                     do j=1,len
                        d_con_vars(s+j-1) % t_jac(:,k)=temp_t_jac(:,k,j)
                     end do
                  end do

                  allocate(temp_q_jac(nchan,nlevels,len))
                  call mpi_recv(temp_q_jac, nl*nchan, true_mpi_real, tovs_send_pe(i,c), &
                     c*11+7, comm, status, ierr)
                  do k = 1, nlevels
                     do j=1,len
                        d_con_vars(s+j-1) % q_jac(:,k)=temp_q_jac(:,k,j)
                     end do
                  end do

                  allocate (temp_ps_jac(nchan,len))
                  call mpi_recv(temp_ps_jac, len*nchan, true_mpi_real, &
                     tovs_send_pe(i,c), c*11+8, comm, status, ierr)
                  do j=1,len
                     d_con_vars(s+j-1) % ps_jac(:)=temp_ps_jac(:,j)
                  end do

                  deallocate (temp_t_jac)
                  deallocate (temp_q_jac)
                  deallocate (temp_ps_jac)
               end if
            end if

         end do
#endif

         if (d_num_rad > 0) then
            if (tovs_batch) then
               call da_rttov_ad (i, nchan, d_num_rad, d_con_vars, &
               d_aux_vars, d_con_vars_ad, d_tb)
            else
               if (.not. use_rttov_kmatrix) then
                  !$OMP PARALLEL DO  &
                  !$OMP PRIVATE ( n )
                   do n=1,d_num_rad
                      call da_rttov_ad (i, nchan, 1, d_con_vars(n:n), &
                         d_aux_vars(n:n), d_con_vars_ad(n:n), d_tb(:,n:n))
                   end do
                  !$OMP END PARALLEL DO
               else
                  do n=1,d_num_rad
                     d_con_vars_ad(n) % t(:) = 0.0
                     d_con_vars_ad(n) % q(:) = 0.0
                     d_con_vars_ad(n) % ps   = 0.0
                     do l = 1, nchan
                        d_con_vars_ad(n) % ps = d_con_vars_ad(n) % ps + &
                                         d_con_vars(n)%ps_jac(l) * d_tb(l,n)
                        do k = 1,nlevels
                           d_con_vars_ad(n) % t(k) = d_con_vars_ad(n) % t(k) + &
                                         d_con_vars(n)%t_jac(l,k) * d_tb(l,n)
                           d_con_vars_ad(n) % q(k) = d_con_vars_ad(n) % q(k) + &
                                         d_con_vars(n)%q_jac(l,k) * d_tb(l,n)
                        end do
                     end do
                  end do
                end if
            end if
         end if

         ! Transfer data back
       
         if (l_num_rad > 0) then
            do n = 1, l_num_rad
               con_vars_ad(n) % t(:) = d_con_vars_ad(n) % t(:) 
               con_vars_ad(n) % q(:) = d_con_vars_ad(n) % q(:)
            end do

            con_vars_ad(1:l_num_rad) % ps = d_con_vars_ad(1:l_num_rad) % ps
         end if
      end if ! d_num_rad > 0

      ! Return the data to other processors. Note the meaning of send_pe and
      ! recv_pe is swapped here

#ifdef DM_PARALLEL
      do c=1,tovs_copy_count(i)
         if (tovs_recv_pe(i,c)==myproc) then
            s=tovs_recv_start(i,c)
            len=tovs_send_count(i,c)
            e=s+len-1
            nl=len*nlevels
            allocate(temp_t(nlevels,len))
            do j=1,len
               temp_t(:,j)=d_con_vars_ad(s+j-1) % t(:)
            end do
            call mpi_send (temp_t,nl, true_mpi_real, tovs_send_pe(i,c), &
               c*11+9, comm, ierr)

            allocate(temp_q(nlevels,len))
            do j=1,len
               temp_q(:,j)=d_con_vars_ad(s+j-1) % q(:)
            end do
            call mpi_send (temp_q,nl, true_mpi_real, tovs_send_pe(i,c), &
               c*11+10, comm, ierr)

            allocate(rtemp(len,1))
            rtemp(:,1) = d_con_vars_ad(s:e) % ps
            call mpi_send (rtemp,len, true_mpi_real, tovs_send_pe(i,c), &
               c*11+11, comm, ierr)

            deallocate (temp_t)
            deallocate (temp_q)
            deallocate (rtemp)
         end if

         if (tovs_send_pe(i,c)==myproc) then
            s=tovs_send_start(i,c)
            len=tovs_send_count(i,c)
            e=s+len-1
            nl=len*nlevels
            allocate(temp_t(nlevels,len))
            call mpi_recv (temp_t,nl, true_mpi_real, tovs_recv_pe(i,c), &
               c*11+9, comm, status, ierr)
            do j=1,len
               con_vars_ad(s+j-1) % t(:)=temp_t(:,j)
            end do

            allocate(temp_q(nlevels,len))
            call mpi_recv (temp_q,nl, true_mpi_real, tovs_recv_pe(i,c), &
               c*11+10, comm, status, ierr)
            do j=1,len
               con_vars_ad(s+j-1) % q(:)=temp_q(:,j)
            end do

            allocate(rtemp(len,1))
            call mpi_recv (rtemp,len, true_mpi_real, tovs_recv_pe(i,c), &
               c*11+11, comm, status, ierr)
            con_vars_ad(s:e) % ps=rtemp(:,1)

            deallocate (temp_t)
            deallocate (temp_q)
            deallocate (rtemp)
         end if

      end do
#endif
      if (d_num_rad > 0) then
         do n=1,d_num_rad
            deallocate (d_con_vars(n) % t)
            deallocate (d_con_vars(n) % q)
            deallocate (d_con_vars_ad(n) % t)
            deallocate (d_con_vars_ad(n) % q)
            if ( use_rttov_kmatrix ) then
               deallocate (d_con_vars(n) % t_jac)
               deallocate (d_con_vars(n) % q_jac)
               deallocate (d_con_vars(n) % ps_jac)
            end if
         end do

         deallocate (d_tb)
         deallocate (d_con_vars)
         deallocate (d_aux_vars)
         deallocate (d_con_vars_ad)
      end if

      ! adjoint of convert to hPa

      if (num_rad > 0) then
         con_vars_ad(:)% ps = con_vars_ad(:)%ps * 0.01 
      end if

      do n=1,num_rad
         ! 4.2 scale transform 

         do k=1, nlevels
            model_mr(k) = con_vars_ad(n) % q(k)

            if (iv%instid(i)%info%zk(k,n) <= 0.0) then
               con_vars_ad(n)%t(k)  = 0.0  !coefs(i) % ref_prfl_t(k,gas_id_watervapour)
               con_vars_ad(n)%q(k)  = 0.0  !coefs(i) % ref_prfl_mr(k,gas_id_watervapour)
            else
               ! adjoint of q(kg/kg) to ppmv

               con_vars_ad(n)%q(k) = model_mr(k) * q2ppmv
            end if
         end do
      end do

      if (num_rad > 0) then
         allocate(temp_t(nlevels,num_rad))
         do n=1,num_rad
            temp_t(:,n) = con_vars_ad(n)% t(:)
         end do
         call da_interp_lin_3d_adj (jo_grad_x%t, iv%instid(i)%info, temp_t)
         deallocate(temp_t)

         allocate(temp_q(nlevels,num_rad))
         do n=1,num_rad
            temp_q(:,n) = con_vars_ad(n)% q(:)
         end do
         call da_interp_lin_3d_adj (jo_grad_x%q, iv%instid(i)%info, temp_q)
         deallocate(temp_q)

         allocate(temp_ps(num_rad))
         do n=1,num_rad
            temp_ps(n) = con_vars_ad(n)% ps
         end do
         call da_interp_lin_2d_adj (jo_grad_x% psfc, iv%instid(i)%info, 1, temp_ps)
         deallocate(temp_ps)
      end if


      if (num_rad > 0) then
         do n = 1, num_rad
            deallocate (con_vars(n)    % t)
            deallocate (con_vars(n)    % q)
            deallocate (con_vars_ad(n) % t)
            deallocate (con_vars_ad(n) % q)
            if ( use_rttov_kmatrix ) then
               deallocate (con_vars(n) % t_jac)
               deallocate (con_vars(n) % q_jac)
               deallocate (con_vars(n) % ps_jac)
            end if
         end do

         deallocate (con_vars)
         deallocate (aux_vars)
         deallocate (con_vars_ad)
         deallocate (model_mr)
      end if
   end do        ! end loop for sensor

   if (trace_use) call da_trace_exit("da_transform_xtoy_rttov_adj")
#else
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $RTTOV option for radiances"/))
#endif
 
end subroutine da_transform_xtoy_rttov_adj

