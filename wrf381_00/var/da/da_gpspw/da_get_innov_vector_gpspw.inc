subroutine da_get_innov_vector_gpspw (it,num_qcstat_conv, grid, ob, iv)

   !----------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !----------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it      ! External iteration.
   type(domain),     intent(in)    :: grid    ! first guess state.
   type(y_type),     intent(inout) :: ob      ! Observation structure.
   type(iv_type),    intent(inout) :: iv      ! O-B structure.
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)


   integer :: n        ! Loop counter.
   integer :: i, j     ! Index dimension.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.
   real    :: model_tpw! Model value u at oblocation.

   !--------------------------------------------------------------------------
   integer :: k            ! Index dimension
   real    :: dpw, ddpw    ! adjustment pw [mm]
   real    :: obs_terr     ! real terrain height at GPS site [m]
   real    :: model_terr   ! model terrain height at GPS site[m]
   real    :: model_q(kts:kte) ! model q at GPS site [kg/kg]
   real    :: model_z(kts:kte) ! model z at GPS site [m]
   real    :: model_rho(kts:kte) ! model rho at GPS site [kg/m^3]

   if (trace_use_dull) call da_trace_entry("da_get_innov_vector_gpspw")

   ! unit_gps = myproc + 140
   !---------------------------------------------------------------------------

   ! GPS TPW Pseudo OBS test:
   if ( (pseudo_var(1:3) == 'tpw' .and. num_pseudo > 0) .and. &
        it == 1 ) then
      ! Deallocate:
      if (iv%info(gpspw)%nlocal > 0) then
         write(unit=stdout, fmt='(a,i4)') 'iv%info(gpspw)%nlocal =', iv%info(gpspw)%nlocal
         deallocate(iv % gpspw)
         deallocate(ob % gpspw)
      end if

      use_gpspwobs = .true.

      iv%info(gpspw)%nlocal   = num_pseudo
      iv%info(gpspw)%plocal(1) = num_pseudo
      iv%info(gpspw)%ntotal   = num_pseudo
      iv%info(pseudo)%nlocal  = 0

      ! Allocate:
      call da_allocate_observations (iv)
      iv%info(gpspw)%n1 = 1
      iv%info(gpspw)%n2 = 1
      allocate (ob % gpspw (1:num_pseudo))
      ob % gpspw(1) % tpw   = 0.0
    
      write(unit=message(1),fmt='(a,i2)')'==> GPS TPW pseudo OBS test: num_pseudo=',num_pseudo
      call da_message(message(1:1))

      iv%info(gpspw)%x(:,1)   = pseudo_x
      iv%info(gpspw)%y(:,1)   = pseudo_y
      iv%info(gpspw)%i(:,1)   = int(pseudo_x)
      iv%info(gpspw)%j(:,1)   = int(pseudo_y)
      iv%info(gpspw)%dx(:,1)  = pseudo_x-real(iv%info(gpspw)%i(1,1))
      iv%info(gpspw)%dy(:,1)  = pseudo_y-real(iv%info(gpspw)%j(1,1))
      iv%info(gpspw)%dxm(:,1) = 1.0 - iv%info(gpspw)%dx(1,1)
      iv%info(gpspw)%dym(:,1) = 1.0 - iv%info(gpspw)%dy(1,1)

      iv % gpspw(1) % tpw % inv   = pseudo_val
      iv % gpspw(1) % tpw % qc    = 0
      iv % gpspw(1) % tpw % error = pseudo_err

      ! To consider the site elevation effect, set the model terrain height
      ! to elevation for pseudo OBS:

      i   = iv%info(gpspw)%i(1,1)
      j   = iv%info(gpspw)%j(1,1)
      dx  = iv%info(gpspw)%dx(1,1)
      dy  = iv%info(gpspw)%dy(1,1)
      dxm = iv%info(gpspw)%dxm(1,1)
      dym = iv%info(gpspw)%dym(1,1)

      iv%info(gpspw)%elv(1) = dym*(dxm*grid%xb%terr(i,j)   + dx*grid%xb%terr(i+1,j)) + & 
                             dy *(dxm*grid%xb%terr(i,j+1) + dx*grid%xb%terr(i+1,j+1))

      ! Set halo:
      if ((iv%info(gpspw)%i(1,1) < its-1) .or.(iv%info(gpspw)%i(1,1) > ite) .or. & 
          (iv%info(gpspw)%j(1,1) < jts-1) .or.(iv%info(gpspw)%j(1,1) > jte)) then 
         call da_error(__FILE__,__LINE__,(/"Should never have obs outside halo by now"/))
         iv%info(gpspw)%proc_domain(:,1) = .false. 
      else 
         iv%info(gpspw)%proc_domain(:,1) = .false.  

         if (iv%info(gpspw)%i(1,1) >= its .and. iv%info(gpspw)%i(1,1) <= ite .and. &  
             iv%info(gpspw)%j(1,1) >= jts .and. iv%info(gpspw)%j(1,1) <= jte) then  
            iv%info(gpspw)%proc_domain(:,1) = .true.  
         end if  
      end if 

      write(unit=message(1),fmt='(a,2f15.5)') pseudo_var, pseudo_val, pseudo_err
      write(unit=message(2),fmt='(3f15.5)')   pseudo_x,  pseudo_y,  pseudo_z
      write(unit=message(3),fmt='(a,f8.2)')   'iv%gpspw: elv=',iv%info(gpspw)%elv(1)
      call da_message(message(1:3))
   end if 

   if (iv%info(gpspw)%nlocal > 0) then


      do n=iv%info(gpspw)%n1,iv%info(gpspw)%n2

         if( iv % gpspw(n) % tpw % qc == fails_error_max .and. it > 1) &
             iv % gpspw(n) % tpw % qc = 0

         ! [1.1] Get horizontal interpolation weights:

         i   = iv%info(gpspw)%i(1,n)
         j   = iv%info(gpspw)%j(1,n)
         dx  = iv%info(gpspw)%dx(1,n)
         dy  = iv%info(gpspw)%dy(1,n)
         dxm = iv%info(gpspw)%dxm(1,n)
         dym = iv%info(gpspw)%dym(1,n)

         model_tpw = dym*(dxm*grid%xb%tpw(i,j)   + dx*grid%xb%tpw(i+1,j)) + &
                     dy *(dxm*grid%xb%tpw(i,j+1) + dx*grid%xb%tpw(i+1,j+1))

            ! To compute the 'inv':
            if (.not.(pseudo_var(1:3) == 'tpw' .and. num_pseudo > 0) ) &
            iv % gpspw(n) % tpw % inv = 0.0
            if (ob % gpspw(n) % tpw > missing_r .AND. &
                iv % gpspw(n) % tpw % qc >= obs_qc_pointer) then
               dpw = 0.0
               obs_terr   = iv%info(gpspw)%elv(n)
               model_terr = dym*(dxm*grid%xb%terr(i,j)   + dx*grid%xb%terr(i+1,j)) + &
                          dy *(dxm*grid%xb%terr(i,j+1) + dx*grid%xb%terr(i+1,j+1))
               if (obs_terr <= model_terr) then
                  model_q(1) = dym*(dxm*grid%xb%q(i,j,1)   + dx*grid%xb%q(i+1,j,1)) + &
                             dy *(dxm*grid%xb%q(i,j+1,1) + dx*grid%xb%q(i+1,j+1,1))
                  model_rho(1) = dym*(dxm*grid%xb%rho(i,j,1) + dx*grid%xb%rho(i+1,j,1)) + &
                             dy *(dxm*grid%xb%rho(i,j+1,1) + dx*grid%xb%rho(i+1,j+1,1))
                  dpw = model_rho(1) * model_q(1) *( obs_terr - model_terr)
               else
                  model_z(1) = dym*(dxm*grid%xb%hf(i,j,1)   + dx*grid%xb%hf(i+1,j,1)) + &
                            dy *(dxm*grid%xb%hf(i,j+1,1) + dx*grid%xb%hf(i+1,j+1,1))
                  do k = kts, kte
                     if (model_z(k) >= obs_terr) exit

                     model_z(k+1) = dym*(dxm*grid%xb%hf(i,j,k+1)   + dx*grid%xb%hf(i+1,j,k+1)) + &
                                dy *(dxm*grid%xb%hf(i,j+1,k+1) + dx*grid%xb%hf(i+1,j+1,k+1))
                     model_q(k) = dym*(dxm*grid%xb%q(i,j,k)   + dx*grid%xb%q(i+1,j,k)) + &
                              dy *(dxm*grid%xb%q(i,j+1,k) + dx*grid%xb%q(i+1,j+1,k))
                     model_rho(k) = dym*(dxm*grid%xb%rho(i,j,k)   + dx*grid%xb%rho(i+1,j,k)) + &
                                dy *(dxm*grid%xb%rho(i,j+1,k) + dx*grid%xb%rho(i+1,j+1,k))

                     if (model_z(k+1) <= obs_terr) then
                        ddpw = model_rho(k) * model_q(k) *( model_z(k+1) - model_z(k))
                     else
                        ddpw = model_rho(k) * model_q(k) *( obs_terr - model_z(k))
                     end if

                     dpw = dpw + ddpw
                  end do
               end if
               if ( (pseudo_var(1:3) == 'tpw' .and. num_pseudo > 0) .and. it == 1 ) then
               ! To compute the 'ob':
                 ob % gpspw(n) % tpw = iv % gpspw(n) % tpw % inv + model_tpw - 0.1*dpw
               else
                 iv % gpspw(n) % tpw % inv = ob % gpspw(n) % tpw - model_tpw + 0.1*dpw
               end if
            end if

      end do

      !------------------------------------------------------------------------
      ! [5.0] Perform optional maximum error check:
      !------------------------------------------------------------------------
      if (.not.(pseudo_var(1:3) == 'tpw' .and. num_pseudo > 0) .and. check_max_iv ) &
         call da_check_max_iv_gpspw(iv, it, num_qcstat_conv) 
   end if
   if (trace_use_dull) call da_trace_exit("da_get_innov_vector_gpspw")

end subroutine da_get_innov_vector_gpspw


