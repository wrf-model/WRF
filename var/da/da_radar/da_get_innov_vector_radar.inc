subroutine da_get_innov_vector_radar (it, grid, ob, iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: it       ! External iteration.
   type(domain),     intent(in)    :: grid     ! first guess state.
   type(y_type),     intent(inout) :: ob       ! Observation structure.
   type(iv_type),    intent(inout) :: iv       ! O-B structure.

   integer :: n        ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.

   real, allocatable :: model_p(:,:)
   real, allocatable :: model_u(:,:)
   real, allocatable :: model_v(:,:)
   real, allocatable :: model_w(:,:)
   real, allocatable :: model_rho(:,:)
   real, allocatable :: model_qrn(:,:)
   real, allocatable :: model_rv(:,:)
   real, allocatable :: model_rf(:,:)
   real, allocatable :: model_ps(:)

   real    :: v_h(kms:kme)      ! Model value h at ob hor. location.

   real    :: xr,yr,zr
   integer :: irv, irvf
   integer :: irf, irff
   
   if (trace_use) call da_trace_entry("da_get_innov_vector_radar")

   irv = 0; irvf = 0; irf = 0; irff = 0

   allocate (model_p(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_u(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_v(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_w(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_rho(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_qrn(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_rv(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_rf(iv%info(radar)%max_lev,iv%info(radar)%n1:iv%info(radar)%n2))
   allocate (model_ps(iv%info(radar)%n1:iv%info(radar)%n2))

   model_p(:,:)    = 0
   model_u(:,:)    = 0
   model_v(:,:)    = 0
   model_w(:,:)    = 0
   model_rho(:,:)  = 0
   model_qrn(:,:)  = 0
   model_rv(:,:)   = 0
   model_rf(:,:)   = 0
   model_ps(:)     = 0

   if ( it > 1 ) then
     do n=iv%info(radar)%n1,iv%info(radar)%n2
        do k=1,iv%info(radar)%levels(n)
           if (iv%radar(n)%rv(k)%qc == fails_error_max) iv%radar(n)%rv(k)%qc = 0
           if (iv%radar(n)%rf(k)%qc == fails_error_max) iv%radar(n)%rf(k)%qc = 0
        end do
     end do
   end if

   do n=iv%info(radar)%n1,iv%info(radar)%n2
      if (iv%info(radar)%levels(n) < 1) cycle

      ! [1.0] Get horizontal interpolation weights:

      i   = iv%info(radar)%i(1,n)
      j   = iv%info(radar)%j(1,n)
      dx  = iv%info(radar)%dx(1,n)
      dy  = iv%info(radar)%dy(1,n)
      dxm = iv%info(radar)%dxm(1,n)
      dym = iv%info(radar)%dym(1,n)

      do k=kts,kte
         v_h(k) = dym*(dxm*grid%xb%h(i,j,k)+dx*grid%xb%h(i+1,j,k)) + dy*(dxm*grid%xb%h(i,j+1,k)+dx*grid%xb%h(i+1,j+1,k))
      end do

      do k=1, iv%info(radar)%levels(n)
         call da_to_zk(iv%radar(n)%height(k), v_h, v_interp_h, iv%info(radar)%zk(k,n))

         if (iv%info(radar)%zk(k,n) < 1.0) then
            iv%radar(n)%height_qc(k) = below_model_surface
         else if (iv%info(radar)%zk(k,n) > mkz) then
            iv%radar(n)%height_qc(k) = above_model_lid
         end if
      end do
   end do

   call da_convert_zk (iv%info(radar))

   ! [2.0] Interpolate horizontally to ob points:

   call da_interp_lin_3d (grid%xb % p,   iv%info(radar), model_p)
#ifdef A2C
   call da_interp_lin_3d (grid%xb % u,   iv%info(radar), model_u,'u')
   call da_interp_lin_3d (grid%xb % v,   iv%info(radar), model_v,'v')
#else
   call da_interp_lin_3d (grid%xb % u,   iv%info(radar), model_u)
   call da_interp_lin_3d (grid%xb % v,   iv%info(radar), model_v)
#endif
   call da_interp_lin_3d (grid%xb % wh,  iv%info(radar), model_w)
   call da_interp_lin_3d (grid%xb % rho, iv%info(radar), model_rho)
   call da_interp_lin_3d (grid%xb % qrn, iv%info(radar), model_qrn)

   ! Test 5.0E-5 as critical value. It can not be smaller.

   do n=iv%info(radar)%n1,iv%info(radar)%n2
      do k=1,iv%info(radar)%levels(n)
         model_qrn(k,n)=amax1(5.0E-5,model_qrn(k,n))
      end do

      i   = iv%info(radar)%i(1,n)
      j   = iv%info(radar)%j(1,n)
      dx  = iv%info(radar)%dx(1,n)
      dy  = iv%info(radar)%dy(1,n)
      dxm = iv%info(radar)%dxm(1,n)
      dym = iv%info(radar)%dym(1,n)


      model_ps(n) = dxm *(dym * grid%xb % psac(i,  j) + dy * grid%xb%psac(i+1,  j)) + &
                 dx  *(dym * grid%xb % psac(i,j+1) + dy * grid%xb%psac(i+1,j+1)) + &
                 grid%xb % ptop

      iv%radar(n)%model_p(:)   = model_p(1:iv%info(radar)%levels(n),n)
      iv%radar(n)%model_rho(:) = model_rho(1:iv%info(radar)%levels(n),n)
      iv%radar(n)%model_qrn(:) = model_qrn(1:iv%info(radar)%levels(n),n)
      iv%radar(n)%model_ps     = model_ps(n)

      ! [3.0] Calculate rv, rf at OBS location and initialise components of &
      ! innovation vector:

      if (fg_format == fg_format_wrf_arw_regional .or. &
          fg_format == fg_format_wrf_arw_global ) then
         call da_llxy_wrf(map_info, &
            iv%radar(n)%stn_loc%lat, iv%radar(n)%stn_loc%lon, &
            iv%radar(n)%stn_loc%x,   iv%radar(n)%stn_loc%y)
      else
         call da_llxy_default( iv%radar(n)%stn_loc%lat, iv%radar(n)%stn_loc%lon, &
            iv%radar(n)%stn_loc%x,   iv%radar(n)%stn_loc%y)
      end if

      xr = grid%xb%ds *(iv%info(radar)%x(1,n) - iv%radar(n)%stn_loc%x)
      yr = grid%xb%ds *(iv%info(radar)%y(1,n) - iv%radar(n)%stn_loc%y)

      do k=1, iv%info(radar)%levels(n)
         iv % radar(n) % rv(k) % inv = 0.0
         iv % radar(n) % rf(k) % inv = 0.0

         if (iv % radar(n) % height_qc(k) /= below_model_surface .and.  &
             iv % radar(n) % height_qc(k) /= above_model_lid) then

            if (use_radar_rv) then
               if (abs(iv % radar(n) % rv(k) % qc - missing_data) > 1) then
                  if (abs(ob % radar(n) % rv(k) - missing_r) > 1.0 .AND. &
                       iv % radar(n) % rv(k) % qc >= obs_qc_pointer) then

                     zr=iv%radar(n)%height(k) - iv%radar(n)%stn_loc%elv

                     call da_radial_velocity(model_rv(k,n), model_p(k,n),  &
                        model_u(k,n), model_v(k,n), model_w(k,n),          &
                        model_qrn(k,n), model_ps(n), xr, yr, zr)

                     iv % radar(n) % rv(k) % inv = ob % radar(n) % rv(k) - model_rv(k,n)
                  end if
               end if
            end if

            if (use_radar_rf) then
               if (abs(iv % radar(n) % rf(k) % qc - missing_data) > 1) then
                  if (abs(ob % radar(n) % rf(k) - missing_r) > 1.0 .AND. &
                     iv % radar(n) % rf(k) % qc >= obs_qc_pointer) then
                     model_rf(k,n) = leh1 + leh2 * alog10(model_rho(k,n) * model_qrn(k,n) * 1.0e+3)
                     iv % radar(n) % rf(k) % inv = ob % radar(n) % rf(k) - model_rf(k,n)
                  end if
               end if
            end if
         end if
      end do
   end do

   !------------------------------------------------------------------------
   ! [4.0] Perform optional maximum error check:  
   !------------------------------------------------------------------------

   if (check_max_iv)  then
      call da_check_max_iv_radar(iv, it, irv, irf, irvf, irff)
   end if

   if (rootproc .and. check_max_iv_print) then
      write(unit = check_max_iv_unit, fmt ='(/,A,i5,A)')   &
         'For outer iteration ', it, ', Total Rejections for radar follows:'

      if (use_radar_rv) then
          write( unit = check_max_iv_unit, fmt = '(/,2(A,I6))') &
            'Number of failed rv observations:     ',irvf, ' on ',irv
      end if

      if (use_radar_rf) then
         write( unit = check_max_iv_unit, fmt = '(/,2(A,I6))') &
            'Number of failed rf observations:     ',irff, ' on ',irf
      end if
   end if

   deallocate (model_p)
   deallocate (model_u)
   deallocate (model_v)
   deallocate (model_w)
   deallocate (model_qrn)
   deallocate (model_rv)
   deallocate (model_rf)
   deallocate (model_ps)
   
   if (trace_use) call da_trace_exit("da_get_innov_vector_radar")

end subroutine da_get_innov_vector_radar


