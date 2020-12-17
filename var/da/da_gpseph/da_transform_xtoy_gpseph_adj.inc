subroutine da_transform_xtoy_gpseph_adj(iv, jo_grad_y, jo_grad_x)

   !-------------------------------------------------------------------------
   ! Purpose: adjoint of y = H(grid%xa)
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)

   REAL, DIMENSION(kts:kte)   :: tmp_refm_adj
   REAL, DIMENSION(kts:kte)   :: tmp_ref_adj
   REAL, DIMENSION(kts:kte)   :: tmp_z
   REAL, DIMENSION(kts:kte)   :: tmp_ref_fw
   REAL, DIMENSION(kts:kte)   :: tmp_z_fw
   REAL, DIMENSION(kts:kte)   :: tmp_ref
   REAL, DIMENSION(kts:kte)   :: refm
   REAL, DIMENSION(kts:kte)   :: tmp_refm
   REAL, DIMENSION(kts:kte)   :: tmp_refm_fw
   REAL, DIMENSION(3,kts:kte) :: cc, a_cc
   REAL, DIMENSION(ids:ide,jds:jde,kds:kde) :: ref_keep
   REAL, DIMENSION(ids:ide,jds:jde,kds:kde) :: ref_mean_h
   REAL, DIMENSION(ids:ide,jds:jde,kds:kde) :: ref_mean_h_adj
   REAL, DIMENSION(ids:ide,jds:jde,kds:kde) :: refm_keep
   real, dimension(kds:kde)  :: mean_h  ! mean altitude in each level of model domain
   integer :: i, j, k, l, m, n, i1, i2, i3, nbot, ntop
   integer :: is, ie, js, je, ks, ke
   integer :: nk, nn
   real :: step, h
   real :: a_rfr
   real,dimension(2) :: w1
   real,dimension(2,2) :: w2
   integer :: ip1,ip2,ip3,je2

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_gpseph_adj")

   is=ids
   ie=ide
   js=jds
   je=jde
   ks=kds
   ke=kde
   nk = kde-kds+1
   step = gps_ray_path_step
   mean_h(:) = global_h_mean(:) !km

   do n=iv%info(gpseph)%n1,iv%info(gpseph)%n2
      a_rfr = 0.
      ref_mean_h_adj(:,:,:) = 0.0
      nbot = gps_rays(n)%nbot
      ntop = gps_rays(n)%ntop
      if ( nbot == 0 .and. ntop == 0 ) cycle
      do i = nbot, ntop
         if ( iv%gpseph(n)%eph(i)%qc < obs_qc_pointer ) cycle

         if ( gps_rays(n)%ilocal(i)==1 ) then
            je2 = gps_rays(n)%je2(i)
            do j = 1, je2
               do k=-1,+1,2
                  h = gps_rays(n)%ip123(i)%h(k*j)
                  if ( h <= mean_h(ke-1) ) then
                     i1 = gps_rays(n)%ip123(i)%i1(k*j)
                     i2 = gps_rays(n)%ip123(i)%i2(k*j)
                     i3 = gps_rays(n)%ip123(i)%i3(k*j)
                     w1 = gps_rays(n)%ip123(i)%w1(1:2,k*j)
                     w2 = gps_rays(n)%ip123(i)%w2(1:2,1:2,k*j)
                     a_rfr = a_rfr + step * jo_grad_y%gpseph(n)%eph(i)
                     do l=1,2
                        do m=1,2
                           do nn=1,2
                              ref_mean_h_adj(i1+l-1,i2+m-1,i3+nn-1) =    &
                                 ref_mean_h_adj(i1+l-1,i2+m-1,i3+nn-1) + &
                                 w2(l,m)*w1(nn)*a_rfr
                           enddo
                        enddo
                     enddo
                     a_rfr = 0.0
                  end if
               end do !-1,+1
            end do !je2
            jo_grad_y%gpseph(n)%eph(i) = 0.

         else if (gps_rays(n)%ilocal(i)==0) then

            ip1 = gps_rays(n)%ip123(i)%i1(0)
            ip2 = gps_rays(n)%ip123(i)%i2(0)
            ip3 = gps_rays(n)%ip123(i)%i3(0)
            w1 = gps_rays(n)%ip123(i)%w1(1:2,0)
            w2 = gps_rays(n)%ip123(i)%w2(1:2,1:2,0)

            a_rfr = a_rfr + step * jo_grad_y%gpseph(n)%eph(i)
            do l=1,2
               do m=1,2
                  do nn=1,2
                     ref_mean_h_adj(ip1+l-1,ip2+m-1,ip3+nn-1) =    &
                        ref_mean_h_adj(ip1+l-1,ip2+m-1,ip3+nn-1) + &
                        w2(l,m)*w1(nn)*a_rfr
                  enddo
               enddo
            enddo
            a_rfr = 0.0
            jo_grad_y%gpseph(n)%eph(i) = 0.

         endif

      enddo !nbot-ntop loop

      tmp_ref_adj(:) = 0.0
      tmp_refm_adj(:) = 0.0
      a_cc(:,:) = 0.0

      ! Interpolation of refractivity on the mean altitude grid (basic)

      do j=js,je
         do i=is,ie

            do k=ks,ke
               tmp_z_fw(k)   = global_h(i,j,k)
               tmp_ref_fw(k) = log(global_ref(i,j,k))
            end do

            CALL da_splinx(nk,tmp_z_fw,tmp_ref_fw,nk,mean_h*1000.,cc,tmp_refm_fw)

            do k=ks,ke
               refm_keep(i,j,k)  = tmp_refm_fw(k)
               ref_mean_h(i,j,k) = exp(tmp_refm_fw(k))
            end do

         end do
      end do

      do j = je,js,-1
         do i = ie,is,-1
            do k = ke,ks,-1
               refm(k)=refm_keep(i,j,k)
               tmp_refm_adj(k) = tmp_refm_adj(k)+ref_mean_h_adj(i,j,k)*exp(refm(k))
               ref_mean_h_adj(i,j,k) = 0.0
            end do

            do k=ks,ke
               tmp_z(k)   = global_h(i,j,k)
               tmp_ref(k) = log(global_ref(i,j,k))
            end do

            call da_splinx_adj( nk,tmp_z,tmp_ref,tmp_ref_adj,nk,mean_h*1000.,cc,a_cc,tmp_refm_adj )

            do k =ke,ks,-1
               !jo_grad_x%ref(i,j,k) = jo_grad_x%ref(i,j,k)+tmp_ref_adj(k)*(1./global_ref(i,j,k))
               global_adj_ref(i,j,k) = global_adj_ref(i,j,k)+tmp_ref_adj(k)*(1./global_ref(i,j,k))
               tmp_ref_adj(k) = 0.
            end do
          end do
       end do

    enddo !n1-n2 loop

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_gpseph_adj")

end subroutine da_transform_xtoy_gpseph_adj

