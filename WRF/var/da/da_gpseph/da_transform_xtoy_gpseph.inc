subroutine da_transform_xtoy_gpseph (iv, y)

   !-------------------------------------------------------------------------
   ! Purpose: y = H(grid%xa)
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv       ! Innovation vector (O-B).
   type (y_type),  intent(inout) :: y        ! y = h (grid%xa)

   integer :: is, ie, js, je, ks, ke
   real, dimension(kds:kde)   :: mean_h   ! mean altitude in each level
   real, dimension(kds:kde)   :: mdl_z
   real, dimension(kds:kde)   :: mdl_ref, mdl_ref9
   real, dimension(kds:kde)   :: refm, refm9
   real, dimension(3,kds:kde) :: cc, cc9
   real, dimension(ids:ide,jds:jde,kds:kde) :: ref_mean_h_tl
   integer :: i, j, k, l, m, n, i1, i2, i3, nbot, ntop, nk, nn
   real :: step, h, tmp_ref
   real,dimension(2) :: w1
   real,dimension(2,2) :: w2
   integer :: ip1,ip2,ip3,je2
   real :: refp

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_gpseph")

   is = ids
   ie = ide
   js = jds
   je = jde
   ks = kds
   ke = kde
   nk = kde-kds+1

   step = gps_ray_path_step
   mean_h(:) = global_h_mean(:) !km

   ! calculate ref increment on mean_h from ref increment on model grid
   do j=js,je
      do i=is,ie

         mdl_z(kds:kde)    = global_h(i,j,kds:kde)
         mdl_ref(kds:kde)  = global_xa_ref(i,j,kds:kde)*(1./global_ref(i,j,kds:kde))
         mdl_ref9(kds:kde) = log(global_ref(i,j,kds:kde))

         call da_splinx_lin(nk,mdl_z,mdl_ref9,mdl_ref,cc9,cc,nk,mean_h*1000.,refm9,refm)

         ref_mean_h_tl(i,j,kds:kde) = refm(kds:kde)*exp(refm9(kds:kde))

      enddo
   enddo

   do n=iv%info(gpseph)%n1,iv%info(gpseph)%n2

      nbot = gps_rays(n)%nbot
      ntop = gps_rays(n)%ntop
      if ( nbot == 0 .and. ntop == 0 ) cycle

      do i=nbot,ntop
         if ( iv%gpseph(n)%eph(i)%qc < obs_qc_pointer ) cycle
         y%gpseph(n)%eph(i)=0.0

         if ( gps_rays(n)%ilocal(i)==1 ) then

            je2 = gps_rays(n)%je2(i)
            do j=1,je2
               do k=-1, 1, 2
                  h = gps_rays(n)%ip123(i)%h(k*j)
                  if ( h <= mean_h(ke-1) ) then
                     i1 = gps_rays(n)%ip123(i)%i1(k*j)
                     i2 = gps_rays(n)%ip123(i)%i2(k*j)
                     i3 = gps_rays(n)%ip123(i)%i3(k*j)
                     w1 = gps_rays(n)%ip123(i)%w1(1:2,k*j)
                     w2 = gps_rays(n)%ip123(i)%w2(1:2,1:2,k*j)
                     tmp_ref=0.0
                     do l=1,2
                        do m=1,2
                           do nn=1,2
                              tmp_ref=tmp_ref+ref_mean_h_tl(i1+l-1,i2+m-1,i3+nn-1)*w2(l,m)*w1(nn)
                           end do
                        end do
                     end do
                     y%gpseph(n)%eph(i)=y%gpseph(n)%eph(i)+step*tmp_ref
                  end if
               end do
            end do

         else if ( gps_rays(n)%ilocal(i)==0 ) then

            ip1 = gps_rays(n)%ip123(i)%i1(0)
            ip2 = gps_rays(n)%ip123(i)%i2(0)
            ip3 = gps_rays(n)%ip123(i)%i3(0)
            w1 = gps_rays(n)%ip123(i)%w1(1:2,0)
            w2 = gps_rays(n)%ip123(i)%w2(1:2,1:2,0)

            refp=0.
            do l=1,2
               do m=1,2
                  do nn=1,2
                     refp=refp+ref_mean_h_tl(ip1+l-1,ip2+m-1,ip3+nn-1)*w2(l,m)*w1(nn)
                  end do
               end do
            end do
            y%gpseph(n)%eph(i)=step*refp

         end if

      end do !nbot-ntop loop
   end do !n1-n2 loop

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_gpseph")

end subroutine da_transform_xtoy_gpseph

