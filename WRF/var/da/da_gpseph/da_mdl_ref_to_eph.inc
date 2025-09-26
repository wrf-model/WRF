SUBROUTINE da_mdl_ref_to_eph (gps_rays, ref_mean_h, model_eph)

!---------------------------------------------------------------------------------------
!  Purpose: calculate model excess phase
!           (Chen et al., 2009, doi:10.3319/TAO.2007.11.29.01(F3C))
!           It is called by da_get_innov_vector_gpseph.
!---------------------------------------------------------------------------------------

   implicit none
   ! input : gps_rays: gps rays information
   !         ref_mean_h: model refractivity on mean height
   ! output : model_eph

   type(gpsrays_type),                     intent(in)  :: gps_rays
   real,dimension(ids:ide,jds:jde,kds:kde),intent(in)  :: ref_mean_h
   real,dimension(kds:kde),                intent(out) :: model_eph

   real,dimension(kds:kde) :: mean_h
   integer :: i, j, k, l, m, nn, i1, i2, i3, nbot, ntop
   integer :: is, ie, js, je, ks, ke
   integer :: i1l,i2l,i1r,i2r
   real :: step, h, tmp_ref
   real,dimension(2)   :: w1
   real,dimension(2,2) :: w2
   integer :: ip1,ip2,ip3,je2
   real :: refp

   step = gps_ray_path_step
   ks = kds
   ke = kde
   nbot = gps_rays%nbot
   ntop = gps_rays%ntop
   mean_h = global_h_mean(:) !km

   do i = nbot, ntop

      ! use the ilocal decided in obs_ref_to_eph for consistency
      ! between obs and model eph calculation

      if ( gps_rays%ilocal(i) == 0 ) then ! local

         ip1 = gps_rays%ip123(i)%i1(0)
         ip2 = gps_rays%ip123(i)%i2(0)
         ip3 = gps_rays%ip123(i)%i3(0)
         w1  = gps_rays%ip123(i)%w1(1:2,0)
         w2  = gps_rays%ip123(i)%w2(1:2,1:2,0)
         refp=0.
         do l=1,2
            do m=1,2
               do nn=1,2
                  refp=refp+ref_mean_h(ip1+l-1,ip2+m-1,ip3+nn-1)*w2(l,m)*w1(nn)
               end do
            end do
         end do

         model_eph(i)=step*refp

      else ! not local

         model_eph(i)=0.
         je2 = gps_rays%je2(i)
         do j=1,je2
            !* calculate S from TP point and integrate to different direction (WRF)
            !* S is asymmetric
            do k=-1,+1,2
               ! transform coordiante from cartesian to sphere coordinate
               h = gps_rays%ip123(i)%h(k*j)
               if (h <= mean_h(ke-1)) then
                  i1 = gps_rays%ip123(i)%i1(k*j)
                  i2 = gps_rays%ip123(i)%i2(k*j)
                  i3 = gps_rays%ip123(i)%i3(k*j)
                  w1 = gps_rays%ip123(i)%w1(1:2,k*j)
                  w2 = gps_rays%ip123(i)%w2(1:2,1:2,k*j)
                  tmp_ref=0.
                  do l=1,2
                     do m=1,2
                        do nn=1,2
                           tmp_ref=tmp_ref+ref_mean_h(i1+l-1,i2+m-1,i3+nn-1)*w2(l,m)*w1(nn)
                        end do
                     end do
                  end do
                  model_eph(i)=model_eph(i)+step*tmp_ref
               end if
            end do !k= -1, 1 loop
         end do !je2 loop

      end if

   end do !kbot, ntop loop

END SUBROUTINE da_mdl_ref_to_eph
