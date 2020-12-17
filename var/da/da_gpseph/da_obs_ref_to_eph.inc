SUBROUTINE da_obs_ref_to_eph ( gps_rays, obs_ref, obs_eph)

!---------------------------------------------------------------------------------------
!  Purpose: calculate observation excess phase
!           (Chen et al., 2009, doi:10.3319/TAO.2007.11.29.01(F3C))
!           It is called by da_get_innov_vector_gpseph.
!---------------------------------------------------------------------------------------

   implicit none

   type(gpsrays_type),  intent(inout) :: gps_rays
   real,                intent(in)    :: obs_ref(kds:kde)
   real,                intent(inout) :: obs_eph(kds:kde)

   real, dimension(kds:kde) :: mean_h
   integer :: i, j, k, l, m, nn, i1, i2, i3, nbot, ntop
   integer :: is, ie, js, je, ks, ke
   real :: step, h, tmp_ref
   real,dimension(3):: rp, rn, rr, rx
   real,dimension(2) :: w1
   real,dimension(2,2) :: w2
   integer :: ip1,ip2,ip3,je2
   real :: refp

   step = gps_ray_path_step
   ks = kds
   ke = kde
   nbot = gps_rays%nbot
   ntop = gps_rays%ntop
   mean_h = global_h_mean(:) !km

   obs_eph(:)=0.0

   do i=nbot,ntop

      !h = gps_rays%ip123(i)%h(0)
      ip1 = gps_rays%ip123(i)%i1(0)
      ip2 = gps_rays%ip123(i)%i2(0)
      ip3 = gps_rays%ip123(i)%i3(0)
      w1  = gps_rays%ip123(i)%w1(1:2,0)
      w2  = gps_rays%ip123(i)%w2(1:2,1:2,0)

      refp=0.
      do l=1,2
         do m=1,2
            do nn=1,2
               refp=refp+obs_ref(ip3+nn-1)*w2(l,m)*w1(nn)
            end do
         end do
      end do

      !if ( .not. gpseph_local ) then
      if ( gpseph_opt > 0 ) then
         gps_rays%ilocal(i)=1
         obs_eph(i)=0.
         je2 = gps_rays%je2(i)
         DO j=1,je2
            do k=-1,+1,2
               h = gps_rays%ip123(i)%h(k*j)
               IF (h <= mean_h(ke-1))then
                  i1 = gps_rays%ip123(i)%i1(k*j)
                  i2 = gps_rays%ip123(i)%i2(k*j)
                  i3 = gps_rays%ip123(i)%i3(k*j)
                  w1 = gps_rays%ip123(i)%w1(1:2,k*j)
                  w2 = gps_rays%ip123(i)%w2(1:2,1:2,k*j)
                  tmp_ref=0.
                  do l=1,2
                     do m=1,2
                        do nn=1,2
                           tmp_ref=tmp_ref+obs_ref(i3+nn-1)*w2(l,m)*w1(nn)
                        end do
                     end do
                  end do
                  obs_eph(i)=obs_eph(i)+step*tmp_ref
               END IF
            end do ! from left to right
         END DO !from the begining to the end
      end if

      !if ( obs_eph(i) == 0. .or. gpseph_local ) then
      if ( obs_eph(i) == 0. .or. gpseph_opt == 0 ) then
         gps_rays%ilocal(i)=0 !set the flag of if_nonlocal
         obs_eph(i)=step*refp
      end if

   end do !nbot, ntop loop

END SUBROUTINE da_obs_ref_to_eph
