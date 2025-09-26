subroutine da_transform_xtotb(grid)

   !----------------------------------------------------------------------
   ! Purpose: TBD
   !----------------------------------------------------------------------

   implicit none

   type (domain), intent(inout)   :: grid

   integer            :: i,j,k

   real               :: psfc,ta,gamma,sst,htpw,speed,alw,zcld,tpw,dum1,zrhom

   if (trace_use) call da_trace_entry("da_transform_xtotb")

   do j=jts,jte
      do i=its,ite
         ! surface pressure (mb) (940 -1030)

         psfc          = 0.01*grid%xb%psfc(i,j)

         ! sea surface temperature (k) (273 - 303) (doesnot change) 

         sst           = grid%xb%tgrn(i,j)

         ! effective surface air temperature (263 - 303)

         ta          = grid%xb%tgrn(i,j) + &
                       (grid%xb%t(i,j,kts)-grid%xb%tgrn(i,j))*log(2.0/0.0001)/ &
                       log((grid%xb%h(i,j,kts) - grid%xb%terr(i,j))/0.0001)

         ! gamma is an emperical formula and zcld is given for now

         gamma   = (ta-270)*0.023 + 5.03  ! effective lapse rate(km) (4.0-6.5)

         zcld    = 1                           ! effective cloud height (km)
                                               ! = 1 if no cloud infomation
         ! total precipitable water in cm
         ! total precipitable water in (kg/m**2) (0 - 70)

         tpw          = grid%xb%tpw(i,j)*10.0
         speed        = grid%xb%speed(i,j)

         ! Column liquid water (kg/m**2)  (0-0.5) (no data now. So do it later)

         alw          = 0.0

         ! Column height weighted moisture density on the grid locally 

         zrhom = 0.0
         do k=kts,kte
            zrhom=zrhom+(grid%xb%hf(i,j,k+1)-grid%xb%hf(i,j,k))*grid%xb%h(i,j,k)*grid%xb%q(i,j,k)* &
               grid%xb%rho(i,j,k)
         end do

         ! Column moisture density on the grid locally

         htpw          = zrhom/tpw/1000.0

         call tb(1,53.0,psfc,ta,gamma,sst,tpw,htpw, &
            speed,alw,zcld,grid%xb%tb19v(i,j),grid%xb%tb19h(i,j))
         call tb(2,53.0,psfc,ta,gamma,sst,tpw,htpw, &
            speed,alw,zcld,grid%xb%tb22v(i,j),dum1)
         call tb(3,53.0,psfc,ta,gamma,sst,tpw,htpw, &
            speed,alw,zcld,grid%xb%tb37v(i,j),grid%xb%tb37h(i,j))
         call tb(4,53.0,psfc,ta,gamma,sst,tpw,htpw, &
            speed,alw,zcld,grid%xb%tb85v(i,j),grid%xb%tb85h(i,j))
      end do
   end do

   if (trace_use) call da_trace_exit("da_transform_xtotb")

end subroutine da_transform_xtotb


