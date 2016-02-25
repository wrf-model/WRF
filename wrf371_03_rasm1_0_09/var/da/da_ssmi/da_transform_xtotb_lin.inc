subroutine da_transform_xtotb_lin(grid)

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   !-------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout) :: grid

   integer :: i,j,k
   real    :: dum1, dum2, zrhom, TGL_zrhom

   real    :: psfc,ta,gamma,sst,htpw,speed,alw,zcld,tpw
   real    :: TGL_psfc,TGL_ta,TGL_gamma,TGL_sst,TGL_tpw
   real    :: TGL_htpw,TGL_speed,TGL_alw,TGL_zcld         

   if (trace_use) call da_trace_entry("da_transform_xtotb_lin")

   ! WHY?
   ! call da_transform_xtotpw(grid)

   do j=jts,jte
      do i=its,ite
         ! surface pressure (mb) (940 -1030)

         psfc     =  0.01*grid%xb%psfc(i,j)
         TGL_psfc =  0.01*grid%xa%psfc(i,j)

         ! sea surface temperature (k) (273 - 303) (doesnot change) 

         sst      = grid%xb%tgrn(i,j)
         ! TGL_sst  = grid%xa%tgrn(1,1)
         TGL_sst  = 0.0

         ! effective surface air temperature (263 - 303)

         ta          = grid%xb%tgrn(i,j) + &
                    (grid%xb%t(i,j,kts)-grid%xb%tgrn(i,j))*log(2.0/0.0001)/ &
                    log((grid%xb%h(i,j,kts)-grid%xb%terr(i,j))/0.0001)

         TGL_ta      = (grid%xa%t(i,j,kts)-0.)*log(2.0/0.0001)/ &
                    log((grid%xb%h(i,j,kts)-grid%xb%terr(i,j))/0.0001)

         ! gamma is an emperical formula and zcld is given for now

         gamma = (ta-270)*0.023 + 5.03  ! effective lapse rate (km) (4.0 - 6.5)
         zcld       = 1                           ! effective cloud height (km)
                                                 ! = 1 if no cloud infomation
         TGL_gamma = TGL_ta*0.023
         TGL_zcld  = 0.0
     

         ! total precipitable water in (kg/m**2) (0 - 70)

         tpw     = grid%xb%tpw(i,j)*10.0
         TGL_tpw = grid%xa%tpw(i,j)*10.0

         ! speed, surface wind speed (m/sec)    (0 - 30) , take 10 m wind

         speed     = grid%xb%speed(i,j)
         TGL_speed = grid%xa%speed(i,j)

         ! Column liquid water (kg/m**2)   (0 - 0.5) (no data now. So, do it later.)

         alw     = 0.0
         TGL_alw = 0.0

         ! Column height weighted moisture density on the grid locally 

         zrhom = 0.0
         do k=kts,kte
            zrhom=zrhom+(grid%xb%hf(i,j,k+1)-grid%xb%hf(i,j,k))*grid%xb%h(i,j,k)* &
               grid%xb%q(i,j,k)*grid%xb%rho(i,j,k)
         end do

         TGL_zrhom = 0.0
         do k = kts,kte
            TGL_zrhom = (grid%xb%hf(i,j,k+1)-grid%xb%hf(i,j,k))*grid%xb%h(i,j,k)* &
               (grid%xb%q(i,j,k)*grid%xa%rho(i,j,k) + &
               grid%xa%q(i,j,k)*grid%xb%rho(i,j,k)) + TGL_zrhom
         end do

         ! WHY?
         ! call da_transform_xtozrhoq(grid%xb, i, j, zh, zf, zrhom)
         ! call da_transform_xtozrhoq_lin(grid, i, j, zh, zf, TGL_zrhom)

         ! Column moisture density on the grid locally

         htpw     = zrhom/tpw/1000.0
         TGL_htpw = TGL_zrhom/tpw/1000.0 &
                  - TGL_tpw/tpw*htpw

         dum1 = 0.0

         call da_tb_tl(1,53.0,psfc,ta,gamma,sst,tpw,htpw,speed,alw,zcld,  &
            ! grid%xb%tb19v(i,j),grid%xb%tb19h(i,j),                      &
            TGL_psfc,TGL_ta,TGL_gamma,TGL_sst,                &
            TGL_tpw,TGL_htpw,TGL_speed,TGL_alw,               &
            TGL_zcld,grid%xa%tb19v(i,j),grid%xa%tb19h(i,j)             )

         call da_tb_tl(2,53.0,psfc,ta,gamma,sst,tpw,htpw,speed,alw,zcld,  &
            ! grid%xb%tb22v(i,j),dum1,                               &
            TGL_psfc,TGL_ta,TGL_gamma,TGL_sst,                &
            TGL_tpw,TGL_htpw,TGL_speed,TGL_alw,               &
            TGL_zcld,grid%xa%tb22v(i,j),dum2                      )

         call da_tb_tl(3,53.0,psfc,ta,gamma,sst,tpw,htpw,speed,alw,zcld,  &
            ! grid%xb%tb37v(i,j),grid%xb%tb37h(i,j),                      &
            TGL_psfc,TGL_ta,TGL_gamma,TGL_sst,                &
            TGL_tpw,TGL_htpw,TGL_speed,TGL_alw,               &
            TGL_zcld,grid%xa%tb37v(i,j),grid%xa%tb37h(i,j)             )

         call da_tb_tl(4,53.0,psfc,ta,gamma,sst,tpw,htpw,speed,alw,zcld,  &
            ! grid%xb%tb85v(i,j),grid%xb%tb85h(i,j),                      &
            TGL_psfc,TGL_ta,TGL_gamma,TGL_sst,                &
            TGL_tpw,TGL_htpw,TGL_speed,TGL_alw,               &
            TGL_zcld,grid%xa%tb85v(i,j),grid%xa%tb85h(i,j)             )
      end do
   end do       

   if (trace_use) call da_trace_exit("da_transform_xtotb_lin")

end subroutine da_transform_xtotb_lin


