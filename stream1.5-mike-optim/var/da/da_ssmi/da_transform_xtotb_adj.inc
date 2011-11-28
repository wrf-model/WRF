subroutine da_transform_xtotb_adj (grid)

   !----------------------------------------------------------------------
   ! Purpose: TBD
   !----------------------------------------------------------------------

   implicit none

   type (domain), intent(inout) :: grid

   integer :: i,j,k
   integer :: is, js, ie, je

   real    :: dx, dy, dxm, dym, zhmkz
   real    :: dum1, dum2, zrhom, ADJ_zrhom

   real    :: psfc,ta,gamma,sst,htpw,speed,alw,zcld,tpw
   real    :: ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,ADJ_tpw
   real    :: ADJ_htpw,ADJ_speed,ADJ_alw,ADJ_zcld        

   if (trace_use) call da_trace_entry("da_transform_xtotb_adj")        

   psfc      = 0.0
   ta        = 0.0
   gamma     = 0.0
   sst       = 0.0
   htpw      = 0.0
   speed     = 0.0
   alw       = 0.0
   zcld      = 0.0
   tpw       = 0.0
   dx        = 0.0
   dy        = 0.0
   dxm       = 0.0
   dym       = 0.0
   zhmkz     = 0.0
   dum1      = 0.0
   dum2      = 0.0
   zrhom     = 0.0
   ADJ_zrhom = 0.0

   is = its
   js = jts

   ie = ite
   je = jte

   if (test_transforms) then
      is = its-1
      js = jts-1

      ie = ite+1
      je = jte+1

      if (is < ids) is = ids
      if (js < jds) js = jds

      if (ie > ide) ie = ide
      if (je > jde) je = jde
   end if

   ! Mean fields

   do j=js, je
      do i=is, ie
         psfc  = 0.01*grid%xb%psfc(i,j)
         ! sst   = grid%xb%tgrn(i,j)
         ta    = grid%xb%tgrn(i,j) + &
                 (grid%xb%t(i,j,kts)-grid%xb%tgrn(i,j))*log(2.0/0.0001)/ &
                 log((grid%xb%h(i,j,kts)- grid%xb%terr(i,j))/0.0001)

         gamma = (ta-270.0)*0.023 + 5.03  ! effective lapse rate   (km) (4.0-6.5)
         zcld  = 1                      ! effective cloud height (km)

         tpw   = grid%xb%tpw(i,j)*10.0
         ! speed = grid%xb%speed(i,j)

         alw   = 0.0

         zrhom = 0.0
         do k=kts,kte
            zrhom=zrhom+(grid%xb%hf(i,j,k+1)-grid%xb%hf(i,j,k))*grid%xb%h(i,j,k)*grid%xb%q(i,j,k)* &
               grid%xb%rho(i,j,k)
         end do

         ! call da_transform_xtozrhoq(grid%xb, i, j, zh, zf, zrhom)

         htpw    = zrhom/tpw/1000.0

         dum1=0.0
         dum2=0.0

         ADJ_gamma    = 0.0
         ADJ_speed    = 0.0
         ADJ_psfc     = 0.0
         ADJ_zcld     = 0.0
         ADJ_htpw     = 0.0
         ADJ_sst      = 0.0
         ADJ_alw      = 0.0
         ADJ_tpw      = 0.0
         ADJ_ta       = 0.0
         ADJ_zrhom    = 0.0

         call da_tb_adj(1,53.0,psfc,ta,gamma,grid%xb%tgrn(i,j),tpw,      &
            htpw,grid%xb%speed(i,j),alw,zcld,               &
            ! grid%xb%tb19v(i,j),grid%xb%tb19h(i,j),               &
            ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,         &
            ADJ_tpw,ADJ_htpw,ADJ_speed,ADJ_alw,        &
            ADJ_zcld,grid%xa%tb19v(i,j),grid%xa%tb19h(i,j)    )

         call da_tb_adj(2,53.0,psfc,ta,gamma,grid%xb%tgrn(i,j),tpw,      &
            htpw,grid%xb%speed(i,j),alw,zcld,               &
            ! grid%xb%tb22v(i,j),dum1,                        &
            ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,         &
            ADJ_tpw,ADJ_htpw,ADJ_speed,ADJ_alw,        &
            ADJ_zcld,grid%xa%tb22v(i,j),dum2              )

         call da_tb_adj(3,53.0,psfc,ta,gamma,grid%xb%tgrn(i,j),tpw,      &
            htpw,grid%xb%speed(i,j),alw,zcld,               &
            ! grid%xb%tb37v(i,j),grid%xb%tb37h(i,j),               &
            ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,         &
            ADJ_tpw,ADJ_htpw,ADJ_speed,ADJ_alw,        &
            ADJ_zcld,grid%xa%tb37v(i,j),grid%xa%tb37h(i,j)    )

         call da_tb_adj(4,53.0,psfc,ta,gamma,grid%xb%tgrn(i,j),tpw,      &
            htpw,grid%xb%speed(i,j),alw,zcld,               &
            ! grid%xb%tb85v(i,j),grid%xb%tb85h(i,j),               &
            ADJ_psfc,ADJ_ta,ADJ_gamma,ADJ_sst,         &
            ADJ_tpw,ADJ_htpw,ADJ_speed,ADJ_alw,        &
            ADJ_zcld,grid%xa%tb85v(i,j),grid%xa%tb85h(i,j)    )

         ADJ_zrhom    = ADJ_htpw/tpw/1000.0
         ADJ_tpw      = ADJ_tpw - ADJ_htpw/tpw*htpw

         do k = kts,kte
            grid%xa%rho(i,j,k) = (grid%xb%hf(i,j,k+1)-grid%xb%hf(i,j,k))*grid%xb%h(i,j,k)* &
               grid%xb%q(i,j,k)*ADJ_zrhom + grid%xa%rho(i,j,k)
            grid%xa%q(i,j,k)   = (grid%xb%hf(i,j,k+1)-grid%xb%hf(i,j,k))*grid%xb%h(i,j,k)* &
               ADJ_zrhom*grid%xb%rho(i,j,k) + grid%xa%q(i,j,k)
         end do

         ! call da_transform_xtozrhoq_adj(grid%xb,grid%xa,i,j,zh,zf,ADJ_zrhom)

         ADJ_alw = 0.0

         grid%xa%speed(i,j)=grid%xa%speed(i,j) + ADJ_speed

         grid%xa%tpw(i,j) = grid%xa%tpw(i,j) + ADJ_tpw*10.0

         ADJ_zcld= 0
         ADJ_ta  = ADJ_ta + ADJ_gamma*0.023

         grid%xa%t(i,j,kts) = grid%xa%t(i,j,kts) + ADJ_ta* &
                   log(2.0/0.0001)/log((grid%xb%h(i,j,kts)-grid%xb%terr(i,j))/0.0001)
         ADJ_sst = 0.0

         grid%xa%psfc(i,j) = grid%xa%psfc(i,j) + ADJ_psfc*0.01 
      end do
   end do   

   if (trace_use) call da_trace_exit("da_transform_xtotb_adj") 

end subroutine da_transform_xtotb_adj


