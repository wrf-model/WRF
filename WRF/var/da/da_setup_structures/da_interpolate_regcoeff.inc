subroutine da_interpolate_regcoeff (iy, iys, kz, kzs, meanl_stats, meanl_xb, meanp_stats, meanp_xb, &
   pb_vert_reg_stats, pb_vert_reg)

   !---------------------------------------------------------------------------
   ! Purpose: Interpolate statistical regression coefficient to new domain.
   !
   ! Method:  i,k,k Interpolation.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: iy                       ! Number of rows in  xb.
   integer, intent(in)  :: iys                      ! Number of rows in stats.
   integer, intent(in)  :: kz                       ! Number of levels in xb.
   integer, intent(in)  :: kzs                      ! Number of levels in stats.
   real,    intent(in)  :: meanl_stats(:)           ! Mean latitude on stats rows.
   real,    intent(in)  :: meanl_xb(:)              ! Mean latitude on xb rows.
   real,    intent(in)  :: meanp_stats(:)           ! Mean pressure on stats levs.
   real,    intent(in)  :: meanp_xb(:)              ! Mean pressure on xb levs.
   real*8,  intent(in)  :: pb_vert_reg_stats(:,:,:) ! Coefficient on stats grid.
   real*8,  intent(out) :: pb_vert_reg(:,:,:)       ! Coefficient on xb grid.
     
   integer :: i, is, i_south           ! Loop counters.
   integer :: k1, k2, k, ks            ! Loop counters.
   integer :: k1s, k2s
   real    :: lat_wgt

   integer :: k_above(1:kz)
   real    :: pb_vert_reg_temp(1:iys,1:kz,1:kz)
   real    :: weight(1:kz)

   if (trace_use) call da_trace_entry("da_interpolate_regcoeff")

   pb_vert_reg = 0.0

   !------------------------------------------------------------------------
   ! [1.0] Find xb levels/rows bounded by stats domain:
   !------------------------------------------------------------------------

   do k = 1, kz
      if (meanp_xb(k) <= meanp_stats(1)) then
         weight(k) = 1.0e-6
         k_above(k) = 1
      else if (meanp_xb(k) >= meanp_stats(kzs)) then
         weight(k) = 1.0-1.0e-6
         k_above(k) = kzs-1
      else
         do ks = 1, kzs-1
            if (meanp_xb(k) >= meanp_stats(ks) .AND. meanp_xb(k) <= meanp_stats(ks+1)) then
               weight(k) = (meanp_xb(k) - meanp_stats(ks)) / (meanp_stats(ks+1) - meanp_stats(ks))
               k_above(k) = ks
               exit
            end if
         end do
      end if
   end do

   !------------------------------------------------------------------------   
   ! [3.0] Interpolate regression coefficient from stats to xb levels:
   !------------------------------------------------------------------------

   pb_vert_reg_temp(1:iys,1:kz,1:kz) = 0.0

   do is = 1, iys
      do k1 = 1, kz
         k1s = k_above(k1)
         do k2 = 1, kz
            k2s = k_above(k2)

            pb_vert_reg_temp(is,k1,k2) = (1.0-weight(k1)) * (1.0-weight(k2)) * &
                                         pb_vert_reg_stats(is,k1s,k2s) + &
                                         weight(k1) * (1.0-weight(k2)) * &
                                         pb_vert_reg_stats(is,k1s+1,k2s) + &
                                         weight(k2) * (1.0-weight(k1)) * &
                                         pb_vert_reg_stats(is,k1s,k2s+1) + &
                                         weight(k2) * weight(k1) * &
                                         pb_vert_reg_stats(is,k1s+1,k2s+1)
         end do
      end do     
   end do
         
   !------------------------------------------------------------------------
   ! [4.0] Interpolate to from statistics latitudes to xb latitudes:
   !------------------------------------------------------------------------

   i_south = 2

   do i = 1, iy
   
      ! Find position of xb latitude in statistics rows:

      if (meanl_xb(i) <= meanl_stats(2)) then
         i_south = 2
         lat_wgt = 0.5
      else if (meanl_xb(i) >= meanl_stats(iys-1)) then
         i_south = iys-2
         lat_wgt = 0.5
      else
         do is = 1, iys-1
            if (meanl_xb(i) >= meanl_stats(is) .AND. meanl_xb(i) <= meanl_stats(is+1)) then

               lat_wgt = (meanl_xb(i) - meanl_stats(is)) / (meanl_stats(is+1) - meanl_stats(is))
               i_south = is
               exit
            end if
         end do
      end if
   
      do k1 = 1, kz
         do k2 = 1, kz
            pb_vert_reg(i,k1,k2) = lat_wgt * pb_vert_reg_temp(i_south+1,k1,k2) + &
               (1.0 - lat_wgt) * pb_vert_reg_temp(i_south,k1,k2)
         end do
      end do     
   end do

   if (print_detail_regression) then
      call da_array_print(1, pb_vert_reg_stats(1,:,:), 'pb_vert_reg_stats(1,:,:)')
      call da_array_print(1, pb_vert_reg(1,:,:),       'pb_vert_reg(1,:,:)')
      call da_array_print(1, pb_vert_reg_stats(:,1,:), 'pb_vert_reg_stats(:,1,:)')
      call da_array_print(1, pb_vert_reg(:,1,:),       'pb_vert_reg(:,1,:)')
   end if

   if (trace_use) call da_trace_exit("da_interpolate_regcoeff")
   
end subroutine da_interpolate_regcoeff


