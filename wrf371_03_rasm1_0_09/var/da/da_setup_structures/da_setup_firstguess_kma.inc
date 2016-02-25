subroutine da_setup_firstguess_kma(xbx, grid)

   !---------------------------------------------------------------------------
   ! Purpose: Define/allocate components of WRF model state.
   !---------------------------------------------------------------------------

   implicit none

   type (xbx_type),intent(out)         :: xbx    ! Header & non-gridded vars.

   type (domain), intent(inout)        :: grid

   integer           :: i, j
   integer           :: is, ie, js, je
   integer           :: max_wavenumber

   if (trace_use) call da_trace_entry("da_setup_firstguess_kma")

   is = grid%xp % its
   ie = grid%xp % ite
   js = grid%xp % jts
   je = grid%xp % jte

   !------------------------------------------------------------------------
   ! [2.0] Copy header info:
   !------------------------------------------------------------------------

   ! rizvi set it to 1 . Actually it should be decided by KMA 
   grid%map_proj = 0
   map_projection = grid%map_proj
   coarse_ix = grid%e_we - grid%s_we + 1
   coarse_jy = grid%e_sn - grid%s_sn + 1

   grid%xb % mix = grid%xp%ide - grid%xp%ids + 1
   grid%xb % mjy = grid%xp%jde - grid%xp%jds + 1
   grid%xb % mkz = grid%xp%kde - grid%xp%kds + 1

   mix = grid%xb % mix
   mjy = grid%xb % mjy
   mkz = grid%xb % mkz

   grid%xb % ds  = 0.001 * grid%dx

   start_x = 1.0
   start_y = 1.0
   start_lat = -90.0
   start_lon = -180.0
   delt_lat = 180.0/real(grid%e_sn - grid%s_sn - 1)
   delt_lon = 360.0/real(grid%e_we - grid%s_we)

   phic        = 0.0
   xlonc       = 0.0
   cone_factor = 0.0


   do j = js,je
      do i = is,ie
         grid%xlat(i,j) = start_lat + real(j-1)*delt_lat
         grid%xlong(i,j) = start_lon + real(i-1)*delt_lon
      end do
   end do

   ! Avoid assigning -90,90 value                                           
   if (grid%xb%jts == grid%xb%jds) then
      grid%xlat(is:ie,j) = -89.9                               
   end if

   if (grid%xb%jte == grid%xb%jde) then
      grid%xlat(is:ie,j) = 89.9                               
   end if

   ! fix map factor and coriolis parameter
      
   grid%f(is:ie,js:je) = 2.0 *earth_omega*sin(pi*grid%xlat(is:ie,js:je)/180.0)

   xbx%inc = 1
   xbx%ni  = grid%e_we - grid%s_we
   xbx%nj  = grid%e_sn - grid%s_sn
   xbx%nk  = grid%e_vert - 1
   xbx% lenwrk    = xbx%ni
   xbx% lenr           = xbx%inc * (xbx%ni - 1) + 1
   max_wavenumber = xbx%ni/2-1
   xbx % alp_size = (xbx%nj+1)*(max_wavenumber+1)*(max_wavenumber+2)/4

   call da_transfer_kmatoxb(xbx, grid)

   if (trace_use) call da_trace_exit("da_setup_firstguess_kma")

end subroutine da_setup_firstguess_kma


