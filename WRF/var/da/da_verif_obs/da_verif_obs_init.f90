MODULE da_verif_obs_init
!----------------------------------------------------------------------------   
! History:
!
!  Abstract:  
!   Main module for 
!   initializing various arrays                   
!
!  Author:   Syed RH Rizvi     NCAR/MMM         05/30/2006
!----------------------------------------------------------------------------   
   USE da_verif_obs_control

CONTAINS

   subroutine da_advance_cymdh( start_date, dh, end_date )

   implicit none

   character (len=10), intent(in)  :: start_date ! In date (ccyymmddhh).
   integer, intent(in)             :: dh         ! Period to advance (-ve for past).
   character (len=10), intent(out) :: end_date   ! Out date (ccyymmddhh).

   integer :: ccyy, mm, dd, hh

   read(start_date(1:10), fmt='(i4, 3i2)')  ccyy, mm, dd, hh

   hh = hh + dh

   do while (hh < 0)
      hh = hh + 24
      call da_change_date ( ccyy, mm, dd, -1 )
   end do

   do while (hh > 23)
      hh = hh - 24
      call da_change_date ( ccyy, mm, dd, 1 )
   end do

   write(UNIT=end_date(1:10), fmt='(i4, 3i2.2)')  ccyy, mm, dd, hh

end subroutine da_advance_cymdh
subroutine da_change_date( ccyy, mm, dd, delta )

   implicit none

   integer, intent(inout) :: ccyy, mm, dd
   integer, intent(in)    :: delta

   integer, dimension(12) :: mmday

   mmday = (/31,28,31,30,31,30,31,31,30,31,30,31/)

   mmday(2) = 28
   if (mod(ccyy,4) == 0) then
      mmday(2) = 29

      if ( mod(ccyy,100) == 0) then
         mmday(2) = 28
      endif

      if(mod(ccyy,400) == 0) then
         mmday(2) = 29
      end if
   endif

   dd = dd + delta

   if(dd == 0) then
      mm = mm - 1

      if(mm == 0) then
         mm = 12
         ccyy = ccyy - 1
      endif

      dd = mmday(mm)
   elseif ( dd .gt. mmday(mm) ) then
      dd = 1
      mm = mm + 1
      if(mm > 12 ) then
         mm = 1
         ccyy = ccyy + 1
      end if
   end if
end subroutine da_change_date
   subroutine initialize_surface_type(surface)
   type(surface_type), intent(inout)    :: surface
!
   call initialize_stats_type(surface%uomb,surface%uoma)
   call initialize_stats_type(surface%vomb,surface%voma)
   call initialize_stats_type(surface%tomb,surface%toma)
   call initialize_stats_type(surface%pomb,surface%poma)
   call initialize_stats_type(surface%qomb,surface%qoma)

   end subroutine initialize_surface_type
!
   subroutine initialize_upr_type(upr)
   type(upr_type), intent(inout)    :: upr
!
   integer    :: k
!
   do k = 1, nstd
   call initialize_stats_type(upr%uomb(k),upr%uoma(k))
   call initialize_stats_type(upr%vomb(k),upr%voma(k))
   call initialize_stats_type(upr%tomb(k),upr%toma(k))
   call initialize_stats_type(upr%qomb(k),upr%qoma(k))
   enddo

   end subroutine initialize_upr_type
!
   subroutine initialize_gpspw_type(gpspw)
   type(gpspw_type), intent(inout)    ::  gpspw
!
      call initialize_stats_type(gpspw%tpwomb,gpspw%tpwoma)
   end subroutine initialize_gpspw_type
!  
   subroutine initialize_gpsref_type(gpsref)
   type(gpsref_type), intent(inout)    ::  gpsref
   integer     :: k

!  
   do k = 1, nstdh
   call initialize_stats_type(gpsref%refomb(k),gpsref%refoma(k))
   enddo
   end subroutine initialize_gpsref_type
!  
   subroutine initialize_stats_type(omb, oma)
   type(stats_value), intent(inout)    :: omb, oma
   omb%num   = 0 ; oma%num   = 0
   omb%bias  = 0 ; oma%bias  = 0
   omb%abias = 0 ; oma%abias = 0
   omb%rmse  = 0 ; oma%rmse  = 0
   end subroutine initialize_stats_type
!
   subroutine initialize_t_tab
   implicit none
   integer :: i

 !
  ! Initalize Student t table for alpha=0.025
  !

  ! Degrees of freedom
   do i=1,30
    alpha(i,1) = i
   end do
   alpha(31,1) = 40.
   alpha(32,1) = 60.
   alpha(33,1) = 120.
   alpha(34,1) = 400.
 
   ! Critical t value
   alpha(1,2) = 12.706
   alpha(2,2) = 4.303
   alpha(3,2) = 3.182
   alpha(4,2) = 2.776
   alpha(5,2) = 2.571
   alpha(6,2) = 2.447
   alpha(7,2) = 2.365
   alpha(8,2) = 2.306
   alpha(9,2) = 2.262
   alpha(10,2) = 2.228
   alpha(11,2) = 2.201
   alpha(12,2) = 2.179
   alpha(13,2) = 2.160
   alpha(14,2) = 2.145
   alpha(15,2) = 2.131
   alpha(16,2) = 2.120
   alpha(17,2) = 2.110
   alpha(18,2) = 2.101
   alpha(19,2) = 2.093
   alpha(20,2) = 2.086
   alpha(21,2) = 2.080
   alpha(22,2) = 2.074
   alpha(23,2) = 2.069
   alpha(24,2) = 2.064
   alpha(25,2) = 2.060
   alpha(26,2) = 2.056
   alpha(27,2) = 2.052
   alpha(28,2) = 2.048
   alpha(29,2) = 2.045
   alpha(30,2) = 2.042
   alpha(31,2) = 2.021
   alpha(32,2) = 2.000
   alpha(33,2) = 1.980
   alpha(34,2) = 1.960

   end subroutine initialize_t_tab

!-------------------------------------------------
end MODULE da_verif_obs_init
