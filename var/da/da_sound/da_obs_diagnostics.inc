subroutine da_obs_diagnostics(num_sound, ob, iv, re)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                    intent(in) :: num_sound
   type (residual_sound_type), intent(in) :: ob(:)
   type (sound_type),          intent(in) :: iv(:)
   type (residual_sound_type), intent(in) :: re(:)

   integer :: n, k
   integer :: sound_diag_unit1
   integer :: sound_diag_unit2
   integer :: sound_diag_unit3
   integer :: sound_diag_unit4

   if (trace_use) call da_trace_entry("da_obs_diagnostics")

   call da_get_unit(sound_diag_unit1)
   call da_get_unit(sound_diag_unit2)
   call da_get_unit(sound_diag_unit3)
   call da_get_unit(sound_diag_unit4)
   open(unit=sound_diag_unit1,file="sound_diag1",status="replace") 
   open(unit=sound_diag_unit2,file="sound_diag2",status="replace") 
   open(unit=sound_diag_unit3,file="sound_diag3",status="replace") 
   open(unit=sound_diag_unit4,file="sound_diag4",status="replace") 

   do n = 1, num_sound
      do k = 1, iv(n) % info % levels
         if (iv(n) % u(k) % qc >= obs_qc_pointer) then
            write(unit=sound_diag_unit1,fmt='(a5,2f9.3,5f17.7,i8)') &
               iv(n) % info % id, &   ! Station
               iv(n) % info % lat, &  ! Latitude
               iv(n) % info % lon, &  ! Longitude
               iv(n) % p(k), &        ! Obs Pressure
               ob(n) % u(k), &        ! O
               iv(n) % u(k) % inv, &  ! O-B
               re(n) % u(k), &        ! O-A
               iv(n) % u(k) % error, &! Obs error
               iv(n) % u(k) % qc      ! QC flag
         end if

         if (iv(n) % v(k) % qc >= obs_qc_pointer) then
            write(unit=sound_diag_unit2,fmt='(a5,2f9.3,5f17.7,i8)') &
               iv(n) % info % id, & ! Station
               iv(n) % info % lat, &  ! Latitude
               iv(n) % info % lon, &  ! Longitude
               iv(n) % h(k), &        ! Obs Pressure
               ob(n) % v(k), &        ! O
               iv(n) % v(k) % inv, &  ! O-B
               re(n) % v(k), &        ! O-A
               iv(n) % v(k) % error, &! Obs error
               iv(n) % v(k) % qc      ! QC flag
         end if

         if (iv(n) % t(k) % qc >= obs_qc_pointer) then
            write(unit=sound_diag_unit3,fmt='(a5,2f9.3,5f17.7,i8)') &
               iv(n) % info % id, & ! Station
               iv(n) % info % lat, &  ! Latitude
               iv(n) % info % lon, &  ! Longitude
               iv(n) % h(k), &        ! Obs Pressure
               ob(n) % t(k), &        ! O
               iv(n) % t(k) % inv, &  ! O-B
               re(n) % t(k), &        ! O-A
               iv(n) % t(k) % error, &! Obs error
               iv(n) % t(k) % qc      ! QC flag
         end if

         if (iv(n) % q(k) % qc >= obs_qc_pointer) then
            write(unit=sound_diag_unit4,fmt='(a5,2f9.3,5f17.7,i8)') &
               iv(n) % info % id, & ! Station
               iv(n) % info % lat, &  ! Latitude
               iv(n) % info % lon, &  ! Longitude
               iv(n) % h(k), &        ! Obs Pressure
               ob(n) % q(k), &        ! O
               iv(n) % q(k) % inv, &  ! O-B (kg/kg)
               re(n) % q(k), &        ! O-A
               iv(n) % q(k) % error, &! Obs error (kg/kg)
               iv(n) % q(k) % qc      ! QC flag 
         end if              
      end do
   end do

   !  End of file markers:
   write(unit=sound_diag_unit1,fmt='(a5,2f9.3,5f17.7,i8)') &
     '00000',0.0,0.0,0.0,0.0,0.0,0.0,0.0,0

   write(unit=sound_diag_unit2,fmt='(a5,2f9.3,5f17.7,i8)') &
     '00000',0.0,0.0,0.0,0.0,0.0,0.0,0.0,0

   write(unit=sound_diag_unit3,fmt='(a5,2f9.3,5f17.7,i8)') &
     '00000',0.0,0.0,0.0,0.0,0.0,0.0,0.0,0

   write(unit=sound_diag_unit4,fmt='(a5,2f9.3,5f17.7,i8)') &
      '00000',0.0,0.0,0.0,0.0,0.0,0.0,0.0,0

   close(sound_diag_unit1)
   close(sound_diag_unit2)
   close(sound_diag_unit3)
   close(sound_diag_unit4)
   call da_free_unit(sound_diag_unit1)
   call da_free_unit(sound_diag_unit2)
   call da_free_unit(sound_diag_unit3)
   call da_free_unit(sound_diag_unit4)

   if (trace_use) call da_trace_exit("da_obs_diagnostics")

 end subroutine da_obs_diagnostics


