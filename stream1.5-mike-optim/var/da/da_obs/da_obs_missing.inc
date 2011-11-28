subroutine da_obs_missing (obs, miss)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(multi_level_type), intent(inout) :: obs
   logical,                intent(out)   :: miss

   integer :: i

   if (trace_use_frequent) call da_trace_entry("da_obs_missing")

   miss = .true.

   ! PW:
   if (abs(obs % loc % pw  % inv - missing_r) > 1.0) then
      miss = .false.
   else
      if (obs % loc % pw  % qc /= missing_data) &
         obs % loc % pw  % qc = missing_data
   end if

   ! SLP:
   if (abs(obs % loc % slp % inv - missing_r) > 1.0) then
      miss = .false.
   else
      if (obs % loc % slp % qc /= missing_data) &
        obs % loc % slp % qc = missing_data
   end if

   do i = 1, obs % info % levels
      ! U:  
      if (abs(obs % each(i) % u    % inv - missing_r) > 1.0) then
         miss = .false.
      else
         if (obs % each(i) % u    % qc /= missing_data) &
            obs % each(i) % u    % qc = missing_data
      end if

      ! V:
      if (abs(obs % each(i) % v    % inv - missing_r) > 1.0) then
         miss = .false.
      else
         if (obs % each(i) % v    % qc /= missing_data) &
            obs % each(i) % v    % qc = missing_data
      end if

      ! P:
      if (abs(obs % each(i) % p    % inv - missing_r) > 1.0) then
         miss = .false.
      else
         if (obs % each(i) % p    % qc /= missing_data) &
            obs % each(i) % p    % qc = missing_data
      end if

      ! T:
      if (abs(obs % each(i) % t    % inv - missing_r) > 1.0) then
         miss = .false.
      else
         if (obs % each(i) % t    % qc /= missing_data) &
            obs % each(i) % t    % qc = missing_data
      end if

      ! Q:
      if (abs(obs % each(i) % q    % inv - missing_r) > 1.0) then
         miss = .false.
      else
         if (obs % each(i) % q    % qc /= missing_data) &
            obs % each(i) % q    % qc = missing_data
      end if

      ! RH:
      if (abs(obs % each(i) % rh  % inv - missing_r) > 1.0) then
         miss = .false.
      else
         if (obs % each(i) % rh  % qc /= missing_data) &
            obs % each(i) % rh  % qc = missing_data
      end if

      ! TD:
      if (abs(obs % each(i) % td   % inv - missing_r) > 1.0) then
         miss = .false.
      else
         if (obs % each(i) % td   % qc /= missing_data) &
            obs % each(i) % td   % qc = missing_data
      end if

      ! SPEED:
      if (abs(obs % each(i) % speed % inv - missing_r) > 1.0) then
         miss = .false.
      else
         if (obs % each(i) % speed % qc /= missing_data) &
            obs % each(i) % speed % qc = missing_data
      end if
   end do 

   if (trace_use_frequent) call da_trace_exit("da_obs_missing")
 
end subroutine da_obs_missing


