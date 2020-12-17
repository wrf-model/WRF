subroutine da_obs_proc_station(obs,fm,uvq_direct)

   !-----------------------------------------------------------------------
   ! Purpose: Processing obs data read in at a station: 
   !
   ! (1) Surface correction
   ! (2) Vertical checks
   ! (3) Missing data check
   !-----------------------------------------------------------------------

   implicit none
                             
   type (multi_level_type), intent(inout) :: obs

   logical,optional :: uvq_direct
   real    :: po, to, rho, es, qs, qvo
   integer :: i,fm

   if (trace_use_dull) call da_trace_entry("da_obs_proc_station")

   do i = 1, obs % info % levels
      !-----------------------------------------------------------------------
      ! Gross check for t, p & q
      !-----------------------------------------------------------------------

      if (obs%each(i)%t %inv <  75.0 .or. obs%each(i)%t %inv > 350.0  ) then
         obs%each(i)%t %inv = missing_r
         obs%each(i)%t %qc  = missing
      end if
      if (obs%each(i)%p %inv <=  0.0 .or. obs%each(i)%p %inv > 110000.0) then
         obs%each(i)%p %inv = missing_r
         obs%each(i)%p %qc  = missing
      end if
      if (obs%each(i)%rh%inv < 0.0)  then
         obs%each(i)%rh%inv = missing_r
         obs%each(i)%rh%qc  = missing
      end if
     
     if(.not. (present(uvq_direct) .and. uvq_direct) .and. fm.ne.161) then
      po   = obs % each(i) % p  % inv
      to   = obs % each(i) % t  % inv
      rho  = obs % each(i) % rh  % inv

      if (ob_format == ob_format_ascii) then ! Calculate q if possible:
         if (abs(po -  missing_r) > 1.0 .and. abs(to -  missing_r) > 1.0 .and. &
             abs(rho-  missing_r) > 1.0) then

            call da_tp_to_qs(to, po, es, qs)

            if (rho > 100.0) then
               qvo = qs
            else
               qvo  = rho * qs / 100.0
            end if
         else
            qvo       = missing_r
         end if

         obs % each(i) % q  % inv = qvo
         obs % each(i) % q  % qc = obs % each(i) % rh % qc
         obs % each(i) % q  % error = obs % each(i) % rh % error
      end if
    else
         obs % each(i) % q  % inv = obs % each(i) % rh  % inv
         obs % each(i) % q  % qc = obs % each(i) % rh % qc
         obs % each(i) % q  % error = obs % each(i) % rh % error
    end if
   end do
   if (trace_use_dull) call da_trace_exit("da_obs_proc_station")

end subroutine da_obs_proc_station


