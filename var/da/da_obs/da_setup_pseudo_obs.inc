subroutine da_setup_pseudo_obs(iv, ob)

   !-------------------------------------------------------------------------
   ! Purpose: Sets up pseudo ob part of observation structure.
   !-------------------------------------------------------------------------

   implicit none

   type(iv_type),    intent(inout) :: iv   ! Obs and header structure.
   type(y_type),     intent(inout) :: ob   ! (Smaller) observation structure.

   integer                       :: n    ! Loop counters.

   if (trace_use_dull) call da_trace_entry("da_setup_pseudo_obs")

   allocate (iv%pseudo(1:iv%info(pseudo)%nlocal))

   do n=1, iv%info(pseudo)%nlocal
      iv%pseudo(n) % u % inv = missing_r
      iv%pseudo(n) % v % inv = missing_r
      iv%pseudo(n) % t % inv = missing_r
      iv%pseudo(n) % p % inv = missing_r
      iv%pseudo(n) % q % inv = missing_r

      iv%pseudo(n) % u % error = missing_r
      iv%pseudo(n) % v % error = missing_r
      iv%pseudo(n) % t % error = missing_r
      iv%pseudo(n) % p % error = missing_r
      iv%pseudo(n) % q % error = missing_r

      iv%pseudo(n) % u % qc  = missing_data
      iv%pseudo(n) % v % qc  = missing_data
      iv%pseudo(n) % t % qc  = missing_data
      iv%pseudo(n) % p % qc  = missing_data
      iv%pseudo(n) % q % qc  = missing_data

      ob%pseudo(n) % u = missing_r
      ob%pseudo(n) % v = missing_r
      ob%pseudo(n) % t = missing_r
      ob%pseudo(n) % p = missing_r
      ob%pseudo(n) % q = missing_r

      !---------------------------------------------------------------
      ! [1.0] Initialise components of innovation vector:
      !---------------------------------------------------------------

      iv%info(pseudo)%x(:,n)  = pseudo_x
      iv%info(pseudo)%y(:,n)  = pseudo_y
      iv%info(pseudo)%zk(:,n) = pseudo_z

      iv%info(pseudo)%i(:,n) = int(pseudo_x)
      iv%info(pseudo)%j(:,n) = int(pseudo_y)
      iv%info(pseudo)%k(:,n) = int(pseudo_z)

      iv%info(pseudo)%dx(:,n) = pseudo_x-real(iv%info(pseudo)%i(:,n))
      iv%info(pseudo)%dy(:,n) = pseudo_y-real(iv%info(pseudo)%j(:,n))
      iv%info(pseudo)%dxm(:,n)=1.0-iv%info(pseudo)%dx(:,n)
      iv%info(pseudo)%dym(:,n)=1.0-iv%info(pseudo)%dy(:,n)
      iv%info(pseudo)%levels(:) = 1


      if (pseudo_var(1:1) == 'u' .or. pseudo_var(1:1) == 'U') then
         iv%pseudo(n) % u % inv = pseudo_val
         iv%pseudo(n) % u % error = pseudo_err
         iv%pseudo(n) % u % qc = 0
      else if (pseudo_var(1:1) == 'v' .or. pseudo_var(1:1) == 'V') then
         iv%pseudo(n) % v % inv = pseudo_val
         iv%pseudo(n) % v % error = pseudo_err
         iv%pseudo(n) % v % qc = 0
      else if (pseudo_var(1:1) == 't' .or. pseudo_var(1:1) == 'T') then
         iv%pseudo(n) % t % inv = pseudo_val
         iv%pseudo(n) % t % error = pseudo_err
         iv%pseudo(n) % t % qc = 0
      else if (pseudo_var(1:1) == 'p' .or. pseudo_var(1:1) == 'P') then
         iv%pseudo(n) % p % inv = pseudo_val
         iv%pseudo(n) % p % error = pseudo_err
         iv%pseudo(n) % p % qc = 0
      else if (pseudo_var(1:1) == 'q' .or. pseudo_var(1:1) == 'Q') then
         iv%pseudo(n) % q % inv = pseudo_val
         iv%pseudo(n) % q % error = pseudo_err
         iv%pseudo(n) % q % qc = 0
      end if 
      
      write(unit=stdout,fmt='(a4,2f15.5)')pseudo_var, pseudo_val, pseudo_err
      write(unit=stdout,fmt='(3f15.5)')pseudo_x, pseudo_y, pseudo_z
   end do

   if (trace_use_dull) call da_trace_exit("da_setup_pseudo_obs")

end subroutine da_setup_pseudo_obs


