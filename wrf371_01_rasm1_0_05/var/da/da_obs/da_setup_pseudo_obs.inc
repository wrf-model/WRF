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
   !do n=1, iv%info(pseudo)%nlocal
      iv%pseudo(:) % u % inv = missing_r
      iv%pseudo(:) % v % inv = missing_r
      iv%pseudo(:) % t % inv = missing_r
      iv%pseudo(:) % p % inv = missing_r
      iv%pseudo(:) % q % inv = missing_r

      iv%pseudo(:) % u % error = missing_r
      iv%pseudo(:) % v % error = missing_r
      iv%pseudo(:) % t % error = missing_r
      iv%pseudo(:) % p % error = missing_r
      iv%pseudo(:) % q % error = missing_r

      iv%pseudo(:) % u % qc  = missing_data
      iv%pseudo(:) % v % qc  = missing_data
      iv%pseudo(:) % t % qc  = missing_data
      iv%pseudo(:) % p % qc  = missing_data
      iv%pseudo(:) % q % qc  = missing_data

      ob%pseudo(:) % u = missing_r
      ob%pseudo(:) % v = missing_r
      ob%pseudo(:) % t = missing_r
      ob%pseudo(:) % p = missing_r
      ob%pseudo(:) % q = missing_r

      !---------------------------------------------------------------
      ! [1.0] Initialise components of innovation vector:
      !---------------------------------------------------------------

      iv%info(pseudo)%x(:,:)  = pseudo_x
      iv%info(pseudo)%y(:,:)  = pseudo_y
      iv%info(pseudo)%zk(:,:) = pseudo_z

      iv%info(pseudo)%i(:,:) = int(pseudo_x)
      iv%info(pseudo)%j(:,:) = int(pseudo_y)
      iv%info(pseudo)%k(:,:) = int(pseudo_z)

      iv%info(pseudo)%dx(:,:) = pseudo_x-real(iv%info(pseudo)%i(:,:))
      iv%info(pseudo)%dy(:,:) = pseudo_y-real(iv%info(pseudo)%j(:,:))
      iv%info(pseudo)%dxm(:,:)=1.0-iv%info(pseudo)%dx(:,:)
      iv%info(pseudo)%dym(:,:)=1.0-iv%info(pseudo)%dy(:,:)
      iv%info(pseudo)%levels(:) = 1


      if (pseudo_var(1:1) == 'u' .or. pseudo_var(1:1) == 'U') then
         iv%pseudo(:) % u % inv = pseudo_val
         iv%pseudo(:) % u % error = pseudo_err
         iv%pseudo(:) % u % qc = 0
      else if (pseudo_var(1:1) == 'v' .or. pseudo_var(1:1) == 'V') then
         iv%pseudo(:) % v % inv = pseudo_val
         iv%pseudo(:) % v % error = pseudo_err
         iv%pseudo(:) % v % qc = 0
      else if (pseudo_var(1:1) == 't' .or. pseudo_var(1:1) == 'T') then
         iv%pseudo(:) % t % inv = pseudo_val
         iv%pseudo(:) % t % error = pseudo_err
         iv%pseudo(:) % t % qc = 0
      else if (pseudo_var(1:1) == 'p' .or. pseudo_var(1:1) == 'P') then
         iv%pseudo(:) % p % inv = pseudo_val
         iv%pseudo(:) % p % error = pseudo_err
         iv%pseudo(:) % p % qc = 0
      else if (pseudo_var(1:1) == 'q' .or. pseudo_var(1:1) == 'Q') then
         iv%pseudo(:) % q % inv = pseudo_val
         iv%pseudo(:) % q % error = pseudo_err
         iv%pseudo(:) % q % qc = 0
      end if 
      
!      write(unit=stdout,fmt='(a4,2f15.5)')pseudo_var, pseudo_val, pseudo_err
!      write(unit=stdout,fmt='(3f15.5)')pseudo_x, pseudo_y, pseudo_z
   !end do

   if (trace_use_dull) call da_trace_exit("da_setup_pseudo_obs")

end subroutine da_setup_pseudo_obs


