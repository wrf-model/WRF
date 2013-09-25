subroutine da_residual_div(iv, y, re, np_missing, np_bad_data, np_obs_used, np_available)

   !-----------------------------------------------------------------------
   ! Purpose: Calculate residuals for pseudo obs
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)    :: iv     ! Innovation vector (O-B).
   type (y_type) , intent(in)    :: y      ! y = H (xa)
   type (y_type) , intent(inout) :: re     ! Residual vector (O-A).
   
   integer       , intent(inout) :: np_available
   integer       , intent(inout) :: np_obs_used
   integer       , intent(inout) :: np_missing
   integer       , intent(inout) :: np_bad_data

   type (bad_data_type) :: n_obs_bad
   integer              :: n

   if (trace_use_dull) call da_trace_entry("da_residual_div")

!    n_obs_bad % u % num = number_type(0, 0, 0)
!    n_obs_bad % v % num = number_type(0, 0, 0)
!    n_obs_bad % t % num = number_type(0, 0, 0)
!    n_obs_bad % p % num = number_type(0, 0, 0)
!    n_obs_bad % q % num = number_type(0, 0, 0)
   n_obs_bad % div % num = number_type(0, 0, 0)
   do n=1, iv%info(idxdiv)%nlocal
  ! do n=1, 1
  !    if (iv%info(idxdiv)%proc_domain(1,n)) then
         np_available = np_available + 1
!          re%pseudo(n)%u = da_residual(n, 0, y%pseudo(n)%u, iv%pseudo(n)%u, n_obs_bad % u)
!          re%pseudo(n)%v = da_residual(n, 0, y%pseudo(n)%v, iv%pseudo(n)%v, n_obs_bad % v)
!          re%pseudo(n)%t = da_residual(n, 0, y%pseudo(n)%t, iv%pseudo(n)%t, n_obs_bad % t)
!          re%pseudo(n)%p = da_residual(n, 0, y%pseudo(n)%p, iv%pseudo(n)%p, n_obs_bad % p)
!          re%pseudo(n)%q = da_residual(n, 0, y%pseudo(n)%q, iv%pseudo(n)%q, n_obs_bad % q)
         re%div(n) = da_residual(n, 0, y%div(n), iv%div(n), n_obs_bad % div)
!      end if
   end do

   np_missing =  n_obs_bad % div % num % miss
   np_bad_data = n_obs_bad % div % num % bad 
   np_obs_used = n_obs_bad % div % num % use

   if (trace_use_dull) call da_trace_exit("da_residual_div")

end subroutine da_residual_div


