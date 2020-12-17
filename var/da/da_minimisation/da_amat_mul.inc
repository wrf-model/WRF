subroutine da_amat_mul(be, grid, cv_size, ntmaxit, neign, eignval, eignvec, qhat, shat, xhat)

   !-------------------------------------------------------------------------
   ! Purpose:  Multiply a control vector by the Analysis Error Cov Matrix A 
   !
   ! Called from da_solve
   !
   ! History: 08/16/2010  Creation (Tom Auligne)
   !
   !-------------------------------------------------------------------------

   implicit none

   type (be_type), intent(in) :: be                     ! Background error structure.
   type (domain),  intent(in) :: grid
   integer,        intent(in) :: cv_size
   integer,        intent(in) :: ntmaxit
   integer,        intent(in) :: neign
   real*8,         intent(in) :: eignvec(ntmaxit, ntmaxit)  
   real*8,         intent(in) :: eignval(ntmaxit)
   real,           intent(in) :: qhat(cv_size, 0:ntmaxit) ! Ritz vectors
   real,           intent(in) :: shat(cv_size)          ! Input vector to multiply by A
   real,           intent(out):: xhat(cv_size)          ! Output vector: xhat = A.shat

   integer                    :: jp_start, jp_end       ! Start/end indices of Jp.
   integer                    :: i, j
   real                       :: dot_cv

   if (trace_use) call da_trace_entry("da_amat_mul")

   jp_start = be % cv % size_jb + be % cv % size_je + 1
   jp_end   = be % cv % size_jb + be % cv % size_je + be % cv % size_jp

   xhat     = 0.0
   do j = 1, neign
      dot_cv = da_dot_cv(cv_size, qhat(:,j), shat, grid, be%cv_mz, be%ncv_mz, jp_start, jp_end)
      do i = 1, neign
         xhat = xhat + qhat(:,i) * &
                SUM(eignvec(i,1:neign)*eignval(1:neign)*eignvec(j,1:neign)) * dot_cv
      end do
   end do
 
   if (trace_use) call da_trace_exit ("da_amat_mul")

end subroutine da_amat_mul
