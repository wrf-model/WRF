subroutine da_kmat_mul(grid, config_flags,            &
                        it, cv_size, xbx, be, iv,     &
                        xhat, qhat, cv, &
                        re, y, j_cost, eignvec, eignval, neign)

   !-------------------------------------------------------------------------
   ! Purpose:  Multiply the innovation vector by the Kalman Gain Matrix K
   !              using the precomputed Lanczos eigenpairs 
   !
   ! Called from da_solve
   !
   ! History: 05/04/2011  Creation (Tom Auligne)
   !          09/06/2012  Modified to allow variable ntmax size for each outerloop (Mike Kavulich)
   !
   !-------------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)       :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags
   integer, intent(in)               :: it                           ! external iteration.
   integer, intent(in)               :: cv_size                      ! Total cv size
   type (xbx_type),intent(inout)     :: xbx                          ! Header & non-gridded vars.
   type (be_type), intent(in)        :: be                           ! background error structure.
   type (iv_type), intent(inout)     :: iv                           ! ob. increment vector.
   real, intent(out)                 :: xhat(1:cv_size)              ! Output vector: xhat=K.d
   real, intent(in)                  :: qhat(1:cv_size, 0:ntmax(it)) ! Ritz vectors
   real, intent(in)                  :: cv(1:cv_size)                ! control variable (local).
   type (y_type), intent(inout)      :: re                           ! residual (o-a) structure.
   type (y_type), intent(inout)      :: y                            ! y = H(x_inc) structure.
   type (j_type), intent(out)        :: j_cost                       ! cost function
   real*8, intent(in)                :: eignvec(ntmax(it), ntmax(it))
   real*8, intent(in)                :: eignval(ntmax(it))
   integer, intent(in)               :: neign

   real                              :: shat(1:cv_size)          ! cv copy.

   if (trace_use) call da_trace_entry("da_kmat_mul")

   write(*,*) 'Computing Analysis Increment: v = K.d = A.H^T.R-1.[y^o-H(x_b)]'

 ! Transfer [y^o-H(x_b)] information from iv(iv_type) into re(y_type)  
   call da_zero_y(iv,y)
   call da_calculate_residual(iv,y,re)
	    
 ! H^T.R^-1.[y^o-H(x_b)]
   call da_calculate_gradj(1,1,cv_size,0,0,0,0,xbx,be,iv,cv,y,shat,grid,config_flags,re)
   shat = - shat    !! Compensate for sign in calculation of grad_v (Jo)
	    
 ! A.H^T.R^-1.[y^o-H(x_b)]
   call da_amat_mul(be, grid, cv_size, ntmax(it), neign, 1.0/eignval, eignvec, qhat, shat, xhat)

   if (trace_use) call da_trace_exit ("da_kmat_mul")

end subroutine da_kmat_mul
