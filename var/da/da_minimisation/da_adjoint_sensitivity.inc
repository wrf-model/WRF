subroutine da_adjoint_sensitivity(grid, config_flags, cv_size, xbx, be, iv, xhat, cv, y, shat)

   !-------------------------------------------------------------------------
   ! Purpose:        Read from WRF adjoint run or define derivative of
   !                 initial sensitivity for observation sensitivity studies
   !                 Shat = dF/dx (e.g. F = 0.5<x',B-1.x'> --> dF/dx=xhat)
   !
   ! Called from da_minimise_lz
   !
   ! History: 12/12/2008  Creation (Tom Auligne)
   !
   !-------------------------------------------------------------------------

   implicit none

   type(domain),   intent(inout)     :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags
   integer,        intent(in)        :: cv_size                ! Total cv size
   type (xbx_type),intent(inout)     :: xbx                    ! Header & non-gridded vars.
   type (be_type), intent(in)        :: be                     ! background error structure.
   type (iv_type), intent(inout)     :: iv                     ! ob. increment vector.
   real,           intent(in)        :: xhat(1:cv_size)        ! control variable (local).
   real,           intent(in)        :: cv(1:cv_size)          ! control variable (local).
   type (y_type),  intent(inout)     :: y                      ! y = H(x_inc) structure.
   real,           intent(out)       :: shat(1:cv_size)        ! control variable (local).
   
   type (y_type)                     :: re
   type (j_type)                     :: j_cost                 ! cost function
   integer                           :: ix, jy, cvs, cve, var
   integer                           :: ibs, ibe, jbs, jbe
   integer                           :: i, j, mz(6)
   integer                           :: jp_start, jp_end       ! Start/end indices of Jp.
   real                              :: value

   if (trace_use) call da_trace_entry("da_adjoint_sensitivity")
   
   shat = 0.0

   select case(sensitivity_option) 
!--------------------------------------------------------
! Read Initial Sensitivity from WRF-Adjoint (NetCDF file)
!--------------------------------------------------------        
   case(0); 
         call da_zero_x (grid%xa)
         call da_transfer_xatowrftl_adj(grid, config_flags, 'gr01')
         call da_transform_xtoxa_adj(grid)
         !as of V3.9, da_zero_vp_type(vp) is commented out in the da_transform_vtox_adj
         call da_zero_vp_type(grid%vp) !as of V3.9, zero out vp here
         call da_transform_vtox_adj (grid, cv_size, xbx, be, grid%ep, grid%vp, grid%vv, shat)

!---------------------------
! Define Initial Sensitivity
!---------------------------
   case(1); 
   ! Shat = dF/dx (e.g. F = 0.5<x',B-1.x'> --> dF/dx=xhat)
   !------------------------------------------------------
      shat = xhat

   case(2); 
    ! VarBC parameters 
    !-----------------
      jp_start = be % cv % size_jb + be % cv % size_je + 1
      jp_end   = be % cv % size_jb + be % cv % size_je + be % cv % size_jp
      if (jp_end >= jp_start) shat(jp_start:jp_end) = xhat(jp_start:jp_end)

   case(3); 
    ! Jo for all observations
    !------------------------
      call da_allocate_y (iv, re)
      call da_calculate_j(1,1,cv_size,0,0,0,0,xbx,be,iv,xhat,cv,re,y,j_cost,grid,config_flags)
      call da_calculate_gradj(1,1,cv_size,0,0,0,0,xbx,be,iv,xhat+cv,y,shat,grid,config_flags,re)
      call da_deallocate_y(re)
			    
   case default;
   end select
   if (trace_use) call da_trace_exit("da_adjoint_sensitivity")

end subroutine da_adjoint_sensitivity

