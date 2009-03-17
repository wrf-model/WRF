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
   type (xbx_type),intent(in)        :: xbx                    ! Header & non-gridded vars.
   type (be_type), intent(in)        :: be                     ! background error structure.
   type (iv_type), intent(inout)     :: iv                     ! ob. increment vector.
   real,           intent(in)        :: xhat(1:cv_size)        ! control variable (local).
   real,           intent(in)        :: cv(1:cv_size)          ! control variable (local).
   type (y_type),  intent(inout)     :: y                      ! y = H(x_inc) structure.
   real,           intent(out)       :: shat(1:cv_size)        ! control variable (local).
   
   type (y_type)                     :: y1, re
   integer                           :: ix, jy, cvs, cve, var
   integer                           :: ibs, ibe, jbs, jbe
   integer                           :: i, j, mz(6)
   real                              :: value

   if (trace_use) call da_trace_entry("da_adjoint_sensitivity")
   
   shat = 0.0

   select case(sensitivity_option) 
   case(0); 
!--------------------------------------------------------
! Read Initial Sensitivity from WRF-Adjoint (NetCDF file)
!--------------------------------------------------------        
         call da_transfer_xatowrftl_adj(grid, config_flags, 'gr01')
         call da_transform_xtoxa_adj(grid)
        
	 ! Temporary removal of lateral boundary conditions (bug in WRF_AD code)
         do i = its, ite
            do j = jts, jte
               if ( i <= ids+5 .or. i >= ide+5 .or. j <= jds+5 .or. j >= jde+5 ) then
                  grid%xa%u(i,j,:)   = 0.0
                  grid%xa%v(i,j,:)   = 0.0
                  grid%xa%t(i,j,:)   = 0.0
                  grid%xa%p(i,j,:)   = 0.0
                  grid%xa%q(i,j,:)   = 0.0
                  grid%xa%rho(i,j,:) = 0.0
                  grid%xa%psfc(i,j ) = 0.0
	       end if
            end do 
         end do	 
      
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
      shat(be%cv%size_jb+be%cv%size_je+1:cv_size) = xhat(be%cv%size_jb+be%cv%size_je+1:cv_size)

   case(3); 
    ! T_u / Pseudo RH / ... (CV = psi/chi_u/t_u/rh/ps_u)
    !---------------------------------------------------
      var = 4 
      ix  = ite - its + 1
      jy  = jte - jts + 1
      cvs = SUM(mz(1:var-1)) * ix*jy + 1
      cve = SUM(mz(1:var))   * ix*jy
      shat(cvs:cve) = xhat(cvs:cve)

   case(4); 
    ! T / Q in physical space: projection to horizontal domain between ibs-ibe and jbs-jbe
    !-------------------------------------------------------------------------------------
      ibs =  15
      ibe =  30
      jbs = -75
      jbe = -60         
      call da_zero_x (grid%xa)
!      call da_transform_vtox (grid, cv_size, xbx, be, grid%ep, xhat, grid%vv, grid%vp)
!      call da_transform_xtoxa(grid)
      do i = its, ite
         do j = jts, jte
            if ( grid%xlat (i,j) < ibs .or. grid%xlat (i,j) > ibe .or. &
	         grid%xlong(i,j) < jbs .or. grid%xlong(i,j) > jbe ) cycle
            grid%xa%u(i,j,:)   = 1.0
            grid%xa%v(i,j,:)   = 1.0
            grid%xa%t(i,j,:)   = 1.0
            grid%xa%p(i,j,:)   = 1.0
            grid%xa%q(i,j,:)   = 1.0
            grid%xa%rho(i,j,:) = 1.0
            grid%xa%psfc(i,j ) = 1.0
         end do 
      end do	 
!!!      call da_transform_xtoxa_adj(grid)
      call da_transform_vtox_adj (grid, cv_size, xbx, be, grid%ep, grid%vp, grid%vv, shat)

   case(5); 
    ! Jo for all observations
    !------------------------
      call da_calculate_gradj(-1,1,cv_size,0,0,0,xbx,be,iv,xhat,cv,re,y,shat,grid,config_flags)
!     call da_calculate_gradj(-1,1,cv_size, be%cv%size_jb, be%cv%size_je, be%cv%size_jp, &
!                           xbx,be,iv,xhat,cv,shat,re,y,shat,grid,config_flags)
			   
   case(6);
    ! Degrees of Freedom for Signal (DFS)
    !------------------------------------
      value = 1.0    ! Initialize y1 to 1.0
      call da_allocate_y  (iv, y1)
      call da_zero_y      (iv, y1, value)
      call da_transform_vtoy_adj(1, cv_size, be, grid%ep, shat, iv, grid%vp, grid%vv, xbx, y1, grid, config_flags, .true.)
      call da_deallocate_y(y1)
 
   case default;
   end select
   if (trace_use) call da_trace_exit("da_adjoint_sensitivity")

end subroutine da_adjoint_sensitivity

