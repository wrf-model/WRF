subroutine reallocate_analysis_grid(grid)

   implicit none

   type(domain), intent(inout)      :: grid
   integer :: sm31, em31, sm32, em32, sm33, em33, ep_dim

   if (trace_use) call da_trace_entry("da_solve_dual_res_init")

   !
   ! First deallocate the arrays associated with alpha and ensemble perturbations
   !

   if (trace_use) call da_trace("da_solve_dual_res_init", &
      Message="Deallocating arrays")


   IF ( ASSOCIATED( grid%ep%v1 ) ) THEN
     DEALLOCATE(grid%ep%v1,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%ep%v1. '
    endif
   ENDIF
   IF ( ASSOCIATED( grid%ep%v2 ) ) THEN
     DEALLOCATE(grid%ep%v2,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%ep%v2. '
    endif
   ENDIF
   IF ( ASSOCIATED( grid%ep%v3 ) ) THEN
     DEALLOCATE(grid%ep%v3,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%ep%v3. '
    endif
   ENDIF
   IF ( ASSOCIATED( grid%ep%v4 ) ) THEN
     DEALLOCATE(grid%ep%v4,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%ep%v4. '
    endif
   ENDIF
   IF ( ASSOCIATED( grid%ep%v5 ) ) THEN
     DEALLOCATE(grid%ep%v5,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%ep%v5. '
    endif
   ENDIF
if ( alpha_hydrometeors ) then
   IF ( ASSOCIATED( grid%ep%cw ) ) THEN
     DEALLOCATE(grid%ep%cw,STAT=ierr)
     if (ierr.ne.0) then
        write(unit=message(1),fmt='(a)') 'Failed to deallocate grid%ep%cw '
        call da_error(__FILE__,__LINE__,message(1:1))
     endif
   ENDIF
   IF ( ASSOCIATED( grid%ep%rn ) ) THEN
     DEALLOCATE(grid%ep%rn,STAT=ierr)
     if (ierr.ne.0) then
        write(unit=message(1),fmt='(a)') 'Failed to deallocate grid%ep%rn '
        call da_error(__FILE__,__LINE__,message(1:1))
     endif
   ENDIF
   IF ( ASSOCIATED( grid%ep%ci ) ) THEN
     DEALLOCATE(grid%ep%ci,STAT=ierr)
     if (ierr.ne.0) then
        write(unit=message(1),fmt='(a)') 'Failed to deallocate grid%ep%ci '
        call da_error(__FILE__,__LINE__,message(1:1))
     endif
   ENDIF
   IF ( ASSOCIATED( grid%ep%sn ) ) THEN
     DEALLOCATE(grid%ep%sn,STAT=ierr)
     if (ierr.ne.0) then
        write(unit=message(1),fmt='(a)') 'Failed to deallocate grid%ep%sn '
        call da_error(__FILE__,__LINE__,message(1:1))
     endif
   ENDIF
   IF ( ASSOCIATED( grid%ep%gr ) ) THEN
     DEALLOCATE(grid%ep%gr,STAT=ierr)
     if (ierr.ne.0) then
        write(unit=message(1),fmt='(a)') 'Failed to deallocate grid%ep%gr '
        call da_error(__FILE__,__LINE__,message(1:1))
     endif
   ENDIF
end if ! alpha_hydrometeors
  IF ( ASSOCIATED( grid%vp%alpha ) ) THEN
     DEALLOCATE(grid%vp%alpha,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%vp%alpha. '
    endif
   ENDIF
   IF ( ASSOCIATED( grid%vv%alpha ) ) THEN
     DEALLOCATE(grid%vv%alpha,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%vv%alpha. '
    endif
   ENDIF


   !
   ! Now, reallocate the arrays with the intermediate grid dimensions
   !

   if (trace_use) call da_trace("da_solve_dual_res_init", &
      Message="Reallocating arrays")

   sm31 = grid%intermediate_grid%sm31
   em31 = grid%intermediate_grid%em31
   sm32 = grid%intermediate_grid%sm32
   em32 = grid%intermediate_grid%em32
   sm33 = grid%intermediate_grid%sm33
   em33 = grid%intermediate_grid%em33

   ep_dim=1
   if ( use_4denvar ) ep_dim=num_fgat_time   !  4D-En-Var
   !
   ! allocate grid%vp%alpha
   !
   ALLOCATE(grid%vp%alpha(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha),STAT=ierr)
   if (ierr.ne.0) then
     print *,' Failed to allocate grid%vp%alpha(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha). '
   endif
   grid%vp%alpha=0.

   !
   ! allocate grid%vv%alpha
   !
   ALLOCATE(grid%vv%alpha(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha),STAT=ierr)
   if (ierr.ne.0) then
     print *,' Failed to allocate grid%vv%alpha(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha). '
   endif
   grid%vv%alpha=0.

   !
   ! allocate grid%ep%v1
   !
   ALLOCATE(grid%ep%v1(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha*ep_dim),STAT=ierr)
   if (ierr.ne.0) then
     print *,' Failed to allocate grid%ep%v1(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha). '
   endif
   grid%ep%v1=0.

   !
   ! allocate grid%ep%v2
   !
   ALLOCATE(grid%ep%v2(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha*ep_dim),STAT=ierr)
   if (ierr.ne.0) then
     print *,' Failed to allocate grid%ep%v2(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha). '
   endif
   grid%ep%v2=0.


   !
   ! allocate grid%ep%v3
   !
   ALLOCATE(grid%ep%v3(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha*ep_dim),STAT=ierr)
   if (ierr.ne.0) then
     print *,' Failed to allocate grid%ep%v3(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha). '
   endif
   grid%ep%v3=0.

   !
   ! allocate grid%ep%v4
   !
   ALLOCATE(grid%ep%v4(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha*ep_dim),STAT=ierr)
   if (ierr.ne.0) then
     print *,' Failed to allocate grid%ep%v4(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha). '
   endif
   grid%ep%v4=0.

   !
   ! allocate grid%ep%v5
   !
   ALLOCATE(grid%ep%v5(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha*ep_dim),STAT=ierr)
   if (ierr.ne.0) then
     print *,' Failed to allocate grid%ep%v5(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha). '
   endif
   grid%ep%v5=0.

if ( alpha_hydrometeors ) then
   !
   ! allocate grid%ep%cw
   !
   ALLOCATE(grid%ep%cw(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha*ep_dim),STAT=ierr)
   if (ierr.ne.0) then
      write(unit=message(1),fmt='(a)') 'Failed to allocate grid%ep%cw(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha) '
      call da_error(__FILE__,__LINE__,message(1:1))
   endif
   grid%ep%cw=0.
   !
   ! allocate grid%ep%rn
   !
   ALLOCATE(grid%ep%rn(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha*ep_dim),STAT=ierr)
   if (ierr.ne.0) then
      write(unit=message(1),fmt='(a)') 'Failed to allocate grid%ep%rn(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha) '
      call da_error(__FILE__,__LINE__,message(1:1))
   endif
   grid%ep%rn=0.
   !
   ! allocate grid%ep%ci
   !
   ALLOCATE(grid%ep%ci(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha*ep_dim),STAT=ierr)
   if (ierr.ne.0) then
      write(unit=message(1),fmt='(a)') 'Failed to allocate grid%ep%ci(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha) '
      call da_error(__FILE__,__LINE__,message(1:1))
   endif
   grid%ep%ci=0.
   !
   ! allocate grid%ep%sn
   !
   ALLOCATE(grid%ep%sn(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha*ep_dim),STAT=ierr)
   if (ierr.ne.0) then
      write(unit=message(1),fmt='(a)') 'Failed to allocate grid%ep%sn(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha) '
      call da_error(__FILE__,__LINE__,message(1:1))
   endif
   grid%ep%sn=0.
   !
   ! allocate grid%ep%gr
   !
   ALLOCATE(grid%ep%gr(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha*ep_dim),STAT=ierr)
   if (ierr.ne.0) then
      write(unit=message(1),fmt='(a)') 'Failed to allocate grid%ep%gr(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha) '
      call da_error(__FILE__,__LINE__,message(1:1))
   endif
   grid%ep%gr=0.
end if ! alpha_hydrometeors

   if (trace_use) call da_trace_exit("da_solve_dual_res_init")

end subroutine reallocate_analysis_grid

!!!!!!!!!!!!!!

subroutine allocate_intermediate_grid(grid)

   type(domain), intent(inout)      :: grid 

   integer :: sm31, em31, sm32, em32, sm33, em33
   integer :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   integer :: sm31y, em31y, sm32y, em32y, sm33y, em33y

   !
   ! First deallocate the arrays
   !

   IF ( ASSOCIATED( grid%xp%vxy ) ) THEN
     DEALLOCATE(grid%xp%vxy,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%xp%vxy. '
    endif
   ENDIF
   IF ( ASSOCIATED( grid%xp%v1z ) ) THEN
     DEALLOCATE(grid%xp%v1z,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%xp%v1z. '
    endif
   ENDIF
   IF ( ASSOCIATED( grid%xp%v1x ) ) THEN
     DEALLOCATE(grid%xp%v1x,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%xp%v1x. '
    endif
   ENDIF
   IF ( ASSOCIATED( grid%xp%v1y ) ) THEN
     DEALLOCATE(grid%xp%v1y,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%xp%v1y. '
    endif
   ENDIF
   IF ( ASSOCIATED( grid%xp%v2z ) ) THEN
     DEALLOCATE(grid%xp%v2z,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%xp%v2z. '
    endif
   ENDIF
   IF ( ASSOCIATED( grid%xp%v2x ) ) THEN
     DEALLOCATE(grid%xp%v2x,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%xp%v2x. '
    endif
   ENDIF
   IF ( ASSOCIATED( grid%xp%v2y ) ) THEN
     DEALLOCATE(grid%xp%v2y,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%xp%v2y. '
    endif
   ENDIF
   IF ( ASSOCIATED( grid%xb%grid_box_area ) ) THEN
     DEALLOCATE(grid%xb%grid_box_area,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%xb%grid_box_area. '
    endif
   ENDIF

   sm31 = grid%sm31
   em31 = grid%em31
   sm32 = grid%sm32
   em32 = grid%em32
   sm33 = grid%sm33
   em33 = grid%em33

   sm31x = grid%sm31x
   em31x = grid%em31x
   sm32x = grid%sm32x
   em32x = grid%em32x
   sm33x = grid%sm33x
   em33x = grid%em33x

   sm31y = grid%sm31y
   em31y = grid%em31y
   sm32y = grid%sm32y
   em32y = grid%em32y
   sm33y = grid%sm33y
   em33y = grid%em33y

!
! allocate grid%xp%vxy
!
  ALLOCATE(grid%xp%vxy(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
      print *,' Failed to allocate grid%xp%vxy'
  endif
  grid%xp%vxy=0.

!
! allocate grid%xp%v1z
!
  ALLOCATE(grid%xp%v1z(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
      print *,' Failed to allocate grid%xp%v1z'
  endif
  grid%xp%v1z=0.

!
! allocate grid%xp%v1x
!
  ALLOCATE(grid%xp%v1x(sm31x:em31x,sm32x:em32x,sm33x:em33x),STAT=ierr)
  if (ierr.ne.0) then
      print *,' Failed to allocate grid%xp%v1x'
  endif
  grid%xp%v1x=0.

!
! allocate grid%xp%v1y
!
 ALLOCATE(grid%xp%v1y(sm31y:em31y,sm32y:em32y,sm33y:em33y),STAT=ierr)
  if (ierr.ne.0) then
      print *,' Failed to allocate grid%xp%v1y'
  endif
  grid%xp%v1y=0.

!
! allocate grid%xp%v2z
!
  ALLOCATE(grid%xp%v2z(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
      print *,' Failed to allocate grid%xp%v2z'
  endif
  grid%xp%v2z=0.


!
! allocate grid%xp%v2x
!
  ALLOCATE(grid%xp%v2x(sm31x:em31x,sm32x:em32x,sm33x:em33x),STAT=ierr)
  if (ierr.ne.0) then
      print *,' Failed to allocate grid%xp%v2x'
  endif
  grid%xp%v2x=0.


!
! allocate grid%xp%v2y
!
  ALLOCATE(grid%xp%v2y(sm31y:em31y,sm32y:em32y,sm33y:em33y),STAT=ierr)
  if (ierr.ne.0) then
      print *,' Failed to allocate grid%xp%v2y'
  endif
  grid%xp%v2y=0.

!
! allocate grid%xb%grid_box_area
!
   ALLOCATE(grid%xb%grid_box_area(sm31:em31,sm32:em32),STAT=ierr)
    if (ierr.ne.0) then
      print *,' Failed to allocate grid%xb%grid_box_area(sm31:em31,sm32:em32)'
    endif
    grid%xb%grid_box_area=0.
  
end subroutine allocate_intermediate_grid


