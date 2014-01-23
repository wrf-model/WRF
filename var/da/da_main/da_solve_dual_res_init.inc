subroutine reallocate_analysis_grid(grid)

   implicit none

   type(domain), intent(inout)      :: grid
   integer :: sm31, em31, sm32, em32, sm33, em33

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
!    print *,' CSS deallocated grid%ep%v1'
   ENDIF
   IF ( ASSOCIATED( grid%ep%v2 ) ) THEN
     DEALLOCATE(grid%ep%v2,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%ep%v2. '
    endif
!    print *,' CSS deallocated grid%ep%v2'
   ENDIF
   IF ( ASSOCIATED( grid%ep%v3 ) ) THEN
     DEALLOCATE(grid%ep%v3,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%ep%v3. '
    endif
!    print *,' CSS deallocated grid%ep%v3'
   ENDIF
   IF ( ASSOCIATED( grid%ep%v4 ) ) THEN
     DEALLOCATE(grid%ep%v4,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%ep%v4. '
    endif
!    print *,' CSS deallocated grid%ep%v4'
   ENDIF
   IF ( ASSOCIATED( grid%ep%v5 ) ) THEN
     DEALLOCATE(grid%ep%v5,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%ep%v5. '
    endif
!    print *,' CSS deallocated grid%ep%v5'
   ENDIF
  IF ( ASSOCIATED( grid%vp%alpha ) ) THEN
     DEALLOCATE(grid%vp%alpha,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%vp%alpha. '
    endif
!    print *,' CSS deallocated grid%vp%alpha'
   ENDIF
   IF ( ASSOCIATED( grid%vv%alpha ) ) THEN
     DEALLOCATE(grid%vv%alpha,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%vv%alpha. '
    endif
!    print *,' CSS deallocated grid%vv%alpha'
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

   print *,' CSS sm31 = ',sm31
   print *,' CSS em31 = ',em31
   print *,' CSS sm32 = ',sm32
   print *,' CSS em32 = ',em32
   print *,' CSS sm33 = ',sm33
   print *,' CSS em33 = ',em33

   print *,' CSS nalpha = ',config_flags%ensdim_alpha

   !
   ! allocate grid%vp%alpha
   !
   ALLOCATE(grid%vp%alpha(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha),STAT=ierr)
   if (ierr.ne.0) then
     print *,' Failed to allocate grid%vp%alpha(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha). '
   else
!    print *,' CSS allocated grid%vp%alpha'
   endif
   grid%vp%alpha=0.

   !
   ! allocate grid%vv%alpha
   !
   ALLOCATE(grid%vv%alpha(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha),STAT=ierr)
   if (ierr.ne.0) then
     print *,' Failed to allocate grid%vv%alpha(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha). '
   else
!    print *,' CSS allocated grid%vv%alpha'
   endif
   grid%vv%alpha=0.

   !
   ! allocate grid%ep%v1
   !
   ALLOCATE(grid%ep%v1(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha),STAT=ierr)
   if (ierr.ne.0) then
     print *,' Failed to allocate grid%ep%v1(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha). '
   else
!    print *,' CSS allocated grid%ep%v1'
   endif
   grid%ep%v1=0.

   !
   ! allocate grid%ep%v2
   !
   ALLOCATE(grid%ep%v2(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha),STAT=ierr)
   if (ierr.ne.0) then
     print *,' Failed to allocate grid%ep%v2(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha). '
   else
!    print *,' CSS allocated grid%ep%v2'
   endif
   grid%ep%v2=0.


   !
   ! allocate grid%ep%v3
   !
   ALLOCATE(grid%ep%v3(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha),STAT=ierr)
   if (ierr.ne.0) then
     print *,' Failed to allocate grid%ep%v3(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha). '
   else
!    print *,' CSS allocated grid%ep%v3'
   endif
   grid%ep%v3=0.

   !
   ! allocate grid%ep%v4
   !
   ALLOCATE(grid%ep%v4(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha),STAT=ierr)
   if (ierr.ne.0) then
     print *,' Failed to allocate grid%ep%v4(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha). '
   else
!    print *,' CSS allocated grid%ep%v4'
   endif
   grid%ep%v4=0.

   !
   ! allocate grid%ep%v5
   !
   ALLOCATE(grid%ep%v5(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha),STAT=ierr)
   if (ierr.ne.0) then
     print *,' Failed to allocate grid%ep%v5(sm31:em31,sm32:em32,sm33:em33,1:config_flags%ensdim_alpha). '
   else
!    print *,' CSS allocated grid%ep%v5'
!    print *,' CSS size grid%ep%v5 = ',size(grid%ep%v5)
!    grid%ep%v5(1,1,1,2) = 32
!    grid%ep%v5(sm31,sm32,sm33,2) = 2
!    grid%ep%v5(sm31,sm32,sm33,3) = 3
!    print *,' CSS alpha sample = ',grid%ep%v5(sm31,sm32,sm33,2),grid%ep%v5(sm31,sm32,sm33,3)
   endif
   grid%ep%v5=0.
 
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
!    print *,' CSS deallocated grid%xp%vxy'
   ENDIF
   IF ( ASSOCIATED( grid%xp%v1z ) ) THEN
     DEALLOCATE(grid%xp%v1z,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%xp%v1z. '
    endif
!    print *,' CSS deallocated grid%xp%v1z'
   ENDIF
   IF ( ASSOCIATED( grid%xp%v1x ) ) THEN
     DEALLOCATE(grid%xp%v1x,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%xp%v1x. '
    endif
!    print *,' CSS deallocated grid%xp%v1x'
   ENDIF
   IF ( ASSOCIATED( grid%xp%v1y ) ) THEN
     DEALLOCATE(grid%xp%v1y,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%xp%v1y. '
    endif
!    print *,' CSS deallocated grid%xp%v1y'
   ENDIF
   IF ( ASSOCIATED( grid%xp%v2z ) ) THEN
     DEALLOCATE(grid%xp%v2z,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%xp%v2z. '
    endif
!    print *,' CSS deallocated grid%xp%v2z'
   ENDIF
   IF ( ASSOCIATED( grid%xp%v2x ) ) THEN
     DEALLOCATE(grid%xp%v2x,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%xp%v2x. '
    endif
!    print *,' CSS deallocated grid%xp%v2x'
   ENDIF
   IF ( ASSOCIATED( grid%xp%v2y ) ) THEN
     DEALLOCATE(grid%xp%v2y,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%xp%v2y. '
    endif
!    print *,' CSS deallocated grid%xp%v2y'
   ENDIF
   IF ( ASSOCIATED( grid%xb%grid_box_area ) ) THEN
     DEALLOCATE(grid%xb%grid_box_area,STAT=ierr)
    if (ierr.ne.0) then
       print *, ' Failed to deallocate grid%xb%grid_box_area. '
    endif
!    print *,' CSS deallocated grid%xb%grid_box_area'
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

   print *,' CSS sm31y = ',sm31y
   print *,' CSS em31y = ',em31y
   print *,' CSS sm32y = ',sm32y
   print *,' CSS em32y = ',em32y
   print *,' CSS sm33y = ',sm33y
   print *,' CSS em33y = ',em33y

   print *,' CSS sm31x = ',sm31x
   print *,' CSS em31x = ',em31x
   print *,' CSS sm32x = ',sm32x
   print *,' CSS em32x = ',em32x
   print *,' CSS sm33x = ',sm33x
   print *,' CSS em33x = ',em33x

!
! allocate grid%xp%vxy
!
  ALLOCATE(grid%xp%vxy(sm31:em31,sm32:em32),STAT=ierr)
  if (ierr.ne.0) then
      print *,' Failed to allocate grid%xp%vxy'
  else
!     print *,' CSS allocated grid%xp%vxy'
  endif
  grid%xp%vxy=0.

!
! allocate grid%xp%v1z
!
  ALLOCATE(grid%xp%v1z(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
      print *,' Failed to allocate grid%xp%v1z'
  else
!     print *,' CSS allocated grid%xp%v1z'
  endif
  grid%xp%v1z=0.

!
! allocate grid%xp%v1x
!
  ALLOCATE(grid%xp%v1x(sm31x:em31x,sm32x:em32x,sm33x:em33x),STAT=ierr)
  if (ierr.ne.0) then
      print *,' Failed to allocate grid%xp%v1x'
  else
!     print *,' CSS allocated grid%xp%v1x'
  endif
  grid%xp%v1x=0.

!
! allocate grid%xp%v1y
!
 ALLOCATE(grid%xp%v1y(sm31y:em31y,sm32y:em32y,sm33y:em33y),STAT=ierr)
  if (ierr.ne.0) then
      print *,' Failed to allocate grid%xp%v1y'
  else
!     print *,' CSS allocated grid%xp%v1y'
  endif
  grid%xp%v1y=0.

!
! allocate grid%xp%v2z
!
  ALLOCATE(grid%xp%v2z(sm31:em31,sm32:em32,sm33:em33),STAT=ierr)
  if (ierr.ne.0) then
      print *,' Failed to allocate grid%xp%v2z'
  else
!     print *,' CSS allocated grid%xp%v2z'
  endif
  grid%xp%v2z=0.


!
! allocate grid%xp%v2x
!
  ALLOCATE(grid%xp%v2x(sm31x:em31x,sm32x:em32x,sm33x:em33x),STAT=ierr)
  if (ierr.ne.0) then
      print *,' Failed to allocate grid%xp%v2x'
  else
!     print *,' CSS allocated grid%xp%v2x'
  endif
  grid%xp%v2x=0.


!
! allocate grid%xp%v2y
!
  ALLOCATE(grid%xp%v2y(sm31y:em31y,sm32y:em32y,sm33y:em33y),STAT=ierr)
  if (ierr.ne.0) then
      print *,' Failed to allocate grid%xp%v2y'
  else
!     print *,' CSS allocated grid%xp%v2y'
  endif
  grid%xp%v2y=0.

!
! allocate grid%xb%grid_box_area
!
   ALLOCATE(grid%xb%grid_box_area(sm31:em31,sm32:em32),STAT=ierr)
    if (ierr.ne.0) then
      print *,' Failed to allocate grid%xb%grid_box_area(sm31:em31,sm32:em32)'
    else
!     print *,' CSS allocated grid%xb%grid_box_area'
    endif
    grid%xb%grid_box_area=0.
  
end subroutine allocate_intermediate_grid


