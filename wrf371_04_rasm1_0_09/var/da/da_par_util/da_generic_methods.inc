subroutine da_res_generic_set_info( res_generic, proc_domain, &
                                      obs_global_index)

   !---------------------------------------------------------------------------
   ! Purpose:  Eliminate redundant code for many obs types.  
   !
   ! Method:   Set common fields in res_generic.
   !---------------------------------------------------------------------------
   
   implicit none

   type(residual_generic_type), intent(inout) :: res_generic  ! generic type
   logical,                      intent(in  ) :: proc_domain
   integer,                      intent(in  ) :: obs_global_index

   res_generic%proc_domain = proc_domain
   res_generic%obs_global_index = obs_global_index
end subroutine da_res_generic_set_info


subroutine da_res_sound_create_template( template)

   !---------------------------------------------------------------------------
   ! Purpose:  Return storage template for specific type stored as 
   !           residual_generic_type.
   !---------------------------------------------------------------------------

   implicit none

   type(residual_template_type), intent(inout) :: template

   ! only vector data for this type
   template%lbnd = 1
   template%ubnd = 4

end subroutine da_res_sound_create_template


subroutine da_res_sound_to_generic( res_specific, iv_num_levels, &
                                      res_generic)

   !---------------------------------------------------------------------------
   ! Purpose:  Eliminate redundant code for many obs types.  
   !
   ! Method:   Specific type res_specific is translated to generic type 
   !           res_generic.  Pointer manipulation is used for vector data, no 
   !           vector data is copied.  Scalar data is copied.  This routine 
   !           allocates memory for res_generic.  The caller must ensure that 
   !           memory is deallocated later.  
   !           iv_num_levels is used as a sanity check and is ignored for 
   !           generic types that do not contain vector data. 
   !---------------------------------------------------------------------------

   implicit none

   type(residual_sound_type),   intent(in  ) :: res_specific ! specific type
   integer,                      intent(in  ) :: iv_num_levels ! levels
   type(residual_generic_type), intent(inout) :: res_generic  ! generic type
   ! Local declarations
   type(residual_template_type) :: template
   integer :: n

   call da_res_sound_create_template( template)
   allocate( res_generic%values(template%lbnd:template%ubnd))
   ! only vector data for this type
   ! store references to vector data
   res_generic%values(1)%ptr => res_specific%u
   res_generic%values(2)%ptr => res_specific%v
   res_generic%values(3)%ptr => res_specific%t
   res_generic%values(4)%ptr => res_specific%q
   ! ASSERTION
   do n=1,4
      !TBH:  NOTE:  We could handle iv_num_levels < size(res_generic%values(n)%ptr) 
      !TBH:         HOWEVER, we would have to add "num_levels" state to 
      !TBH:         residual_generic_type AND send this around.  Better to fix 
      !TBH:         allocation in specific types to avoid wasting memory!  
      if (size(res_generic%values(n)%ptr) /= iv_num_levels) then
         call da_error(__FILE__,__LINE__, &
           (/'residual_sound_to_generic:  mismatched levels'/))
      end if
   end do

end subroutine da_res_sound_to_generic


subroutine da_res_sound_from_generic( res_generic, res_specific)

   !---------------------------------------------------------------------------
   ! Purpose:  Eliminate redundant code for many obs types.  
   !
   ! Method:   Generic type res_generic is translated to specific type 
   !           res_specific.  Pointer manipulation is used for vector data, no 
   !           vector data is copied.  Scalar data is copied. 
   !---------------------------------------------------------------------------

   implicit none

   type(residual_generic_type), intent(in  ) :: res_generic  ! generic type
   type(residual_sound_type),   intent(inout) :: res_specific ! specific type

   ! only vector data for this type
   ! store references to vector data
   res_specific%u => res_generic%values(1)%ptr
   res_specific%v => res_generic%values(2)%ptr
   res_specific%t => res_generic%values(3)%ptr
   res_specific%q => res_generic%values(4)%ptr

end subroutine da_res_sound_from_generic


subroutine da_res_synop_create_template( template)

   !---------------------------------------------------------------------------
   ! Purpose:  Return storage template for specific type stored as 
   !           residual_generic_type.  
   !---------------------------------------------------------------------------

   implicit none

   type(residual_template_type), intent(inout) :: template

   ! only scalar data for this type
   template%lbnd = 0
   template%ubnd = 0

end subroutine da_res_synop_create_template


subroutine da_res_synop_to_generic( res_specific, iv_num_levels, &
                                      res_generic)

   !---------------------------------------------------------------------------
   ! Purpose:  Eliminate redundant code for many obs types.  
   !
   ! Method:   Specific type res_specific is translated to generic type 
   !           res_generic.  Pointer manipulation is used for vector data, no 
   !           vector data is copied.  Scalar data is copied.  This routine 
   !           allocates memory for res_generic.  The caller must ensure that 
   !           memory is deallocated later.  
   !           iv_num_levels is used as a sanity check and is ignored for 
   !           generic types that do not contain vector data.
   !---------------------------------------------------------------------------

   implicit none

   type(residual_synop_type),   intent(in  ) :: res_specific ! specific type
   integer,                      intent(in  ) :: iv_num_levels ! levels
   type(residual_generic_type), intent(inout) :: res_generic  ! generic type
   ! Local declarations
   type(residual_template_type) :: template

   ! use to avoid compiler warning
   if (iv_num_levels==0) then
   end if

   call da_res_synop_create_template( template)
   allocate( res_generic%values(template%lbnd:template%ubnd))
   ! only scalar data for this type
   allocate( res_generic%values(0)%ptr(5))
   ! copy scalar data
   res_generic%values(0)%ptr(1) = res_specific%u
   res_generic%values(0)%ptr(2) = res_specific%v
   res_generic%values(0)%ptr(3) = res_specific%t
   res_generic%values(0)%ptr(4) = res_specific%p
   res_generic%values(0)%ptr(5) = res_specific%q

end subroutine da_res_synop_to_generic


subroutine da_res_synop_from_generic( res_generic, res_specific)

   !---------------------------------------------------------------------------
   ! Purpose:  Eliminate redundant code for many obs types.  
   !
   ! Method:   Generic type res_generic is translated to specific type 
   !           res_specific.  Pointer manipulation is used for vector data, no 
   !           vector data is copied.  Scalar data is copied.
   !---------------------------------------------------------------------------

   implicit none

   type(residual_generic_type), intent(in  ) :: res_generic  ! generic type
   type(residual_synop_type),   intent(inout) :: res_specific ! specific type

   ! only scalar data for this type
   ! copy scalar data
   res_specific%u = res_generic%values(0)%ptr(1)
   res_specific%v = res_generic%values(0)%ptr(2)
   res_specific%t = res_generic%values(0)%ptr(3)
   res_specific%p = res_generic%values(0)%ptr(4)
   res_specific%q = res_generic%values(0)%ptr(5)

end subroutine da_res_synop_from_generic


subroutine da_y_facade_create( slice, num_obs, num_obs_glo, template)

   !---------------------------------------------------------------------------
   ! Purpose:  Create a y_facade_type containing specified number of 
   !           residual_generic_type objects.  
   !
   ! Method:   Allocate memory and call residual_generic_create.  
   !           Call y_facade_free() to deallocate memory allocated here.  
   !           If template is not present, residual_generic_type objects will be 
   !           left uninitialized.  If template is present, then 
   !           residual_generic_type objects will be allocated but no values 
   !           will be copied into them.  
   !
   !---------------------------------------------------------------------------

   implicit none

   type(y_facade_type),          intent(inout) :: slice ! selected residual obs
   integer,                       intent(in  ) :: num_obs
   integer,                       intent(in  ) :: num_obs_glo
   type(residual_template_type), optional, intent(in  ) :: template
   ! Local declarations
   integer :: n

   slice%num_obs     = num_obs
   slice%num_obs_glo = num_obs_glo
   allocate( slice%obs(slice%num_obs))
   do n=1, slice%num_obs
      call da_res_generic_create( slice%obs(n), template=template)
   end do

end subroutine da_y_facade_create


subroutine da_res_generic_create( res_generic, template)

   !---------------------------------------------------------------------------
   ! Purpose:  Create a residual_generic_type object.  This must be called 
   !           before any other operation is performed on a 
   !           residual_generic_type object.  Do not pass an already-allocated 
   !           object into this routine as res_generic or you will cause a 
   !           memory leak!  
   !
   ! Method:   If template is not present, create an empty object.  
   !           If template is present, create an uninitialized object with 
   !             space allocated to match template.  Caller is responsible 
   !             for deallocation via call to residual_generic_free().  Note 
   !             that this is *NOT* a copy-constructor because values are not 
   !             copied.  Also, proc_domain and obs_global_index fields are 
   !             not copied from the template.  Finally, memory is not allocated 
   !             for vector or scalar data, these pointers 
   !            (res_generic%values(:)%ptr) are nullified.
   !---------------------------------------------------------------------------

   implicit none

   type(residual_generic_type), intent(inout) :: res_generic  ! generic type
   type(residual_template_type), optional, intent(in  ) :: template
   ! Local declarations
   integer :: i

   nullify( res_generic%values)
   if (present( template)) then
      allocate( res_generic%values(template%lbnd:template%ubnd))
      do i=template%lbnd, template%ubnd
         nullify( res_generic%values(i)%ptr)
      end do
   end if
end subroutine da_res_generic_create


subroutine da_res_generic_alloc_and_set( res_generic, val_index, values)

   ! TOdo:  replace this with a full-blown copy-constructor!  
   !---------------------------------------------------------------------------
   ! Purpose:  Allocates and initializes one of the values(either scalar or 
   !           vector) in an already-created residual_generic_type object.  
   !
   ! Method:   Allocate pointer and copy data from values.  Note that a call
   !           to residual_generic_free() will deallocate scalar data but leave
   !           vector data alone.  It is still the callers responsibility to
   !           deallocate pointers to vector data.  In this particular case, 
   !           any vector data allocated here is later passed by reference to 
   !           a global specific object via call to residual_*_from_generic() 
   !          (called from y_type_insert_sound_global()) and later explictly 
   !           deallocated via call to free_global_*().
   !---------------------------------------------------------------------------

   implicit none

   type(residual_generic_type), intent(inout) :: res_generic  ! generic type
   integer,                      intent(in  ) :: val_index    ! which value
   real,                         intent(in  ) :: values(:)    ! values

   if (( val_index < LBOUND(res_generic%values,1)) .OR. &
       ( val_index > UBOUND(res_generic%values,1))) then
      call da_error(__FILE__,__LINE__, &
        (/'residual_generic_alloc_and_set:  bad val_index'/))
   end if
   allocate( res_generic%values(val_index)%ptr(size(values)))
   res_generic%values(val_index)%ptr(:) = values(:)

end subroutine da_res_generic_alloc_and_set


logical function da_res_generic_has_vector( res_generic)

   !---------------------------------------------------------------------------
   ! Purpose:  Returns .true. iff res_generic stores vector values
   !---------------------------------------------------------------------------

   implicit none

   type(residual_generic_type), intent(in) :: res_generic  ! generic type
   da_res_generic_has_vector =( UBOUND(res_generic%values,1) > 0)

end function da_res_generic_has_vector


logical function da_res_generic_has_scalar( res_generic)

   !---------------------------------------------------------------------------
   ! Purpose:  Returns .true. iff res_generic stores scalar values. 
   !---------------------------------------------------------------------------

   implicit none

   type(residual_generic_type), intent(in) :: res_generic  ! generic type
   da_res_generic_has_scalar =( LBOUND(res_generic%values,1) == 0)

end function da_res_generic_has_scalar


subroutine da_res_generic_free( res_generic)

   !---------------------------------------------------------------------------
   ! Purpose:  Free memory for residual_generic_type object.  
   !
   ! Method: pointers to vector values are assumed to point to memory allocated 
   !         by others and are not deallocated.  
   !         This will fail if called for a residual_generic_type object that 
   !         has never been created.  
   !---------------------------------------------------------------------------

   implicit none

   type(residual_generic_type), intent(inout) :: res_generic  ! generic type
   ! Local declarations
   logical :: oktofreevalues

   oktofreevalues = .false.
   if (da_res_generic_has_scalar( res_generic)) then
      ! free scalar memory
      deallocate( res_generic%values(0)%ptr)
      oktofreevalues = .true.
   end if
   if (da_res_generic_has_vector( res_generic)) then
      oktofreevalues = .true.
   end if
   if (oktofreevalues) then
      deallocate( res_generic%values)
   end if

end subroutine da_res_generic_free


subroutine da_y_facade_free( slice)

   !---------------------------------------------------------------------------
   ! Purpose:  Free memory for y_facade_type object.  
   !
   ! Method:   Brute force.  May want to make this smarter... 
   !---------------------------------------------------------------------------

   implicit none

   type(y_facade_type), intent(inout) :: slice
   ! Local declarations
   integer :: n
   if (associated( slice%obs)) then
      do n=1, size(slice%obs)
         call da_res_generic_free( slice%obs(n))
      end do
      deallocate( slice%obs)
   end if
end subroutine da_y_facade_free


