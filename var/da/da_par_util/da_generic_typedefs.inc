   ! generic vector type
   type generic_vector_type
      real, pointer :: ptr(:)
   end type generic_vector_type

   ! Generic residual type contains lists of vectors and scalars.  
   ! Implementation notes:   
   !  - Vector values are always assumed to be stored by reference in
   !    self%values(1:UBOUND(self%values,1))%ptr.  The size of each vector
   !    is the same.  These pointers are used to reference arrays that 
   !    will be deallocated elsewhere, they are not deallocated by 
   !    destructor residual_generic_free().  When 
   !    UBOUND(self%values,1)==0, there are no vector values.
   !  - Scalars are copied into and out of self%values(0)%ptr.  This pointer
   !    is be allocated and deallocated by this class.  When
   !    LBOUND(self%values,1)==1, there are no scalar values.  The
   !    duplication of scalars is unavoidable without changing
   !    implementations of the specific residual_*_type classes.
   !  - It would be better to store references to model_loc_type and
   !    info_type objects here rather than copying proc_domain and
   !    obs_global_index.  Make this change later when there is
   !    time to modify other objects to also store references(there should 
   !    be only one object responsible for each model_loc_type and info_type
   !    object, but in Fortran90 this must be implemented as a pointer).
   type residual_generic_type
      logical                             :: proc_domain
      integer                             :: obs_global_index
      type(generic_vector_type), pointer :: values(:) ! vectors & scalars
   end type residual_generic_type

   ! template for allocating memory
   type residual_template_type
      integer                             :: lbnd  ! lower bound
      integer                             :: ubnd  ! upper bound
   end type residual_template_type

   ! single-obs generic y_type
   type y_facade_type
      integer                               :: num_obs
      integer                               :: num_obs_glo
      type(residual_generic_type), pointer :: obs(:)
   end type y_facade_type


