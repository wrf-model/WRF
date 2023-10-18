module hashtable
    use iso_fortran_env, only: int64
  implicit none

  type kv_type
     integer(kind=int64) :: key
     integer(kind=int64) :: value
  end type kv_type

  type node_type
     type(kv_type), allocatable :: kv
     type(node_type), pointer :: next => null()

   contains
     ! If kv is not allocated, allocate and set to the key, value passed in.
     ! If key is present and the same as the key passed in, overwrite the value.
     ! Otherwise, defer to the next node (allocate if not allocated)
     procedure :: node_set

     ! If kv is not allocated, fail and return 0.
     ! If key is present and the same as the key passed in, return the value in kv.
     ! If next pointer is associated, delegate to it.
     ! Otherwise, fail and return 0.
     procedure :: node_get

     ! If kv is not allocated, fail and return
     ! If key is present and node is first in bucket, set first node in bucket to 
     !   the next node of first. Return success
     ! If key is present and the node is another member of the linked list, link the 
     !   previous node's next node to this node's next node, deallocate this node, 
     !   return success

     ! Deallocate kv is allocated.
     ! Call the clear method of the next node if the next pointer associated.
     ! Deallocate and nullify the next pointer.
     procedure :: node_clear

     ! Return the length of the linked list start from the current node.
     procedure :: node_depth
  end type node_type

  public
  type hash_t
     integer(kind=int64) :: n_buckets = 0
     integer(kind=int64) :: n_keys = 0
     type(node_type), allocatable :: buckets(:)
   contains
     procedure, public :: bucket_count
     procedure, public :: n_collisions
     ! Returns number of keys.
     procedure, public :: key_count
     ! Set the value at a given a key.
     procedure, public :: set
     procedure, public :: set_all
     procedure, public :: set_all_idx
     ! Get the value at the given key.
     procedure, public :: get
     ! Clear all the allocated memory (must be called to prevent memory leak).
     procedure, public :: clear
     ! Private hashing function
     procedure, private :: hash
  end type hash_t

contains

  function hash(this,key_value) result(bucket)

    class(hash_t), intent(inout) :: this
    integer(kind=int64), intent(in) :: key_value
    integer(kind=int64) bucket

    bucket = key_value

  end function hash

  function bucket_count(this)
    class(hash_t), intent(inout) :: this
    integer(kind=int64) bucket_count

    bucket_count = this%n_buckets
  end function bucket_count

  function n_collisions(this)
    class(hash_t), intent(inout) :: this
    integer(kind=int64) n_collisions
    integer(kind=int64) i

    n_collisions = 0
    do i = 1, this%n_buckets
       n_collisions = n_collisions + node_depth(this%buckets(i)) - 1
    enddo
  end function n_collisions

  recursive function node_depth(this) result(depth)
    class(node_type), intent(inout) :: this
    integer(kind=int64) depth

    if (.not. associated(this%next)) then
       depth = 1
    else
       depth = 1 + node_depth(this%next)
    endif
  end function node_depth

  pure function key_count(this)
    class(hash_t), intent(in) :: this
    integer(kind=int64) key_count

    key_count = this%n_keys
  end function key_count

  subroutine set(this, key, value)
    class(hash_t), intent(inout) :: this
    integer(kind=int64), intent(in) :: key
    integer(kind=int64), intent(in) :: value
    integer(kind=int64) bucket_id
    logical :: is_new

    bucket_id = modulo(this%hash(key), this%n_buckets) + 1

    call this%buckets(bucket_id)%node_set(key, value)

    if (is_new) this%n_keys = this%n_keys + 1
  end subroutine set

  subroutine set_all_idx(this, keys, length)
    class(hash_t), intent(inout) :: this
    integer(kind=int64), intent(in) :: keys(:)
    integer, optional, intent(in) :: length
    integer(kind=int64) :: i
    integer(kind=int64) bucket_id, n

    if(present(length)) then
       n = length
    else
       n = size(keys)
    end if
    
    this%n_buckets = n
    allocate(this%buckets(n))

    do i = 1, n
       bucket_id = modulo(this%hash(keys(i)),this%n_buckets) + 1
       call this%buckets(bucket_id)%node_set(keys(i), i)
       this%n_keys = this%n_keys + 1
    end do
  end subroutine set_all_idx

  subroutine set_all(this, keys, values)
    class(hash_t), intent(inout) :: this
    integer(kind=int64), intent(in) :: keys(:)
    integer(kind=int64), intent(in) :: values(:)
    integer(kind=int64) bucket_id, i, n

    n = size(keys)

    this%n_buckets = n
    allocate(this%buckets(n))

    do i = 1, n
       bucket_id = modulo(this%hash(keys(i)), this%n_buckets) + 1
       call this%buckets(bucket_id)%node_set(keys(i), values(i))
       this%n_keys = this%n_keys + 1
    end do
  end subroutine set_all

  recursive subroutine node_set(this, key, value)
    class(node_type), intent(inout) :: this
    integer(kind=int64), intent(in) :: key
    integer(kind=int64), intent(in) :: value

    if (.not. allocated(this%kv)) then
       allocate(this%kv)
       this%kv%key = key
       this%kv%value = value
    else if (this%kv%key == key) then
       this%kv%value = this%kv%value
    else
       if (.not. associated(this%next)) then
          allocate(this%next)
       end if
       call this%next%node_set(key, value)
    endif
  end subroutine node_set

  subroutine get(this, key, value, success)
    class(hash_t), intent(inout) :: this
    integer(kind=int64), intent(in) :: key
    integer(kind=int64), intent(out) :: value
    logical, intent(out) :: success
    integer(kind=int64) bucket_id

    success = .false.
    if(this%n_buckets == 0) return
    bucket_id = modulo(key,this%n_buckets) + 1
    call this%buckets(bucket_id)%node_get(key, value, success)
  end subroutine get

  recursive subroutine node_get(this, key, value, success)
    class(node_type), intent(inout) :: this
    integer(kind=int64), intent(in) :: key
    integer(kind=int64), intent(out) :: value
    logical, intent(out) :: success

    success = .false.

    if (.not. allocated(this%kv)) then
       ! Not found. (Initial node in the bucket not set)
       success = .false.
    else if (this%kv%key == key) then
       value = this%kv%value
       success = .true.
    else if (associated(this%next)) then
       call this%next%node_get(key, value, success)
    else
       success = .false.
    endif
  end subroutine node_get

  subroutine clear(this)
    class(hash_t), intent(inout) :: this
    integer(kind=int64) i

    if (.not. allocated(this%buckets)) return

    do i = 1, size(this%buckets)
       if (associated(this%buckets(i)%next)) then 
          call this%buckets(i)%next%node_clear()
          deallocate(this%buckets(i)%next)
          if(allocated(this%buckets(i)%kv)) then
             deallocate(this%buckets(i)%kv)
          endif
       end if
    enddo
    deallocate(this%buckets)
    this%n_keys = 0
    this%n_buckets = 0
  end subroutine clear

  recursive subroutine node_clear(this)
    class(node_type), intent(inout) :: this

    if (associated(this%next)) then
       call this%next%node_clear()
       deallocate(this%next)
       deallocate(this%kv)
       nullify(this%next)
    endif
  end subroutine node_clear

end module hashtable
