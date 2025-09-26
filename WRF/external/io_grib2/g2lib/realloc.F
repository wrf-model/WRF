      module re_alloc

      interface realloc
         module procedure realloc_c1
         module procedure realloc_r
         module procedure realloc_i
!!         subroutine realloc_c1(c,n,m,istat)
!!            character(len=1),pointer,dimension(:) :: c
!!            integer :: n,m
!!            integer :: istat
!!         end subroutine
!!         subroutine realloc_r(c,n,m,istat)
!!            real,pointer,dimension(:) :: c
!!            integer :: n,m
!!            integer :: istat
!!         end subroutine
!!         subroutine realloc_i(c,n,m,istat)
!!            integer,pointer,dimension(:) :: c
!!            integer :: n,m
!!            integer :: istat
!!         end subroutine
      end interface

      contains

         subroutine realloc_c1(c,n,m,istat)
            character(len=1),pointer,dimension(:) :: c
            integer,intent(in) :: n,m
            integer,intent(out) :: istat
            integer :: num
            character(len=1),pointer,dimension(:) :: tmp

            istat=0
            if ( (n<0) .OR. (m<=0) ) then
               istat=10
               return
            endif
 
            if ( .not. associated(c) ) then
               allocate(c(m),stat=istat)   ! allocate new memory
               return
            endif

            tmp=>c                      ! save pointer to original mem
            nullify(c)
            allocate(c(m),stat=istat)   ! allocate new memory
            if ( istat /= 0 ) then
               c=>tmp
               return
            endif
            if ( n /= 0 ) then
               num=min(n,m)
               c(1:num)=tmp(1:num)      ! copy data from orig mem to new loc.
            endif
            deallocate(tmp)             ! deallocate original memory
            return
         end subroutine

         subroutine realloc_r(c,n,m,istat)
            real,pointer,dimension(:) :: c
            integer,intent(in) :: n,m
            integer,intent(out) :: istat
            integer :: num
            real,pointer,dimension(:) :: tmp

            istat=0
            if ( (n<0) .OR. (m<=0) ) then
               istat=10
               return
            endif
 
            if ( .not. associated(c) ) then
               allocate(c(m),stat=istat)   ! allocate new memory
               return
            endif

            tmp=>c                      ! save pointer to original mem
            nullify(c)
            allocate(c(m),stat=istat)   ! allocate new memory
            if ( istat /= 0 ) then
               c=>tmp
               return
            endif
            if ( n /= 0 ) then
               num=min(n,m)
               c(1:num)=tmp(1:num)      ! copy data from orig mem to new loc.
            endif
            deallocate(tmp)             ! deallocate original memory
            return
         end subroutine

         subroutine realloc_i(c,n,m,istat)
            integer,pointer,dimension(:) :: c
            integer,intent(in) :: n,m
            integer,intent(out) :: istat
            integer :: num
            integer,pointer,dimension(:) :: tmp

            istat=0
            if ( (n<0) .OR. (m<=0) ) then
               istat=10
               return
            endif
 
            if ( .not. associated(c) ) then
               allocate(c(m),stat=istat)   ! allocate new memory
               return
            endif

            tmp=>c                      ! save pointer to original mem
            nullify(c)
            allocate(c(m),stat=istat)   ! allocate new memory
            if ( istat /= 0 ) then
               c=>tmp
               return
            endif
            if ( n /= 0 ) then
               num=min(n,m)
               c(1:num)=tmp(1:num)      ! copy data from orig mem to new loc.
            endif
            deallocate(tmp)             ! deallocate original memory
            return
         end subroutine

      end module re_alloc
