module fortglob
    use iso_c_binding
    implicit none

    private
    public :: glob_t, globfiles

    integer, parameter :: PATH_MAX = 4096

    type :: glob_t
        integer :: nfiles
        character(len=PATH_MAX), allocatable :: filenames(:)
    end type

    type, bind(c) :: globrec_c
        integer (c_size_t) :: nfiles
        type(c_ptr) :: filenames
        type(c_ptr) :: globptr
    end type globrec_c

    interface
        function globfiles_c(pattern) bind(c)
            use iso_c_binding
            character(kind=c_char), dimension(*), intent(in) :: pattern
            type(c_ptr) :: globfiles_c
        end function globfiles_c

        subroutine freeglobrec(recptr) bind (c)
            use iso_c_binding
            type(c_ptr), intent(in) :: recptr
        end subroutine

        function strlen(s) bind(c)
            use iso_c_binding
            type(c_ptr), intent(in), value :: s
            integer(c_size_t) :: strlen
        end function strlen
    end interface

    contains

    function globfiles(pattern)
        use iso_c_binding

        type(glob_t) :: globfiles
        character(len=*), intent(in) :: pattern

        integer :: i, j, slen
        character(len=PATH_MAX) :: pattern_c
        type(c_ptr) :: glob_c_ptr
        type(globrec_c), pointer :: glob_f_ptr
        type(c_ptr), pointer :: c_strs(:)
        character(kind=c_char), pointer, dimension(:) :: temp_fstrp

        pattern_c = pattern
        slen = len_trim(pattern_c)
        pattern_c(slen+1:slen+1) = c_null_char
        glob_c_ptr = globfiles_c(pattern_c)
        call c_f_pointer(glob_c_ptr, glob_f_ptr)
        globfiles%nfiles = glob_f_ptr%nfiles

        allocate(globfiles%filenames(globfiles%nfiles))

        call c_f_pointer(glob_f_ptr%filenames, c_strs, [glob_f_ptr%nfiles])
        do i = 1, globfiles%nfiles
            slen = strlen(c_strs(i))
            call c_f_pointer(c_strs(i), temp_fstrp, [slen])
            globfiles%filenames(i) = ''
            do j = 1, slen
                globfiles%filenames(i)(j:j) = temp_fstrp(j)
            end do
        end do

        call freeglobrec(glob_f_ptr%globptr)
    end function
end module
