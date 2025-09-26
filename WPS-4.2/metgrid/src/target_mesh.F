module target_mesh

    type target_mesh_type
        logical :: valid = .false.
        integer :: irank = 0
        integer :: nLat = 0
        integer :: nLon = 0
        real :: startLat = 0.0
        real :: endLat = 0.0
        real :: startLon = 0.0
        real :: endLon = 0.0
        real, dimension(:,:), pointer :: lats => null()
        real, dimension(:,:), pointer :: lons => null()
    end type target_mesh_type


    contains


    integer function target_mesh_setup(mesh, lat2d, lon2d) result(stat)

        implicit none

        type (target_mesh_type), intent(out) :: mesh
        real, dimension(:,:), target, optional :: lat2d
        real, dimension(:,:), target, optional :: lon2d

        integer :: i, j
        integer :: iostatus
        integer :: eqIdx
        real :: delta
        logical :: exists
        character (len=64) :: spec
        real, parameter :: pi_const = 2.0 * asin(1.0)

        stat = 0

        !
        ! If 2-d arrays of latitude and longitude are provided, we can just
        ! point to those arrays rather than generate lat/lon values based on
        ! a specified target domain
        !
        if (present(lat2d) .and. present(lon2d)) then

            mesh % irank = 1
            mesh % nLat = size(lat2d,2)
            mesh % nLon = size(lon2d,1)
            mesh % lats => lat2d
            mesh % lons => lon2d
            mesh % valid = .true.

            return
        end if


        !
        ! Try to parse nLat, nLon from target_domain file
        !
        inquire(file='target_domain', exist=exists)
        if (exists) then
            write(0,*) ' '
            write(0,*) 'Reading target domain specification from file ''target_domain'''

            mesh % startLat = -90.0
            mesh % endLat   =  90.0
            mesh % startLon = -180.0
            mesh % endLon   =  180.0
            mesh % nLat = 360
            mesh % nLon = 720

            open(22, file='target_domain', form='formatted')
            read(22,fmt='(a)',iostat=iostatus) spec
            j = 1
            do while (iostatus >= 0) 
                call despace(spec)
                eqIdx = index(spec, '=')
                if (eqIdx /= 0) then
                    if (spec(1:eqIdx-1) == 'nlat') then
                        read(spec(eqIdx+1:len_trim(spec)),fmt=*) mesh % nLat
                        write(0,*) 'Setting nlat = ', mesh % nLat
                    else if (spec(1:eqIdx-1) == 'nlon') then
                        read(spec(eqIdx+1:len_trim(spec)),fmt=*) mesh % nLon
                        write(0,*) 'Setting nlon = ', mesh % nLon
                    else if (spec(1:eqIdx-1) == 'startlat') then
                        read(spec(eqIdx+1:len_trim(spec)),fmt=*) mesh % startLat
                        write(0,*) 'Setting startlat = ', mesh % startLat
                    else if (spec(1:eqIdx-1) == 'endlat') then
                        read(spec(eqIdx+1:len_trim(spec)),fmt=*) mesh % endLat
                        write(0,*) 'Setting endlat = ', mesh % endLat
                    else if (spec(1:eqIdx-1) == 'startlon') then
                        read(spec(eqIdx+1:len_trim(spec)),fmt=*) mesh % startLon
                        write(0,*) 'Setting startlon = ', mesh % startLon
                    else if (spec(1:eqIdx-1) == 'endlon') then
                        read(spec(eqIdx+1:len_trim(spec)),fmt=*) mesh % endLon
                        write(0,*) 'Setting endlon = ', mesh % endLon
                    else
                        write(0,*) 'Unrecognized keyword on line ', j, ' of file ''target_domain'': '//spec(1:eqIdx-1)
                        stat = 1
                        close(22)
                        return
                    end if
                else
                    write(0,*) 'Syntax error on line ', j, ' of file ''target_domain'': ''='' not found'
                    stat = 1
                    close(22)
                    return
                end if
                read(22,fmt='(a)',iostat=iostatus) spec
                j = j + 1
            end do
            close(22)
        else
            write(0,*) ' '
            write(0,*) 'Target domain specification file ''target_domain'' not found.'
            write(0,*) 'Default 0.5-degree global target domain will be used.'
            write(0,*) ' '

            mesh % startLat = -90.0
            mesh % endLat   =  90.0
            mesh % startLon = -180.0
            mesh % endLon   =  180.0
            mesh % nLat = 360
            mesh % nLon = 720
        end if


        allocate(mesh % lats(1, mesh % nLat))
        allocate(mesh % lons(mesh % nLon, 1))

        delta = (mesh % endLat - mesh % startLat) / real(mesh % nLat)
        do i=0,mesh % nLat-1
           mesh % lats(1,i+1) = mesh % startLat + 0.5 * delta + real(i) * delta
           mesh % lats(1,i+1) = mesh % lats(1,i+1) * pi_const / 180.0
        end do

        delta = (mesh % endLon - mesh % startLon) / real(mesh % nLon)
        do i=0,mesh % nLon-1
           mesh % lons(i+1,1) = mesh % startLon + 0.5 * delta + real(i) * delta
           mesh % lons(i+1,1) = mesh % lons(i+1,1) * pi_const / 180.0
        end do

        mesh % valid = .true.

    end function target_mesh_setup


    integer function target_mesh_free(mesh) result(stat)

        implicit none

        type (target_mesh_type), intent(inout) :: mesh


        stat = 0

        mesh % valid = .false.
        mesh % nLat = 0
        mesh % nLon = 0
        mesh % startLat = 0.0
        mesh % endLat = 0.0
        mesh % startLon = 0.0
        mesh % endLon = 0.0

        !
        ! When irank == 0, we allocated the lats and lons arrays
        ! internally and should therefore deallocate them
        !
        if (mesh % irank == 0) then
           if (associated(mesh % lats)) deallocate(mesh % lats)
           if (associated(mesh % lons)) deallocate(mesh % lons)
        end if

    end function target_mesh_free


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: despace
   !
   ! Purpose: Remove all space and tab characters from a string, thus
   !          compressing the string to the left.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine despace(string)

       implicit none

       ! Arguments
       character (len=*), intent(inout) :: string

       ! Local variables
       integer :: i, j, length, iquoted

       length = len(string)

       iquoted = 0
       j = 1
       do i=1,length
           ! Check for a quote mark
           if (string(i:i) == '"' .or. string(i:i) == '''') iquoted = mod(iquoted+1,2)
 
           ! Check for non-space, non-tab character, or if we are inside quoted
           ! text
           if ((string(i:i) /= ' ' .and. string(i:i) /= achar(9)) .or. iquoted == 1) then
               string(j:j) = string(i:i)
               j = j + 1
           end if
       end do
 
       do i=j,length
           string(i:i) = ' '
       end do

   end subroutine despace

end module target_mesh
