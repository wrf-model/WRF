module mpas_mesh

    use scan_input

    type mpas_mesh_type
        logical :: valid = .false.
        integer :: nCells = 0
        integer :: nVertices = 0
        integer :: nEdges = 0
        integer :: maxEdges = 0
        integer, dimension(:), pointer :: landmask => null()
        integer, dimension(:), pointer :: nEdgesOnCell => null()
        integer, dimension(:,:), pointer :: cellsOnCell => null()
        integer, dimension(:,:), pointer :: verticesOnCell => null()
        integer, dimension(:,:), pointer :: cellsOnVertex => null()
        integer, dimension(:,:), pointer :: edgesOnCell => null()
        integer, dimension(:,:), pointer :: cellsOnEdge => null()
        real, dimension(:), pointer :: latCell => null()
        real, dimension(:), pointer :: lonCell => null()
        real, dimension(:), pointer :: latVertex => null()
        real, dimension(:), pointer :: lonVertex => null()
        real, dimension(:), pointer :: latEdge => null()
        real, dimension(:), pointer :: lonEdge => null()
    end type mpas_mesh_type


    contains


    integer function mpas_mesh_setup(mesh_filename, mesh) result(stat)

        implicit none

        character (len=*), intent(in) :: mesh_filename
        type (mpas_mesh_type), intent(out) :: mesh

        type (input_handle_type) :: handle
        type (input_field_type) :: field

        stat = 0

        if (scan_input_open(mesh_filename, handle) /= 0) then
            stat = 1
            return
        end if

        !
        ! nEdgesOnCell
        !
        if (scan_input_for_field(handle, 'nEdgesOnCell', field) /= 0) then
            stat = 1
            return
        end if

        mesh % nCells = field % dimlens(1)
        mesh % nVertices = 2 * (mesh % nCells - 2)
        mesh % nEdges = 3 * (mesh % nCells - 2)

        stat = scan_input_read_field(field)
        allocate(mesh % nEdgesOnCell(mesh % nCells))
        mesh % nEdgesOnCell(:) = field % array1i(:)
        stat = scan_input_free_field(field)

        !
        ! landmask
        !
        if (scan_input_for_field(handle, 'landmask', field) == 0) then
            stat = scan_input_read_field(field)
            allocate(mesh % landmask(mesh % nCells))
            mesh % landmask(:) = field % array1i(:)
            stat = scan_input_free_field(field)
        else     ! no landmask available
            allocate(mesh % landmask(mesh % nCells))
            mesh % landmask(:) = 1
        end if

        !
        ! cellsOnCell
        !
        if (scan_input_for_field(handle, 'cellsOnCell', field) /= 0) then
            stat = 1
            return
        end if

        mesh % maxEdges = field % dimlens(1)

        stat = scan_input_read_field(field)
        allocate(mesh % cellsOnCell(mesh % maxEdges, mesh % nCells))
        mesh % cellsOnCell(:,:) = field % array2i(:,:)
        stat = scan_input_free_field(field)

        !
        ! verticesOnCell
        !
        if (scan_input_for_field(handle, 'verticesOnCell', field) /= 0) then
            stat = 1
            return
        end if

        stat = scan_input_read_field(field)
        allocate(mesh % verticesOnCell(mesh % maxEdges, mesh % nCells))
        mesh % verticesOnCell(:,:) = field % array2i(:,:)
        stat = scan_input_free_field(field)

        !
        ! cellsOnVertex
        !
        if (scan_input_for_field(handle, 'cellsOnVertex', field) /= 0) then
            stat = 1
            return
        end if

        stat = scan_input_read_field(field)
        allocate(mesh % cellsOnVertex(3, mesh % nVertices))
        mesh % cellsOnVertex(:,:) = field % array2i(:,:)
        stat = scan_input_free_field(field)

        !
        ! edgesOnCell
        !
        if (scan_input_for_field(handle, 'edgesOnCell', field) /= 0) then
            stat = 1
            return
        end if

        stat = scan_input_read_field(field)
        allocate(mesh % edgesOnCell(mesh % maxEdges, mesh % nCells))
        mesh % edgesOnCell(:,:) = field % array2i(:,:)
        stat = scan_input_free_field(field)

        !
        ! cellsOnEdge
        !
        if (scan_input_for_field(handle, 'cellsOnEdge', field) /= 0) then
            stat = 1
            return
        end if

        stat = scan_input_read_field(field)
        allocate(mesh % cellsOnEdge(2, mesh % nEdges))
        mesh % cellsOnEdge(:,:) = field % array2i(:,:)
        stat = scan_input_free_field(field)

        !
        ! latCell
        !
        if (scan_input_for_field(handle, 'latCell', field) /= 0) then
            stat = 1
            return
        end if

        stat = scan_input_read_field(field)
        allocate(mesh % latCell(mesh % nCells))
        if (field % xtype == FIELD_TYPE_REAL) then
            mesh % latCell(:) = field % array1r(:)
        else if (field % xtype == FIELD_TYPE_DOUBLE) then
            mesh % latCell(:) = real(field % array1d(:))
        end if
        stat = scan_input_free_field(field)

        !
        ! lonCell
        !
        if (scan_input_for_field(handle, 'lonCell', field) /= 0) then
            stat = 1
            return
        end if

        stat = scan_input_read_field(field)
        allocate(mesh % lonCell(mesh % nCells))
        if (field % xtype == FIELD_TYPE_REAL) then
            mesh % lonCell(:) = field % array1r(:)
        else if (field % xtype == FIELD_TYPE_DOUBLE) then
            mesh % lonCell(:) = real(field % array1d(:))
        end if
        stat = scan_input_free_field(field)

        !
        ! latVertex
        !
        if (scan_input_for_field(handle, 'latVertex', field) /= 0) then
            stat = 1
            return
        end if

        stat = scan_input_read_field(field)
        allocate(mesh % latVertex(mesh % nVertices))
        if (field % xtype == FIELD_TYPE_REAL) then
            mesh % latVertex(:) = field % array1r(:)
        else if (field % xtype == FIELD_TYPE_DOUBLE) then
            mesh % latVertex(:) = real(field % array1d(:))
        end if
        stat = scan_input_free_field(field)

        !
        ! lonVertex
        !
        if (scan_input_for_field(handle, 'lonVertex', field) /= 0) then
            stat = 1
            return
        end if

        stat = scan_input_read_field(field)
        allocate(mesh % lonVertex(mesh % nVertices))
        if (field % xtype == FIELD_TYPE_REAL) then
            mesh % lonVertex(:) = field % array1r(:)
        else if (field % xtype == FIELD_TYPE_DOUBLE) then
            mesh % lonVertex(:) = real(field % array1d(:))
        end if
        stat = scan_input_free_field(field)

        !
        ! latEdge
        !
        if (scan_input_for_field(handle, 'latEdge', field) /= 0) then
            stat = 1
            return
        end if

        stat = scan_input_read_field(field)
        allocate(mesh % latEdge(mesh % nEdges))
        if (field % xtype == FIELD_TYPE_REAL) then
            mesh % latEdge(:) = field % array1r(:)
        else if (field % xtype == FIELD_TYPE_DOUBLE) then
            mesh % latEdge(:) = real(field % array1d(:))
        end if
        stat = scan_input_free_field(field)

        !
        ! lonEdge
        !
        if (scan_input_for_field(handle, 'lonEdge', field) /= 0) then
            stat = 1
            return
        end if

        stat = scan_input_read_field(field)
        allocate(mesh % lonEdge(mesh % nEdges))
        if (field % xtype == FIELD_TYPE_REAL) then
            mesh % lonEdge(:) = field % array1r(:)
        else if (field % xtype == FIELD_TYPE_DOUBLE) then
            mesh % lonEdge(:) = real(field % array1d(:))
        end if
        stat = scan_input_free_field(field)
 
        stat = scan_input_close(handle)

        mesh % valid = .true.

    end function mpas_mesh_setup


    integer function mpas_mesh_free(mesh) result(stat)

        implicit none

        type (mpas_mesh_type), intent(inout) :: mesh


        stat = 0

        mesh % valid = .false.
        mesh % nCells = 0
        mesh % nVertices = 0
        mesh % nEdges = 0
        mesh % maxEdges = 0

        if (associated(mesh % landmask)) then
            deallocate(mesh % landmask)
        end if
        if (associated(mesh % nEdgesOnCell)) then
            deallocate(mesh % nEdgesOnCell)
        end if
        if (associated(mesh % cellsOnCell)) then
            deallocate(mesh % cellsOnCell)
        end if
        if (associated(mesh % verticesOnCell)) then
            deallocate(mesh % verticesOnCell)
        end if
        if (associated(mesh % cellsOnVertex)) then
            deallocate(mesh % cellsOnVertex)
        end if
        if (associated(mesh % edgesOnCell)) then
            deallocate(mesh % edgesOnCell)
        end if
        if (associated(mesh % cellsOnEdge)) then
            deallocate(mesh % cellsOnEdge)
        end if
        if (associated(mesh % latCell)) then
            deallocate(mesh % latCell)
        end if
        if (associated(mesh % lonCell)) then
            deallocate(mesh % lonCell)
        end if
        if (associated(mesh % latVertex)) then
            deallocate(mesh % latVertex)
        end if
        if (associated(mesh % lonVertex)) then
            deallocate(mesh % lonVertex)
        end if
        if (associated(mesh % latEdge)) then
            deallocate(mesh % latEdge)
        end if
        if (associated(mesh % lonEdge)) then
            deallocate(mesh % lonEdge)
        end if

    end function mpas_mesh_free

end module mpas_mesh
