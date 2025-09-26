MODULE wrfsi_static
  use wrf_data_pio
  include 'wrf_status_codes.h'
  type (iosystem_desc_t), pointer :: iosystem

CONTAINS
  SUBROUTINE open_wrfsi_static(dataroot, FileDesc)
    use pio
    use pio_kinds
    IMPLICIT NONE
    CHARACTER(LEN=*),  INTENT(IN)  :: dataroot
    type(file_desc_t), INTENT(OUT) :: FileDesc
    CHARACTER(LEN=255)             :: staticfile
    LOGICAL                        :: static_exists
    INTEGER                        :: status

    allocate(iosystem)

    staticfile = TRIM(dataroot) // '/static/static.wrfsi'
    INQUIRE(FILE=staticfile, EXIST=static_exists)
    IF (static_exists) THEN
      status = PIO_openfile(iosystem, FileDesc, &
                            PIO_iotype_pnetcdf, TRIM(staticfile))
      IF (status .NE. PIO_NOERR) THEN
        PRINT '(A,I5)', 'NetCDF error opening WRF static file: ',status
        STOP 'open_wrfsi_static'
      END IF 
    ELSE
      staticfile = TRIM(dataroot) // '/static/static.wrfsi.rotlat'
      INQUIRE(FILE=staticfile, EXIST=static_exists)
      IF(static_exists) THEN
        status = PIO_openfile(iosystem, FileDesc, &
                              PIO_iotype_pnetcdf, TRIM(staticfile))
        IF(status .NE. PIO_NOERR) THEN
          PRINT '(A,I5)', 'NetCDF error opening WRF static file: ',status
          STOP 'open_wrfsi_static'
        END IF
      ELSE
        PRINT '(A)', 'rotlat Static file not found, either: ', staticfile
        STOP 'open_wrfsi_static'
      ENDIF

    ENDIF

    RETURN
  END SUBROUTINE open_wrfsi_static      

!--------------------------------------------------------------------
  SUBROUTINE get_wrfsi_static_dims(dataroot, nx, ny)
  
    ! Subroutine to return the horizontal dimensions of WRF static file
    ! contained in the input dataroot

    use pio
    use pio_kinds

    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)  :: dataroot
    INTEGER         , INTENT(OUT) :: nx
    INTEGER         , INTENT(OUT) :: ny

    INTEGER                       :: vid, status
    type (file_desc_t)            :: FileDesc

    CALL open_wrfsi_static(dataroot, FileDesc)
    status = pio_inq_dimid(FileDesc, 'x', vid)
    status = pio_inq_dimlen(FileDesc, vid, nx)
    status = pio_inq_dimid(FileDesc, 'y', vid)
    status = pio_inq_dimlen(FileDesc, vid, ny)
    write(unit=*, fmt='(2(A,I5))') 'WRF X-dimension = ',nx, &
                                 ', WRF Y-dimension = ',ny  
    call pio_closefile(FileDesc)
    deallocate(iosystem)
    RETURN
  END SUBROUTINE get_wrfsi_static_dims     

!--------------------------------------------------------------------
  SUBROUTINE get_wrfsi_static_2d(dataroot, varname, data)
    use pio
    use pio_kinds
    IMPLICIT NONE
   !Gets any 2D variable from the static file
    CHARACTER(LEN=*), INTENT(IN)  :: dataroot
    CHARACTER(LEN=*), INTENT(IN)  :: varname
    REAL, INTENT(OUT)             :: data(:,:)
 
    INTEGER                       :: vid, status
    type (file_desc_t)            :: FileDesc
   
    CALL open_wrfsi_static(dataroot, FileDesc)
    status = pio_inq_varid(FileDesc, varname, vid)
    status = pio_get_var(FileDesc, vid, data)
   !status = get_var_2d_real(FileDesc, vid, data)
    IF(status .NE. PIO_NOERR) THEN
      write(unit=*, fmt='(A)') 'Problem getting 2D data.'
    ENDIF 
    call pio_closefile(FileDesc)
    deallocate(iosystem)
    RETURN
  END SUBROUTINE get_wrfsi_static_2d    
END MODULE wrfsi_static

