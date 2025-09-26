MODULE wrfsi_static

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE open_wrfsi_static(dataroot,cdfid)
  
    IMPLICIT NONE
    INCLUDE "netcdf.inc"
    CHARACTER(LEN=*), INTENT(IN)   :: dataroot
    INTEGER, INTENT(OUT)           :: cdfid
    CHARACTER(LEN=255)            :: staticfile
    LOGICAL                       :: static_exists
    INTEGER                       :: status

    staticfile = TRIM(dataroot) // '/static/static.wrfsi'
    INQUIRE(FILE=staticfile, EXIST=static_exists)
    IF (static_exists) THEN
      status = NF_OPEN(TRIM(staticfile),NF_NOWRITE,cdfid)
      IF (status .NE. NF_NOERR) THEN
        PRINT '(A,I5)', 'NetCDF error opening WRF static file: ',status
        STOP 'open_wrfsi_static'
      END IF 

    ELSE

!mp
!	search for rotlat version??
!      PRINT '(A)', 'Static file not found ', staticfile
!      PRINT '(A)', 'Look for NMM version'
      staticfile = TRIM(dataroot) // '/static/static.wrfsi.rotlat'
      INQUIRE(FILE=staticfile, EXIST=static_exists)
		    IF (static_exists) THEN
		 status = NF_OPEN(TRIM(staticfile),NF_NOWRITE,cdfid)
      IF (status .NE. NF_NOERR) THEN
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE get_wrfsi_static_dims(dataroot, nx, ny)
  
    ! Subroutine to return the horizontal dimensions of WRF static file
    ! contained in the input dataroot

    IMPLICIT NONE
    INCLUDE "netcdf.inc"
    CHARACTER(LEN=*), INTENT(IN)  :: dataroot
    INTEGER         , INTENT(OUT) :: nx
    INTEGER         , INTENT(OUT) :: ny

    INTEGER                       :: cdfid,vid, status

    CALL open_wrfsi_static(dataroot,cdfid)
    status = NF_INQ_DIMID(cdfid, 'x', vid)
    status = NF_INQ_DIMLEN(cdfid, vid, nx)
    status = NF_INQ_DIMID(cdfid, 'y', vid)
    status = NF_INQ_DIMLEN(cdfid, vid, ny) 
      PRINT '(A,I5,A,I5)', 'WRF X-dimension = ',nx, &
        ' WRF Y-dimension = ',ny  
    status = NF_CLOSE(cdfid)  
    RETURN
  END SUBROUTINE get_wrfsi_static_dims     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE get_wrfsi_static_2d(dataroot, varname, data)

    IMPLICIT NONE
    INCLUDE "netcdf.inc"
    ! Gets any 2D variable from the static file
    CHARACTER(LEN=*), INTENT(IN)  :: dataroot
    CHARACTER(LEN=*), INTENT(IN)  :: varname
    REAL, INTENT(OUT)             :: data(:,:)
 
    INTEGER                             :: cdfid, vid, status
   
    CALL open_wrfsi_static(dataroot,cdfid)
    status = NF_INQ_VARID(cdfid,varname,vid)
    status = NF_GET_VAR_REAL(cdfid,vid,data)
    IF (status .NE. NF_NOERR) THEN
      PRINT '(A)', 'Problem getting 2D data.'
    ENDIF 
    status = NF_CLOSE(cdfid) 
    RETURN
  END SUBROUTINE get_wrfsi_static_2d    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
END MODULE wrfsi_static
