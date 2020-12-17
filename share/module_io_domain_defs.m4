! 1 input or output  (direction of io)
! 2 aux or nothing
! 3 _ or nothing
! 4 input or output  (stream)
! 5 number of stream or nothing

define( DATASET_IO_ROUTINE,
`  SUBROUTINE $1_$2$3$4 ( fid , grid , config_flags , ierr )
    IMPLICIT NONE
    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
    INTEGER, INTENT(IN) :: fid
    INTEGER, INTENT(INOUT) :: ierr
    IF ( config_flags%io_form_$2$3$4 .GT. 0 ) THEN
      CALL $1_wrf ( fid , grid , config_flags , $2$3$4_only , ierr )
    ENDIF
    RETURN
  END SUBROUTINE $1_$2$3$4'
)

DATASET_IO_ROUTINE(output,,input,)
DATASET_IO_ROUTINE(output,aux,input,1)
DATASET_IO_ROUTINE(output,aux,input,2)
DATASET_IO_ROUTINE(output,aux,input,3)
DATASET_IO_ROUTINE(output,aux,input,4)
DATASET_IO_ROUTINE(output,aux,input,5)
DATASET_IO_ROUTINE(output,aux,input,6)
DATASET_IO_ROUTINE(output,aux,input,7)
DATASET_IO_ROUTINE(output,aux,input,8)
DATASET_IO_ROUTINE(output,aux,input,9)
DATASET_IO_ROUTINE(output,aux,input,10)
DATASET_IO_ROUTINE(output,aux,input,11)
DATASET_IO_ROUTINE(output,,history,)
DATASET_IO_ROUTINE(output,aux,hist,1)
DATASET_IO_ROUTINE(output,aux,hist,2)
DATASET_IO_ROUTINE(output,aux,hist,3)
DATASET_IO_ROUTINE(output,aux,hist,4)
DATASET_IO_ROUTINE(output,aux,hist,5)
DATASET_IO_ROUTINE(output,aux,hist,6)
DATASET_IO_ROUTINE(output,aux,hist,7)
DATASET_IO_ROUTINE(output,aux,hist,8)
DATASET_IO_ROUTINE(output,aux,hist,9)
DATASET_IO_ROUTINE(output,aux,hist,10)
DATASET_IO_ROUTINE(output,aux,hist,11)

DATASET_IO_ROUTINE(input,input,)
DATASET_IO_ROUTINE(input,aux,input,1)
DATASET_IO_ROUTINE(input,aux,input,2)
DATASET_IO_ROUTINE(input,aux,input,3)
DATASET_IO_ROUTINE(input,aux,input,4)
DATASET_IO_ROUTINE(input,aux,input,5)
DATASET_IO_ROUTINE(input,aux,input,6)
DATASET_IO_ROUTINE(input,aux,input,7)
DATASET_IO_ROUTINE(input,aux,input,8)
DATASET_IO_ROUTINE(input,aux,input,9)
DATASET_IO_ROUTINE(input,aux,input,10)
DATASET_IO_ROUTINE(input,aux,input,11)
DATASET_IO_ROUTINE(input,history,)
DATASET_IO_ROUTINE(input,aux,hist,1)
DATASET_IO_ROUTINE(input,aux,hist,2)
DATASET_IO_ROUTINE(input,aux,hist,3)
DATASET_IO_ROUTINE(input,aux,hist,4)
DATASET_IO_ROUTINE(input,aux,hist,5)
DATASET_IO_ROUTINE(input,aux,hist,6)
DATASET_IO_ROUTINE(input,aux,hist,7)
DATASET_IO_ROUTINE(input,aux,hist,8)
DATASET_IO_ROUTINE(input,aux,hist,9)
DATASET_IO_ROUTINE(input,aux,hist,10)
DATASET_IO_ROUTINE(input,aux,hist,11)
