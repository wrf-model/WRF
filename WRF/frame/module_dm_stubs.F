!WRF:PACKAGE:NODM
!
MODULE module_dm
  USE module_driver_constants

  LOGICAL intercomm_active( max_domains ), domain_active_this_task( max_domains )

  CONTAINS
   SUBROUTINE init_module_dm
      intercomm_active = .TRUE.
      domain_active_this_task = .TRUE.
   END SUBROUTINE init_module_dm

   REAL FUNCTION wrf_dm_max_real ( inval )
      IMPLICIT NONE
      REAL inval
      wrf_dm_max_real = inval
   END FUNCTION wrf_dm_max_real

   REAL FUNCTION wrf_dm_min_real ( inval )
      IMPLICIT NONE
      REAL inval
      wrf_dm_min_real = inval
   END FUNCTION wrf_dm_min_real

   SUBROUTINE wrf_dm_min_reals ( inval, retval, n )
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n
      REAL, INTENT(IN) :: inval(:)
      REAL, INTENT(OUT) :: retval(:)
      retval(:) = inval(:)
   END SUBROUTINE wrf_dm_min_reals

   REAL FUNCTION wrf_dm_sum_real ( inval )
      IMPLICIT NONE
      REAL inval
      wrf_dm_sum_real = inval
   END FUNCTION wrf_dm_sum_real

   SUBROUTINE wrf_dm_sum_reals ( inval, retval )
      IMPLICIT NONE
      REAL, INTENT(IN) :: inval(:)
      REAL, INTENT(OUT) :: retval(:)
      retval(:) = inval(:)
   END SUBROUTINE wrf_dm_sum_reals

   INTEGER FUNCTION wrf_dm_sum_integer ( inval )
      IMPLICIT NONE
      INTEGER inval
      wrf_dm_sum_integer = inval
   END FUNCTION wrf_dm_sum_integer

   SUBROUTINE wrf_dm_sum_integers ( inval, retval )
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: inval(:)
      INTEGER, INTENT(OUT) :: retval(:)
      retval(:) = inval(:)
   END SUBROUTINE wrf_dm_sum_integers

   INTEGER FUNCTION wrf_dm_bxor_integer ( inval )
      IMPLICIT NONE
      INTEGER inval
      wrf_dm_bxor_integer = inval
   END FUNCTION wrf_dm_bxor_integer

   SUBROUTINE wrf_dm_maxval ( val, idex, jdex )
      IMPLICIT NONE
      REAL val
      INTEGER idex, jdex
      RETURN
   END SUBROUTINE wrf_dm_maxval

  SUBROUTINE wrf_dm_minval ( val, idex, jdex )
     IMPLICIT NONE
     REAL val
     INTEGER idex, jdex
     RETURN
  END SUBROUTINE wrf_dm_minval

   SUBROUTINE wrf_dm_maxtile_real ( val , tile)
      IMPLICIT NONE
      REAL val
      INTEGER tile
   END SUBROUTINE wrf_dm_maxtile_real

   SUBROUTINE wrf_dm_mintile_double ( val , tile)
      IMPLICIT NONE
      DOUBLE PRECISION val
      INTEGER tile
   END SUBROUTINE wrf_dm_mintile_double

   SUBROUTINE wrf_dm_tile_val_int ( val , tile)
      IMPLICIT NONE
      INTEGER val
      INTEGER tile
   END SUBROUTINE wrf_dm_tile_val_int

! stub
   SUBROUTINE wrf_dm_move_nest ( parent, nest, dx, dy )
      USE module_domain
      TYPE (domain), INTENT(INOUT) :: parent, nest
      INTEGER, INTENT(IN)          :: dx,dy
      RETURN
   END SUBROUTINE wrf_dm_move_nest

END MODULE module_dm

!=========================================================================

! These are stub functions that do the right thing (usually nothing)
! in case DM_PARALLEL is not compiled for.
! This file, src/module_dm_stubs.F is copied to src/module_dm.F  when
! the code is built.
! If, on the other hand, a DM package is specified, the module_dm.F 
! provided with that package (e.g. RSL) is copied from /external/RSL/module_dm.F
! into src/module_dm.F.
! It is important to recognize this, because changes directly to src/module_dm.F
! will be lost!

LOGICAL FUNCTION wrf_dm_on_monitor()
  wrf_dm_on_monitor = .true.
END FUNCTION wrf_dm_on_monitor

INTEGER FUNCTION wrf_dm_monitor_rank()
  wrf_dm_monitor_rank = 0
END FUNCTION wrf_dm_monitor_rank

SUBROUTINE wrf_get_myproc( myproc )
  IMPLICIT NONE
  INTEGER myproc
  myproc = 0
  RETURN
END SUBROUTINE wrf_get_myproc

SUBROUTINE wrf_get_nproc( nprocs )
  IMPLICIT NONE
  INTEGER nprocs
  nprocs = 1
  RETURN
END SUBROUTINE wrf_get_nproc

SUBROUTINE wrf_get_nprocx( nprocs )
  IMPLICIT NONE
  INTEGER nprocs
  nprocs = 1
  RETURN
END SUBROUTINE wrf_get_nprocx

SUBROUTINE wrf_get_nprocy( nprocs )
  IMPLICIT NONE
  INTEGER nprocs
  nprocs = 1
  RETURN
END SUBROUTINE wrf_get_nprocy

SUBROUTINE wrf_dm_bcast_string ( buf , size )
  IMPLICIT NONE
  INTEGER size
  INTEGER BUF(*)
  RETURN
END SUBROUTINE wrf_dm_bcast_string

SUBROUTINE wrf_dm_bcast_bytes ( buf , size )
  IMPLICIT NONE
  INTEGER size
  INTEGER BUF(*)
  RETURN
END SUBROUTINE wrf_dm_bcast_bytes

SUBROUTINE wrf_dm_bcast_integer( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
   INTEGER  buf(*)
   RETURN
END SUBROUTINE wrf_dm_bcast_integer

SUBROUTINE wrf_dm_bcast_real( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
   REAL  buf(*)
   RETURN
END SUBROUTINE wrf_dm_bcast_real

SUBROUTINE wrf_dm_bcast_logical( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
   LOGICAL  buf(*)
   RETURN
END SUBROUTINE wrf_dm_bcast_logical

SUBROUTINE wrf_dm_halo ( domdesc , comms , stencil_id )
   IMPLICIT NONE
   INTEGER domdesc , comms(*) , stencil_id
   RETURN
END SUBROUTINE wrf_dm_halo

SUBROUTINE wrf_dm_boundary ( domdesc , comms , period_id , &
                             periodic_x , periodic_y )
   IMPLICIT NONE
   INTEGER domdesc , comms(*) , period_id
   LOGICAL , INTENT(IN)      :: periodic_x, periodic_y
   RETURN
END SUBROUTINE wrf_dm_boundary

SUBROUTINE wrf_dm_xpose_z2x ( domdesc , comms , xpose_id  )
   IMPLICIT NONE
   INTEGER domdesc , comms(*), xpose_id
   RETURN
END SUBROUTINE wrf_dm_xpose_z2x
SUBROUTINE wrf_dm_xpose_x2y ( domdesc , comms , xpose_id  )
   IMPLICIT NONE
   INTEGER domdesc , comms(*), xpose_id
   RETURN
END SUBROUTINE wrf_dm_xpose_x2y
SUBROUTINE wrf_dm_xpose_y2z ( domdesc , comms , xpose_id  )
   IMPLICIT NONE
   INTEGER domdesc , comms(*), xpose_id
   RETURN
END SUBROUTINE wrf_dm_xpose_y2z

SUBROUTINE wrf_dm_define_comms ( grid )
   USE module_domain
   IMPLICIT NONE
   TYPE(domain) , INTENT (INOUT) :: grid
   RETURN
END SUBROUTINE wrf_dm_define_comms

SUBROUTINE wrf_get_dm_communicator ( communicator )
   IMPLICIT NONE
   INTEGER , INTENT(OUT) :: communicator
   communicator = 0
   RETURN
END SUBROUTINE wrf_get_dm_communicator

SUBROUTINE wrf_get_dm_iocommunicator ( iocommunicator )
   IMPLICIT NONE
   INTEGER , INTENT(OUT) :: iocommunicator
   iocommunicator = 0
   RETURN
END SUBROUTINE wrf_get_dm_iocommunicator

SUBROUTINE wrf_dm_shutdown
      RETURN
END SUBROUTINE wrf_dm_shutdown
SUBROUTINE wrf_abort
      STOP 'wrf_abort'
END SUBROUTINE wrf_abort

SUBROUTINE wrf_patch_to_global_real (buf,globbuf,domdesc,ndim,&
                                       ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe )
   IMPLICIT NONE
   INTEGER                             ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe
   INTEGER fid,domdesc,ndim,glen(3),llen(3)
   REAL globbuf(*)
   REAL buf(*)
   RETURN
END SUBROUTINE wrf_patch_to_global_real

SUBROUTINE wrf_global_to_patch_real (globbuf,buf,domdesc,ndim,&
                                       ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe )
   IMPLICIT NONE
   INTEGER                             ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe
   INTEGER fid,domdesc,ndim,glen(3),llen(3)
   REAL globbuf(*)
   REAL buf(*)
   RETURN
END SUBROUTINE wrf_global_to_patch_real


SUBROUTINE wrf_patch_to_global_double (buf,globbuf,domdesc,ndim,&
                                       ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe )
   IMPLICIT NONE
   INTEGER                             ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe
   INTEGER fid,domdesc,ndim,glen(3),llen(3)
   DOUBLE PRECISION globbuf(*)
   DOUBLE PRECISION buf(*)
   RETURN
END SUBROUTINE wrf_patch_to_global_double

SUBROUTINE wrf_global_to_patch_double (globbuf,buf,domdesc,ndim,&
                                       ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe )
   IMPLICIT NONE
   INTEGER                             ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe
   INTEGER fid,domdesc,ndim,glen(3),llen(3)
   DOUBLE PRECISION globbuf(*)
   DOUBLE PRECISION buf(*)
   RETURN
END SUBROUTINE wrf_global_to_patch_double

SUBROUTINE wrf_patch_to_global_integer (buf,globbuf,domdesc,ndim,&
                                       ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe )
   IMPLICIT NONE
   INTEGER                             ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe
   INTEGER fid,domdesc,ndim,glen(3),llen(3)
   INTEGER globbuf(*)
   INTEGER buf(*)
   RETURN
END SUBROUTINE wrf_patch_to_global_integer

SUBROUTINE wrf_global_to_patch_integer (globbuf,buf,domdesc,ndim,&
                                       ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe )
   IMPLICIT NONE
   INTEGER                             ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe
   INTEGER fid,domdesc,ndim,glen(3),llen(3)
   INTEGER globbuf(*)
   INTEGER buf(*)
   RETURN
END SUBROUTINE wrf_global_to_patch_integer

SUBROUTINE wrf_patch_to_global_logical (buf,globbuf,domdesc,ndim,&
                                       ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe )
   IMPLICIT NONE
   INTEGER                             ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe
   INTEGER fid,domdesc,ndim,glen(3),llen(3)
   LOGICAL globbuf(*)
   LOGICAL buf(*)
   RETURN
END SUBROUTINE wrf_patch_to_global_logical

SUBROUTINE wrf_global_to_patch_LOGICAL (globbuf,buf,domdesc,ndim,&
                                       ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe )
   IMPLICIT NONE
   INTEGER                             ids,ide,jds,jde,kds,kde,&
                                       ims,ime,jms,jme,kms,kme,&
                                       ips,ipe,jps,jpe,kps,kpe
   INTEGER fid,domdesc,ndim,glen(3),llen(3)
   LOGICAL globbuf(*)
   LOGICAL buf(*)
   RETURN
END SUBROUTINE wrf_global_to_patch_LOGICAL

#if ( HWRF == 1 )
   SUBROUTINE hwrf_coupler_init
   END SUBROUTINE hwrf_coupler_init
#endif

   SUBROUTINE push_communicators_for_domain( id )
      IMPLICIT NONE
      INTEGER, OPTIONAL, INTENT(IN) :: id   ! if specified also does an instate for grid id
   END SUBROUTINE push_communicators_for_domain
   SUBROUTINE pop_communicators_for_domain
   END SUBROUTINE pop_communicators_for_domain
   SUBROUTINE instate_communicators_for_domain( id )
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: id
   END SUBROUTINE instate_communicators_for_domain
   SUBROUTINE store_communicators_for_domain( id )
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: id
   END SUBROUTINE store_communicators_for_domain

