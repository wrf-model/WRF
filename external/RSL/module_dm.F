!WRF:PACKAGE:RSL 
!
MODULE module_dm

   USE module_machine
   USE module_state_description
   USE module_wrf_error

#include <rsl.inc>

   INTEGER msg_z, msg_x, msg_y
   INTEGER msg,messages(168)
   INTEGER invalid_message_value
   INTEGER x_period_flag, y_period_flag
   INTEGER msg_msg
   INTEGER &
      n5w5 ,n5w4 ,n5w3 ,n5w2 ,n5w ,n5 ,n5e ,n5e2 ,n5e3 ,n5e4 ,n5e5  &
     ,n4w5 ,n4w4 ,n4w3 ,n4w2 ,n4w ,n4 ,n4e ,n4e2 ,n4e3 ,n4e4 ,n4e5  &
     ,n3w5 ,n3w4 ,n3w3 ,n3w2 ,n3w ,n3 ,n3e ,n3e2 ,n3e3 ,n3e4 ,n3e5  &
     ,n2w5 ,n2w4 ,n2w3 ,n2w2 ,n2w ,n2 ,n2e ,n2e2 ,n2e3 ,n2e4 ,n2e5  &
     ,nw5  ,nw4  ,nw3  ,nw2  ,nw  ,n1 ,ne  ,ne2  ,ne3  ,ne4  ,ne5   &
     ,w5   ,w4   ,w3   ,w2   ,w1      ,e1  ,e2   ,e3   ,e4   ,e5    &
     ,sw5  ,sw4  ,sw3  ,sw2  ,sw  ,s1 ,se  ,se2  ,se3  ,se4  ,se5   &
     ,s2w5 ,s2w4 ,s2w3 ,s2w2 ,s2w ,s2 ,s2e ,s2e2 ,s2e3 ,s2e4 ,s2e5  &
     ,s3w5 ,s3w4 ,s3w3 ,s3w2 ,s3w ,s3 ,s3e ,s3e2 ,s3e3 ,s3e4 ,s3e5  &
     ,s4w5 ,s4w4 ,s4w3 ,s4w2 ,s4w ,s4 ,s4e ,s4e2 ,s4e3 ,s4e4 ,s4e5  &
     ,s5w5 ,s5w4 ,s5w3 ,s5w2 ,s5w ,s5 ,s5e ,s5e2 ,s5e3 ,s5e4 ,s5e5
   INTEGER glen(3), llen(3), decomp(3), decompx(3), decompy(3), decompxy(3)
   INTEGER glen2d(2), llen2d(2), decomp2d(2), decompx2d(2), decompy2d(2), decompxy2d(2)
   INTEGER glenx(3), gleny(3), glenxy(3)
   INTEGER llenx(3), lleny(3), llenxy(3)
   INTEGER glenx2d(2), gleny2d(2), glenxy2d(2)
   INTEGER llenx2d(2), lleny2d(2), llenxy2d(2)
   INTEGER llen_tx(3)
   INTEGER llen_ty(3)
   INTEGER ips_save, jps_save
   INTEGER ipe_save, jpe_save
   INTEGER, PRIVATE :: mpi_comm_local
   INTEGER, PRIVATE :: nproc_lt, nproc_ln

#if ( RWORDSIZE != DWORDSIZE )
   INTERFACE add_msg_xpose
     MODULE PROCEDURE add_msg_xpose_real, add_msg_xpose_integer, add_msg_xpose_doubleprecision
   END INTERFACE
   INTERFACE add_msg_4pt
     MODULE PROCEDURE add_msg_4pt_real, add_msg_4pt_integer, add_msg_4pt_doubleprecision
   END INTERFACE
   INTERFACE add_msg_8pt
     MODULE PROCEDURE add_msg_8pt_real, add_msg_8pt_integer, add_msg_8pt_doubleprecision
   END INTERFACE
   INTERFACE add_msg_12pt
     MODULE PROCEDURE add_msg_12pt_real, add_msg_12pt_integer, add_msg_12pt_doubleprecision
   END INTERFACE
   INTERFACE add_msg_24pt
     MODULE PROCEDURE add_msg_24pt_real, add_msg_24pt_integer, add_msg_24pt_doubleprecision
   END INTERFACE
   INTERFACE add_msg_48pt
     MODULE PROCEDURE add_msg_48pt_real, add_msg_48pt_integer, add_msg_48pt_doubleprecision
   END INTERFACE
   INTERFACE add_msg_80pt
     MODULE PROCEDURE add_msg_80pt_real, add_msg_80pt_integer, add_msg_80pt_doubleprecision
   END INTERFACE
   INTERFACE add_msg_120pt
     MODULE PROCEDURE add_msg_120pt_real, add_msg_120pt_integer, add_msg_120pt_doubleprecision
   END INTERFACE
#else
   INTERFACE add_msg_xpose
     MODULE PROCEDURE add_msg_xpose_real, add_msg_xpose_integer
   END INTERFACE
   INTERFACE add_msg_4pt
     MODULE PROCEDURE add_msg_4pt_real, add_msg_4pt_integer
   END INTERFACE
   INTERFACE add_msg_8pt
     MODULE PROCEDURE add_msg_8pt_real, add_msg_8pt_integer
   END INTERFACE
   INTERFACE add_msg_12pt
     MODULE PROCEDURE add_msg_12pt_real, add_msg_12pt_integer
   END INTERFACE
   INTERFACE add_msg_24pt
     MODULE PROCEDURE add_msg_24pt_real, add_msg_24pt_integer
   END INTERFACE
   INTERFACE add_msg_48pt
     MODULE PROCEDURE add_msg_48pt_real, add_msg_48pt_integer
   END INTERFACE
   INTERFACE add_msg_80pt
     MODULE PROCEDURE add_msg_80pt_real, add_msg_80pt_integer
   END INTERFACE
   INTERFACE add_msg_120pt
     MODULE PROCEDURE add_msg_120pt_real, add_msg_120pt_integer
   END INTERFACE
#endif

#ifdef D3VAR_KLUDGE
#define TRUE_RSL_REAL     RSL_DOUBLE
#define TRUE_RSL_REAL_F90 RSL_DOUBLE_F90
#else
#define TRUE_RSL_REAL     RSL_REAL
#define TRUE_RSL_REAL_F90 RSL_REAL_F90
#endif

CONTAINS

   SUBROUTINE MPASPECT( P, MINM, MINN, PROCMIN_M, PROCMIN_N )

! <DESCRIPTION>
! This is a routine provided by the rsl external comm layer.
! and is defined in external/RSL/module_dm.F, which is copied
! into frame/module_dm.F at compile time.  Changes to frame/module_dm.F
! will be lost.
!
! Given a total number of tasks, P, work out a two-dimensional mesh of
! processors that is MINM processors in the M dimension and MINN
! processors in the N dimension. The algorithm attempts to find two
! numbers that divide the total number of processors without a remainder.
! The best it might do, sometimes, is 1 and P. It attempts to divide
! the M dimension over the smaller number.
! 
! The PROCMIN arguments are a holdover from MM5. The represent the
! minimum number of processors the algorithm is allowed to use for M and
! N. This is a holdover from MM5 which had static (compile-time) array
! sizes ; PROCMIN_M and PROCMIN_N  should always be 1 in WRF.
! 
! </DESCRIPTION>

      INTEGER P, M, N, MINI, MINM, MINN, PROCMIN_M, PROCMIN_N
      MINI = 2*P
      MINM = 1
      MINN = P
      DO M = 1, P
        IF ( MOD( P, M ) .EQ. 0 ) THEN
          N = P / M
          IF ( ABS(M-N) .LT. MINI                &
               .AND. M .GE. PROCMIN_M            &
               .AND. N .GE. PROCMIN_N            &
             ) THEN
            MINI = ABS(M-N)
            MINM = M
            MINN = N
          ENDIF
        ENDIF
      ENDDO
      IF ( MINM .LT. PROCMIN_M .OR. MINN .LT. PROCMIN_N ) THEN
        WRITE( wrf_err_message , * )'MPASPECT: UNABLE TO GENERATE PROCESSOR MESH.  STOPPING.'
        CALL wrf_message ( TRIM ( wrf_err_message ) )
        WRITE(0,*)' PROCMIN_M ', PROCMIN_M
        WRITE( wrf_err_message , * )' PROCMIN_M ', PROCMIN_M
        CALL wrf_message ( TRIM ( wrf_err_message ) )
        WRITE( wrf_err_message , * )' PROCMIN_N ', PROCMIN_N
        CALL wrf_message ( TRIM ( wrf_err_message ) )
        WRITE( wrf_err_message , * )' P         ', P
        CALL wrf_message ( TRIM ( wrf_err_message ) )
        WRITE( wrf_err_message , * )' MINM      ', MINM
        CALL wrf_message ( TRIM ( wrf_err_message ) )
        WRITE( wrf_err_message , * )' MINN      ', MINN
        CALL wrf_message ( TRIM ( wrf_err_message ) )
        CALL wrf_error_fatal ( 'module_dm: mpaspect' )
      ENDIF
   RETURN
   END SUBROUTINE MPASPECT


   SUBROUTINE wrf_dm_initialize
! <DESCRIPTION>
! This is a routine provided by the RSL external comm layer.
! and is defined in external/RSL/module_dm.F, which is copied
! into frame/module_dm.F at compile time.  Changes to frame/module_dm.F
! will be lost.
!
! This routine is used to complete initialization the rsl external comm
! layer, once the namelist.input file has been read-in and broadcast to
! all the tasks.  It must be called <em>after</em> the call to <a
! href=init_module_dm.html>init_module_dm</a>.
! 
! Wrf_dm_initialize calls RSL_SET_REGULAR_DECOMP to set up a regular
! domain decompostion (subdomains will be rectangular) and then looks to
! see if the namelist variables nproc_x and nproc_y have been set.  If
! these have been set it uses these to map the MPI tasks to a
! two-dimensional processor mesh.  Otherwise, it uses the <a
! href=mpaspect.html>mpaspect</a> routine to compute the mesh.  The
! dimensions of the mesh are then provided to rsl with call to RSL_MESH.
! 
! The WRF EM core uses the default pad area (the area of extra memory
! that will be allocated around each local processor subdomain). The
! default, defined in external/RSL/RSL/rsl.h, is 4. Other dycores, such
! as NMM, may need a different size.  A non-default pad area is set in
! rsl using a call to RSL_SET_PADAREA.
! 
! </DESCRIPTION>
      CALL RSL_SET_REGULAR_DECOMP
      CALL get_nproc_x ( nproc_ln )
      CALL get_nproc_y ( nproc_lt )
! check if user has specified in the namelist
      IF ( nproc_ln .GT. 0 .OR. nproc_lt .GT. 0 ) THEN
        ! if only nproc_ln is specified then make it 1-d decomp in i
        IF      ( nproc_ln .GT. 0 .AND. nproc_lt .EQ. -1 ) THEN
          nproc_ln = rsl_nproc
          nproc_lt = 1
        ! if only nproc_lt is specified then make it 1-d decomp in j
        ELSE IF ( nproc_ln .EQ. -1 .AND. nproc_lt .GT. 0 ) THEN
          nproc_ln = 1
          nproc_lt = rsl_nproc
        ENDIF
        ! make sure user knows what they're doing (when both specified)
        IF ( nproc_ln * nproc_lt .NE. rsl_nproc ) THEN
          WRITE( wrf_err_message , * )'WRF_DM_INITIALIZE (RSL): nproc_x * nproc_y in namelist ne ',rsl_nproc
          CALL wrf_error_fatal ( wrf_err_message )
        ENDIF
      ELSE
        ! When neither is specified, work out mesh with MPASPECT
        ! Pass nproc_ln and nproc_nt so that number of procs in 
        ! i-dim (nproc_ln) is equal or lesser.
        CALL mpaspect( rsl_nproc , nproc_ln , nproc_lt , 1 , 1 )
      ENDIF
       !                X          Y
      CALL RSL_MESH( nproc_ln, nproc_lt )
#ifdef NMM_CORE
      CALL rsl_set_padarea ( 6 )
#endif
      invalid_message_value = RSL_INVALID
      x_period_flag         = RSL_M
      y_period_flag         = RSL_N
      RETURN
   END SUBROUTINE wrf_dm_initialize

! xpose additions, 20000302

   SUBROUTINE reset_msgs_xpose
      IMPLICIT NONE
      CALL rsl_create_message ( msg_z )
      CALL rsl_create_message ( msg_x )
      CALL rsl_create_message ( msg_y )
   END SUBROUTINE reset_msgs_xpose

   SUBROUTINE add_msg_xpose_real( fld_z, fld_x, fld_y, dim )
      IMPLICIT NONE
      real fld_z(*), fld_x(*), fld_y(*)
      integer dim
      if (      dim == 3 ) then
        CALL rsl_build_message(msg_z,TRUE_RSL_REAL_F90,fld_z,dim,decomp(1),glen(1),llen(1))
#ifdef D3VAR_IRY_KLUDGE
!
! The transpose utility was developed in rsl originally with MM5 3DVAR in mind, and the way
! the implementers worked around the I->Y issue was by calling I->X in the Registry but then
! calling the other (Z2Y) transpose throughout the code to effect the correct orientation.
! The result is that although it's basically wrong here for codes that actually are I->X,
! we cannot fix it without breaking 3DVAR. Hence the KLUDGE to keep it the old wrong way
! for 3DVAR and fix it for everyone else. There was already a D3VAR_KLUDGE needed in the
! registry anyway, to make sure the WRF I/O API gets called properly -- again, to manage
! this MM5 assumption that was hidden in the 3DVAR code -- so we can make use of that here.
! This needs to be revisited once the I->Y assumption is purged from 3DVAR. JM 20020910
!
        CALL rsl_build_message(msg_x,TRUE_RSL_REAL_F90,fld_x,dim,decomp(1),glen(1),llen_tx(1))
        CALL rsl_build_message(msg_y,TRUE_RSL_REAL_F90,fld_y,dim,decomp(1),glen(1),llen_ty(1))
#else
        CALL rsl_build_message(msg_y,TRUE_RSL_REAL_F90,fld_x,dim,decomp(1),glen(1),llen_tx(1))  ! msg_y->msg_x 20020908
        CALL rsl_build_message(msg_x,TRUE_RSL_REAL_F90,fld_y,dim,decomp(1),glen(1),llen_ty(1))  ! msg_x->msg_y 20020908
#endif
      endif
   END SUBROUTINE add_msg_xpose_real

#if ( RWORDSIZE != DWORDSIZE )
   SUBROUTINE add_msg_xpose_doubleprecision( fld_z, fld_x, fld_y, dim )
      IMPLICIT NONE
      doubleprecision fld_z(*), fld_x(*), fld_y(*)
      integer dim
      if (      dim == 3 ) then
        CALL rsl_build_message(msg_z,RSL_DOUBLE_F90,fld_z,dim,decomp(1),glen(1),llen(1))
#ifdef D3VAR_IRY_KLUDGE
        CALL rsl_build_message(msg_x,RSL_DOUBLE_F90,fld_x,dim,decomp(1),glen(1),llen_tx(1))
        CALL rsl_build_message(msg_y,RSL_DOUBLE_F90,fld_y,dim,decomp(1),glen(1),llen_ty(1))
#else
        CALL rsl_build_message(msg_y,RSL_DOUBLE_F90,fld_x,dim,decomp(1),glen(1),llen_tx(1))  ! msg_y->msg_x 20020908
        CALL rsl_build_message(msg_x,RSL_DOUBLE_F90,fld_y,dim,decomp(1),glen(1),llen_ty(1))  ! msg_x->msg_y 20020908
#endif
      endif
   END SUBROUTINE add_msg_xpose_doubleprecision
#endif


   SUBROUTINE add_msg_xpose_integer ( fld_z, fld_x, fld_y, dim )
      IMPLICIT NONE
      integer fld_z(*), fld_x(*), fld_y(*)
      integer dim
      if (      dim == 3 ) then
        CALL rsl_build_message(msg_z,TRUE_RSL_REAL_F90,fld_z,dim,decomp(1),glen(1),llen(1))
#ifdef D3VAR_IRY_KLUDGE
        CALL rsl_build_message(msg_x,RSL_INTEGER_F90,fld_x,dim,decomp(1),glen(1),llen_tx(1))
        CALL rsl_build_message(msg_y,RSL_INTEGER_F90,fld_y,dim,decomp(1),glen(1),llen_ty(1))
#else
        CALL rsl_build_message(msg_y,RSL_INTEGER_F90,fld_x,dim,decomp(1),glen(1),llen_tx(1))  ! msg_y->msg_x 20020908
        CALL rsl_build_message(msg_x,RSL_INTEGER_F90,fld_y,dim,decomp(1),glen(1),llen_ty(1))  ! msg_x->msg_y 20020908
#endif
      endif
   END SUBROUTINE add_msg_xpose_integer

   SUBROUTINE define_xpose ( did, xp )
      IMPLICIT NONE
      INTEGER did , xp
      CALL rsl_create_xpose ( xp )
      CALL rsl_describe_xpose ( did , xp , msg_z , msg_x , msg_y )
   END SUBROUTINE define_xpose

! end xpose additions, 20000302

!      n5w5 ,n5w4 ,n5w3 ,n5w2 ,n5w ,n5 ,n5e ,n5e2 ,n5e3 ,n5e4 ,n5e5  &
!     ,n4w5 ,n4w4 ,n4w3 ,n4w2 ,n4w ,n4 ,n4e ,n4e2 ,n4e3 ,n4e4 ,n4e5  &
!     ,n3w5 ,n3w4 ,n3w3 ,n3w2 ,n3w ,n3 ,n3e ,n3e2 ,n3e3 ,n3e4 ,n3e5  &
!     ,n2w5 ,n2w4 ,n2w3 ,n2w2 ,n2w ,n2 ,n2e ,n2e2 ,n2e3 ,n2e4 ,n2e5  &
!     ,nw5  ,nw4  ,nw3  ,nw2  ,nw  ,n1 ,ne  ,ne2  ,ne3  ,ne4  ,ne5   &
!     ,w5   ,w4   ,w3   ,w2   ,w1      ,e1  ,e2   ,e3   ,e4   ,e5    &
!     ,sw5  ,sw4  ,sw3  ,sw2  ,sw  ,s1 ,se  ,se2  ,se3  ,se4  ,se5   &
!     ,s2w5 ,s2w4 ,s2w3 ,s2w2 ,s2w ,s2 ,s2e ,s2e2 ,s2e3 ,s2e4 ,s2e5  &
!     ,s3w5 ,s3w4 ,s3w3 ,s3w2 ,s3w ,s3 ,s3e ,s3e2 ,s3e3 ,s3e4 ,s3e5  &
!     ,s4w5 ,s4w4 ,s4w3 ,s4w2 ,s4w ,s4 ,s4e ,s4e2 ,s4e3 ,s4e4 ,s4e5  &
!     ,s5w5 ,s5w4 ,s5w3 ,s5w2 ,s5w ,s5 ,s5e ,s5e2 ,s5e3 ,s5e4 ,s5e5

   SUBROUTINE reset_msgs_120pt
      CALL reset_msgs_80pt
#if 0
      CALL rsl_create_message(n5w5)
      CALL rsl_create_message(n5w4)
      CALL rsl_create_message(n5w3)
      CALL rsl_create_message(n5w2)
      CALL rsl_create_message(n5w )
      CALL rsl_create_message(n5)
      CALL rsl_create_message(n5e )
      CALL rsl_create_message(n5e2)
      CALL rsl_create_message(n5e3)
      CALL rsl_create_message(n5e4)
      CALL rsl_create_message(n5e5)
      CALL rsl_create_message(n4w5)
      CALL rsl_create_message(n3w5)
      CALL rsl_create_message(n2w5)
      CALL rsl_create_message(nw5)
      CALL rsl_create_message(w5)
      CALL rsl_create_message(sw5)
      CALL rsl_create_message(s2w5)
      CALL rsl_create_message(s3w5)
      CALL rsl_create_message(s4w5)
      CALL rsl_create_message(n4e5)
      CALL rsl_create_message(n3e5)
      CALL rsl_create_message(n2e5)
      CALL rsl_create_message(ne5)
      CALL rsl_create_message(e5)
      CALL rsl_create_message(se5)
      CALL rsl_create_message(s2e5)
      CALL rsl_create_message(s3e5)
      CALL rsl_create_message(s4e5)
      CALL rsl_create_message(s5w5)
      CALL rsl_create_message(s5w4)
      CALL rsl_create_message(s5w3)
      CALL rsl_create_message(s5w2)
      CALL rsl_create_message(s5w )
      CALL rsl_create_message(s5)
      CALL rsl_create_message(s5e )
      CALL rsl_create_message(s5e2)
      CALL rsl_create_message(s5e3)
      CALL rsl_create_message(s5e4)
      CALL rsl_create_message(s5e5)
#endif
   END SUBROUTINE reset_msgs_120pt

   SUBROUTINE reset_msgs_80pt
#if 1
      CALL rsl_create_message(msg_msg)
#else
      CALL reset_msgs_48pt
      CALL rsl_create_message(n4w4)
      CALL rsl_create_message(n4w3)
      CALL rsl_create_message(n4w2)
      CALL rsl_create_message(n4w )
      CALL rsl_create_message(n4)
      CALL rsl_create_message(n4e )
      CALL rsl_create_message(n4e2)
      CALL rsl_create_message(n4e3)
      CALL rsl_create_message(n4e4)
      CALL rsl_create_message(n3w4)
      CALL rsl_create_message(n2w4)
      CALL rsl_create_message(nw4)
      CALL rsl_create_message(w4)
      CALL rsl_create_message(sw4)
      CALL rsl_create_message(s2w4)
      CALL rsl_create_message(s3w4)
      CALL rsl_create_message(n3e4)
      CALL rsl_create_message(n2e4)
      CALL rsl_create_message(ne4)
      CALL rsl_create_message(e4)
      CALL rsl_create_message(se4)
      CALL rsl_create_message(s2e4)
      CALL rsl_create_message(s3e4)
      CALL rsl_create_message(s4w4)
      CALL rsl_create_message(s4w3)
      CALL rsl_create_message(s4w2)
      CALL rsl_create_message(s4w )
      CALL rsl_create_message(s4)
      CALL rsl_create_message(s4e )
      CALL rsl_create_message(s4e2)
      CALL rsl_create_message(s4e3)
      CALL rsl_create_message(s4e4)
#endif
   END SUBROUTINE reset_msgs_80pt

   SUBROUTINE reset_msgs_48pt
      CALL reset_msgs_24pt
      CALL rsl_create_message(n3w3)
      CALL rsl_create_message(n3w2)
      CALL rsl_create_message(n3w )
      CALL rsl_create_message(n3)
      CALL rsl_create_message(n3e )
      CALL rsl_create_message(n3e2)
      CALL rsl_create_message(n3e3)
      CALL rsl_create_message(n2w3)
      CALL rsl_create_message(n2e3)
      CALL rsl_create_message(nw3)
      CALL rsl_create_message(ne3)
      CALL rsl_create_message(w3)
      CALL rsl_create_message(e3)
      CALL rsl_create_message(sw3)
      CALL rsl_create_message(se3)
      CALL rsl_create_message(s2w3)
      CALL rsl_create_message(s2e3)
      CALL rsl_create_message(s3w3)
      CALL rsl_create_message(s3w2)
      CALL rsl_create_message(s3w )
      CALL rsl_create_message(s3)
      CALL rsl_create_message(s3e )
      CALL rsl_create_message(s3e2)
      CALL rsl_create_message(s3e3)
      RETURN
   END SUBROUTINE reset_msgs_48pt

   SUBROUTINE reset_msgs_24pt
      CALL reset_msgs_12pt
      CALL rsl_create_message(n2w2)
      CALL rsl_create_message(n2w)
      CALL rsl_create_message(n2e)
      CALL rsl_create_message(n2e2)
      CALL rsl_create_message(nw2)
      CALL rsl_create_message(ne2)
      CALL rsl_create_message(sw2)
      CALL rsl_create_message(se2)
      CALL rsl_create_message(s2w2)
      CALL rsl_create_message(s2w)
      CALL rsl_create_message(s2e)
      CALL rsl_create_message(s2e2)
      RETURN
   END SUBROUTINE reset_msgs_24pt

   SUBROUTINE reset_msgs_12pt
      CALL reset_msgs_8pt
      call rsl_create_message(n2)
      call rsl_create_message(w2)
      call rsl_create_message(e2)
      call rsl_create_message(s2)
      RETURN
   END SUBROUTINE reset_msgs_12pt

   SUBROUTINE reset_msgs_8pt
      call reset_msgs_4pt
      call rsl_create_message(ne)
      call rsl_create_message(nw)
      call rsl_create_message(se)
      call rsl_create_message(sw)
      RETURN
   END SUBROUTINE reset_msgs_8pt

   SUBROUTINE reset_msgs_4pt
      call rsl_create_message(n1)
      call rsl_create_message(w1)
      call rsl_create_message(e1)
      call rsl_create_message(s1)
      RETURN
   END SUBROUTINE reset_msgs_4pt

   SUBROUTINE reset_msgs_y_shift
      call rsl_create_message(s5)
      call rsl_create_message(s4)
      call rsl_create_message(s3)
      call rsl_create_message(s2)
      call rsl_create_message(s1)
      call rsl_create_message(n1)
      call rsl_create_message(n2)
      call rsl_create_message(n3)
      call rsl_create_message(n4)
      call rsl_create_message(n5)
      RETURN
   END SUBROUTINE reset_msgs_y_shift

   SUBROUTINE reset_msgs_x_shift
      call rsl_create_message(w5)
      call rsl_create_message(w4)
      call rsl_create_message(w3)
      call rsl_create_message(w2)
      call rsl_create_message(w1)
      call rsl_create_message(e1)
      call rsl_create_message(e2)
      call rsl_create_message(e3)
      call rsl_create_message(e4)
      call rsl_create_message(e5)
      RETURN
   END SUBROUTINE reset_msgs_x_shift

   SUBROUTINE add_msg_x_shift_real ( fld, kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      real fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim
         CASE DEFAULT
      END SELECT
      if      ( kdim  > 1 ) then
        CALL rsl_build_message(w5,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(w4,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(w3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(w2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(w1,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e1,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e4,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e5,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(w5,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(w4,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(w3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(w2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(w1,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e1,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e4,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e5,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_x_shift_real
   SUBROUTINE add_msg_y_shift_real ( fld, kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      real fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim
         CASE DEFAULT
      END SELECT
      if      ( kdim  > 1 ) then
        CALL rsl_build_message(s5,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s4,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s1,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n1,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n4,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n5,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(s5,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s4,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s1,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n1,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n4,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n5,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_y_shift_real

   SUBROUTINE add_msg_x_shift_integer ( fld, kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      integer fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim
         CASE DEFAULT
      END SELECT
      if      ( kdim  > 1 ) then
        CALL rsl_build_message(w5,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(w4,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(w3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(w2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(w1,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e1,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e4,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e5,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(w5,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(w4,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(w3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(w2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(w1,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e1,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e4,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e5,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_x_shift_integer
   SUBROUTINE add_msg_y_shift_integer ( fld, kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      integer fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim
         CASE DEFAULT
      END SELECT
      if      ( kdim  > 1 ) then
        CALL rsl_build_message(s5,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s4,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s1,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n1,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n4,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n5,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(s5,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s4,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s1,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n1,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n4,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n5,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_y_shift_integer

   SUBROUTINE add_msg_x_shift_doubleprecision ( fld, kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      doubleprecision fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim
         CASE DEFAULT
      END SELECT
      if      ( kdim  > 1 ) then
        CALL rsl_build_message(w5,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(w4,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(w3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(w2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(w1,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e1,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e4,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e5,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(w5,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(w4,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(w3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(w2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(w1,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e1,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e4,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e5,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_x_shift_doubleprecision
   SUBROUTINE add_msg_y_shift_doubleprecision ( fld, kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      doubleprecision fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim
         CASE DEFAULT
      END SELECT
      if      ( kdim  > 1 ) then
        CALL rsl_build_message(s5,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s4,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s1,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n1,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n4,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n5,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(s5,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s4,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s1,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n1,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n4,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n5,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_y_shift_doubleprecision

   SUBROUTINE add_msg_4pt_real ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      real fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim   
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim   
         CASE DEFAULT
      END SELECT
      if      ( kdim  > 1 ) then
        CALL rsl_build_message(w1,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s1,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e1,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n1,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(w1,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s1,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e1,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n1,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_4pt_real

#if (  RWORDSIZE != DWORDSIZE )
   SUBROUTINE add_msg_4pt_doubleprecision ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      doubleprecision fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim
         CASE DEFAULT
      END SELECT
      if      ( kdim  > 1 ) then
        CALL rsl_build_message(w1,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s1,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e1,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n1,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(w1,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s1,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e1,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n1,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_4pt_doubleprecision
#endif


   SUBROUTINE add_msg_4pt_integer ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      integer fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim   
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim   
         CASE DEFAULT
      END SELECT
      if      ( kdim  > 1 ) then
        CALL rsl_build_message(w1,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s1,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e1,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n1,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(w1,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s1,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e1,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n1,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_4pt_integer

   SUBROUTINE add_msg_8pt_real ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      real fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim   
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim   
         CASE DEFAULT
      END SELECT
      CALL add_msg_4pt ( fld , kdim )
      if (      kdim >  1 ) then
        CALL rsl_build_message(nw,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(sw,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(ne,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(se,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(nw,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(sw,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(ne,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(se,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_8pt_real

#if ( RWORDSIZE != DWORDSIZE )
   SUBROUTINE add_msg_8pt_doubleprecision ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      doubleprecision fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim
         CASE DEFAULT
      END SELECT
      CALL add_msg_4pt ( fld , kdim )
      if (      kdim >  1 ) then
        CALL rsl_build_message(nw,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(sw,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(ne,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(se,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(nw,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(sw,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(ne,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(se,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_8pt_doubleprecision
#endif


   SUBROUTINE add_msg_8pt_integer( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      integer fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim  
         CASE DEFAULT
      END SELECT
      CALL add_msg_4pt ( fld , kdim )
      if (      kdim >  1 ) then
        CALL rsl_build_message(nw,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(sw,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(ne,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(se,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(nw,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(sw,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(ne,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(se,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_8pt_integer

   SUBROUTINE add_msg_12pt_real ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      real fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim   
         CASE DEFAULT
      END SELECT
      CALL add_msg_8pt ( fld , kdim )
      if      ( kdim >  1 ) then
        CALL rsl_build_message(w2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(w2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_12pt_real

#if ( RWORDSIZE != DWORDSIZE )
   SUBROUTINE add_msg_12pt_doubleprecision ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      doubleprecision fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim
         CASE DEFAULT
      END SELECT
      CALL add_msg_8pt ( fld , kdim )
      if      ( kdim >  1 ) then
        CALL rsl_build_message(w2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(w2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_12pt_doubleprecision
#endif


   SUBROUTINE add_msg_12pt_integer ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      integer fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim
         CASE DEFAULT
      END SELECT
      CALL add_msg_8pt ( fld , kdim )
      if      ( kdim >  1 ) then
        CALL rsl_build_message(w2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(w2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_12pt_integer

   SUBROUTINE add_msg_24pt_real ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      real fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim   
         CASE DEFAULT
      END SELECT
      CALL add_msg_8pt ( fld , kdim )
      if      ( kdim >  1 ) then
        CALL rsl_build_message(n2w2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2w,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2e,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2e2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(nw2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(ne2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(sw2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(se2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2w2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2w,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2e,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2e2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(n2w2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2w,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2e,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2e2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(nw2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(ne2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(sw2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(se2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2w2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2w,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2e,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2e2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_24pt_real

#if ( RWORDSIZE != DWORDSIZE )
   SUBROUTINE add_msg_24pt_doubleprecision ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      doubleprecision fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim
         CASE DEFAULT
      END SELECT
      CALL add_msg_8pt ( fld , kdim )
      if      ( kdim >  1 ) then
        CALL rsl_build_message(n2w2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2w,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2e,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2e2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(nw2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(ne2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(sw2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(se2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2w2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2w,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2e,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2e2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(n2w2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2w,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2e,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2e2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(nw2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(ne2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(sw2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(se2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2w2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2w,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2e,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2e2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_24pt_doubleprecision
#endif


   SUBROUTINE add_msg_24pt_integer ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      integer fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim
         CASE DEFAULT
      END SELECT
      CALL add_msg_8pt ( fld , kdim )
      if      ( kdim >  1 ) then
        CALL rsl_build_message(n2w2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2w,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2e,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2e2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(nw2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(ne2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(sw2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(se2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2w2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2w,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2e,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2e2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(n2w2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2w,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2e,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2e2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(nw2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(ne2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(sw2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(se2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2w2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2w,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2e,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2e2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_24pt_integer

   SUBROUTINE add_msg_48pt_real ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      real fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim   
         CASE DEFAULT
      END SELECT
      CALL add_msg_24pt ( fld , kdim )
      if      ( kdim >  1 ) then
        CALL rsl_build_message(n3w3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3w2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3w,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3e,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3e2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3e3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2w3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2e3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(nw3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(ne3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(w3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(sw3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(se3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2w3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2e3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3w3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3w2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3w,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3e,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3e2,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3e3,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(n3w3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3w2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3w,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3e,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3e2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3e3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2w3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2e3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(nw3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(ne3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(w3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(sw3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(se3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2w3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2e3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3w3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3w2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3w,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3e,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3e2,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3e3,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_48pt_real

#if ( RWORDSIZE != DWORDSIZE )
   SUBROUTINE add_msg_48pt_doubleprecision ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      doubleprecision fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim   
         CASE DEFAULT
      END SELECT
      CALL add_msg_24pt ( fld , kdim )
      if      ( kdim >  1 ) then
        CALL rsl_build_message(n3w3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3w2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3w,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3e,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3e2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3e3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2w3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2e3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(nw3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(ne3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(w3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(sw3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(se3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2w3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2e3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3w3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3w2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3w,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3e,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3e2,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3e3,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(n3w3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3w2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3w,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3e,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3e2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3e3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2w3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2e3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(nw3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(ne3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(w3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(sw3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(se3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2w3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2e3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3w3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3w2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3w,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3e,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3e2,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3e3,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_48pt_doubleprecision
#endif

   SUBROUTINE add_msg_48pt_integer ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      integer fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim   
         CASE DEFAULT
      END SELECT
      CALL add_msg_24pt ( fld , kdim )
      if      ( kdim >  1 ) then
        CALL rsl_build_message(n3w3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3w2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3w,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3e,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3e2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n3e3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2w3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(n2e3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(nw3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(ne3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(w3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(e3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(sw3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(se3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2w3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s2e3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3w3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3w2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3w,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3e,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3e2,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
        CALL rsl_build_message(s3e3,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(n3w3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3w2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3w,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3e,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3e2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n3e3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2w3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(n2e3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(nw3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(ne3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(w3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(e3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(sw3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(se3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2w3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s2e3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3w3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3w2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3w,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3e,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3e2,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
        CALL rsl_build_message(s3e3,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_48pt_integer


   SUBROUTINE add_msg_80pt_real ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      real fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim   
         CASE DEFAULT
      END SELECT
      if      ( kdim >  1 ) then
        CALL rsl_build_message(msg_msg,TRUE_RSL_REAL_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(msg_msg,TRUE_RSL_REAL_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_80pt_real

#if ( RWORDSIZE != DWORDSIZE )
   SUBROUTINE add_msg_80pt_doubleprecision ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      doubleprecision fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim   
         CASE DEFAULT
      END SELECT
      if      ( kdim >  1 ) then
        CALL rsl_build_message(msg_msg,RSL_DOUBLE_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(msg_msg,RSL_DOUBLE_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_80pt_doubleprecision 
#endif

   SUBROUTINE add_msg_80pt_integer ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      integer fld(*)
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = kdim    ; ll(2) = kdim
           gl(3) = glen(3) ; ll(3) = llen(3)
         CASE ( DATA_ORDER_XYZ )
           gl(1) = glen(1) ; ll(1) = llen(1)
           gl(2) = glen(2) ; ll(2) = llen(2)
           gl(3) = kdim    ; ll(3) = kdim
         CASE DEFAULT
      END SELECT
      if      ( kdim >  1 ) then
        CALL rsl_build_message(msg_msg,RSL_INTEGER_F90,fld,3,decomp(1),gl(1),ll(1))
      else if ( kdim == 1 ) then
        CALL rsl_build_message(msg_msg,RSL_INTEGER_F90,fld,2,decomp2d(1),glen2d(1),llen2d(1))
      endif
      RETURN
   END SUBROUTINE add_msg_80pt_integer

   SUBROUTINE add_msg_120pt_real ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      real fld(*)
      CALL add_msg_80pt ( fld , kdim )
      RETURN
   END SUBROUTINE add_msg_120pt_real

#if ( RWORDSIZE != DWORDSIZE )
   SUBROUTINE add_msg_120pt_doubleprecision ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      doubleprecision fld(*)
      CALL add_msg_80pt ( fld , kdim )
      RETURN
   END SUBROUTINE add_msg_120pt_doubleprecision
#endif

   SUBROUTINE add_msg_120pt_integer ( fld , kdim )
      IMPLICIT NONE
      integer kdim, gl(3), ll(3)
      integer fld(*)
      CALL add_msg_80pt ( fld , kdim )
      RETURN
   END SUBROUTINE add_msg_120pt_integer

   SUBROUTINE stencil_x_shift ( did , stenid )
      IMPLICIT NONE
      INTEGER did, stenid
      messages = RSL_INVALID
      messages(22) = w3
      messages(23) = w2
      messages(24) = w1
      messages(25) = e1
      messages(26) = e2
      messages(27) = e3
      CALL rsl_create_stencil( stenid )
      CALL rsl_describe_stencil ( did, stenid, RSL_48PT, messages )
      RETURN
   END SUBROUTINE stencil_x_shift

   SUBROUTINE stencil_y_shift ( did , stenid )
      IMPLICIT NONE
      INTEGER did, stenid
      messages = RSL_INVALID
      messages( 4) = n3
      messages(11) = n2
      messages(18) = n1
      messages(31) = s1
      messages(38) = s2
      messages(45) = s3
      CALL rsl_create_stencil( stenid )
      CALL rsl_describe_stencil ( did, stenid, RSL_48PT, messages )
      RETURN
   END SUBROUTINE stencil_y_shift

   SUBROUTINE stencil_4pt ( did, stenid )
      IMPLICIT NONE
      INTEGER did, stenid
      messages(1) =          n1
      messages(2) =   w1
      messages(3) =                 e1
      messages(4) =          s1
      CALL rsl_create_stencil( stenid )
      CALL rsl_describe_stencil ( did, stenid, RSL_4PT, messages )
      RETURN
   END SUBROUTINE stencil_4pt

   SUBROUTINE stencil_8pt ( did, stenid )
      IMPLICIT NONE
      INTEGER did, stenid
      messages(1) =   nw
      messages(2) =          n1
      messages(3) =                 ne
      messages(4) =   w1
      messages(5) =                 e1
      messages(6) =   sw
      messages(7) =          s1
      messages(8) =                 se
      CALL rsl_create_stencil( stenid )
      CALL rsl_describe_stencil ( did, stenid, RSL_8PT, messages )
      RETURN
   END SUBROUTINE stencil_8pt

   SUBROUTINE stencil_12pt ( did, stenid )
      IMPLICIT NONE
      INTEGER did, stenid
      messages(1)  =                 n2
      messages(2)  =          nw
      messages(3)  =                 n1
      messages(4)  =                           ne
      messages(5)  =  w2
      messages(6)  =          w1                  
      messages(7)  =                           e1
      messages(8)  =                                    e2
      messages(9)  =          sw
      messages(10) =                 s1
      messages(11) =                           se
      messages(12) =                 s2
      CALL rsl_create_stencil( stenid )
      CALL rsl_describe_stencil ( did, stenid, RSL_12PT, messages )
      RETURN
   END SUBROUTINE stencil_12pt

   SUBROUTINE stencil_24pt ( did, stenid )
      IMPLICIT NONE
      INTEGER did, stenid, i
      messages( 1) = n2w2
      messages( 2) = n2w
      messages( 3) = n2
      messages( 4) = n2e
      messages( 5) = n2e2
      messages( 6) = nw2
      messages( 7) = nw
      messages( 8) = n1
      messages( 9) = ne
      messages(10) = ne2
      messages(11) = w2
      messages(12) = w1
      messages(13) = e1
      messages(14) = e2
      messages(15) = sw2
      messages(16) = sw
      messages(17) = s1
      messages(18) = se
      messages(19) = se2
      messages(20) = s2w2
      messages(21) = s2w
      messages(22) = s2
      messages(23) = s2e
      messages(24) = s2e2
      CALL rsl_create_stencil( stenid )
      CALL rsl_describe_stencil ( did, stenid, RSL_24PT, messages )
      RETURN
   END SUBROUTINE stencil_24pt

   SUBROUTINE stencil_48pt ( did, stenid )
      IMPLICIT NONE
      INTEGER did, stenid, i
      messages( 1) = n3w3
      messages( 2) = n3w2
      messages( 3) = n3w
      messages( 4) = n3
      messages( 5) = n3e
      messages( 6) = n3e2
      messages( 7) = n3e3
      messages( 8) = n2w3
      messages( 9) = n2w2
      messages(10) = n2w
      messages(11) = n2
      messages(12) = n2e
      messages(13) = n2e2
      messages(14) = n2e3
      messages(15) = nw3
      messages(16) = nw2
      messages(17) = nw
      messages(18) = n1
      messages(19) = ne
      messages(20) = ne2
      messages(21) = ne3
      messages(22) = w3
      messages(23) = w2
      messages(24) = w1
      messages(25) = e1
      messages(26) = e2
      messages(27) = e3
      messages(28) = sw3
      messages(29) = sw2
      messages(30) = sw
      messages(31) = s1
      messages(32) = se
      messages(33) = se2
      messages(34) = se3
      messages(35) = s2w3
      messages(36) = s2w2
      messages(37) = s2w
      messages(38) = s2
      messages(39) = s2e
      messages(40) = s2e2
      messages(41) = s2e3
      messages(42) = s3w3
      messages(43) = s3w2
      messages(44) = s3w
      messages(45) = s3
      messages(46) = s3e
      messages(47) = s3e2
      messages(48) = s3e3
      CALL rsl_create_stencil( stenid )
      CALL rsl_describe_stencil ( did, stenid, RSL_48PT, messages )
      RETURN
   END SUBROUTINE stencil_48pt

   SUBROUTINE stencil_80pt ( did, stenid )
      IMPLICIT NONE
      INTEGER did, stenid, i
#if 1
      do i = 1, 80
         messages(i) = msg_msg
      enddo
#else
messages(1)=    n4w4
messages(2)=    n4w3
messages(3)=    n4w2
messages(4)=    n4w
messages(5)=    n4
messages(6)=    n4e
messages(7)=    n4e2
messages(8)=    n4e3
messages(9)=    n4e4
messages(10)=   n3w4
messages(11)=   n3w3
messages(12)=   n3w2
messages(13)=   n3w
messages(14)=   n3
messages(15)=   n3e
messages(16)=   n3e2
messages(17)=   n3e3
messages(18)=   n3e4
messages(19)=   n2w4
messages(20)=   n2w3
messages(21)=   n2w2
messages(22)=   n2w
messages(23)=   n2
messages(24)=   n2e
messages(25)=   n2e2
messages(26)=   n2e3
messages(27)=   n2e4
messages(28)=   nw4
messages(29)=   nw3
messages(30)=   nw2
messages(31)=   nw
messages(32)=   n1
messages(33)=   ne
messages(34)=   ne2
messages(35)=   ne3
messages(36)=   ne4
messages(37)=   w4
messages(38)=   w3
messages(39)=   w2
messages(40)=   w1
messages(41)=   e1
messages(42)=   e2
messages(43)=   e3
messages(44)=   e4
messages(45)=   sw4
messages(46)=   sw3
messages(47)=   sw2
messages(48)=   sw
messages(49)=   s1
messages(50)=   se
messages(51)=   se2
messages(52)=   se3
messages(53)=   se4
messages(54)=   s2w4
messages(55)=   s2w3
messages(56)=   s2w2
messages(57)=   s2w
messages(58)=   s2
messages(59)=   s2e
messages(60)=   s2e2
messages(61)=   s2e3
messages(62)=   s2e4
messages(63)=   s3w4
messages(64)=   s3w3
messages(65)=   s3w2
messages(66)=   s3w
messages(67)=   s3
messages(68)=   s3e
messages(69)=   s3e2
messages(70)=   s3e3
messages(71)=   s3e4
messages(72)=   s4w4
messages(73)=   s4w3
messages(74)=   s4w2
messages(75)=   s4w
messages(76)=   s4
messages(77)=   s4e
messages(78)=   s4e2
messages(79)=   s4e3
messages(80)=   s4e4
#endif
      CALL rsl_create_stencil( stenid )
      CALL rsl_describe_stencil ( did, stenid, RSL_80PT, messages )
      RETURN
   END SUBROUTINE stencil_80pt

   SUBROUTINE stencil_120pt ( did, stenid )
      IMPLICIT NONE
      INTEGER did, stenid, i
#if 1
      do i = 1, 120
         messages(i) = msg_msg
      enddo
#else
messages(1)=    n5w5
messages(2)=    n5w4
messages(3)=    n5w3
messages(4)=    n5w2
messages(5)=    n5w
messages(6)=    n5
messages(7)=    n5e
messages(8)=    n5e2
messages(9)=    n5e3
messages(10)=   n5e4
messages(11)=   n5e5
messages(12)=   n4w5
messages(13)=   n4w4
messages(14)=   n4w3
messages(15)=   n4w2
messages(16)=   n4w
messages(17)=   n4
messages(18)=   n4e
messages(19)=   n4e2
messages(20)=   n4e3
messages(21)=   n4e4
messages(22)=   n4e5
messages(23)=   n3w5
messages(24)=   n3w4
messages(25)=   n3w3
messages(26)=   n3w2
messages(27)=   n3w
messages(28)=   n3
messages(29)=   n3e
messages(30)=   n3e2
messages(31)=   n3e3
messages(32)=   n3e4
messages(33)=   n3e5
messages(34)=   n2w5
messages(35)=   n2w4
messages(36)=   n2w3
messages(37)=   n2w2
messages(38)=   n2w
messages(39)=   n2
messages(40)=   n2e
messages(41)=   n2e2
messages(42)=   n2e3
messages(43)=   n2e4
messages(44)=   n2e5
messages(45)=   nw5
messages(46)=   nw4
messages(47)=   nw3
messages(48)=   nw2
messages(49)=   nw
messages(50)=   n1
messages(51)=   ne
messages(52)=   ne2
messages(53)=   ne3
messages(54)=   ne4
messages(55)=   ne5
messages(56)=   w5
messages(57)=   w4
messages(58)=   w3
messages(59)=   w2
messages(60)=   w1
messages(61)=   e1
messages(62)=   e2
messages(63)=   e3
messages(64)=   e4
messages(65)=   e5
messages(66)=   sw5
messages(67)=   sw4
messages(68)=   sw3
messages(69)=   sw2
messages(70)=   sw
messages(71)=   s1
messages(72)=   se
messages(73)=   se2
messages(74)=   se3
messages(75)=   se4
messages(76)=   se5
messages(77)=   s2w5
messages(78)=   s2w4
messages(79)=   s2w3
messages(80)=   s2w2
messages(81)=   s2w
messages(82)=   s2
messages(83)=   s2e
messages(84)=   s2e2
messages(85)=   s2e3
messages(86)=   s2e4
messages(87)=   s2e5
messages(88)=   s3w5
messages(89)=   s3w4
messages(90)=   s3w3
messages(91)=   s3w2
messages(92)=   s3w
messages(93)=   s3
messages(94)=   s3e
messages(95)=   s3e2
messages(96)=   s3e3
messages(97)=   s3e4
messages(98)=   s3e5
messages(99)=   s4w5
messages(100)=  s4w4
messages(101)=  s4w3
messages(102)=  s4w2
messages(103)=  s4w
messages(104)=  s4
messages(105)=  s4e
messages(106)=  s4e2
messages(107)=  s4e3
messages(108)=  s4e4
messages(109)=  s4e5
messages(110)=  s5w5
messages(111)=  s5w4
messages(112)=  s5w3
messages(113)=  s5w2
messages(114)=  s5w
messages(115)=  s5
messages(116)=  s5e
messages(117)=  s5e2
messages(118)=  s5e3
messages(119)=  s5e4
messages(120)=  s5e5
#endif
      CALL rsl_create_stencil( stenid )
      CALL rsl_describe_stencil ( did, stenid, RSL_120PT, messages )
      RETURN
   END SUBROUTINE stencil_120pt

   SUBROUTINE setup_halo_rsl( grid )
       USE module_domain
       IMPLICIT NONE
       TYPE(domain) , INTENT (INOUT) :: grid 
      INTEGER i, kms, ims, jms
   ! executable
      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_ZXY )
            kms = grid%sm31
            ims = grid%sm32
            jms = grid%sm33
            decomp(1) = RSL_NOTDECOMPOSED
            decomp(2) = RSL_M
            decomp(3) = RSL_N
            decomp2d(1) = RSL_M
            decomp2d(2) = RSL_N
            glen2d(1) = grid%ed32 - grid%sd32 + 1
            glen2d(2) = grid%ed33 - grid%sd33 + 1
            llen2d(1) = grid%em32 - grid%sm32 + 1
            llen2d(2) = grid%em33 - grid%sm33 + 1
         CASE ( DATA_ORDER_XYZ )
            kms = grid%sm33
            ims = grid%sm31
            jms = grid%sm32
            decomp(1) = RSL_M
            decomp(2) = RSL_N
            decomp(3) = RSL_NOTDECOMPOSED
            decomp2d(1) = RSL_M
            decomp2d(2) = RSL_N
            glen2d(1) = grid%ed31 - grid%sd31 + 1
            glen2d(2) = grid%ed32 - grid%sd32 + 1
            llen2d(1) = grid%em31 - grid%sm31 + 1
            llen2d(2) = grid%em32 - grid%sm32 + 1
         CASE ( DATA_ORDER_XZY )
            kms = grid%sm32
            ims = grid%sm31
            jms = grid%sm33
            decomp(1) = RSL_M
            decomp(2) = RSL_NOTDECOMPOSED
            decomp(3) = RSL_N
            decomp2d(1) = RSL_M
            decomp2d(2) = RSL_N
            glen2d(1) = grid%ed31 - grid%sd31 + 1
            glen2d(2) = grid%ed33 - grid%sd33 + 1
            llen2d(1) = grid%em31 - grid%sm31 + 1
            llen2d(2) = grid%em33 - grid%sm33 + 1
         CASE ( DATA_ORDER_YXZ )
            kms = grid%sm33
            ims = grid%sm32
            jms = grid%sm31
            decomp(1) = RSL_N
            decomp(2) = RSL_M
            decomp(3) = RSL_NOTDECOMPOSED
            decomp2d(1) = RSL_N
            decomp2d(2) = RSL_M
            glen2d(1) = grid%ed32 - grid%sd32 + 1
            glen2d(2) = grid%ed31 - grid%sd31 + 1
            llen2d(1) = grid%em32 - grid%sm32 + 1
            llen2d(2) = grid%em31 - grid%sm31 + 1
      END SELECT

      glen(1)   = grid%ed31 - grid%sd31 + 1
      glen(2)   = grid%ed32 - grid%sd32 + 1
      glen(3)   = grid%ed33 - grid%sd33 + 1
      llen(1)   = grid%em31 - grid%sm31 + 1
      llen(2)   = grid%em32 - grid%sm32 + 1
      llen(3)   = grid%em33 - grid%sm33 + 1

   END SUBROUTINE setup_halo_rsl


   SUBROUTINE setup_xpose_rsl( grid )
       USE module_domain
       IMPLICIT NONE
       TYPE(domain) , INTENT (INOUT) :: grid 
      INTEGER i, kms, ims, jms

      CALL setup_halo_rsl ( grid )

      llen_tx(1) = grid%em31x - grid%sm31x + 1
      llen_tx(2) = grid%em32x - grid%sm32x + 1
      llen_tx(3) = grid%em33x - grid%sm33x + 1
      llen_ty(1) = grid%em31y - grid%sm31y + 1
      llen_ty(2) = grid%em32y - grid%sm32y + 1
      llen_ty(3) = grid%em33y - grid%sm33y + 1

   END SUBROUTINE setup_xpose_rsl

   SUBROUTINE setup_period_rsl( grid )
       USE module_domain
       IMPLICIT NONE
       TYPE(domain) , INTENT (INOUT) :: grid 
      INTEGER i, kms, ims, jms

      CALL setup_xpose_rsl ( grid )

   ! Define periodic BC's -- for the period routines, the glen
   ! array contains the actual logical size of the field (that is,
   ! staggering is explicitly stated).  Llen is not affected.

      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_XZY )

      glen(1)    = grid%ed31 - grid%sd31
      glen(2)    = grid%ed32 - grid%sd32 + 1
      glen(3)    = grid%ed33 - grid%sd33
      glenx(1)   = glen(1)
      glenx(2)   = glen(2)
      glenx(3)   = glen(3)
      gleny(1)   = glen(1)
      gleny(2)   = glen(2)
      gleny(3)   = glen(3)
      glenxy(1)   = glen(1)
      glenxy(2)   = glen(2)
      glenxy(3)   = glen(3)
      llenx(1)   = llen(1)
      llenx(2)   = llen(2)
      llenx(3)   = llen(3)
      lleny(1)   = llen(1)
      lleny(2)   = llen(2)
      lleny(3)   = llen(3)
      llenxy(1)   = llen(1)
      llenxy(2)   = llen(2)
      llenxy(3)   = llen(3)

      glen2d(1)    = grid%ed31 - grid%sd31
      glen2d(2)    = grid%ed33 - grid%sd33
      glenx2d(1)   = glen2d(1)
      glenx2d(2)   = glen2d(2)
      gleny2d(1)   = glen2d(1)
      gleny2d(2)   = glen2d(2)
      glenxy2d(1)  = glen2d(1)
      glenxy2d(2)  = glen2d(2)
      llenx2d(1)   = llen2d(1)
      llenx2d(2)   = llen2d(2)
      lleny2d(1)   = llen2d(1)
      lleny2d(2)   = llen2d(2)
      llenxy2d(1)   = llen2d(1)
      llenxy2d(2)   = llen2d(2)

      decompx(1)   = RSL_M_STAG
      decompx(2)   = RSL_NOTDECOMPOSED
      decompx(3)   = RSL_N
      decompy(1)   = RSL_M
      decompy(2)   = RSL_NOTDECOMPOSED
      decompy(3)   = RSL_N_STAG
      decompxy(1)  = RSL_M_STAG
      decompxy(2)  = RSL_NOTDECOMPOSED
      decompxy(3)  = RSL_N_STAG

      decomp2d(1)  = RSL_M
      decomp2d(2)  = RSL_N

      decompx2d(1)  = RSL_M_STAG
      decompx2d(2)  = RSL_N

      decompy2d(1)  = RSL_M
      decompy2d(2)  = RSL_N_STAG

      decompxy2d(1)  = RSL_M_STAG
      decompxy2d(2)  = RSL_N_STAG

         CASE DEFAULT
            CALL wrf_error_fatal ( "module_dm: setup_period_rsl: unsuppported data order" )

      END SELECT

      RETURN
   END SUBROUTINE setup_period_rsl

!------------------------------------------------------------------
   INTEGER FUNCTION intermediate_mapping ( w1, w2, info, m, n, py, px )
      IMPLICIT NONE
      INTEGER, DIMENSION(*)   :: w1, w2
      REAL, DIMENSION(*)      :: info
      INTEGER, INTENT(IN)     :: m, n, py, px
      INTEGER                 :: nest_m, nest_n, nri, nrj, nest_domdesc, shw
! <DESCRIPTION>
! This is a routine provided by the rsl external comm layer.
! and is defined in external/RSL/module_dm.F, which is copied
! into frame/module_dm.F at compile time.  Changes to frame/module_dm.F
! will be lost.
! 
! This routine is related to nesting and is used by the rsl domain
! decomposition algorithm to decompose an domain that serves as an
! intermediary between the parent domain and the nest. This intermediate
! domain is at the coarse domain's resolution but it is only large enough
! to cover the region of the nested domain plus an extra number of cells
! out onto the coarse domain around the region of the nest (this number
! is specified by the namelist variable shw, default 2). The intermediate
! domain is decomposed using the nested domain's decomposition
! information so that all interpolations from coarse domain data to the
! nest may be done locally on the processor without communication.  (The
! communication occurs during the transfer of data between the parent
! domain and the intermediate domain.  See <a
! href=interp_domain_em_part1.html>interp_domain_em_part1</a>, <a
! href=interp_domain_em_part2.html>interp_domain_em_part2</a>, <a
! href=force_domain_em_part1.html>force_domain_em_part1</a>, <a
! href=feedback_domain_em_part1.html>feedback_domain_em_part1</a>, and <a
! href=feedback_domain_em_part2.html>feedback_domain_em_part2</a>.)
! 
! This routine and it's companion intermediate_mapping2 call the rsl
! routine GET_DOMAIN_DECOMP passing it the rsl domain descriptor for the
! nest to retrieve from rsl the nested decomposition.  This information
! is then used to decomposed the intermediate domain.
! 
! Rsl is given the intermediate_mapping function to use when decomposing
! the intermediate domain with a call to:
! 
!   <tt>CALL set_def_decomp_fcn1 ( intermediate_domdesc, intermediate_mapping )</tt>
! 
! inside the routine <a href=patch_domain_rsl.html>patch_domain_rsl</a>
! that is also defined in external/RSL/module_dm.F.
!
! </DESCRIPTION>

      nest_m = int(info(1)+.01) ; nest_n = int(info(2)+.01) ; nest_domdesc = int(info(3)+.01)
      nri = int(info(4)+.01)    ; nrj    = int(info(5)+.01)
      shw = int(info(6)+.01)
      CALL  intermediate_mapping2 ( w1, w2, info, m, n, nest_m, nest_n, nest_domdesc, py, px, nri, nrj, shw )
      intermediate_mapping = 0
      RETURN
   END FUNCTION intermediate_mapping

   SUBROUTINE intermediate_mapping2 ( w1, w2, info, m, n, nest_m, nest_n, nest_domdesc, py, px, nri, nrj, shw )
      IMPLICIT NONE
      INTEGER, DIMENSION(*)   :: w1, w2
      REAL, DIMENSION(*)      :: info
      INTEGER, INTENT(IN)     :: m, n, nest_m, nest_n, nest_domdesc, py, px, nri, nrj, shw
      INTEGER                 :: nest_decomp( nest_m, nest_n )
      INTEGER                 :: i, j
! <DESCRIPTION>
! See <a href=intermediate_mapping.html>intermediate_mapping</a>.
! </DESCRIPTION>


      CALL GET_DOMAIN_DECOMP ( nest_domdesc, nest_decomp, nest_m*nest_n )
      DO j = 1, nest_n, nrj
        DO i = 1, nest_m, nri
          w2((i/nri+1+shw) + (j/nrj+1-1+shw)*m) = nest_decomp(i,j)
        ENDDO
      ENDDO
#if 1
      ! fill out the stencil to the edges of the intermediate domain
      do j = 1,n
        do i = 1,shw
          w2(i+(j-1)*m) = w2(shw+1+(j-1)*m)
        enddo
        do i = m,m-shw-1,-1
          w2(i+(j-1)*m) = w2(m-shw-2+(j-1)*m)
        enddo
      enddo
      do i = 1,m
        do j = 1,shw
          w2(i+(j-1)*m) = w2(i+(shw+1-1)*m)
        enddo
        do j = n,n-shw-1,-1
          w2(i+(j-1)*m) = w2(i+(n-shw-2-1)*m)
        enddo
      enddo
#endif

      RETURN
   END SUBROUTINE intermediate_mapping2

!------------------------------------------------------------------

   SUBROUTINE patch_domain_rsl( id  , domdesc , parent, parent_id , parent_domdesc , &
                                sd1 , ed1 , sp1 , ep1 , sm1 , em1 ,        &
                                sd2 , ed2 , sp2 , ep2 , sm2 , em2 ,        &
                                sd3 , ed3 , sp3 , ep3 , sm3 , em3 ,        &
                                            sp1x , ep1x , sm1x , em1x ,        &
                                            sp2x , ep2x , sm2x , em2x ,        &
                                            sp3x , ep3x , sm3x , em3x ,        &
                                            sp1y , ep1y , sm1y , em1y ,        &
                                            sp2y , ep2y , sm2y , em2y ,        &
                                            sp3y , ep3y , sm3y , em3y ,        &
                                bdx , bdy )

      USE module_domain
      USE module_machine

      IMPLICIT NONE
      INTEGER, INTENT(IN)   :: sd1 , ed1 , sd2 , ed2 , sd3 , ed3 , bdx , bdy
      INTEGER, INTENT(OUT)  :: sp1 , ep1 , sp2 , ep2 , sp3 , ep3 , &
                               sm1 , em1 , sm2 , em2 , sm3 , em3
      INTEGER, INTENT(OUT)  :: sp1x , ep1x , sp2x , ep2x , sp3x , ep3x , &
                               sm1x , em1x , sm2x , em2x , sm3x , em3x
      INTEGER, INTENT(OUT)  :: sp1y , ep1y , sp2y , ep2y , sp3y , ep3y , &
                               sm1y , em1y , sm2y , em2y , sm3y , em3y
      INTEGER, INTENT(IN)   :: id
      INTEGER, INTENT(OUT)  :: domdesc
      INTEGER, INTENT(IN)   :: parent_id
      INTEGER, INTENT(IN)   :: parent_domdesc
      TYPE(domain),POINTER  :: parent

! <DESCRIPTION>
! This is a routine provided by the rsl external comm layer.
! and is defined in external/RSL/module_dm.F, which is copied
! into frame/module_dm.F at compile time.  Changes to frame/module_dm.F
! will be lost.
! 
! This routine is called by <a
! href=wrf_dm_patch_domain.html>wrf_dm_patch_domain</a>, the rsl
! package-supplied routine that is called by <a
! href=wrf_patch_domain.html>wrf_patch_domain</a> in the course of
! setting up a new domain when running WRF on distributed memory parallel
! computers.  This provides the rsl-specific mechanisms for defining and
! decomposing a domain, and for associating it within rsl to it's parent
! domain (in the case of a nest).
!
! The routine takes as input arguments the domain id, the index of the
! domain in the namelist (top-most domain is id=1) the parent's id and
! rsl domain descriptor (if there is a parent), and the the global
! (undecomposed) dimensions of the new domain. The routine returns the
! patch dimensions (computational extent),  memory dimensions (local
! array sizes on each task), and an rsl domain descriptor for the new
! domain.  The width of the x and y boundary regions is also passed in
! (defined in <a href=../../share/module_bc.f>share/module_bc.F</a>) and
! are used in the calculation of the memory dimensions.
! 
! <b>Nesting </b>
! 
! This routine also defines, decomposes, and associates the intermediate
! domain that is used to transfer forcing and feedback data between a
! nest and its parent domain.
! 
! The relationship between a parent domain, the nest, and this
! intermediate domain is stored partly in rsl and partly in WRF as fields
! in the TYPE(domain) data structure (defined in <a
! href=../../frame/module_domain.f>frame/module_domain.F</a>).
! 
! Basically, the rsl-maintained relationship is between the parent domain
! and the intermediate domain; for purposes of interprocessor
! communication and forcing and feedback, rsl considers the nest a
! standalone domain. This is because all of the rsl-mediated
! communication for moving data between processors for forcing and
! feedback is between the parent and the intermediate domain.  The
! movement of data between the intermediate domain and the nest is all
! on-processor, and therefore does not involve rsl to a large extent.
! 
! The WRF-maintained relationship between a parent and a nest is
! represented through pointers in TYPE(domain).  The parent domain
! maintains an array of pointers to its children through the
! <em>nests</em> field of TYPE(domain).  The nest has a back-pointer to
! its parent through <em>parents</em> (there is only ever one parent of a
! nest in WRF).  The nest also holds the pointer to the intermediate
! domain, called <em>intermediate_grid</em>.
! 
! The actual forcing and feedback between parent, nest, and intermediate
! domains are handled by other routines defined in
! external/RSL/module_dm.F. See See <a
! href=interp_domain_em_part1.html>interp_domain_em_part1</a>, <a
! href=interp_domain_em_part2.html>interp_domain_em_part2</a>, <a
! href=force_domain_em_part1.html>force_domain_em_part1</a>, <a
! href=feedback_domain_em_part1.html>feedback_domain_em_part1</a>, and <a
! href=feedback_domain_em_part2.html>feedback_domain_em_part2</a>.)
!
! </DESCRIPTION>

! Local variables
      INTEGER               :: c_sd1 , c_ed1 , c_sd2 , c_ed2 , c_sd3 , c_ed3
      INTEGER               :: c_sp1 , c_ep1 , c_sp2 , c_ep2 , c_sp3 , c_ep3 , &
                               c_sm1 , c_em1 , c_sm2 , c_em2 , c_sm3 , c_em3
      INTEGER               :: c_sp1x , c_ep1x , c_sp2x , c_ep2x , c_sp3x , c_ep3x , &
                               c_sm1x , c_em1x , c_sm2x , c_em2x , c_sm3x , c_em3x
      INTEGER               :: c_sp1y , c_ep1y , c_sp2y , c_ep2y , c_sp3y , c_ep3y , &
                               c_sm1y , c_em1y , c_sm2y , c_em2y , c_sm3y , c_em3y

      INTEGER               :: mloc , nloc , zloc         ! all k on same proc
      INTEGER               :: mloc_x , nloc_x , zloc_x   ! all x on same proc
      INTEGER               :: mloc_y , nloc_y , zloc_y   ! all y on same proc
      INTEGER               :: c_mloc , c_nloc , c_zloc         ! all k on same proc
      INTEGER               :: c_mloc_x , c_nloc_x , c_zloc_x   ! all x on same proc
      INTEGER               :: c_mloc_y , c_nloc_y , c_zloc_y   ! all y on same proc
      INTEGER               :: mglob , nglob
      INTEGER               :: idim , jdim , kdim , i
      INTEGER , PARAMETER   :: rsl_jjx_x = 2047
      INTEGER , DIMENSION( rsl_jjx_x ) :: rsl_js_x0 , rsl_je_x0 , rsl_is_x0 , rsl_ie_x0
      INTEGER                          :: rsl_xinest_x0 , rsl_idif_x0 , rsl_jdif_x0
      INTEGER               :: i_parent_start , j_parent_start
      INTEGER               :: ids, ide, jds, jde, kds, kde
      INTEGER               :: c_ids, c_ide, c_jds, c_jde, c_kds, c_kde
      INTEGER               :: parent_grid_ratio
      INTEGER               :: shw
      INTEGER               :: idim_cd, jdim_cd, intermediate_domdesc
      INTEGER               :: intermediate_mloc, intermediate_nloc
      INTEGER               :: intermediate_mglob, intermediate_nglob
      REAL                  :: info(7)
      TYPE(domain), POINTER :: intermediate_grid
      TYPE(domain), POINTER  :: nest_grid

      SELECT CASE ( model_data_order )
         ! need to finish other cases
         CASE ( DATA_ORDER_ZXY )
            idim = ed2-sd2+1
            jdim = ed3-sd3+1
            kdim = ed1-sd1+1
         CASE ( DATA_ORDER_XYZ )
            idim = ed1-sd1+1
            jdim = ed2-sd2+1
            kdim = ed3-sd3+1
         CASE ( DATA_ORDER_XZY )
            idim = ed1-sd1+1
            jdim = ed3-sd3+1
            kdim = ed2-sd2+1
         CASE ( DATA_ORDER_YXZ)
            idim = ed2-sd2+1
            jdim = ed1-sd1+1
            kdim = ed3-sd3+1
      END SELECT
      if ( id == 1 ) then
! <DESCRIPTION>
! <b> Main Domain </b>
!
! The top-level WRF domain (id = 1) is set up when <a
! href=alloc_and_configure_domain.html>alloc_and_configure_domain</a> is
! called from <a href=wrf.html>wrf</a>.  This is done here in
! rsl_patch_domain with a call to RSL_MOTHER_DOMAIN3D.  The global domain
! dimensions are converted to the length of each dimension in i, j, and k
! for the domain (based on model_data_order, which is defined in <a
! href=../../frame/module_driver_constants.f>frame/module_driver_constants.F</a>,
! based on the dimspec entries in the Registry.  In WRF the X/I dimension
! corresponds to the the first dimension, the Z/K dimension the second,
! and the Y/J the third.
!
! An rsl tag denoting the largest stencil to be used on the domain is
! also provided. This is RSL_24PT for the EM core; the NMM core uses a
! wider maximum stencil, RSL_120PT.  On return, the RSL domain descriptor
! for the domain will be defined along with rsl's advice on the minimum
! memory required for the memory dimensions on this task.
! 
! Rsl supports
! alternate decompositions of the domain -- X/Z and Y/Z -- and
! transposition operations between these decompositions. These are used
! in WRF 3DVAR but not in the EM version of the WRF model itself, which
! is always only an X/Y decomposition.
!
! As a diagnostic, the rsl routine SHOW_DOMAIN_DECOMP is called, which
! outputs a text file with information on the decomposition to the
! file show_domain_0000 from processor zero.
!
! The actual memory dimensions that patch_domain_rsl are computed in a
! call to <a
! href=compute_memory_dims_using_rsl.html>compute_memory_dims_using_rsl</a>,
! also defined in external/RSL/module_dm.F. Once these have been computed
! the patch_domain_rsl returns.
! 
! </DESCRIPTION>

#ifndef NMM_CORE
         CALL rsl_mother_domain3d(domdesc, RSL_24PT,               &
#else
         CALL rsl_mother_domain3d(domdesc, RSL_120PT,               &
#endif
                                  idim   ,  jdim   ,  kdim   ,     &
                                  mloc   ,  nloc   ,  zloc   ,     &
#ifdef D3VAR_IRY_KLUDGE
! see comment above, 20020910 JM
                                  mloc_x ,  nloc_x ,  zloc_x ,     &
                                  mloc_y ,  nloc_y ,  zloc_y       )
#else
                                  mloc_y ,  nloc_y ,  zloc_y ,     &   ! x->y 20020908
                                  mloc_x ,  nloc_x ,  zloc_x       )   ! y->x 20020908
#endif
         CALL show_domain_decomp(domdesc)
         ! this computes the dimension information for the
         ! nest and passes these back
         CALL compute_memory_dims_using_rsl (          &
                   domdesc ,                           &
                   mloc   ,  nloc   ,  zloc   ,        &
                   mloc_x ,  nloc_x ,  zloc_x ,        &
                   mloc_y ,  nloc_y ,  zloc_y ,        &
                   sd1,  ed1,  sd2,  ed2,  sd3,  ed3,  &
                   sp1,  ep1,  sp2,  ep2,  sp3,  ep3,  &
                   sp1x, ep1x, sp2x, ep2x, sp3x, ep3x, &
                   sp1y, ep1y, sp2y, ep2y, sp3y, ep3y, &
                   sm1,  em1,  sm2,  em2,  sm3,  em3,  &
                   sm1x, em1x, sm2x, em2x, sm3x, em3x, &
                   sm1y, em1y, sm2y, em2y, sm3y, em3y  )

      else

! <DESCRIPTION>
! <b> Nested Domain </b>
! For nested domains (id greater than 1), the patch_domain_rsl first
! defines the nest itself in rsl as a stand-alone domain (as far as RSL
! knows it has no parent), then sets up the the intermediate domain that,
! from rsl's point of view, is a nest of the parent with a refinement
! ratio of 1 to 1 (same resolution).
! 
! As with the top-most domain, the nested domain is defined using
! RSL_MOTHER_DOMAIN3D and its memory dimensions are computed calling
! compute_memory_dims_using_rsl, as above.
!
! </DESCRIPTION>
         !
         ! first spawn the actual nest. It is not
         ! directly associated in rsl with the parent
         ! so we spawn it as an unassociated domain
         ! (another "mother")
         !
#ifndef NMM_CORE
         CALL rsl_mother_domain3d(domdesc, RSL_24PT,               &
#else
         CALL rsl_mother_domain3d(domdesc, RSL_120PT,               &
#endif
                                  idim   ,  jdim   ,  kdim   ,     &
                                  mloc   ,  nloc   ,  zloc   ,     &
#  ifdef D3VAR_IRY_KLUDGE
! see comment above, 20020910 JM
                                  mloc_x ,  nloc_x ,  zloc_x ,     &
                                  mloc_y ,  nloc_y ,  zloc_y       )
#  else
                                  mloc_y ,  nloc_y ,  zloc_y ,     &     ! x->y 20020910
                                  mloc_x ,  nloc_x ,  zloc_x       )     ! y->x 20020910
#  endif
         CALL show_domain_decomp(domdesc)
         ! this computes the dimension information for the
         ! nest and passes these back
         CALL compute_memory_dims_using_rsl (          &
                   domdesc ,                           &
                   mloc   ,  nloc   ,  zloc   ,        &
                   mloc_x ,  nloc_x ,  zloc_x ,        &
                   mloc_y ,  nloc_y ,  zloc_y ,        &
                   sd1,  ed1,  sd2,  ed2,  sd3,  ed3,  &
                   sp1,  ep1,  sp2,  ep2,  sp3,  ep3,  &
                   sp1x, ep1x, sp2x, ep2x, sp3x, ep3x, &
                   sp1y, ep1y, sp2y, ep2y, sp3y, ep3y, &
                   sm1,  em1,  sm2,  em2,  sm3,  em3,  &
                   sm1x, em1x, sm2x, em2x, sm3x, em3x, &
                   sm1y, em1y, sm2y, em2y, sm3y, em3y  )

! <DESCRIPTION>
! Once the nest is defined, the intermediate
! domain is defined and associated as a nest with the parent.
! Here, SET_DEF_DECOMP_FCN1 is called, which directs rsl to use a special decomposition function,
! <a href=intermediate_mapping.html>intermediate_mapping</a>, that
! generates a decomposition of the intermediate domain in which
! intermediate domain points are assigned to the same task as the nested
! points they overlay (allowing the interpolation to be task-local).
! This applies only to the intermediate domain; the default decmposition function
! for other domains is not affected.
! This decomposition algorithm also requires knowledge of the dimensions
! of the nest, the nests rsl descriptor (defined above), the nesting
! ratio, and the extra amount the intermediate domain should cover in the
! coarse domain to allow for the stencil of the interpolator (the <a
! href=sint.html>sint</a> routine.  This information is packed into an
! "info" vector that is provided to rsl with a call to
! SET_DEF_DECOMP_INFO.
! 
! </DESCRIPTION>


         CALL get_shw( id, shw )
         CALL get_i_parent_start( id , i_parent_start )
         CALL get_j_parent_start( id , j_parent_start )
         CALL get_parent_grid_ratio( id, parent_grid_ratio )

         info(1) = idim               ! nest i dimension for intermediate mapping
         info(2) = jdim               ! nest j dimension for intermediate mapping
         info(3) = domdesc            ! nest domain descriptor
         info(4) = parent_grid_ratio  ! nesting ratio in i
         info(5) = parent_grid_ratio  ! nesting ratio in j
         info(6) = shw                ! stencil half-width

# if 1
   ! tells which descriptor will be given back next when intermediate domain is spawned below
   ! that is used to associate the decomposition information from the nested domain with
   ! this intermediate domain, so that it will be decomposed identically, through 
   ! the intermediate mapping function.
         CALL get_next_domain_descriptor ( intermediate_domdesc )
         CALL set_def_decomp_fcn1 ( intermediate_domdesc, intermediate_mapping )
         CALL set_def_decomp_info ( intermediate_domdesc, info )
# endif

         ! now spawn the intermediate domain that will serve as the
         ! nest-decomposed area of the CD domain, onto which data
         ! will be transferred from the CD for interpolation
         ! ** need to make sure the decomposition matches the
         ! ** nested decomposition

! <DESCRIPTION>
! The undecomposed dimensions of the intermediate domain are computed along
! with the location of the intermediate domain's lower left-hand point and these
! are passed to the RSL_SPAWN_REGULAR_NEST1 routine, which defines the intermediate
! domain as a nest with 1:1 refinement within the parent domain. The memory dimensions
! of the intermediate domain are computed by calling COMPUTE_MEMORY_DIMS_USING_RSL
! and then the intermediate domain is allocated as a WRF grid of TYPE(domain).
! The flow of control here resembles that of <a href=alloc_and_configure_domain.html>
! alloc_and_configure_domain</a>, in <a href=../../frame/module_domain.f>
! frame/module_domain.F</a>.
! </DESCRIPTION>

         idim_cd = idim / parent_grid_ratio + 1 + 2*shw + 1
         jdim_cd = jdim / parent_grid_ratio + 1 + 2*shw + 1

         c_ids = i_parent_start-shw ; c_ide = c_ids + idim_cd - 1
         c_jds = j_parent_start-shw ; c_jde = c_jds + jdim_cd - 1
         c_kds = sd2                ; c_kde = ed2                   ! IKJ ONLY

         CALL RSL_SPAWN_REGULAR_NEST1(                  &
                intermediate_domdesc,                   &
                parent_domdesc,                         &
#ifndef NMM_CORE
                RSL_24PT,                               &
#else
                RSL_120PT,                               &
#endif
                c_ids, c_jds,                               &
                idim_cd,jdim_cd,                        &
                1, 1,                                   &
                intermediate_mloc,intermediate_nloc,    &
                intermediate_mglob,intermediate_nglob)

         zloc = kdim
         ! compute dims for intermediate domain
         CALL show_domain_decomp(intermediate_domdesc)
         CALL compute_memory_dims_using_rsl (          &
                   intermediate_domdesc ,              &
                   intermediate_mloc   ,  intermediate_nloc   ,  zloc   ,        &
                   c_mloc_x ,  c_nloc_x ,  c_zloc_x ,        &
                   c_mloc_y ,  c_nloc_y ,  c_zloc_y ,        &
                   c_ids,  c_ide,  c_kds,  c_kde,  c_jds,  c_jde,  &   ! IKJ ONLY
                   c_sp1,  c_ep1,  c_sp2,  c_ep2,  c_sp3,  c_ep3, &
                   c_sp1x, c_ep1x, c_sp2x, c_ep2x, c_sp3x, c_ep3x, &
                   c_sp1y, c_ep1y, c_sp2y, c_ep2y, c_sp3y, c_ep3y, &
                   c_sm1,  c_em1,  c_sm2,  c_em2,  c_sm3,  c_em3,  &
                   c_sm1x, c_em1x, c_sm2x, c_em2x, c_sm3x, c_em3x, &
                   c_sm1y, c_em1y, c_sm2y, c_em2y, c_sm3y, c_em3y  )
         ! since the RSL_SPAWN_REGULAR_NEST1 does not do the vert dimension
         ! we need to set that manually  >>>>> IKJ ONLY
         c_sp2 = c_kds   !IKJ ONLY
         c_ep2 = c_kde   !IKJ ONLY
         c_sm2 = c_kds   !IKJ ONLY
         c_em2 = c_kde   !IKJ ONLY

         ! global dims are same as CD
         ! good for IKJ only
         c_sd1 = parent%sd31       ; c_ed1 = parent%ed31
         c_sd2 = parent%sd32       ; c_ed2 = parent%ed32
         c_sd3 = parent%sd33       ; c_ed3 = parent%ed33


         ! Sequence of calls to create a new, intermediate domain
         ! data structures that can be used to store the CD data
         ! that will be used as input to the forcing interpolation
         ! on each processor.
         ALLOCATE ( intermediate_grid )
         ALLOCATE ( intermediate_grid%parents( max_parents ) )
         ALLOCATE ( intermediate_grid%nests( max_nests ) )

         NULLIFY( intermediate_grid%sibling )
         DO i = 1, max_nests
            NULLIFY( intermediate_grid%nests(i)%ptr )
         ENDDO
         NULLIFY  (intermediate_grid%next)
         NULLIFY  (intermediate_grid%same_level)
         NULLIFY  (intermediate_grid%i_start)
         NULLIFY  (intermediate_grid%j_start)
         NULLIFY  (intermediate_grid%i_end)
         NULLIFY  (intermediate_grid%j_end)

         intermediate_grid%id = id
         intermediate_grid%domdesc = intermediate_domdesc
         intermediate_grid%num_nests = 0
         intermediate_grid%num_siblings = 0
         intermediate_grid%num_parents = 1
         intermediate_grid%max_tiles   = 0
         intermediate_grid%num_tiles_spec   = 0
         ! hook up some pointers
         
! <DESCRIPTION>
! However, the pointers in the nested hierachy must be set up differently
! in this case.  First, the pointer to the nests TYPE(domain) is
! retrieved in a somewhat roundabout way, by searching the domain
! hierarcy rooted at head_grid (defined in frame/module_domain.F) with a
! call to <a href=find_grid_by_id.html>find_grid_by_id</a>.  The nested
! grid has already been added to the hierarchy by WRF because that is
! done in <a
! href=alloc_and_configure_domain.html>alloc_and_configure_domain</a>
! before <a href=wrf_patch_domain.html>wrf_patch_domain</a> is called,
! but the arguments to patch_domain_rsl, here, do not include a pointer to
! the nest domain, only the id (could be changed).  Once the pointer
! to the nested grid's domain data structure is located, the nest's
! intermediate_grid pointer is set to the the domain data struture for
! the newly created created intermediate_domain.  In a curious twist of
! geneology, however, the intermediate_grid (from WRF domain hierarchy
! point of view) is set to consider the nest its parent. This is because,
! from the WRF framework's point of view, the intermediate domain does
! not exist (it only exists because of code in external/RSL/module_dm.F,
! an external-package supplied module).  It remains only to allocate
! the fields in the intermediate domain's domain data type, set a few
! other fields such as dx, dy, and dt (to the parent domain's values) and 
! return.
! 
! </DESCRIPTION>

         CALL find_grid_by_id ( id, head_grid, nest_grid )
         nest_grid%intermediate_grid => intermediate_grid  ! nest grid now has a pointer to this baby
         intermediate_grid%parents(1)%ptr => nest_grid     ! the intermediate grid considers nest its parent
         intermediate_grid%num_parents = 1

         c_sm1x = 1 ; c_em1x = 1 ; c_sm2x = 1 ; c_em2x = 1 ; c_sm3x = 1 ; c_em3x = 1
         c_sm1y = 1 ; c_em1y = 1 ; c_sm2y = 1 ; c_em2y = 1 ; c_sm3y = 1 ; c_em3y = 1

         ! allocate space for the intermediate domain
         CALL alloc_space_field ( intermediate_grid, id ,          &   ! use same id as nest
                               c_sd1, c_ed1, c_sd2, c_ed2, c_sd3, c_ed3,       &
                               c_sm1,  c_em1,  c_sm2,  c_em2,  c_sm3,  c_em3,  &
                               c_sm1x, c_em1x, c_sm2x, c_em2x, c_sm3x, c_em3x, &   ! x-xpose
                               c_sm1y, c_em1y, c_sm2y, c_em2y, c_sm3y, c_em3y  )   ! y-xpose

         intermediate_grid%sd31                            =   c_sd1
         intermediate_grid%ed31                            =   c_ed1
         intermediate_grid%sp31                            = c_sp1
         intermediate_grid%ep31                            = c_ep1
         intermediate_grid%sm31                            = c_sm1
         intermediate_grid%em31                            = c_em1
         intermediate_grid%sd32                            =   c_sd2
         intermediate_grid%ed32                            =   c_ed2
         intermediate_grid%sp32                            = c_sp2
         intermediate_grid%ep32                            = c_ep2
         intermediate_grid%sm32                            = c_sm2
         intermediate_grid%em32                            = c_em2
         intermediate_grid%sd33                            =   c_sd3
         intermediate_grid%ed33                            =   c_ed3
         intermediate_grid%sp33                            = c_sp3
         intermediate_grid%ep33                            = c_ep3
         intermediate_grid%sm33                            = c_sm3
         intermediate_grid%em33                            = c_em3

         CALL med_add_config_info_to_grid ( intermediate_grid )

         intermediate_grid%dx = parent%dx
         intermediate_grid%dy = parent%dy
         intermediate_grid%dt = parent%dt

         CALL wrf_dm_define_comms ( intermediate_grid )

      endif

      RETURN
  END SUBROUTINE patch_domain_rsl

  SUBROUTINE compute_memory_dims_using_rsl (        &
                domdesc ,                           &
                mloc   ,  nloc   ,  zloc   ,        &
                mloc_x ,  nloc_x ,  zloc_x ,        &
                mloc_y ,  nloc_y ,  zloc_y ,        &
                sd1,  ed1,  sd2,  ed2,  sd3,  ed3,  &
                sp1,  ep1,  sp2,  ep2,  sp3,  ep3,  &
                sp1x, ep1x, sp2x, ep2x, sp3x, ep3x, &
                sp1y, ep1y, sp2y, ep2y, sp3y, ep3y, &
                sm1,  em1,  sm2,  em2,  sm3,  em3,  &
                sm1x, em1x, sm2x, em2x, sm3x, em3x, &
                sm1y, em1y, sm2y, em2y, sm3y, em3y  )
      USE module_machine
      IMPLICIT NONE
      ! Arguments
      INTEGER, INTENT(IN ) :: domdesc
      INTEGER, INTENT(IN ) :: mloc , nloc , zloc         ! all k on same proc
      INTEGER, INTENT(IN ) :: mloc_x , nloc_x , zloc_x   ! all x on same proc
      INTEGER, INTENT(IN ) :: mloc_y , nloc_y , zloc_y   ! all y on same proc
      INTEGER, INTENT(IN ) :: sd1, ed1, sd2, ed2, sd3, ed3
      INTEGER, INTENT(OUT) :: sp1, ep1, sp2, ep2, sp3, ep3
      INTEGER, INTENT(OUT) :: sp1x, ep1x, sp2x, ep2x, sp3x, ep3x
      INTEGER, INTENT(OUT) :: sp1y, ep1y, sp2y, ep2y, sp3y, ep3y
      INTEGER, INTENT(OUT) :: sm1, em1, sm2, em2, sm3, em3
      INTEGER, INTENT(OUT) :: sm1x, em1x, sm2x, em2x, sm3x, em3x
      INTEGER, INTENT(OUT) :: sm1y, em1y, sm2y, em2y, sm3y, em3y
! <DESCRIPTION>
! For a given domain (referred to by it's rsl domain descriptor) interrogate 
! rsl and compute the patch and memory dimensions for the section of the
! domain that is computed on this task.  rsl has this information already
! and it is necessary only to (1) assign the information to the correct
! dimension in WRF, based on the setting of model_data_order (
! defined in <a href=../../frame/module_driver_constants.f>frame/module_driver_constants.F</a>,
! based on the dimspec entries in the Registry), and (2) convert the
! start and end of each dimension
! from local (as they are carried in rsl, a holdover from MM5) to global.
! 
! </DESCRIPTION>
      ! Local data
      INTEGER , PARAMETER   :: rsl_jjx_x = 2047
      INTEGER , DIMENSION( rsl_jjx_x ) :: rsl_js_x0 , rsl_je_x0 , rsl_is_x0 , rsl_ie_x0
      INTEGER                          :: rsl_xinest_x0 , rsl_idif_x0 , rsl_jdif_x0

      CALL RSL_REG_RUN_INFOP(domdesc , 0 ,               &
                             rsl_jjx_x ,                 &
                             rsl_xinest_x0 ,             &
                             rsl_is_x0 , rsl_ie_x0 ,     &
                             rsl_js_x0 , rsl_je_x0 ,     &
                             rsl_idif_x0 , rsl_jdif_x0   )

      SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )

           CALL rsl_reg_patchinfo_mn ( domdesc ,                       &
                       sp2  , ep2  , sp3  , ep3  ,  sp1  , ep1   )
           sp1 = sp1 - ( 1 - sd1 ) ; ep1 = ep1 - ( 1 - sd1 )   ! adjust if domain start not 1
           sp2 = sp2 - ( 1 - sd2 ) ; ep2 = ep2 - ( 1 - sd2 )
           sp3 = sp3 - ( 1 - sd3 ) ; ep3 = ep3 - ( 1 - sd3 )
           sm2 = sp2 - rsl_padarea
           em2 = sm2 + mloc - 1
           sm3 = sp3 - rsl_padarea
           em3 = sm3 + nloc - 1
           sm1 = sp1
           em1 = sm1 + zloc - 1

#ifdef D3VAR_IRY_KLUDGE
           CALL rsl_reg_patchinfo_mz ( domdesc ,                       &
#else
           CALL rsl_reg_patchinfo_nz ( domdesc ,                       &                    ! switched m->n 20020910
#endif
                       sp2x , ep2x , sp3x , ep3x ,  sp1x , ep1x  )
           sp1x = sp1x - ( 1 - sd1 ) ; ep1x = ep1x - ( 1 - sd1 )   ! adjust if domain start not 1
           sp2x = sp2x - ( 1 - sd2 ) ; ep2x = ep2x - ( 1 - sd2 )
           sp3x = sp3x - ( 1 - sd3 ) ; ep3x = ep3x - ( 1 - sd3 )
           sm2x = sp2x - rsl_padarea
           em2x = sm2x + mloc_x - 1
           sm3x = sp3x - rsl_padarea
           em3x = sm3x + nloc_x - 1
           sm1x = sp1x
           em1x = sm1x + zloc_x - 1

#ifdef D3VAR_IRY_KLUDGE
           CALL rsl_reg_patchinfo_nz ( domdesc ,                       &
#else
           CALL rsl_reg_patchinfo_mz ( domdesc ,                       &                    ! switched n->m 20020910
#endif
                       sp2y , ep2y , sp3y , ep3y ,  sp1y , ep1y  )
           sp1y = sp1y - ( 1 - sd1 ) ; ep1y = ep1y - ( 1 - sd1 )   ! adjust if domain start not 1
           sp2y = sp2y - ( 1 - sd2 ) ; ep2y = ep2y - ( 1 - sd2 )
           sp3y = sp3y - ( 1 - sd3 ) ; ep3y = ep3y - ( 1 - sd3 )
           sm2y = sp2y - rsl_padarea
           em2y = sm2y + mloc_y - 1
           sm3y = sp3y - rsl_padarea
           em3y = sm3y + nloc_y - 1
           sm1y = sp1y
           em1y = sm1y + zloc_y - 1

         CASE ( DATA_ORDER_XZY )

           CALL rsl_reg_patchinfo_mn ( domdesc ,                       &
                       sp1  , ep1  , sp3  , ep3  ,  sp2  , ep2   )

           sp1 = sp1 - ( 1 - sd1 ) ; ep1 = ep1 - ( 1 - sd1 )   ! adjust if domain start not 1
           sp2 = sp2 - ( 1 - sd2 ) ; ep2 = ep2 - ( 1 - sd2 )
           sp3 = sp3 - ( 1 - sd3 ) ; ep3 = ep3 - ( 1 - sd3 )

           sm1 = sp1 - rsl_padarea
           em1 = sm1 + mloc - 1
           sm3 = sp3 - rsl_padarea
           em3 = sm3 + nloc - 1
           sm2 = sp2
           em2 = sm2 + zloc - 1

#ifdef D3VAR_IRY_KLUDGE
           CALL rsl_reg_patchinfo_mz ( domdesc ,                       &
#else
           CALL rsl_reg_patchinfo_nz ( domdesc ,                       &   ! switched m->n 20020908
#endif
                       sp1x , ep1x , sp3x , ep3x ,  sp2x , ep2x  )
           sp1x = sp1x - ( 1 - sd1 ) ; ep1x = ep1x - ( 1 - sd1 )   ! adjust if domain start not 1
           sp2x = sp2x - ( 1 - sd2 ) ; ep2x = ep2x - ( 1 - sd2 )
           sp3x = sp3x - ( 1 - sd3 ) ; ep3x = ep3x - ( 1 - sd3 )
           sm1x = sp1x - rsl_padarea
           em1x = sm1x + mloc_x - 1
           sm3x = sp3x - rsl_padarea
           em3x = sm3x + nloc_x - 1
           sm2x = sp2x
           em2x = sm2x + zloc_x - 1

#ifdef D3VAR_IRY_KLUDGE
           CALL rsl_reg_patchinfo_nz ( domdesc ,                       &
#else
           CALL rsl_reg_patchinfo_mz ( domdesc ,                       &   ! switched n->m 20020908
#endif
                       sp1y , ep1y , sp3y , ep3y ,  sp2y , ep2y  )
           sp1y = sp1y - ( 1 - sd1 ) ; ep1y = ep1y - ( 1 - sd1 )   ! adjust if domain start not 1
           sp2y = sp2y - ( 1 - sd2 ) ; ep2y = ep2y - ( 1 - sd2 )
           sp3y = sp3y - ( 1 - sd3 ) ; ep3y = ep3y - ( 1 - sd3 )
           sm1y = sp1y - rsl_padarea
           em1y = sm1y + mloc_y - 1
           sm3y = sp3y - rsl_padarea
           em3y = sm3y + nloc_y - 1
           sm2y = sp2y
           em2y = sm2y + zloc_y - 1

         CASE ( DATA_ORDER_XYZ )

           CALL rsl_reg_patchinfo_mn ( domdesc ,                       &
                       sp1  , ep1  , sp2  , ep2  ,  sp3  , ep3   )
           sp1 = sp1 - ( 1 - sd1 ) ; ep1 = ep1 - ( 1 - sd1 )   ! adjust if domain start not 1
           sp2 = sp2 - ( 1 - sd2 ) ; ep2 = ep2 - ( 1 - sd2 )
           sp3 = sp3 - ( 1 - sd3 ) ; ep3 = ep3 - ( 1 - sd3 )
           sm1 = sp1 - rsl_padarea
           em1 = sm1 + mloc - 1
           sm2 = sp2 - rsl_padarea
           em2 = sm2 + nloc - 1
           sm3 = sp3
           em3 = sm3 + zloc - 1

#ifdef D3VAR_IRY_KLUDGE
           CALL rsl_reg_patchinfo_mz ( domdesc ,                       &
#else
           CALL rsl_reg_patchinfo_nz ( domdesc ,                       &     ! switched m->n 20020910
#endif
                       sp1x , ep1x , sp2x , ep2x ,  sp3x , ep3x  )
           sp1x = sp1x - ( 1 - sd1 ) ; ep1x = ep1x - ( 1 - sd1 )   ! adjust if domain start not 1
           sp2x = sp2x - ( 1 - sd2 ) ; ep2x = ep2x - ( 1 - sd2 )
           sp3x = sp3x - ( 1 - sd3 ) ; ep3x = ep3x - ( 1 - sd3 )
           sm1x = sp1x - rsl_padarea
           em1x = sm1x + mloc_x - 1
           sm2x = sp2x - rsl_padarea
           em2x = sm2x + nloc_x - 1
           sm3x = sp3x
           em3x = sm3x + zloc_x - 1

#ifdef D3VAR_IRY_KLUDGE
           CALL rsl_reg_patchinfo_nz ( domdesc ,                       &
#else
           CALL rsl_reg_patchinfo_mz ( domdesc ,                       &     ! switched n->m 20020910
#endif
                       sp1y , ep1y , sp2y , ep2y ,  sp3y , ep3y  )
           sp1y = sp1y - ( 1 - sd1 ) ; ep1y = ep1y - ( 1 - sd1 )   ! adjust if domain start not 1
           sp2y = sp2y - ( 1 - sd2 ) ; ep2y = ep2y - ( 1 - sd2 )
           sp3y = sp3y - ( 1 - sd3 ) ; ep3y = ep3y - ( 1 - sd3 )
           sm1y = sp1y - rsl_padarea
           em1y = sm1y + mloc_y - 1
           sm2y = sp2y - rsl_padarea
           em2y = sm2y + nloc_y - 1
           sm3y = sp3y
           em3y = sm3y + zloc_y - 1

         CASE ( DATA_ORDER_YXZ )

           CALL rsl_reg_patchinfo_mn ( domdesc ,                       &
                       sp2  , ep2  , sp1  , ep1  ,  sp3  , ep3   )

           sp1 = sp1 - ( 1 - sd1 ) ; ep1 = ep1 - ( 1 - sd1 )   ! adjust if domain start not 1
           sp2 = sp2 - ( 1 - sd2 ) ; ep2 = ep2 - ( 1 - sd2 )
           sp3 = sp3 - ( 1 - sd3 ) ; ep3 = ep3 - ( 1 - sd3 )
           sm2 = sp2 - rsl_padarea
           em2 = sm2 + mloc - 1
           sm1 = sp1 - rsl_padarea
           em1 = sm1 + nloc - 1
           sm3 = sp3
           em3 = sm3 + zloc - 1

#ifdef D3VAR_IRY_KLUDGE
           CALL rsl_reg_patchinfo_mz ( domdesc ,                       &
#else
           CALL rsl_reg_patchinfo_nz ( domdesc ,                       &     ! switched n->m 20020910
#endif
                       sp2x , ep2x , sp1x , ep1x ,  sp3x , ep3x  )
           sp1x = sp1x - ( 1 - sd1 ) ; ep1x = ep1x - ( 1 - sd1 )   ! adjust if domain start not 1
           sp2x = sp2x - ( 1 - sd2 ) ; ep2x = ep2x - ( 1 - sd2 )
           sp3x = sp3x - ( 1 - sd3 ) ; ep3x = ep3x - ( 1 - sd3 )
           sm2x = sp2x - rsl_padarea
           em2x = sm2x + mloc_x - 1
           sm1x = sp1x - rsl_padarea
           em1x = sm1x + nloc_x - 1
           sm3x = sp3x
           em3x = sm3x + zloc_x - 1

#ifdef D3VAR_IRY_KLUDGE
           CALL rsl_reg_patchinfo_nz ( domdesc ,                       &
#else
           CALL rsl_reg_patchinfo_mz ( domdesc ,                       &     ! switched m->n 20020910
#endif
                       sp2y , ep2y , sp1y , ep1y ,  sp3y , ep3y  )
           sp1y = sp1y - ( 1 - sd1 ) ; ep1y = ep1y - ( 1 - sd1 )   ! adjust if domain start not 1
           sp2y = sp2y - ( 1 - sd2 ) ; ep2y = ep2y - ( 1 - sd2 )
           sp3y = sp3y - ( 1 - sd3 ) ; ep3y = ep3y - ( 1 - sd3 )
           sm2y = sp2y - rsl_padarea
           em2y = sm2y + mloc_y - 1
           sm1y = sp1y - rsl_padarea
           em1y = sm1y + nloc_y - 1
           sm3y = sp3y
           em3y = sm3y + zloc_y - 1

      END SELECT

      RETURN
   END SUBROUTINE compute_memory_dims_using_rsl

   SUBROUTINE init_module_dm
      IMPLICIT NONE
      INTEGER ierr, mytask
      EXTERNAL rsl_patch_decomp
! <DESCRIPTION>
! This is the first part of the initialization of rsl for distributed
! memory parallel execution.  The routine first interrogates MPI to find
! out if it needs to be intialized (it may not, since 
! <a href=init_module_ext_quilt.html>init_module_ext_quilt</a> may
! have done this already) and if so, calls mpi_init.  Standard output
! and standard error on each process is directed to a separate file
! with a call to <a href=wrf_termio_dup.html>wrf_termio_dup</a> and,
! in the case where we <em>are</em> calling mpi_init here, MPI_COMM_WORLD
! is set as the communicator (it would not be in the case of quilting).
! 
! Finally, rsl itself is initialized and the default decomposition
! algorithm in rsl is set to the rsl-provided algorithm RSL_PATCH_DECOMP.
!
! Certain parts of this algorithm are #ifdef'd out in case -DSTUBMPI
! is specified in the configure.wrf file at compile time.  This allows
! rsl's nesting functionality to be used on a single processor (for nesting, for example) without using MPI.
! 
! </DESCRIPTION>
#ifndef STUBMPI
      INCLUDE 'mpif.h'
      LOGICAL mpi_inited
      CALL mpi_initialized( mpi_inited, ierr )
      IF ( .NOT. mpi_inited ) THEN
        ! If MPI has not been initialized then initialize it and 
        ! make comm_world the communicator
        ! Otherwise, something else (e.g. quilt-io) has already 
        ! initialized MPI, so just grab the communicator that
        ! should already be stored and use that.
        CALL mpi_init ( ierr )
        CALL wrf_termio_dup
        CALL wrf_set_dm_communicator ( MPI_COMM_WORLD )
      ENDIF
      CALL wrf_get_dm_communicator( mpi_comm_local )
      CALL wrf_termio_dup
#endif
      CALL rsl_initialize1( mpi_comm_local )
      CALL set_def_decomp_fcn ( rsl_patch_decomp )
   END SUBROUTINE init_module_dm


   REAL FUNCTION wrf_dm_max_real ( inval )
      IMPLICIT NONE
      REAL inval, retval, ierr
! <DESCRIPTION>
! Collective operation. Each processor calls passing a local value; on return
! all processors are passed back the maximum of all values passed.
!
! </DESCRIPTION>
#ifndef STUBMPI
      INCLUDE 'mpif.h'
      CALL mpi_allreduce ( inval, retval , 1, MPI_REAL, MPI_MAX, mpi_comm_local, ierr )
      wrf_dm_max_real = retval
#else
      wrf_dm_max_real = inval
#endif
   END FUNCTION wrf_dm_max_real

   REAL FUNCTION wrf_dm_min_real ( inval )
      IMPLICIT NONE
      REAL inval, retval, ierr
! <DESCRIPTION>
! Collective operation. Each processor calls passing a local value; on return
! all processors are passed back the minumum of all values passed.
!
! </DESCRIPTION>
#ifndef STUBMPI
      INCLUDE 'mpif.h'
      CALL mpi_allreduce ( inval, retval , 1, MPI_REAL, MPI_MIN, mpi_comm_local, ierr )
      wrf_dm_min_real = retval
#else
      wrf_dm_min_real = inval
#endif
   END FUNCTION wrf_dm_min_real

   REAL FUNCTION wrf_dm_sum_real ( inval )
      IMPLICIT NONE
      REAL inval, retval, ierr
! <DESCRIPTION>
! Collective operation. Each processor calls passing a local value; on return
! all processors are passed back the sum of all values passed.
!
! </DESCRIPTION>
#ifndef STUBMPI
      INCLUDE 'mpif.h'
      CALL mpi_allreduce ( inval, retval , 1, MPI_REAL, MPI_SUM, mpi_comm_local, ierr )
      wrf_dm_sum_real = retval
#else
      wrf_dm_sum_real = inval
#endif
   END FUNCTION wrf_dm_sum_real

   SUBROUTINE wrf_dm_maxval ( val, idex, jdex )
      IMPLICIT NONE
      REAL val, val_all( rsl_nproc )
      INTEGER idex, jdex, ierr
      INTEGER dex(2)
      INTEGER dex_all (2,rsl_nproc)
! <DESCRIPTION>
! Collective operation. Each processor calls passing a local value and its index; on return
! all processors are passed back the maximum of all values passed and its index.
!
! </DESCRIPTION>
      INTEGER i, comm
#ifndef STUBMPI
      INCLUDE 'mpif.h'

      CALL wrf_get_dm_communicator ( comm )
      dex(1) = idex ; dex(2) = jdex
      CALL mpi_allgather ( dex, 2, MPI_INTEGER, dex_all , 2, MPI_INTEGER, comm, ierr )
      CALL mpi_allgather ( val, 1, MPI_REAL, val_all , 1, MPI_REAL, comm, ierr )
      val = val_all(1)
      idex = dex_all(1,1) ; jdex = dex_all(2,1)
      DO i = 2, rsl_nproc
        IF ( val_all(i) .GT. val ) THEN
           val = val_all(i)
           idex = dex_all(1,i)
           jdex = dex_all(2,i)
        ENDIF
      ENDDO
#endif
   END SUBROUTINE wrf_dm_maxval



END MODULE module_dm

!=========================================================================
! wrf_dm_patch_domain has to be outside the module because it is called
! by a routine in module_domain but depends on module domain


SUBROUTINE wrf_dm_patch_domain ( id  , domdesc , parent_id , parent_domdesc , &
                          sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &
                          sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &
                          sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &
                                      sp1x , ep1x , sm1x , em1x , &
                                      sp2x , ep2x , sm2x , em2x , &
                                      sp3x , ep3x , sm3x , em3x , &
                                      sp1y , ep1y , sm1y , em1y , &
                                      sp2y , ep2y , sm2y , em2y , &
                                      sp3y , ep3y , sm3y , em3y , &
                          bdx , bdy )
   USE module_domain
   USE module_dm
   IMPLICIT NONE

   INTEGER, INTENT(IN)   :: sd1 , ed1 , sd2 , ed2 , sd3 , ed3 , bdx , bdy
   INTEGER, INTENT(OUT)  :: sp1 , ep1 , sp2 , ep2 , sp3 , ep3 , &
                            sm1 , em1 , sm2 , em2 , sm3 , em3
   INTEGER, INTENT(OUT)  :: sp1x , ep1x , sp2x , ep2x , sp3x , ep3x , &
                            sm1x , em1x , sm2x , em2x , sm3x , em3x
   INTEGER, INTENT(OUT)  :: sp1y , ep1y , sp2y , ep2y , sp3y , ep3y , &
                            sm1y , em1y , sm2y , em2y , sm3y , em3y
   INTEGER, INTENT(INOUT):: id  , domdesc , parent_id , parent_domdesc

   TYPE(domain), POINTER :: parent, grid_ptr

! <DESCRIPTION>
! The rsl-package supplied routine that computes the patch and memory dimensions
! for this task. See also <a href=patch_domain_rsl.html>patch_domain_rsl</a>
! 
! </DESCRIPTION>

   ! this is necessary because we cannot pass parent directly into 
   ! wrf_dm_patch_domain because creating the correct interface definitions
   ! would generate a circular USE reference between module_domain and module_dm
   ! see comment this date in module_domain for more information. JM 20020416

   NULLIFY( parent )
   grid_ptr => head_grid
   CALL find_grid_by_id( parent_id , grid_ptr , parent )

   CALL patch_domain_rsl ( id  , domdesc , parent, parent_id , parent_domdesc , & 
                           sd1 , ed1 , sp1 , ep1 , sm1 , em1 , & 
                           sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &
                           sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &
                                       sp1x , ep1x , sm1x , em1x , &
                                       sp2x , ep2x , sm2x , em2x , &
                                       sp3x , ep3x , sm3x , em3x , &
                                       sp1y , ep1y , sm1y , em1y , &
                                       sp2y , ep2y , sm2y , em2y , &
                                       sp3y , ep3y , sm3y , em3y , &
                           bdx , bdy )


   RETURN
END SUBROUTINE wrf_dm_patch_domain

SUBROUTINE wrf_termio_dup
  IMPLICIT NONE
  INTEGER mytask, ntasks, ierr
! <DESCRIPTION>
! Redirect standard output and standard error to separate files for each processor.
!
! </DESCRIPTION>
#ifndef STUBMPI
  INCLUDE 'mpif.h'
  CALL mpi_comm_size(MPI_COMM_WORLD, ntasks, ierr )
  CALL mpi_comm_rank(MPI_COMM_WORLD, mytask, ierr )
#else
  ntasks = 1
  mytask = 0
#endif
  write(0,*)'starting wrf task ',mytask,' of ',ntasks
  CALL rsl_error_dup1( mytask )
END SUBROUTINE wrf_termio_dup

SUBROUTINE wrf_get_myproc( myproc )
  IMPLICIT NONE
! <DESCRIPTION>
! Pass back the task number (usually MPI rank) on this process.
!
! </DESCRIPTION>
# include <rsl.inc>
  INTEGER myproc
  myproc = rsl_myproc
  RETURN
END SUBROUTINE wrf_get_myproc

SUBROUTINE wrf_get_nproc( nproc )
  IMPLICIT NONE
# include <rsl.inc>
  INTEGER nproc
! <DESCRIPTION>
! Pass back the number of distributed-memory tasks.
!
! </DESCRIPTION>
  nproc = rsl_nproc_all
  RETURN
END SUBROUTINE wrf_get_nproc

SUBROUTINE wrf_get_nprocx( nprocx )
  IMPLICIT NONE
# include <rsl.inc>
  INTEGER nprocx
! <DESCRIPTION>
! Pass back the number of distributed-memory tasks decomposing the X dimension of the domain.
!
! </DESCRIPTION>
  nprocx = rsl_nproc_min
  RETURN
END SUBROUTINE wrf_get_nprocx

SUBROUTINE wrf_get_nprocy( nprocy )
  IMPLICIT NONE
# include <rsl.inc>
  INTEGER nprocy
! <DESCRIPTION>
! Pass back the number of distributed-memory tasks decomposing the Y dimension of the domain.
!
! </DESCRIPTION>
  nprocy = rsl_nproc_maj
  RETURN
END SUBROUTINE wrf_get_nprocy

SUBROUTINE wrf_dm_bcast_bytes ( buf , size )
   USE module_dm
   IMPLICIT NONE
   INTEGER size
   INTEGER*1 BUF(size)
! <DESCRIPTION>
! Collective operation. Given a buffer and a size in bytes on task zero, broadcast and return that buffer on all tasks.
!
! </DESCRIPTION>
   CALL rsl_mon_bcast( buf , size )
   RETURN
END SUBROUTINE wrf_dm_bcast_bytes

SUBROUTINE wrf_dm_bcast_string( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
! <DESCRIPTION>
! Collective operation. Given a string and a size in characters on task zero, broadcast and return that buffer on all tasks.
!
! </DESCRIPTION>
   CHARACTER*(*) buf
   INTEGER ibuf(256),i,n
   CHARACTER*256 tstr
   n = n1
   IF (n .GT. 256) n = 256
   IF (n .GT. 0 ) then
     CALL rsl_mon_bcast( n , IWORDSIZE )
     DO i = 1, n
       ibuf(I) = ichar(buf(I:I))
     ENDDO
     CALL rsl_mon_bcast( ibuf, n*IWORDSIZE )
     buf = ''
     DO i = 1, n
       buf(i:i) = char(ibuf(i))
     ENDDO
   ENDIF
   RETURN
END SUBROUTINE wrf_dm_bcast_string

SUBROUTINE wrf_dm_bcast_integer( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
   INTEGER  buf(*)
! <DESCRIPTION>
! Collective operation. Given an array of integers and length on task zero, broadcast and return that array of values on all tasks.
!
! </DESCRIPTION>
   CALL rsl_mon_bcast( BUF , N1 * IWORDSIZE )
   RETURN
END SUBROUTINE wrf_dm_bcast_integer

SUBROUTINE wrf_dm_bcast_double( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
! <DESCRIPTION>
! Collective operation. Given an array of doubles and length on task zero, broadcast and return that array of values on all tasks.
!
! </DESCRIPTION>
   DOUBLEPRECISION  buf(*)
   CALL rsl_mon_bcast( BUF , N1 * DWORDSIZE )
   RETURN
END SUBROUTINE wrf_dm_bcast_double

SUBROUTINE wrf_dm_bcast_real( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
! <DESCRIPTION>
! Collective operation. Given an array of reals and length on task zero, broadcast and return that array of values on all tasks.
!
! </DESCRIPTION>
   REAL  buf(*)
   CALL rsl_mon_bcast( BUF , N1 * RWORDSIZE )
   RETURN
END SUBROUTINE wrf_dm_bcast_real

SUBROUTINE wrf_dm_bcast_logical( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
! <DESCRIPTION>
! Collective operation. Given an array of logicals and length on task zero, broadcast and return that array of values on all tasks.
!
! </DESCRIPTION>
   LOGICAL  buf(*)
   CALL rsl_mon_bcast( BUF , N1 * LWORDSIZE )
   RETURN
END SUBROUTINE wrf_dm_bcast_logical

SUBROUTINE wrf_dm_halo ( domdesc , comms , stencil_id )
   USE module_dm
   IMPLICIT NONE
   INTEGER domdesc , comms(*) , stencil_id
   CALL rsl_exch_stencil ( domdesc , comms( stencil_id ) )
   RETURN
END SUBROUTINE wrf_dm_halo

SUBROUTINE wrf_dm_xpose_z2y ( domdesc , comms , xpose_id )
   USE module_dm
   IMPLICIT NONE
   INTEGER domdesc , comms(*) , xpose_id
#ifdef D3VAR_IRY_KLUDGE
   CALL rsl_xpose_mn_nz ( domdesc , comms( xpose_id ) )
#else
   CALL rsl_xpose_mn_mz ( domdesc , comms( xpose_id ) )      ! switched nz->mz 20020910
#endif
   RETURN
END SUBROUTINE wrf_dm_xpose_z2y

SUBROUTINE wrf_dm_xpose_y2z ( domdesc , comms , xpose_id )
   USE module_dm
   IMPLICIT NONE
   INTEGER domdesc , comms(*) , xpose_id
#ifdef D3VAR_IRY_KLUDGE
   CALL rsl_xpose_nz_mn ( domdesc , comms( xpose_id ) )
#else
   CALL rsl_xpose_mz_mn ( domdesc , comms( xpose_id ) )      ! switched nz->mz 20020910
#endif
   RETURN
END SUBROUTINE wrf_dm_xpose_y2z

SUBROUTINE wrf_dm_xpose_y2x ( domdesc , comms , xpose_id )
   USE module_dm
   IMPLICIT NONE
   INTEGER domdesc , comms(*) , xpose_id
#ifdef D3VAR_IRY_KLUDGE
   CALL rsl_xpose_nz_mz ( domdesc , comms( xpose_id ) )
#else
   CALL rsl_xpose_mz_nz ( domdesc , comms( xpose_id ) )      ! switched nz<->mz 20020910
#endif
   RETURN
END SUBROUTINE wrf_dm_xpose_y2x

SUBROUTINE wrf_dm_xpose_x2y ( domdesc , comms , xpose_id )
   USE module_dm
   IMPLICIT NONE
   INTEGER domdesc , comms(*) , xpose_id
#ifdef D3VAR_IRY_KLUDGE
   CALL rsl_xpose_mz_nz ( domdesc , comms( xpose_id ) )
#else
   CALL rsl_xpose_nz_mz ( domdesc , comms( xpose_id ) )      ! switched nz<->mz 20020910
#endif
   RETURN
END SUBROUTINE wrf_dm_xpose_x2y

SUBROUTINE wrf_dm_xpose_x2z ( domdesc , comms , xpose_id )
   USE module_dm
   IMPLICIT NONE
   INTEGER domdesc , comms(*) , xpose_id
#ifdef D3VAR_IRY_KLUDGE
   CALL rsl_xpose_mz_mn ( domdesc , comms( xpose_id ) )
#else
   CALL rsl_xpose_nz_mn ( domdesc , comms( xpose_id ) )      ! switched mz->nz 20020910
#endif
   RETURN
END SUBROUTINE wrf_dm_xpose_x2z

SUBROUTINE wrf_dm_xpose_z2x ( domdesc , comms , xpose_id )
   USE module_dm
   IMPLICIT NONE
   INTEGER domdesc , comms(*) , xpose_id
#ifdef D3VAR_IRY_KLUDGE
   CALL rsl_xpose_mn_mz ( domdesc , comms( xpose_id ) )
#else
   CALL rsl_xpose_mn_nz ( domdesc , comms( xpose_id ) )      ! switched mz->nz 20020910
#endif
   RETURN
END SUBROUTINE wrf_dm_xpose_z2x

#if 0
SUBROUTINE wrf_dm_boundary ( domdesc , comms , period_id , &
                             periodic_x , periodic_y )
   USE module_dm
   IMPLICIT NONE
   INTEGER domdesc , comms(*) , period_id
   LOGICAL , INTENT(IN)      :: periodic_x, periodic_y
# include <rsl.inc>

   IF ( periodic_x ) THEN
     CALL rsl_exch_period ( domdesc , comms( period_id ) , RSL_M )
   END IF
   IF ( periodic_y ) THEN
     CALL rsl_exch_period ( domdesc , comms( period_id ) , RSL_N )
   END IF
   RETURN
END SUBROUTINE wrf_dm_boundary
#endif

SUBROUTINE wrf_dm_define_comms ( grid )
   USE module_domain
   USE module_dm
   IMPLICIT NONE
   TYPE(domain) , INTENT (INOUT) :: grid 
   INTEGER dyn_opt
   INTEGER idum1, idum2, icomm

#ifdef DEREF_KLUDGE
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
#endif

#include "deref_kludge.h"

   CALL get_dyn_opt( dyn_opt )

   CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )

! rsl interface has been restructured so there is no longer a 
! need to call a dyncore specific define_comms routine here.
! Removed 6/2001. JM

   DO icomm = 1, max_comms
     grid%comms(icomm) = invalid_message_value
   ENDDO
   grid%shift_x = invalid_message_value
   grid%shift_y = invalid_message_value

   RETURN
END SUBROUTINE wrf_dm_define_comms

SUBROUTINE write_68( grid, v , s , &
                   ids, ide, jds, jde, kds, kde, &
                   ims, ime, jms, jme, kms, kme, &
                   its, ite, jts, jte, kts, kte )
  USE module_domain
  IMPLICIT NONE
  TYPE(domain) , INTENT (INOUT) :: grid 
  CHARACTER *(*) s
  INTEGER ids, ide, jds, jde, kds, kde, &
          ims, ime, jms, jme, kms, kme, &
          its, ite, jts, jte, kts, kte
  REAL, DIMENSION( ims:ime , kms:kme, jms:jme ) :: v
# include <rsl.inc>

  INTEGER i,j,k

  logical, external :: wrf_dm_on_monitor
  real globbuf( ids:ide, kds:kde, jds:jde )
  character*3 ord, stag

  if ( kds == kde ) then
    ord = 'xy'
    stag = 'xy'
  CALL wrf_patch_to_global_real ( v, globbuf, grid%domdesc, stag, ord, &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     its, ite, jts, jte, kts, kte )
  else

    stag = 'xyz' 
    ord = 'xzy'
  CALL wrf_patch_to_global_real ( v, globbuf, grid%domdesc, stag, ord, &
                     ids, ide, kds, kde, jds, jde, &
                     ims, ime, kms, kme, jms, jme, &
                     its, ite, kts, kte, jts, jte )
  endif


  if ( wrf_dm_on_monitor() ) THEN
    WRITE(68,*) ide-ids+1, jde-jds+1 , s
    DO j = jds, jde
    DO i = ids, ide
       WRITE(68,*) globbuf(i,1,j)
    ENDDO
    ENDDO
  endif

  RETURN
END

   SUBROUTINE wrf_abort
! <DESCRIPTION>
! Kill the run. Calls MPI_ABORT.
!
! </DESCRIPTION>
#ifndef STUBMPI
      INCLUDE 'mpif.h'
      CALL mpi_abort(MPI_COMM_WORLD,1,ierr)
#else
      STOP
#endif
   END SUBROUTINE wrf_abort

   SUBROUTINE wrf_dm_shutdown
# include <rsl.inc>
! <DESCRIPTION>
! Shutdown (gracefully) the underlying comm layer.
!
! </DESCRIPTION>
      CALL RSL_SHUTDOWN
      RETURN
   END SUBROUTINE wrf_dm_shutdown

   LOGICAL FUNCTION wrf_dm_on_monitor()
      LOGICAL rsl_iammonitor
      EXTERNAL rsl_iammonitor
! <DESCRIPTION>
! Return true on task zero, false otherwise.
!
! </DESCRIPTION>
      wrf_dm_on_monitor = rsl_iammonitor()
      RETURN
   END FUNCTION wrf_dm_on_monitor

   SUBROUTINE wrf_get_dm_communicator ( communicator )
      IMPLICIT NONE
      INTEGER , INTENT(OUT) :: communicator
! <DESCRIPTION>
! Return the communicator the underlying comm layer is using.
!
! </DESCRIPTION>
      CALL rsl_get_communicator ( communicator )
      RETURN
   END SUBROUTINE wrf_get_dm_communicator

   SUBROUTINE wrf_get_dm_iocommunicator ( iocommunicator )
      IMPLICIT NONE
      INTEGER , INTENT(OUT) :: iocommunicator
! <DESCRIPTION>
! Return the io communicator the underlying comm layer is using.  Not used.
!
! </DESCRIPTION>
      CALL rsl_get_communicator ( iocommunicator )  ! same as regular communicator
      RETURN
   END SUBROUTINE wrf_get_dm_iocommunicator

   SUBROUTINE wrf_set_dm_communicator ( communicator )
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: communicator
! <DESCRIPTION>
! Set the communicator the underlying comm layer is to use.
!
! </DESCRIPTION>
      CALL rsl_set_communicator ( communicator )
      RETURN
   END SUBROUTINE wrf_set_dm_communicator

   SUBROUTINE wrf_set_dm_iocommunicator ( iocommunicator )
      IMPLICIT NONE
      INTEGER , INTENT(IN) :: iocommunicator
! <DESCRIPTION>
! Set the io communicator the underlying comm layer is to use. Not used.
!
! </DESCRIPTION>
!      CALL rsl_set_communicator ( iocommunicator )  ! same as regular communicator
      RETURN
   END SUBROUTINE wrf_set_dm_iocommunicator


!!!!!!!!!!!!!!!!!!!!!!! PATCH TO GLOBAL !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE wrf_patch_to_global_real (buf,globbuf,domdesc,stagger,ordering,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       IMPLICIT NONE
#include <rsl.inc>
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       CHARACTER *(*) stagger,ordering
       INTEGER fid,domdesc
       REAL globbuf(*)
       REAL buf(*)
! <DESCRIPTION>
! Collective operation. Given a buffer of type real corresponding to a 2- or 3-dimensional patch on a local processor,
! return on task zero the global array assembled from the pieces stored on each processor.
! 
! </DESCRIPTION>

       CALL wrf_patch_to_global_generic (buf,globbuf,domdesc,stagger,ordering,TRUE_RSL_REAL,&
                                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                         MS1,ME1,MS2,ME2,MS3,ME3,&
                                         PS1,PE1,PS2,PE2,PS3,PE3 )

       RETURN
   END SUBROUTINE wrf_patch_to_global_real 

#ifndef PEDANTIC
   SUBROUTINE wrf_patch_to_global_doubleprecision (buf,globbuf,domdesc,stagger,ordering,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       IMPLICIT NONE
#include <rsl.inc>
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       CHARACTER *(*) stagger,ordering
       INTEGER fid,domdesc
       DOUBLEPRECISION globbuf(*)
       DOUBLEPRECISION buf(*)
! <DESCRIPTION>
! Collective operation. Given a buffer of type double corresponding to a 2- or 3-dimensional patch on a local processor,
! return on task zero the global array assembled from the pieces stored on each processor.
! 
! </DESCRIPTION>

       CALL wrf_patch_to_global_generic (buf,globbuf,domdesc,stagger,ordering,RSL_DOUBLE,&
                                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                         MS1,ME1,MS2,ME2,MS3,ME3,&
                                         PS1,PE1,PS2,PE2,PS3,PE3 )

       RETURN
   END SUBROUTINE wrf_patch_to_global_doubleprecision
#endif


   SUBROUTINE wrf_patch_to_global_integer (buf,globbuf,domdesc,stagger,ordering,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       IMPLICIT NONE
#include <rsl.inc>
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       CHARACTER *(*) stagger,ordering
       INTEGER fid,domdesc
       INTEGER globbuf(*)
       INTEGER buf(*)
! <DESCRIPTION>
! Collective operation. Given a buffer of type integer corresponding to a 2- or 3-dimensional patch on a local processor,
! return on task zero the global array assembled from the pieces stored on each processor.
! 
! </DESCRIPTION>

       CALL wrf_patch_to_global_generic (buf,globbuf,domdesc,stagger,ordering,rsl_integer,&
                                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                         MS1,ME1,MS2,ME2,MS3,ME3,&
                                         PS1,PE1,PS2,PE2,PS3,PE3 )

       RETURN
   END SUBROUTINE wrf_patch_to_global_integer 

   SUBROUTINE wrf_patch_to_global_generic (buf,globbuf,domdesc,stagger,ordering,type,&
                                       DS1a,DE1a,DS2a,DE2a,DS3a,DE3a,&
                                       MS1a,ME1a,MS2a,ME2a,MS3a,ME3a,&
                                       PS1a,PE1a,PS2a,PE2a,PS3a,PE3a )
       USE module_driver_constants
       USE module_timing
       USE module_wrf_error
       IMPLICIT NONE
#include <rsl.inc>
       INTEGER                         DS1a,DE1a,DS2a,DE2a,DS3a,DE3a,&
                                       MS1a,ME1a,MS2a,ME2a,MS3a,ME3a,&
                                       PS1a,PE1a,PS2a,PE2a,PS3a,PE3A 
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       CHARACTER *(*) stagger,ordering
       INTEGER fid,domdesc,type
       REAL globbuf(*)
       REAL buf(*)

       LOGICAL, EXTERNAL :: has_char
       INTEGER glen(3),llen(3),glen2d(3),llen2d(3)
       INTEGER i, j, k, ord, ord2d, ndim
       INTEGER mlen, nlen, zlen

       DS1 = DS1a ; DE1 = DE1a ; DS2=DS2a ; DE2 = DE2a ; DS3 = DS3a ; DE3 = DE3a
       MS1 = MS1a ; ME1 = ME1a ; MS2=MS2a ; ME2 = ME2a ; MS3 = MS3a ; ME3 = ME3a
       PS1 = PS1a ; PE1 = PE1a ; PS2=PS2a ; PE2 = PE2a ; PS3 = PS3a ; PE3 = PE3a

       ndim = len(TRIM(ordering))

       CALL rsl_get_glen( domdesc, glen(1), glen(2), glen(3) )

       SELECT CASE ( TRIM(ordering) )
         CASE ( 'xyz','xy' )
           ord = io3d_ijk_internal ; ord2d = io2d_ij_internal
            ! the non-staggered variables come in at one-less than
            ! domain dimensions, but RSL wants full domain spec, so 
            ! adjust if not staggered
           IF ( .NOT. has_char( stagger, 'x' ) ) DE1 = DE1+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE2 = DE2+1
           IF ( .NOT. has_char( stagger, 'z' ) ) DE3 = DE3+1
         CASE ( 'yxz','yx' )
#ifndef D3VAR_IRY_KLUDGE
           ord = io3d_jik_internal ; ord2d = io2d_ji_internal
           IF ( .NOT. has_char( stagger, 'x' ) ) DE2 = DE2+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE1 = DE1+1
           IF ( .NOT. has_char( stagger, 'z' ) ) DE3 = DE3+1
#else
           ord = io3d_ijk_internal ; ord2d = io2d_ij_internal
           IF ( .NOT. has_char( stagger, 'x' ) ) DE2 = DE2+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE1 = DE1+1
           IF ( .NOT. has_char( stagger, 'z' ) ) DE3 = DE3+1
#endif
         CASE ( 'zxy' )
           IF ( .NOT. has_char( stagger, 'x' ) ) DE2 = DE2+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE3 = DE3+1
           IF ( .NOT. has_char( stagger, 'z' ) ) DE1 = DE1+1
           ord = io3d_kij_internal ; ord2d = io2d_ij_internal
#if 0
         CASE ( 'zyx' )
           ord = io3d_kji_internal ; ord2d = io2d_ji_internal
         CASE ( 'yzx' )
           ord = io3d_jki_internal ; ord2d = io2d_ji_internal
#endif
         CASE ( 'xzy' )
           IF ( .NOT. has_char( stagger, 'x' ) ) DE1 = DE1+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE3 = DE3+1
           IF ( .NOT. has_char( stagger, 'z' ) ) DE2 = DE2+1
           ord = io3d_ikj_internal ; ord2d = io2d_ij_internal
         CASE DEFAULT
           ord = -1 ; ord2d = -1
       END SELECT


       glen(1) = DE1-DS1+1   ; glen(2) = DE2-DS2+1   ; glen(3) = DE3-DS3+1
       llen(1) = ME1-MS1+1   ; llen(2) = ME2-MS2+1   ; llen(3) = ME3-MS3+1
       glen2d(1) = DE1-DS1+1 ; glen2d(2) = DE2-DS2+1
       llen2d(1) = ME1-MS1+1 ; llen2d(2) = ME2-MS2+1

       IF ( wrf_at_debug_level(500) ) THEN
         CALL start_timing
       ENDIF

       IF ( ndim .EQ. 3 ) THEN
         CALL rsl_write(globbuf,ord,buf,domdesc,type,glen,llen)
       ELSE
         CALL rsl_write(globbuf,ord2d,buf,domdesc,type,glen2d,llen2d)
       ENDIF
       IF ( wrf_at_debug_level(500) ) THEN
         CALL end_timing('wrf_patch_to_global_generic')
       ENDIF
       RETURN
    END SUBROUTINE wrf_patch_to_global_generic

!!!!!!!!!!!!!!!!!!!!!!! GLOBAL TO PATCH !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE wrf_global_to_patch_real (globbuf,buf,domdesc,stagger,ordering,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       IMPLICIT NONE
#include <rsl.inc>
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       CHARACTER *(*) stagger,ordering
       INTEGER fid,domdesc
       REAL globbuf(*)
       REAL buf(*)
! <DESCRIPTION>
! Collective operation. Given a global 2- or 3-dimensional array of type real on task zero,
! return the appropriate decomposed section (patch) on each processor.
! 
! </DESCRIPTION>

       CALL wrf_global_to_patch_generic (globbuf,buf,domdesc,stagger,ordering,TRUE_RSL_REAL,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       RETURN
    END SUBROUTINE wrf_global_to_patch_real

#ifndef PEDANTIC
    SUBROUTINE wrf_global_to_patch_doubleprecision (globbuf,buf,domdesc,stagger,ordering,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       IMPLICIT NONE
#include <rsl.inc>
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       CHARACTER *(*) stagger,ordering
       INTEGER fid,domdesc
       DOUBLEPRECISION globbuf(*)
       DOUBLEPRECISION buf(*)
! <DESCRIPTION>
! Collective operation. Given a global 2- or 3-dimensional array of type double on task zero,
! return the appropriate decomposed section (patch) on each processor.
! 
! </DESCRIPTION>

       CALL wrf_global_to_patch_generic (globbuf,buf,domdesc,stagger,ordering,RSL_DOUBLE,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       RETURN
    END SUBROUTINE wrf_global_to_patch_doubleprecision
#endif


    SUBROUTINE wrf_global_to_patch_integer (globbuf,buf,domdesc,stagger,ordering,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       IMPLICIT NONE
#include <rsl.inc>
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       CHARACTER *(*) stagger,ordering
       INTEGER fid,domdesc
       INTEGER globbuf(*)
       INTEGER buf(*)
! <DESCRIPTION>
! Collective operation. Given a global 2- or 3-dimensional array of type integer on task zero,
! return the appropriate decomposed section (patch) on each processor.
! 
! </DESCRIPTION>

       CALL wrf_global_to_patch_generic (globbuf,buf,domdesc,stagger,ordering,rsl_integer,&
                                       DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3 )
       RETURN
    END SUBROUTINE wrf_global_to_patch_integer

    SUBROUTINE wrf_global_to_patch_generic (globbuf,buf,domdesc,stagger,ordering,type,&
                                       DS1a,DE1a,DS2a,DE2a,DS3a,DE3a,&
                                       MS1a,ME1a,MS2a,ME2a,MS3a,ME3a,&
                                       PS1a,PE1a,PS2a,PE2a,PS3a,PE3a )
       USE module_driver_constants
       IMPLICIT NONE
#include <rsl.inc>
       INTEGER                         DS1a,DE1a,DS2a,DE2a,DS3a,DE3a,&
                                       MS1a,ME1a,MS2a,ME2a,MS3a,ME3a,&
                                       PS1a,PE1a,PS2a,PE2a,PS3a,PE3A 
       INTEGER                         DS1,DE1,DS2,DE2,DS3,DE3,&
                                       MS1,ME1,MS2,ME2,MS3,ME3,&
                                       PS1,PE1,PS2,PE2,PS3,PE3
       CHARACTER *(*) stagger,ordering
       INTEGER fid,domdesc,type
       REAL globbuf(*)
       REAL buf(*)
       LOGICAL, EXTERNAL :: has_char

       INTEGER i,j,k,ord,ord2d,ndim
       INTEGER glen(3),llen(3),glen2d(3),llen2d(3)

       DS1 = DS1a ; DE1 = DE1a ; DS2=DS2a ; DE2 = DE2a ; DS3 = DS3a ; DE3 = DE3a
       MS1 = MS1a ; ME1 = ME1a ; MS2=MS2a ; ME2 = ME2a ; MS3 = MS3a ; ME3 = ME3a
       PS1 = PS1a ; PE1 = PE1a ; PS2=PS2a ; PE2 = PE2a ; PS3 = PS3a ; PE3 = PE3a

       ndim = len(TRIM(ordering))

       SELECT CASE ( TRIM(ordering) )
         CASE ( 'xyz','xy' )
           ord = io3d_ijk_internal ; ord2d = io2d_ij_internal
            ! the non-staggered variables come in at one-less than
            ! domain dimensions, but RSL wants full domain spec, so 
            ! adjust if not staggered
           IF ( .NOT. has_char( stagger, 'x' ) ) DE1 = DE1+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE2 = DE2+1
           IF ( .NOT. has_char( stagger, 'z' ) ) DE3 = DE3+1
         CASE ( 'yxz','yx' )
#ifndef D3VAR_IRY_KLUDGE
           ord = io3d_jik_internal ; ord2d = io2d_ji_internal
           IF ( .NOT. has_char( stagger, 'x' ) ) DE2 = DE2+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE1 = DE1+1
           IF ( .NOT. has_char( stagger, 'z' ) ) DE3 = DE3+1
#else
           ord = io3d_ijk_internal ; ord2d = io2d_ij_internal
           IF ( .NOT. has_char( stagger, 'x' ) ) DE2 = DE2+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE1 = DE1+1
           IF ( .NOT. has_char( stagger, 'z' ) ) DE3 = DE3+1
#endif
         CASE ( 'zxy' )
           ord = io3d_kij_internal ; ord2d = io2d_ij_internal
           IF ( .NOT. has_char( stagger, 'x' ) ) DE2 = DE2+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE3 = DE3+1
           IF ( .NOT. has_char( stagger, 'z' ) ) DE1 = DE1+1
#if 0
         CASE ( 'zyx' )
           ord = io3d_kji_internal ; ord2d = io2d_ji_internal
         CASE ( 'yzx' )
           ord = io3d_jki_internal ; ord2d = io2d_ji_internal
#endif
         CASE ( 'xzy' )
           ord = io3d_ikj_internal ; ord2d = io2d_ij_internal
           IF ( .NOT. has_char( stagger, 'x' ) ) DE1 = DE1+1
           IF ( .NOT. has_char( stagger, 'y' ) ) DE3 = DE3+1
           IF ( .NOT. has_char( stagger, 'z' ) ) DE2 = DE2+1
         CASE DEFAULT
           ord = -1 ; ord2d = -1
       END SELECT

       glen(1) = DE1-DS1+1   ; glen(2) = DE2-DS2+1   ; glen(3) = DE3-DS3+1
       llen(1) = ME1-MS1+1   ; llen(2) = ME2-MS2+1   ; llen(3) = ME3-MS3+1
       glen2d(1) = DE1-DS1+1 ; glen2d(2) = DE2-DS2+1
       llen2d(1) = ME1-MS1+1 ; llen2d(2) = ME2-MS2+1

       IF ( ndim .EQ. 3 ) THEN
         CALL rsl_read(globbuf,ord,buf,domdesc,type,glen,llen)
       ELSE
         CALL rsl_read(globbuf,ord2d,buf,domdesc,type,glen2d,llen2d)
       ENDIF
       RETURN
    END SUBROUTINE wrf_global_to_patch_generic


!------------------------------------------------------------------

#if ( EM_CORE == 1 )

!------------------------------------------------------------------

   SUBROUTINE force_domain_em_part2 ( grid, ngrid, config_flags ,  &
!
#include "em_dummy_args.inc"
!
                 )
      USE module_domain
      USE module_configure
      USE module_dm
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
#include <em_dummy_decl.inc>
#include <em_i1_decl.inc>
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(500)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
! <DESCRIPTION>
! Description is to do...
! </DESCRIPTION>
#define COPY_IN
#include <em_scalar_derefs.inc>
#ifdef DM_PARALLEL
#    define REGISTER_I1
#      include <em_data_calls.inc>
#endif

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1

#  include "em_nest_interpdown_unpack.inc"

#include "HALO_EM_FORCE_DOWN.inc"

      ! code here to interpolate the data into the nested domain
#  include "em_nest_forcedown_interp.inc"

#define COPY_OUT
#include <em_scalar_derefs.inc>
      RETURN
   END SUBROUTINE force_domain_em_part2



!------------------------------------------------------------------

   SUBROUTINE interp_domain_em_part1 ( grid, intermediate_grid, config_flags ,  &
!
#include "em_dummy_args.inc"
!
                 )
      USE module_domain
      USE module_configure
      USE module_dm
      USE module_timing
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: intermediate_grid
#include <em_dummy_decl.inc>
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(500)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
#define COPY_IN
#include <em_scalar_derefs.inc>
!
      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  intermediate_grid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1

#  include "em_nest_interpdown_pack.inc"

      CALL rsl_bcast_msgs

#define COPY_OUT
#include <em_scalar_derefs.inc>
      RETURN
   END SUBROUTINE interp_domain_em_part1

   SUBROUTINE interp_domain_em_part2 ( grid, ngrid, config_flags ,  &
!
#include "em_dummy_args.inc"
!
                 )
      USE module_domain
      USE module_configure
      USE module_dm
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
#include <em_dummy_decl.inc>
#include <em_i1_decl.inc>
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(500)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
#define COPY_IN
#include <em_scalar_derefs.inc>
#ifdef DM_PARALLEL
#    define REGISTER_I1
#      include <em_data_calls.inc>
#endif
      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1 

#  include "em_nest_interpdown_unpack.inc"

#include "HALO_EM_INTERP_DOWN.inc"
      ! code here to interpolate the data into the nested domain

#  include "em_nest_interpdown_interp.inc"

#define COPY_OUT
#include <em_scalar_derefs.inc>
      RETURN
   END SUBROUTINE interp_domain_em_part2

!------------------------------------------------------------------
! This routine exists only to call a halo on a domain (the nest)
! gets called from feedback_domain_em_part1, below.  This is needed
! because the halo code expects the fields being exchanged to have
! been dereferenced from the grid data structure, but in feedback_domain_em_part1
! the grid data structure points to the coarse domain, not the nest.
! And we want the halo exchange on the nest, so that the code in 
! em_nest_feedbackup_interp.inc will work correctly on multi-p. JM 20040308
!
   SUBROUTINE feedback_nest_prep ( grid, config_flags ,  &
!
#include "em_dummy_args.inc"
!
)
      USE module_domain
      USE module_configure
      USE module_dm
      USE module_state_description
!
      TYPE(domain), TARGET :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE (grid_config_rec_type) :: config_flags ! configureation flags, has vertical dim of 
                                                  ! soil temp, moisture, etc., has vertical dim
                                                  ! of soil categories
#include <em_dummy_decl.inc>

#define COPY_IN
#include <em_scalar_derefs.inc>

#ifdef DM_PARALLEL
#      include <em_data_calls.inc>
#endif

#ifdef DM_PARALLEL
# include "HALO_EM_INTERP_UP.inc"
#endif

#define COPY_OUT
#include <em_scalar_derefs.inc>

   END SUBROUTINE feedback_nest_prep

   SUBROUTINE feedback_domain_em_part1 ( grid, ngrid, config_flags ,  &
!
#include "em_dummy_args.inc"
!
                 )
      USE module_domain
      USE module_configure
      USE module_dm
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
#include <em_dummy_decl.inc>
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE(domain), POINTER :: xgrid
      TYPE (grid_config_rec_type)            :: config_flags, nconfig_flags
      REAL xv(500)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe

      INTERFACE 
          SUBROUTINE feedback_nest_prep ( grid, config_flags ,  &
!
#include "em_dummy_args.inc"
!
)
             USE module_domain
             USE module_configure
             USE module_dm
             USE module_state_description
!
             TYPE (grid_config_rec_type)            :: config_flags
             TYPE(domain), TARGET                   :: grid
#include <em_dummy_decl.inc>
          END SUBROUTINE feedback_nest_prep

      END INTERFACE

#ifdef DEREF_KLUDGE
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
#endif

#include "deref_kludge.h"

#define COPY_IN
#include <em_scalar_derefs.inc>

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  ngrid ,                  &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1

      ips_save = ngrid%i_parent_start   ! used in feedback_domain_em_part2 below
      jps_save = ngrid%j_parent_start
      ipe_save = ngrid%i_parent_start + (nide-nids+1) / ngrid%parent_grid_ratio - 1
      jpe_save = ngrid%j_parent_start + (njde-njds+1) / ngrid%parent_grid_ratio - 1

      CALL model_to_grid_config_rec ( ngrid%id , model_config_rec , nconfig_flags )
      CALL set_scalar_indices_from_config ( ngrid%id , idum1 , idum2 )

      xgrid => grid
      grid => ngrid 

#include "deref_kludge.h"
      CALL feedback_nest_prep ( grid, nconfig_flags ,  &
!
#include "em_actual_args.inc"
!
)

      grid => xgrid
      CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )

#  include "em_nest_feedbackup_interp.inc"

#define COPY_OUT
#include <em_scalar_derefs.inc>
      RETURN
   END SUBROUTINE feedback_domain_em_part1

!------------------------------------------------------------------

   SUBROUTINE feedback_domain_em_part2 ( grid, intermediate_grid, ngrid , config_flags ,  &
!
#include "em_dummy_args.inc"
!
                 )
      USE module_domain
      USE module_configure
      USE module_dm
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: intermediate_grid
      TYPE(domain), POINTER :: ngrid
#include <em_dummy_decl.inc>
#include <em_i1_decl.inc>
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(500)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      LOGICAL, EXTERNAL  :: em_cd_feedback_mask
#define COPY_IN
#include <em_scalar_derefs.inc>
#ifdef DM_PARALLEL
#    define REGISTER_I1
#      include <em_data_calls.inc>
#endif

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  intermediate_grid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1

#  include "em_nest_feedbackup_pack.inc"

      CALL rsl_merge_msgs

#  include "em_nest_feedbackup_unpack.inc"

      ! smooth coarse grid 

      CALL get_ijk_from_grid (  ngrid,                           &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

#  include "HALO_EM_INTERP_UP.inc"
#  include "em_nest_feedbackup_smooth.inc"

#define COPY_OUT
#include <em_scalar_derefs.inc>
      RETURN
   END SUBROUTINE feedback_domain_em_part2

#endif

!------------------------------------------------------------------

#ifndef STUBMPI
   SUBROUTINE wrf_gatherv_real (Field, field_ofst,            &
                                my_count ,                    &    ! sendcount
                                globbuf, glob_ofst ,          &    ! recvbuf
                                counts                      , &    ! recvcounts
                                displs                      , &    ! displs
                                root                        , &    ! root
                                communicator                , &    ! communicator
                                ierr )
   IMPLICIT NONE
   INCLUDE 'mpif.h'
   INTEGER field_ofst, glob_ofst
   INTEGER my_count, communicator, root, ierr
   INTEGER , DIMENSION(*) :: counts, displs
   REAL, DIMENSION(*) :: Field, globbuf

           CALL mpi_gatherv( Field( field_ofst ),      &    ! sendbuf
                            my_count ,                       &    ! sendcount
                            MPI_REAL         ,               &    ! sendtype
                            globbuf( glob_ofst ) ,                 &    ! recvbuf
                            counts                         , &    ! recvcounts
                            displs                         , &    ! displs
                            MPI_REAL                       , &    ! recvtype
                            root                           , &    ! root
                            communicator                   , &    ! communicator
                            ierr )

   END SUBROUTINE wrf_gatherv_real

   SUBROUTINE wrf_gatherv_integer (Field, field_ofst,            &
                                my_count ,                    &    ! sendcount
                                globbuf, glob_ofst ,          &    ! recvbuf
                                counts                      , &    ! recvcounts
                                displs                      , &    ! displs
                                root                        , &    ! root
                                communicator                , &    ! communicator
                                ierr )
   IMPLICIT NONE
   INCLUDE 'mpif.h'
   INTEGER field_ofst, glob_ofst
   INTEGER my_count, communicator, root, ierr
   INTEGER , DIMENSION(*) :: counts, displs
   INTEGER, DIMENSION(*) :: Field, globbuf

           CALL mpi_gatherv( Field( field_ofst ),      &    ! sendbuf
                            my_count ,                       &    ! sendcount
                            MPI_INTEGER         ,               &    ! sendtype
                            globbuf( glob_ofst ) ,                 &    ! recvbuf
                            counts                         , &    ! recvcounts
                            displs                         , &    ! displs
                            MPI_INTEGER                       , &    ! recvtype
                            root                           , &    ! root
                            communicator                   , &    ! communicator
                            ierr )

   END SUBROUTINE wrf_gatherv_integer

#endif
