! to compile this
!
! g95
! gcc -c -DF2CSTYLE task_for_point.c ; g95 -ffree-form -ffree-line-length-huge tfp_tester.F task_for_point.o
! ifort
! icc -c task_for_point.c ; ifort -FR tfp_tester.F task_for_point.o
! ibm
! cc -c -DNOUNDERSCORE task_for_point.c ; xlf -qfree=f90 tfp_tester.F task_for_point.o

MODULE module_driver_constants

   !  0. The following tells the rest of the model what data ordering we are
   !     using

   INTEGER , PARAMETER :: DATA_ORDER_XYZ = 1
   INTEGER , PARAMETER :: DATA_ORDER_YXZ = 2
   INTEGER , PARAMETER :: DATA_ORDER_ZXY = 3
   INTEGER , PARAMETER :: DATA_ORDER_ZYX = 4
   INTEGER , PARAMETER :: DATA_ORDER_XZY = 5
   INTEGER , PARAMETER :: DATA_ORDER_YZX = 6
   INTEGER , PARAMETER :: DATA_ORDER_XY = DATA_ORDER_XYZ
   INTEGER , PARAMETER :: DATA_ORDER_YX = DATA_ORDER_YXZ

!#include "model_data_order.inc"

   !  1. Following are constants for use in defining maximal values for array
   !     definitions.  
   !

   !  The maximum number of levels in the model is how deeply the domains may
   !  be nested.

   INTEGER , PARAMETER :: max_levels      =  20

   !  The maximum number of nests that can depend on a single parent and other way round

   INTEGER , PARAMETER :: max_nests        =  20

   !  The maximum number of parents that a nest can have (simplified assumption -> one only)

   INTEGER , PARAMETER :: max_parents      =  1

   !  The maximum number of domains is how many grids the model will be running.

#define MAX_DOMAINS_F 10
   INTEGER , PARAMETER :: max_domains     =   ( MAX_DOMAINS_F - 1 ) / 2 + 1

   !  The maximum number of nest move specifications allowed in a namelist

   INTEGER , PARAMETER :: max_moves       =   50

   !  The maximum number of eta levels

   INTEGER , PARAMETER :: max_eta         =   501

   !  The maximum number of outer iterations (for DA minimisation)

   INTEGER , PARAMETER :: max_outer_iterations = 10

   !  The maximum number of instruments (for radiance DA)

   INTEGER , PARAMETER :: max_instruments =   30

   !  2. Following related to driver leve data structures for DM_PARALLEL communications

#ifdef DM_PARALLEL
   INTEGER , PARAMETER :: max_comms       =   1024
#else
   INTEGER , PARAMETER :: max_comms       =   1
#endif

   !  3. Following is information related to the file I/O.

   !  These are the bounds of the available FORTRAN logical unit numbers for the file I/O.
   !  Only logical unti numbers within these bounds will be chosen for I/O unit numbers.

   INTEGER , PARAMETER :: min_file_unit = 10
   INTEGER , PARAMETER :: max_file_unit = 99

   !  4. Unfortunately, the following definition is needed here (rather
   !     than the more logical place in share/module_model_constants.F)
   !     for the namelist reads in frame/module_configure.F, and for some
   !     conversions in share/set_timekeeping.F
   !     Actually, using it here will mean that we don't need to set it
   !     in share/module_model_constants.F, since this file will be
   !     included (USEd) in:
   !        frame/module_configure.F
   !     which will be USEd in:
   !        share/module_bc.F
   !     which will be USEd in:
   !        phys/module_radiation_driver.F
   !     which is the other important place for it to be, and where
   !     it is passed as a subroutine parameter to any physics subroutine.
   !
   !     P2SI is the number of SI seconds in an planetary solar day
   !     divided by the number of SI seconds in an earth solar day
#if defined MARS
   !     For Mars, P2SI = 88775.2/86400.
   REAL , PARAMETER :: P2SI = 1.0274907
#elif defined TITAN
   !     For Titan, P2SI = 1378080.0/86400.
   REAL , PARAMETER :: P2SI = 15.95
#else
   !     Default for Earth
   REAL , PARAMETER :: P2SI = 1.0
#endif
 CONTAINS
   SUBROUTINE init_module_driver_constants
   END SUBROUTINE init_module_driver_constants
END MODULE module_driver_constants

MODULE module_machine

   USE module_driver_constants

   !  Machine characteristics and utilities here.

   ! Tile strategy defined constants
   INTEGER, PARAMETER :: TILE_X = 1, TILE_Y = 2, TILE_XY = 3

   TYPE machine_type
      INTEGER                       :: tile_strategy
   END TYPE machine_type

   TYPE (machine_type) machine_info

   CONTAINS

   RECURSIVE SUBROUTINE rlocproc(p,maxi,nproc,ml,mr,ret)
   IMPLICIT NONE
   INTEGER, INTENT(IN)  :: p, maxi, nproc, ml, mr
   INTEGER, INTENT(OUT) :: ret
   INTEGER              :: width, rem, ret2, bl, br, mid, adjust, &
                           p_r, maxi_r, nproc_r, zero
   adjust = 0
   rem = mod( maxi, nproc )
   width = maxi / nproc
   mid = maxi / 2
   IF ( rem>0 .AND. (((mod(rem,2).EQ.0).OR.(rem.GT.2)).OR.(p.LE.mid))) THEN
     width = width + 1
   END IF
   IF ( p.LE.mid .AND. mod(rem,2).NE.0 ) THEN
     adjust = adjust + 1
   END IF
   bl = max(width,ml) ;
   br = max(width,mr) ;
   IF      (p<bl) THEN
     ret = 0
   ELSE IF (p>maxi-br-1) THEN
     ret = nproc-1
   ELSE
     p_r = p - bl
     maxi_r = maxi-bl-br+adjust
     nproc_r = max(nproc-2,1)
     zero = 0
     CALL rlocproc( p_r, maxi_r, nproc_r, zero, zero, ret2 )  ! Recursive
     ret = ret2 + 1
   END IF
   RETURN
   END SUBROUTINE rlocproc

   INTEGER FUNCTION locproc( i, m, numpart )
   implicit none
   integer, intent(in) :: i, m, numpart 
   integer             :: retval, ii, im, inumpart, zero
   ii = i
   im = m
   inumpart = numpart
   zero = 0
   CALL rlocproc( ii, im, inumpart, zero, zero, retval )
   locproc = retval
   RETURN
   END FUNCTION locproc

   SUBROUTINE patchmap( res, y, x, py, px )
   implicit none
   INTEGER, INTENT(IN)                    :: y, x, py, px
   INTEGER, DIMENSION(x,y), INTENT(OUT)   :: res
   INTEGER                                :: i, j, p_min, p_maj
   DO j = 0,y-1
     p_maj = locproc( j, y, py )
     DO i = 0,x-1
       p_min = locproc( i, x, px )
       res(i+1,j+1) = p_min + px*p_maj
     END DO
   END DO
   RETURN
   END SUBROUTINE patchmap

   SUBROUTINE region_bounds( region_start, region_end, &
                             num_p, p,                 &
                             patch_start, patch_end )
   ! 1-D decomposition routine: Given starting and ending indices of a
   ! vector, the number of patches dividing the vector, and the number of
   ! the patch, give the start and ending indices of the patch within the
   ! vector.  This will work with tiles too.  Implementation note.  This is
   ! implemented somewhat inefficiently, now, with a loop, so we can use the
   ! locproc function above, which returns processor number for a given
   ! index, whereas what we want is index for a given processor number.
   ! With a little thought and a lot of debugging, we can come up with a
   ! direct expression for what we want.  For time being, we loop...
   ! Remember that processor numbering starts with zero.
                      
   IMPLICIT NONE
   INTEGER, INTENT(IN)                    :: region_start, region_end, num_p, p
   INTEGER, INTENT(OUT)                   :: patch_start, patch_end
   INTEGER                                :: offset, i
   patch_end = -999999999
   patch_start = 999999999
   offset = region_start
   do i = 0, region_end - offset
     if ( locproc( i, region_end-region_start+1, num_p ) == p ) then
       patch_end = max(patch_end,i)
       patch_start = min(patch_start,i)
     endif
   enddo
   patch_start = patch_start + offset
   patch_end   = patch_end   + offset
   RETURN
   END SUBROUTINE region_bounds

   SUBROUTINE least_aspect( nparts, minparts_y, minparts_x, nparts_y, nparts_x )
   IMPLICIT NONE
   !  Input data.
   INTEGER, INTENT(IN)           :: nparts,                &
                                    minparts_y, minparts_x
   ! Output data. 
   INTEGER, INTENT(OUT)          :: nparts_y, nparts_x
   ! Local data.
   INTEGER                       :: x, y, mini
   mini = 2*nparts
   nparts_y = 1
   nparts_x = nparts
   DO y = 1, nparts
      IF ( mod( nparts, y ) .eq. 0 ) THEN
         x = nparts / y
         IF (       abs( y-x ) .LT. mini       &
              .AND. y .GE. minparts_y                &
              .AND. x .GE. minparts_x    ) THEN
            mini = abs( y-x )
            nparts_y = y
            nparts_x = x
         END IF
      END IF
   END DO
   END SUBROUTINE least_aspect

   SUBROUTINE init_module_machine
      machine_info%tile_strategy = TILE_Y
   END SUBROUTINE init_module_machine

END MODULE module_machine

SUBROUTINE compute_memory_dims_rsl_lite  (      &
                   id , maxhalowidth ,            &
                   shw , bdx,  bdy ,              &
                   ntasks_x, ntasks_y, &
                   mytask_x, mytask_y, &
                   ids,  ide,  jds,  jde,  kds,  kde, &
                   ims,  ime,  jms,  jme,  kms,  kme, &
                   imsx, imex, jmsx, jmex, kmsx, kmex, &
                   imsy, imey, jmsy, jmey, kmsy, kmey, &
                   ips,  ipe,  jps,  jpe,  kps,  kpe, &
                   ipsx, ipex, jpsx, jpex, kpsx, kpex, &
                   ipsy, ipey, jpsy, jpey, kpsy, kpey )

    USE module_machine
    IMPLICIT NONE
    INTEGER, INTENT(IN)               ::  id , maxhalowidth
    INTEGER, INTENT(IN)               ::  shw, bdx, bdy
    INTEGER, INTENT(IN)               ::  ntasks_x, ntasks_y
    INTEGER, INTENT(IN)               ::  mytask_x, mytask_y
    INTEGER, INTENT(IN)     ::  ids, ide, jds, jde, kds, kde
    INTEGER, INTENT(OUT)    ::  ims, ime, jms, jme, kms, kme
    INTEGER, INTENT(OUT)    ::  imsx, imex, jmsx, jmex, kmsx, kmex
    INTEGER, INTENT(OUT)    ::  imsy, imey, jmsy, jmey, kmsy, kmey
    INTEGER, INTENT(OUT)    ::  ips, ipe, jps, jpe, kps, kpe
    INTEGER, INTENT(OUT)    ::  ipsx, ipex, jpsx, jpex, kpsx, kpex
    INTEGER, INTENT(OUT)    ::  ipsy, ipey, jpsy, jpey, kpsy, kpey

    INTEGER Px, Py, P, i, j, k, ierr

#if ( ! NMM_CORE == 1 )

! xy decomposition

    ips = -1
    j = jds
    ierr = 0
    DO i = ids, ide
       CALL task_for_point ( i, j, ids, ide, jds, jde, ntasks_x, ntasks_y, Px, Py, &
                             maxhalowidth, maxhalowidth, ierr )
       IF ( Px .EQ. mytask_x ) THEN
          ipe = i
          IF ( ips .EQ. -1 ) THEN
            ips = i
          ENDIF
       ENDIF
    ENDDO
    IF ( ierr .NE. 0 ) THEN
       CALL tfp_message(__FILE__,__LINE__)
    ENDIF
    ! handle setting the memory dimensions where there are no X elements assigned to this proc
    IF (ips .EQ. -1 ) THEN
       ipe = -1
       ips = 0
    ENDIF
    jps = -1
    i = ids
    ierr = 0
    DO j = jds, jde
       CALL task_for_point ( i, j, ids, ide, jds, jde, ntasks_x, ntasks_y, Px, Py, &
                             maxhalowidth, maxhalowidth, ierr )
       IF ( Py .EQ. mytask_y ) THEN
          jpe = j
          IF ( jps .EQ. -1 ) jps = j
       ENDIF
    ENDDO
    IF ( ierr .NE. 0 ) THEN
       CALL tfp_message(__FILE__,__LINE__)
    ENDIF
    ! handle setting the memory dimensions where there are no Y elements assigned to this proc
    IF (jps .EQ. -1 ) THEN
       jpe = -1
       jps = 0
    ENDIF

!begin: wig; 12-Mar-2008
! This appears redundant with the conditionals above, but we get cases with only
! one of the directions being set to "missing" when turning off extra processors.
! This may break the handling of setting only one of nproc_x or nproc_y via the namelist.
    IF (ipe .EQ. -1 .or. jpe .EQ. -1) THEN
       ipe = -1
       ips = 0
       jpe = -1
       jps = 0
    ENDIF
!end: wig; 12-Mar-2008

! 
! description of transpose decomposition strategy for RSL LITE. 20061231jm
!
! Here is the tranpose scheme that is implemented for RSL_LITE. Upper-case
! XY corresponds to the dimension of the processor mesh, lower-case xyz
! corresponds to grid dimension.
! 
!      xy        zy        zx
! 
!     XxYy <--> XzYy <--> XzYx <- note x decomposed over Y procs
!       ^                  ^
!       |                  |
!       +------------------+  <- this edge is costly; see below
! 
! The aim is to avoid all-to-all communication over whole
! communicator. Instead, when possible, use a transpose scheme that requires
! all-to-all within dimensional communicators; that is, communicators
! defined for the processes in a rank or column of the processor mesh. Note,
! however, it is not possible to create a ring of transposes between
! xy-yz-xz decompositions without at least one of the edges in the ring
! being fully all-to-all (in other words, one of the tranpose edges must
! rotate and not just transpose a plane of the model grid within the
! processor mesh). The issue is then, where should we put this costly edge
! in the tranpose scheme we chose? To avoid being completely arbitrary, 
! we chose a scheme most natural for models that use parallel spectral
! transforms, where the costly edge is the one that goes from the xz to
! the xy decomposition.  (May be implemented as just a two step transpose
! back through yz).
!
! Additional notational convention, below. The 'x' or 'y' appended to the
! dimension start or end variable refers to which grid dimension is all
! on-processor in the given decomposition. That is ipsx and ipex are the
! start and end for the i-dimension in the zy decomposition where x is
! on-processor. ('z' is assumed for xy decomposition and not appended to
! the ips, ipe, etc. variable names).
! 

! XzYy decomposition

    kpsx = -1
    j = jds ;
    ierr = 0
    DO k = kds, kde
       CALL task_for_point ( k, j, kds, kde, jds, jde, ntasks_x, ntasks_y, Px, Py, &
                             1, maxhalowidth, ierr )
       IF ( Px .EQ. mytask_x ) THEN
          kpex = k
          IF ( kpsx .EQ. -1 ) kpsx = k
       ENDIF
    ENDDO
    IF ( ierr .NE. 0 ) THEN
       CALL tfp_message(__FILE__,__LINE__)
    ENDIF 
    
! handle case where no levels are assigned to this process
! no iterations.  Do same for I and J. Need to handle memory alloc below.
    IF (kpsx .EQ. -1 ) THEN
       kpex = -1
       kpsx = 0
    ENDIF

    jpsx = -1
    k = kds ;
    ierr = 0
    DO j = jds, jde
       CALL task_for_point ( k, j, kds, kde, jds, jde, ntasks_x, ntasks_y, Px, Py, &
                             1, maxhalowidth, ierr )
       IF ( Py .EQ. mytask_y ) THEN
          jpex = j
          IF ( jpsx .EQ. -1 ) jpsx = j
       ENDIF
    ENDDO
    IF ( ierr .NE. 0 ) THEN
       CALL tfp_message(__FILE__,__LINE__)
    ENDIF 
    IF (jpsx .EQ. -1 ) THEN
       jpex = -1
       jpsx = 0
    ENDIF

!begin: wig; 12-Mar-2008
! This appears redundant with the conditionals above, but we get cases with only
! one of the directions being set to "missing" when turning off extra processors.
! This may break the handling of setting only one of nproc_x or nproc_y via the namelist.
    IF (ipex .EQ. -1 .or. jpex .EQ. -1) THEN
       ipex = -1
       ipsx = 0
       jpex = -1
       jpsx = 0
    ENDIF
!end: wig; 12-Mar-2008

! XzYx decomposition  (note, x grid dim is decomposed over Y processor dim)

    kpsy = kpsx   ! same as above
    kpey = kpex   ! same as above

    ipsy = -1
    k = kds ;
    ierr = 0
    DO i = ids, ide
       CALL task_for_point ( i, k, ids, ide, kds, kde, ntasks_y, ntasks_x, Py, Px, &
                             maxhalowidth, 1, ierr ) ! x and y for proc mesh reversed
       IF ( Py .EQ. mytask_y ) THEN
          ipey = i
          IF ( ipsy .EQ. -1 ) ipsy = i
       ENDIF
    ENDDO
    IF ( ierr .NE. 0 ) THEN
       CALL tfp_message(__FILE__,__LINE__)
    ENDIF 
    IF (ipsy .EQ. -1 ) THEN
       ipey = -1
       ipsy = 0
    ENDIF


#else

! In case of NMM CORE, the domain only ever runs from ids..ide-1 and jds..jde-1 so
! adjust decomposition to reflect.  20051020 JM
    ips = -1
    j = jds
    ierr = 0
    DO i = ids, ide-1
       CALL task_for_point ( i, j, ids, ide-1, jds, jde-1, ntasks_x, ntasks_y, Px, Py, &
                             maxhalowidth, maxhalowidth , ierr )
       IF ( Px .EQ. mytask_x ) THEN
          ipe = i
          IF ( Px .EQ. ntasks_x-1 ) ipe = ipe + 1
          IF ( ips .EQ. -1 ) ips = i
       ENDIF
    ENDDO
    IF ( ierr .NE. 0 ) THEN
       CALL tfp_message(__FILE__,__LINE__)
    ENDIF 
    jps = -1
    i = ids ;
    ierr = 0
    DO j = jds, jde-1
       CALL task_for_point ( i, j, ids, ide-1, jds, jde-1, ntasks_x, ntasks_y, Px, Py, &
                             maxhalowidth , maxhalowidth , ierr )
       IF ( Py .EQ. mytask_y ) THEN
          jpe = j
          IF ( Py .EQ. ntasks_y-1 ) jpe = jpe + 1
          IF ( jps .EQ. -1 ) jps = j
       ENDIF
    ENDDO
    IF ( ierr .NE. 0 ) THEN
       CALL tfp_message(__FILE__,__LINE__)
    ENDIF 
#endif

! extend the patch dimensions out shw along edges of domain
    IF ( ips < ipe .and. jps < jpe ) THEN           !wig; 11-Mar-2008
       IF ( mytask_x .EQ. 0 ) THEN
          ips = ips - shw
          ipsy = ipsy - shw
       ENDIF
       IF ( mytask_x .EQ. ntasks_x-1 ) THEN
          ipe = ipe + shw
          ipey = ipey + shw
       ENDIF
       IF ( mytask_y .EQ. 0 ) THEN
          jps = jps - shw
          jpsx = jpsx - shw
       ENDIF
       IF ( mytask_y .EQ. ntasks_y-1 ) THEN
          jpe = jpe + shw
          jpex = jpex + shw
       ENDIF
    ENDIF                                           !wig; 11-Mar-2008

    kps = 1
    kpe = kde-kds+1

    kms = 1
    kme = kpe
    kmsx = kpsx
    kmex = kpex
    kmsy = kpsy
    kmey = kpey

    ! handle setting the memory dimensions where there are no levels assigned to this proc
    IF ( kpsx .EQ. 0 .AND. kpex .EQ. -1 ) THEN
      kmsx = 0
      kmex = 0
    ENDIF
    IF ( kpsy .EQ. 0 .AND. kpey .EQ. -1 ) THEN
      kmsy = 0
      kmey = 0
    ENDIF

    IF ( (jps .EQ. 0 .AND. jpe .EQ. -1) .OR. (ips .EQ. 0 .AND. ipe .EQ. -1) ) THEN
      ims = 0
      ime = 0
    ELSE
      ims = max( ips - max(shw,maxhalowidth), ids - bdx ) - 1
      ime = min( ipe + max(shw,maxhalowidth), ide + bdx ) + 1
    ENDIF
    imsx = ids
    imex = ide
    ipsx = imsx
    ipex = imex
    ! handle setting the memory dimensions where there are no Y elements assigned to this proc
    IF ( ipsy .EQ. 0 .AND. ipey .EQ. -1 ) THEN
      imsy = 0
      imey = 0
    ELSE
      imsy = ipsy
      imey = ipey
    ENDIF

    IF ( (jps .EQ. 0 .AND. jpe .EQ. -1) .OR. (ips .EQ. 0 .AND. ipe .EQ. -1) ) THEN
      jms = 0
      jme = 0
    ELSE
      jms = max( jps - max(shw,maxhalowidth), jds - bdy ) - 1
      jme = min( jpe + max(shw,maxhalowidth), jde + bdy ) + 1
    ENDIF
    jmsx = jpsx
    jmex = jpex
    jmsy = jds
    jmey = jde
    ! handle setting the memory dimensions where there are no X elements assigned to this proc
    IF ( jpsx .EQ. 0 .AND. jpex .EQ. -1 ) THEN
      jmsx = 0
      jmex = 0
    ELSE
      jpsy = jmsy
      jpey = jmey
    ENDIF
END SUBROUTINE compute_memory_dims_rsl_lite

SUBROUTINE tfp_message( fname, lno )
   CHARACTER*(*) fname
   INTEGER lno
   CHARACTER*1024 mess
#ifndef STUBMPI
   WRITE(mess,*)'tfp_message: ',trim(fname),lno
   CALL wrf_message(mess)
# ifdef ALLOW_OVERDECOMP
     CALL task_for_point_message  ! defined in RSL_LITE/task_for_point.c
# else
     CALL wrf_error_fatal(mess)
# endif
#endif
END SUBROUTINE tfp_message

SUBROUTINE wrf_message( mess )
  CHARACTER*(*) mess
  PRINT*,'info: ',TRIM(mess)
END SUBROUTINE wrf_message

SUBROUTINE wrf_error_fatal( mess )
  CHARACTER*(*) mess
  PRINT*,'fatal: ',TRIM(mess)
  STOP
END SUBROUTINE wrf_error_fatal


PROGRAM tfp_tester
     INTEGER       id , maxhalowidth ,            &
                   shw , bdx,  bdy ,              &
                   ntasks_x, ntasks_y, &
                   mytask_x, mytask_y, &
                   ids,  ide,  jds,  jde,  kds,  kde, &
                   ims,  ime,  jms,  jme,  kms,  kme, &
                   imsx, imex, jmsx, jmex, kmsx, kmex, &
                   imsy, imey, jmsy, jmey, kmsy, kmey, &
                   ips,  ipe,  jps,  jpe,  kps,  kpe, &
                   ipsx, ipex, jpsx, jpex, kpsx, kpex, &
                   ipsy, ipey, jpsy, jpey, kpsy, kpey

     INTEGER i, j

     PRINT*,'id,maxhalowidth,shw,bdx,bdy ? '
     READ(*,*)id,maxhalowidth,shw,bdx,bdy
     PRINT*,'ids,ide,jds,jde,kds,kde '
     READ(*,*)ids,  ide,  jds,  jde,  kds,  kde
     PRINT*,'ntasks_x,ntasks_y'
     READ(*,*)ntasks_x,ntasks_y

     
     DO mytask_y = 0, ntasks_y-1
     DO mytask_x = 0, ntasks_x-1
       CALL compute_memory_dims_rsl_lite  (      &
                     id , maxhalowidth ,            &
                     shw , bdx,  bdy ,              &
                     ntasks_x, ntasks_y, &
                     mytask_x, mytask_y, &
                     ids,  ide,  jds,  jde,  kds,  kde, &
                     ims,  ime,  jms,  jme,  kms,  kme, &
                     imsx, imex, jmsx, jmex, kmsx, kmex, &
                     imsy, imey, jmsy, jmey, kmsy, kmey, &
                     ips,  ipe,  jps,  jpe,  kps,  kpe, &
                     ipsx, ipex, jpsx, jpex, kpsx, kpex, &
                     ipsy, ipey, jpsy, jpey, kpsy, kpey )

       PRINT*,' mytask_x, mytask_y ',mytask_x, mytask_y
       PRINT*,' ips,  ipe,  jps,  jpe,  kps,  kpe  ',ips,  ipe,  jps,  jpe,  kps,  kpe
       PRINT*,' ims,  ime,  jms,  jme,  kms,  kme  ',ims,  ime,  jms,  jme,  kms,  kme
       PRINT*,' ipsx, ipex, jpsx, jpex, kpsx, kpex ',ipsx, ipex, jpsx, jpex, kpsx, kpex
       PRINT*,' imsx, imex, jmsx, jmex, kmsx, kmex ',imsx, imex, jmsx, jmex, kmsx, kmex
       PRINT*,' ipsy, ipey, jpsy, jpey, kpsy, kpey ',ipsy, ipey, jpsy, jpey, kpsy, kpey
       PRINT*,' imsy, imey, jmsy, jmey, kmsy, kmey ',imsy, imey, jmsy, jmey, kmsy, kmey
     ENDDO
     ENDDO
END PROGRAM tfp_tester

