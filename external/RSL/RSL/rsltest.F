#include "LoopMacros.inc"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC FORTRAN 90 MODULE DEFINITION FOR A 'DOMAIN'
CC
CC User program definition of domain data structure and
CC manipulation routines.
CC
      module domains_module
        integer :: maxkids              ! maximum number of children
        parameter ( maxkids = 5 )
C
C Domainstruct is the principal domain data structure in the user
C program.  It contains the size of the domain in local
C memory and logically.  Also pointers to parent and child domains,
C if any.  Also the RSL domain handle.  Also, the domain state
C arrays themselves (unallocated, so virtually no storage expended
C unless the domain is actually used).
C
        type domainstruct
C
C This section has "meta" information about the domain.
C
          type( domainstruct ), pointer ::
     $           parent,        ! parent domain
     $           child(:)       ! children
          integer nkids         ! number of (active) children
          integer nestlevel     ! nestlevel of this domain
          integer domdesc       ! RSL domain handle
          logical active        ! flag
          integer m, mloc       ! global and local dimensions in m
          integer n, nloc       ! global and local dimensions in n
          integer sten          ! stencil descriptor
          integer period        ! periodic boundary descriptor
C
C This section has the state arrays for the domain.
C
          real, pointer    :: X(:,:)
          real, pointer    :: Y(:,:)
        endtype domainstruct
C
C Declaration of the top-level domain (all others are children of this one)
C
        type ( domainstruct ), target :: mother
C
C Domain manipulation routines
C
        contains
C
C Initialize a domain data structure
C
        subroutine init_domain( d )
          type(domainstruct) :: d
          d%active = .false.
          d%m = 0
          d%n = 0
        end subroutine
C
C Allocate the fields of the domain (including state arrays).  Once
C this is called, the domain will take more than nominal storage.
C
        subroutine allocate_domain( d, m, n, mloc, nloc )
          type(domainstruct) :: d       ! domain structure
          integer m, n, mloc, nloc      ! global and local sizes
          integer k                     ! child index

          if ( d%active ) then
            write(0,*) 'allocate_domain: domain already active.'
            stop
          endif
C
C Create and initialize structures for future children.   Note: only
C the child domain structures and not the child state arrays are
C being allocated here -- thus, the children require only nominal
C storage until actually activated and allocated.
C
          allocate (d%child(maxkids))
          do k = 1, maxkids
            call init_domain( d%child(k) )
          enddo
C
C Set the size information about this domain
C
          d%m = m
          d%n = n
          d%mloc = mloc
          d%nloc = nloc
          d%active = .true.
          d%nkids = 0
C
C Allocate storage for state arrays.
C
          allocate (d%X(mloc,nloc))
          allocate (d%Y(mloc,nloc))
        end subroutine
      end module
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC MAIN PROGRAM
CC
CC This is the main routine for a Fortran-90/RSL implementation of 
CC a simple relaxation problem on a grid, with an irregularly shaped
CC nested domain.
CC 
CC The bulk of the main routine, here, is involved with defining
CC and allocating the problem, which can be done entirely at run-time
CC by exploiting modern features of F90 such as derived data types
CC and dynamic memory allocation.
CC 
CC The model itself is is executed in one call to another routine,
CC ITERATE_MODEL
CC at the bottom of this main program.
CC 
      program relaxmain
      use domains_module
      implicit none
      type(domainstruct), pointer :: d
      include "rsl.inc"
      integer m,n,mloc,nloc,did         ! coarse domain params
      integer m_n,n_n,mloc_n,nloc_n,nid ! nested domain params
      integer mtrim, ntrim              ! trim for the nest
      integer xlist(20), ylist(20), npoints   ! outline of nest
      integer iter                      ! number of iterations to perform 
      integer k                         ! child index
      integer nproc_m, nproc_n          ! number of procs in m, n
      integer rsl_patch_decomp
      external rsl_patch_decomp
C
C Initialize RSL
C
      call rsl_initialize
C
C Input run-time problem configuration information
C
      if ( rsl_iammonitor() ) then
         read(7,*)m,n              ! mother domain size specif. at run time
         read(7,*)iter             ! number of mother domain iterations
         read(7,*)nproc_m, nproc_n ! number of processors specif. at run time
      endif
      call rsl_mon_bcast( m , 4 )
      call rsl_mon_bcast( n , 4 )
      call rsl_mon_bcast( iter , 4 )
      call rsl_mon_bcast( nproc_m , 4 )
      call rsl_mon_bcast( nproc_n , 4 )
       write(0,*)nproc_m,nproc_n
      call rsl_mesh ( nproc_m, nproc_n )
      CALL RSL_ERROR_DUP
      CALL SET_DEF_DECOMP_FCN( rsl_patch_decomp )

C
C Mother domain.  Note that the RSL routine is called before the
C domain is allocated.  This allows the local state arrays to be
C allocated only as large as necessary (using the size information
C returned by rsl_mother_domain in mloc and nloc).
C
      call init_domain(mother)
C
      call rsl_mother_domain(
     $                       did,               ! output: RSL domain handle
     $                       RSL_8PT,           ! input: max stencil
     $                       m, n,              ! input: global size
     $                       mloc, nloc )       ! output: local size
C
      call show_domain_decomp ( did )
      call allocate_domain( mother, m, n, mloc, nloc )
      mother%domdesc = did                      ! store RSL handle in domain
      mother%nestlevel = 1                      ! nest level of mother is 1
#if 0
C
C Specify the "trim" for the nest
C
      mtrim = 2
      ntrim = 2
C
C Spawn an irregular nest using the outline and trim information
C specified above.
C
      call RSL_SPAWN_REGULAR_NEST1(
     $              nid,                        ! output: nest handle
     $              mother%domdesc,             ! input: parent handle
     $              RSL_8PT,                    ! input: max stencil
     $              10, 12,
     $              37, 40,
     $              3, 3,                       ! input: nesting ratios
     $              mloc_n, nloc_n,             ! output: local memory size
     $              m_n, n_n )                  ! output: global size
      CALL SHOW_DOMAIN_DECOMP( did )
      CALL SHOW_DOMAIN_DECOMP( nid )
C
C Allocate using local size information returned by RSL.
C
      call allocate_domain( mother%child(1), m_n, n_n, mloc_n, nloc_n )
C
C Set fields in domain structures associating nest with parent
C
      mother%nkids = 1                          ! mother has one nest
      mother%child(1)%parent    => mother       ! back pointer to parent
      mother%child(1)%domdesc   =  nid          ! store RSL handle
      mother%child(1)%nestlevel =  mother%nestlevel+1
#else
      mother%nkids = 0                          ! mother has no nest
#endif
C
C Define the stencil communications on the mother and all subnests
C
      call define_data( mother )                ! user routine: recursive
C
C Initialize the interior of the mother and the boundary cells
C
      call init_grid( mother, mloc, nloc, 1 )
C
C Initialize the nests with data from the mother.  (There is only
C one nest in this example, but this code allows for more).
C
      do k = 1, mother%nkids
        call initial_nest_data( mother, mother%child(k) )
      enddo
C
C Write initial state of the model
C
      call output_domains( mother )             ! recursive
C
C Execute 'iter' iterations of the simulation.  The following call
C executes on the mother and recursively on all nests.
C
      call iterate_model( mother, 1, iter )     ! main time loop (recursive)
C
      call rsl_shutdown
      stop
      end
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC ITERATE_MODEL
CC
CC Main computational routine -- loop over time, iteration over mother
CC and all subnests, and control of inter-domain data exchanges.
CC
      recursive subroutine iterate_model( d, iter1, itern )
      use domains_module
      implicit none
      include "rsl.inc"
      type(domainstruct) :: d           ! input: domain 
      integer iter1, itern              ! input: starting and ending steps

      integer t                         ! local: time step on this domain
      integer k                         ! local: child index

      do t = iter1, itern
        call relax_grid( d, d%mloc, d%nloc, 1 )         ! compute this domain
        do k = 1, d%nkids                               ! for each nest...
          call force_domain( d, d%child(k) )            !   force
          call iterate_model( d%child(k), 1, 3 )        !   RECURSIVE CALL
          call merge_domain( d, d%child(k) )            !   feedback
        enddo
        if ( d%nestlevel .eq. 1 .and.
     $      ((mod(t-1, 5) .eq. 0) .or. (t .eq. itern))
     $                                            ) then
          call output_domains( d )                      ! if time, output

        endif
      enddo
      return
      end
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC RELAX_GRID
CC
CC This is the main computational routine that is called for one step
CC on a domain.  The new value for each point is computed as the
CC average of the values of 8 neighbors.  Prior to the computation,
CC a stencil exchange is performed to ensure that off-processor data
CC will be available for the computation.
CC
      subroutine relax_grid( d, ilen, jlen, klen )
      use domains_module
      implicit none
      include "rsl.inc"
      type(domainstruct) :: d           ! input: domain being computed
      integer ilen,                     ! input: local array sizes in m
     $        jlen,                     ! input: local array sizes in n
     $        klen                      ! input: local array sizes in vertical
      RSL_DECLARE_RUN_VARS( 500 )      ! declares looping information
      integer m, n, i, j                ! local: misc info
      real New( ilen, jlen )            ! local: temporary array
      integer bdyinfo(ilen,jlen, CROSS_BDY_INFO_LEN)
                                        ! local: proximity information
      real, pointer :: X(:,:)           ! local: state array pointer
C
C Initialize RSL loop constructs
C
      call rsl_get_run_info( d%domdesc, 500, RSL_RUN_VARS )
C
C Get boundary proximity information for each cell from RSL.
C
      call rsl_get_bdy_larray( d%domdesc, bdyinfo, CROSS_BDY_INFO_LEN )
C
C Exchange data with other processors on this domain using the stencil
C information defined for and stored with the domain.
C

         write(60+rsl_myproc,*) jlen, ilen, ' XA '
         do i = 1, ilen
         do j = 1, jlen
            write(60+rsl_myproc,*) d%X(i,j)
         enddo
         enddo
         write(70+rsl_myproc,*) jlen, ilen, ' YA '
         do i = 1, ilen
         do j = 1, jlen
            write(70+rsl_myproc,*) d%Y(i,j)
         enddo
         enddo

      call rsl_exch_period( d%domdesc, d%period )

         write(60+rsl_myproc,*) jlen, ilen, ' XB '
         do i = 1, ilen
         do j = 1, jlen
            write(60+rsl_myproc,*) d%X(i,j)
         enddo
         enddo
         write(70+rsl_myproc,*) jlen, ilen, ' YB '
         do i = 1, ilen
         do j = 1, jlen
            write(70+rsl_myproc,*) d%Y(i,j)
         enddo
         enddo

      call rsl_exch_stencil( d%domdesc, d%sten )

C
C Set pointer to domain state array.  Size info.
C
      X => d%X
      m = d%m
      n = d%n
C
C Main loop over horizontal dimensions of partition of array that
C is stored on local processor.  If a boundary cell, hold fixed,
C otherwise compute average.  Boundary cells are those with a boundary
C proximity of zero (i.e. they are zero cells away from a boundary).
C
      RSL_MAJOR_LOOP(j)
        RSL_MINOR_LOOP(i)
          if ( bdyinfo(i,j,RSL_DBDY) .eq. 1 ) then
             New(i,j) = X(i,j)
          else
             New(i,j) =  (
     $          X(i+1,j-1)   +  X(i+1,j)  +  X(i+1,j+1) +
     $          X(i,j-1)     +               X(i,j+1)   +
     $          X(i-1,j-1)   +  X(i-1,j)  +  X(i-1,j+1)
     $                 ) / 8.0
          endif
        RSL_END_MINOR_LOOP
      RSL_END_MAJOR_LOOP
C
C Update X.
C
      RSL_MAJOR_LOOP(j)
        RSL_MINOR_LOOP(i)
          X(i,j) = New(i,j)
        RSL_END_MINOR_LOOP
      RSL_END_MAJOR_LOOP
C
      return
      end
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC OUTPUT_DOMAINS
CC
CC Called initially and periodically.  Outputs state of model on 
CC to separate files for each nest level.
CC
      recursive subroutine output_domains( d )
      use domains_module
      implicit none
      include "rsl.inc"
      type(domainstruct) :: d                   ! input: domain
      integer k                                 ! local: child index
      integer glen(2), llen(2)                  ! local: size arrays
C
      glen(1) = d%m
      glen(2) = d%n
      llen(1) = d%mloc
      llen(2) = d%nloc
C
C Output top level domain
C
      call rsl_write( 18+d%nestlevel-1,         ! Fortran unit for output
     $                IO2D_IJ,                  ! describe record
     $                d%X,                      ! data for record
     $                d%domdesc,                ! domain descriptor
     $                RSL_REAL,                 ! type of each element
     $                glen, llen )              ! size info
C
C Foreach nest, output it and its subnests recursively.
C
      do k = 1, d%nkids
        call output_domains( d%child(k) )       ! recurse
      enddo
      return
      end
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC INITIAL_NEST_DATA
CC
CC Set the cells in the nest to values transferred down from the parent.
CC This is called to initialize the nest.
CC
      subroutine initial_nest_data ( d, nst )
      use domains_module
      implicit none
      include "rsl.inc"
      type(domainstruct) :: d, nst              ! input: parent and nest
      integer pi, pj, pig, pjg                  ! local: parent indices
      integer ni, nj, nig, njg                  ! local: nest indices
      integer m, n, i, j, msize                 ! local: misc variables
      integer cm, cn                            ! local: relative nest index
      integer retval                            ! local: return value
      real, pointer :: X(:,:)                   ! local: pointer to state array
C
C Point to parent's state arrays.
C
      X => d%X
C
C Build a message for each point on the nest using data from the
C overlying cell in the parent domain.  Loop goes until we have handled
C data from all the parent domain points on this processor.
C
      call rsl_to_child_info( d%domdesc, nst%domdesc, 4,
     $                        i, j, pig, pjg, cm, cn, nig, njg, retval )
      do while ( retval .eq. 1 )
        call rsl_to_child_msg( 4, X(i,j) )
        call rsl_to_child_info( d%domdesc, nst%domdesc, 4,
     $                          i, j, pig, pjg, cm, cn, nig, njg, retval )
      enddo
C
C Exchange the data using RSL inter-domain communication.
C
      call rsl_bcast_msgs
C
C Now, point to nest state data
C
      X => nst%X
      X = 0.
C
C Unpack the message on each point of the nest.  Loop goes until we
C have unpacked all the nested domain points that are local to this
C processor.
C
      call rsl_from_parent_info( i, j, nig, njg, cm, cn, pig, pjg, retval )
      do while ( retval .eq. 1 )
        call rsl_from_parent_msg( 4, X(i,j) )
        call rsl_from_parent_info( i, j, nig, njg, cm, cn, pig, pjg, retval )
      enddo
C
      return
      end
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC FORCE_DOMAIN
CC
CC Similar to init_domain, except this is called at
CC each step to force only the boundaries of the nest
CC (not the entire domain, as in init_domain).
CC
      subroutine force_domain ( d, nst )
      use domains_module
      implicit none
      include "rsl.inc"
      type(domainstruct) :: d, nst              ! input: parent and nest
      integer pi, pj, pig, pjg                  ! local: parent indices
      integer ni, nj, nig, njg                  ! local: nest indices
      integer m, n, i, j, msize                 ! local: misc variables
      integer cm, cn                            ! local: relative nest index
      integer retval                            ! local: return value
      real, pointer :: X(:,:)                   ! local: pointer to state array
      integer bdyinfo(CROSS_BDY_INFO_LEN)       ! local: boundary proximity
C
C Point to parent's state arrays.
C
      X => d%X
C
C Build a message for ONLY THOSE POINTS on the nest that are on a boundary.
C The call to rsl_get_bdy_gpt gets the proximity information for a nested
C point.  The information is then used to decide whether or not data should
C be packed for that point.  Note: RSL will automatically size the messages
C between processors to exchange only that data that is packed.
C
      call rsl_to_child_info( d%domdesc, nst%domdesc, 4,
     $                        i, j, pig, pjg, cm, cn, nig, njg, retval )
      do while ( retval .eq. 1 )
        call rsl_get_bdy_gpt( nst%domdesc, bdyinfo, CROSS_BDY_INFO_LEN, nig, njg )
        if ( bdyinfo(RSL_DBDY) .ge. 1 .and. bdyinfo(RSL_DBDY) .le. 2 ) then
          write(0,999)'xx ' ,rsl_myproc,nig, njg, pig, pjg, cm, cn
 999      format(a3,7i5) 
          call rsl_to_child_msg( 4, X(i,j) )
        endif
        call rsl_to_child_info( d%domdesc, nst%domdesc, 4,
     $                          i, j, pig, pjg, cm, cn, nig, njg, retval )
      enddo
C
C Exchange the data using RSL inter-domain communication.
C
      call rsl_bcast_msgs
C
C Now, point to nest state data
C
      X => nst%X
C
C Unpack the message on each point of the nest.  Because the first phase
C of the exchange packed only data for the boundary points, this loop 
C will iterate only over those.
C
      call rsl_from_parent_info( i, j, nig, njg, cm, cn, pig, pjg, retval )
      do while ( retval .eq. 1 )
        call rsl_from_parent_msg( 4, X(i,j) )
        call rsl_from_parent_info( i, j, nig, njg, cm, cn, pig, pjg, retval )
      enddo

      return
      end
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC MERGE_DOMAIN
CC
CC This implements the feedback of data from the nest to the parent.
CC The structure is similar to FORCE_DOMAIN, except that the flow
CC of information is in the opposite direction.  Data is returned only
CC for the center nest point under each coarse domain point (cm=2, cn=2).
CC
      subroutine merge_domain ( d, nst )
      use domains_module
      implicit none
      include "rsl.inc"
      type(domainstruct) :: d, nst              ! input: parent and nest
      integer pi, pj, pig, pjg                  ! local: parent indices
      integer ni, nj, nig, njg                  ! local: nest indices
      integer m, n, i, j, msize                 ! local: misc variables
      integer cm, cn                            ! local: relative nest index
      integer retval                            ! local: return value
      real, pointer :: X(:,:)                   ! local: pointer to state array
C
      X =>nst%X
      call rsl_to_parent_info( d%domdesc, nst%domdesc, 4,
     $                        i, j, nig, njg, cm, cn, pig, pjg, retval )
      do while ( retval .eq. 1 )
        if ( cm .eq. 1 .and. cn .eq. 1 ) then
          call rsl_to_parent_msg( 4, X(i,j) )
        endif
        call rsl_to_parent_info( d%domdesc, nst%domdesc, 4,
     $                          i, j, nig, njg, cm, cn, pig, pjg, retval )
      enddo
C
      call rsl_merge_msgs
C
      X => d%X
      call rsl_from_child_info( i, j, pig, pjg, cm, cn, nig, njg, retval )
      do while ( retval .eq. 1 )
        if ( cm .eq. 2 .and. cn .eq. 2 ) then
          call rsl_from_child_msg( 4, X(i,j) )
        endif
        call rsl_from_child_info( i, j, pig, pjg, cm, cn, nig, njg, retval )
      enddo
C
      return
      end
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC INIT_GRID
CC
CC This is a computational routine whose purpose it is to assign
CC a domain with initial values.  Boundary cells receive a non-zero
CC initial value.  The interior receives a zero value.
CC
      subroutine init_grid( d, ilen, jlen, klen )
      use domains_module
      implicit none
      include "rsl.inc"
      type(domainstruct) :: d
      integer ilen, jlen, klen
      RSL_DECLARE_RUN_VARS( 500 )
      integer bdyinfo(ilen,jlen,CROSS_BDY_INFO_LEN)
      integer m, n, i, j
      integer cn, cm
      real, pointer :: X(:,:), Y(:,:)
C
      call rsl_get_run_info( d%domdesc, 500, RSL_RUN_VARS )
      call rsl_get_bdy_larray( d%domdesc, bdyinfo, CROSS_BDY_INFO_LEN )
C
      X    => d%X
      Y    => d%Y
      m = d%m
      n = d%n
      X = 0.
      Y = 0.
C
      RSL_MAJOR_LOOP(j)
        RSL_MINOR_LOOP(i)
          if ( bdyinfo(i,j,RSL_DBDY) .eq. 1 ) then
C              X(i,j) = 10.0
              X(i,j) = J + I *.01
          else
              X(i,j) = 0.0
          endif
          if ( bdyinfo(i,j,RSL_DBDY_X) .eq. 1 ) then
              Y(i,j) = J + I *.01
          else
              Y(i,j) = 0.0
          endif
        RSL_END_MINOR_LOOP
      RSL_END_MAJOR_LOOP
C
      return
      end
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC DEFINE_DATA
CC
CC This is called only once, prior to the first step of the model, and
CC is used to define the stencils on all the domains in the simulation.
CC Stencils, though identical, must be assigned individually for each
CC domain.  This routine does that recursively for the domain 'd', and
CC all nests under 'd'.
CC
      recursive subroutine define_data( d )
      use domains_module
      include "rsl.inc"
      type(domainstruct) :: d           ! input: domain
      integer decomp(3)                 ! local: how dimensions are decomposed
      integer llen(3)                   ! local: local size in each dim.
      integer glen(3)                   ! local: global size in each dim.
      integer mesg                      ! local: a message definition
      integer messages(8)               ! local: message for each stencil pt.
      integer k                         ! local: child index
!      interface
!        subroutine rsl_build_message( mesg, type, var, dim , d1, d2, d3 )
!          integer mesg, type, dim, d1(*), d2(*), d3(*)
!          real    var(:,:)
!        end subroutine rsl_build_message
!      end interface
C
      decomp(1) = RSL_NORTHSOUTH        ! m is decomposed by n/s processors
      decomp(2) = RSL_EASTWEST          ! n is decomposed by e/w processors
      glen(1) = d%m                     ! global sizes set
      glen(2) = d%n
      llen(1) = d%mloc                  ! local sizes set
      llen(2) = d%nloc
C
C Create and build a message descriptor containing the state array X.
C
      call rsl_create_message( mesg )
         write(0,*)'loc of d%X',loc(d%X), loc(d%X(1,1))
      call rsl_build_message( mesg,RSL_REAL,d%X(1,1),2,decomp,glen,llen )
      call rsl_build_message( mesg,RSL_REAL,d%Y(1,1),2,decomp,glen,llen )
C
C Create and build a stencil with the message on each of the 8 pts.
C
      call rsl_create_stencil( d%sten )
      messages(1) =    mesg
      messages(2) =           mesg
      messages(3) =                 mesg
      messages(4) =    mesg
      messages(5) =                 mesg
      messages(6) =    mesg
      messages(7) =           mesg
      messages(8) =                 mesg
      call rsl_describe_stencil( d%domdesc, d%sten, RSL_8PT, messages )

C Periodic boundary
      call rsl_create_message( mesg )
      call rsl_build_message( mesg,RSL_REAL,d%X(1,1),2,decomp,glen,llen )
      glen(1) = d%m-1
      glen(2) = d%n-1
      call rsl_build_message( mesg,RSL_REAL,d%Y(1,1),2,decomp,glen,llen )
      glen(1) = d%m
      glen(2) = d%n
      call rsl_create_period( d%period )
      call rsl_describe_period( d%domdesc, d%period, 1, mesg )
C
C Define the stencils for all the child domains of this domains
C
      do k = 1, d%nkids
        call define_data( d%child(k) )  ! RECURSION
      enddo
C
      return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

