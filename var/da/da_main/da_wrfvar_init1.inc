subroutine da_wrfvar_init1(no_init1)

   !-----------------------------------------------------------------------
   ! Purpose: WRFVAR initialization routine, part 1
   !-----------------------------------------------------------------------

   implicit none

   logical, optional, intent(in) :: no_init1

   !<DESCRIPTION>
   ! Program_name, a global variable defined in frame/module_domain.F, is
   ! set, then a routine <a href=init_modules.html>init_modules</a> is
   ! called. This calls all the init programs that are provided by the
   ! modules that are linked into WRF.  These include initialization of
   ! external I/O packages.   Also, some key initializations for
   ! distributed-memory parallelism occur here if DM_PARALLEL is specified
   ! in the compile: setting up I/O quilt processes to act as I/O servers
   ! and dividing up MPI communicators among those as well as initializing
   ! external communication packages.
   !
   !</DESCRIPTION>

   ! Set "program_name", which will be printed to output and netcdf metadata
   program_name = "WRFDA "//release_version

   ! Initialize WRF modules:  
   ! Phase 1 returns after mpi_init() (if it is called)
   if (.NOT. present(no_init1)) then
      call init_modules (1)
      ! Initialize utilities (time manager, etc.)
#ifdef NO_LEAP_CALENDAR
      call wrfu_initialize (defaultCalKind=WRFU_CAL_NOLEAP)
#else
      call wrfu_initialize (defaultCalKind=WRFU_CAL_GREGORIAN)
#endif
   end if
   ! Phase 2 resumes after mpi_init() (if it is called)
   call init_modules (2)

   !<DESCRIPTION>
   ! The wrf namelist.input file is read and stored in the use associated
   ! structure model_config_rec, defined in frame/module_configure.F, by the
   ! call to <a href=initial_config.html>initial_config</a>.  On distributed
   ! memory parallel runs this is done only on one processor, and then
   ! broadcast as a buffer.  For distributed-memory, the broadcast of the
   ! configuration information is accomplished by first putting the
   ! configuration information into a buffer (<a
   ! href=get_config_as_buffer.html>get_config_as_buffer</a>), broadcasting
   ! the buffer, then setting the configuration information (<a
   ! href=set_config_as_buffer.html>set_config_as_buffer</a>).
   !
   !</DESCRIPTION>

   ! Don't use stdout here, too early
   write(unit=6,fmt='(/A)') "*** VARIATIONAL ANALYSIS ***"
   write(unit=6,fmt='(A,A/)') "    ",program_name

#ifdef DM_PARALLEL
   call wrf_get_dm_communicator (comm)
   call mpi_comm_size (comm, num_procs, ierr)
   call mpi_comm_rank (comm, myproc, ierr)
#else
   num_procs = 1
   myproc = 0
   comm = 0
#endif

   if (myproc==0) then
      rootproc=.true.
   else
      rootproc=.false.
   end if

#ifdef DM_PARALLEL
   if (rootproc) then
      call initial_config
   end if
   call get_config_as_buffer (configbuf, configbuflen, nbytes)
   call wrf_dm_bcast_bytes (configbuf, nbytes)
   call set_config_as_buffer (configbuf, configbuflen)
   call wrf_dm_initialize
#else
   call initial_config
#endif

   ! Copy namelist variables to da_control

#define SOURCE_RECORD model_config_rec%
#define DEST_RECORD

#include "config_assigns.inc"

   if (.NOT. rootproc) check_max_iv_print=.false.

end subroutine da_wrfvar_init1


