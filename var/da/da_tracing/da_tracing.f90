module da_tracing

#ifdef DM_PARALLEL
!  use mpi, only : mpi_character
#endif

   use da_control, only : num_procs, documentation_url, use_html, ierr, &
      trace_pe, trace_memory, trace_unit, trace_csv_unit, &
      trace_csv, myproc, comm, rootproc, trace_max_depth, &
      trace_repeat_head, trace_repeat_body, trace_start_points, trace_all_pes
   use da_par_util1, only : da_proc_sum_ints, da_proc_sum_real, da_proc_sum_int
   use da_reporting, only : da_error

#ifdef DM_PARALLEL
   include 'mpif.h'
#endif
   interface
      ! c code
      subroutine da_memory(memory_used)
         integer, intent(out) :: memory_used
      end subroutine da_memory
   end interface

   integer, parameter :: TraceIndentAmount      = 2   ! default indent
   integer, parameter :: MaxNoRoutines          = 440 ! maxium number of subroutines
   integer, parameter :: TraceNameLen           = 31  ! Length of trace name

   character (LEN=*), parameter :: &
      pad = "                                                                "


   ! Variables

   integer :: TraceDepth                   ! Current depth of trace
   integer :: NoRoutines                   ! Number of routines so far
   integer :: NoCalls(MaxNoRoutines)       ! Number of calls to each routine
   integer :: NoCallsBody(MaxNoRoutines)   ! Number of calls in body of each routine
   integer :: CalledBy(MaxNoRoutines)
   integer :: MaxHeap(MaxNoRoutines)
   integer :: EntryHeap(MaxNoRoutines)
   integer :: Pointer                      ! pointer to routine arrays in TIMER.
   integer :: BaseElapsedTime
   real :: BaseCPUTime
   integer :: LastSpace

   ! All CPU times in seconds

   real    :: CPUTimeStart(MaxNoRoutines)
   real    :: CPUTimeLocalStart
   real    :: CPUTime(MaxNoRoutines)
   real    :: CPUTimeLocal(MaxNoRoutines)
   real    :: CPUTimeThisCall(MaxNoRoutines)

   ! All Elapsed times based on wall clock in seconds

   real    :: ElapsedTimeStart(MaxNoRoutines)
   real    :: ElapsedTimeLocalStart
   real    :: ElapsedTime(MaxNoRoutines)
   real    :: ElapsedTimeLocal(MaxNoRoutines)
   real    :: ElapsedTimeThisCall(MaxNoRoutines)

   logical :: TraceActive = .false.        ! Is it active in this routine?

   character (LEN=TraceNameLen) :: TraceStartedBy  ! Subroutine name 
                                                   ! that activated trace
   character (LEN=TraceNameLen) :: TimerNames(MaxNoRoutines) ! Subroutine names
   character (LEN=TraceNameLen) :: TraceNames(MaxNoRoutines) ! for timing and tracing

   logical :: trace_write = .false.


contains

#include "da_trace_init.inc"
#include "da_trace_entry.inc"
#include "da_trace.inc"
#include "da_trace_exit.inc"
#include "da_trace_int_sort.inc"
#include "da_trace_real_sort.inc"
#include "da_trace_report.inc"


end module da_tracing
