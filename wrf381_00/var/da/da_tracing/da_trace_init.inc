subroutine da_trace_init

   implicit none

   !--------------------------------------------------------------------
   ! Purpose: Initialise tracing
   !--------------------------------------------------------------------

   integer :: IOStatus             ! I/O return code
   integer :: Loop1
   character (len=200) :: TraceFile
   character(len=200) :: csvname
   character (len=10)  :: temp

   IOStatus = 0

   if (trace_all_pes .OR. myproc == trace_pe) then
     trace_write = .true.  
   end if

   !-----------------------------------------------------------------
   ! Open trace output file. 
   !-----------------------------------------------------------------

   if (trace_write .AND. trace_unit /= 0) then
      if (use_html) then
         write(unit=temp,fmt='(I10)') myproc
         TraceFile="trace/"//trim(adjustl(temp))//".html"
         open (&
            unit=trace_unit, &   ! i:
            file=trim(tracefile), &   ! i:
            status="replace", & ! i:
            action="write", &   ! i:
            iostat=iostatus)    ! o:
      else   
         write(unit=temp,fmt='(i10)') myproc
         tracefile="trace/"//trim(adjustl(temp))//".txt"
         open (&
            unit=trace_unit, &   ! i:
            file=trim(tracefile), &   ! i:
            status="replace", & ! i:
            action="write", &   ! i:
            iostat=IOStatus)    ! O:
      end if

      if (IOStatus /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open trace file "//TraceFile/))
      end if
   end if

   if (trace_csv .and. rootproc) then
         write(unit=csvname,fmt='(I10,A)') myproc,'.csv'
      open(unit=trace_csv_unit,file="trace/"//trim(adjustl(csvname)), &
         status="replace",iostat=IOStatus)
      if (IOStatus /= 0) then
         call da_error(__FILE__,__LINE__,(/"Cannot open "//csvname/))
      end if
   end if

   !-----------------------------------------------------------------
   ! Find out whether to trace memory usage. The Cray routine to check
   ! memory usage is very slow, so it is best to only switch on memory
   ! checking if actively required.
   !-----------------------------------------------------------------

   !-----------------------------------------------------------------
   ! Set up timing and memory usage
   !-----------------------------------------------------------------

   do Loop1=1,MaxNoRoutines
      CPUTimeStart(Loop1)     = 0.0
      ElapsedTimeStart(Loop1) = 0.0
      ElapsedTime(Loop1)      = 0.0
      ElapsedTimeLocal(Loop1) = 0.0
      CPUTime(Loop1)          = 0.0
      CPUTimeLocal(Loop1)     = 0.0
      NoCalls(Loop1)          = 0
      NoCallsBody(Loop1)      = 0
      CalledBy(Loop1)         = 0
      MaxHeap(Loop1)          = 0
      TimerNames(Loop1)       = ""
   end do

   Pointer     = 0
   NoRoutines  = 0

   call system_clock(&
      COUNT=BaseElapsedTime)

   call cpu_time(BaseCPUTime)

   ! start trace output here so memory calculations are not distorted
   ! by IO buffer being grabbed later

   TraceDepth = 0

   if (trace_write) then
      if (use_html) then
         write (unit=trace_unit,fmt='(A)') "<html><head><title>Tracing</title></head>"
         write (unit=trace_unit,fmt='(A)') "<body><h1>Trace Output</h1>"
         write (unit=trace_unit,fmt='(A)') "<ul>"
         write (unit=trace_unit,fmt='(A)') "<li><a href=#tree>Calling Tree</a>"
         write (unit=trace_unit,fmt='(A)') "<li><a href=#local>Local routine timings</a>"
         write (unit=trace_unit,fmt='(A)') "<li><a href=#overall>Overall routine timings</a>"
         write (unit=trace_unit,fmt='(A)') "<li><a href=#memory>Memory usage</a>"
         write (unit=trace_unit,fmt='(A)') "</ul>"
         write (unit=trace_unit,fmt='(A)') "<a name=tree><h2>Calling Tree</h2></a><pre>"
      else
         write (unit=trace_unit,fmt='(A)') "Trace Output"
         write (unit=trace_unit,fmt='(A)') ""
      end if
   end if

end subroutine da_trace_init


