subroutine da_trace_entry(&
   Name, &                       ! in
   Message, &                    ! in, optional
   Messages, &                   ! in, optional
   MaxNoCalls)                   ! in, optional

   !-----------------------------------------------------------------------
   ! Purpose: Trace entry point to subroutine
   !-----------------------------------------------------------------------

   implicit none

   character (len=*),           intent(in) :: Name         ! Routine name
   character (len=*), optional, intent(in) :: Message      ! message
   character (len=*), optional, intent(in) :: Messages(:)  ! message array
   integer, optional,           intent(in) :: MaxNoCalls   ! max no calls to show


   integer           :: IOStatus        ! I-O return code
   integer           :: Loop            ! General loop counter
   integer           :: Count
   integer           :: OldPointer
   integer           :: TotalSpace
   integer           :: LocalMaxNoCalls
   real              :: CPUTime1
   real              :: temp1
   real              :: temp2
   logical           :: NewRoutine

   call cpu_time(CPUTime1)

   call system_clock(&
      COUNT=Count)

   !-----------------------------------------------------------------------
   ! check if tracing active. If not check whether to switch it on
   !-----------------------------------------------------------------------

   if (.NOT. TraceActive) then
      if (trace_start_points == 0) then
         ! start with first call
         TraceActive = .true.
      else
         do Loop=1,trace_start_points
            if (Name == TraceNames(Loop)(1:LEN(Name))) then
               TraceActive    = .true.
               TraceDepth     = 0
               TraceStartedBy = Name
               exit
            end if
         end do
      end if
      if (.NOT. TraceActive) then
         ! did not want to start trace, so leave
         return
      end if
   end if

   !-----------------------------------------------------------------------
   ! timing and maximum heap usage
   !-----------------------------------------------------------------------

   ! Increment the local elapsed time and local CPU time since the
   ! last trace entry, if any

   if (Pointer /= 0) then
      temp1 = real(Count - BaseElapsedTime) - ElapsedTimeLocalStart
      temp2 = CPUTime1 - CPUTimeLocalStart
      ElapsedTimeLocal(Pointer)    = ElapsedTimeLocal(Pointer) + temp1
      ElapsedTimeThisCall(Pointer) = ElapsedTimeThisCall(Pointer) + temp1
      CPUTimeLocal(Pointer)        = CPUTimeLocal(Pointer) + temp2
      CPUTimeThisCall(Pointer)     = CPUTimeThisCall(Pointer) + temp2
   end if

   OldPointer=Pointer

   ! Check subroutine name 

   NewRoutine = .true.
   do Pointer=1,NoRoutines     
      if (TimerNames(Pointer) == Name) then
         NewRoutine=.false.
         exit
      end if
   end do

   if (NewRoutine) then
      ! New subroutine entered
      if (NoRoutines >= MaxNoRoutines)then ! too many to trace
          call da_error(__FILE__,__LINE__, &
             (/"Too many routines. Not timing " // Name/))

         !All the timings etc are put instead to the calling routine,
         ! which therefore may have incorrect summaries.
         !The best solution is to increase MaxNoRoutines.
         Pointer = OldPointer
         ! Fix to get the correct NoCalls(OldPointer) despite the +1 later
         NoCalls(Pointer)=NoCalls(Pointer)-1

      else ! Pointer=NoRoutines+1 (from the end of earlier do loop)
         NoRoutines=NoRoutines+1
         TimerNames(NoRoutines)=Name
      end if
   end if

   NoCalls(Pointer)=NoCalls(Pointer)+1

   CPUTimeThisCall(Pointer)     = 0.0
   ElapsedTimeThisCall(Pointer) = 0.0

   CalledBy(Pointer)=OldPointer

   if (trace_memory) then
      call da_memory(&
         TotalSpace)
      EntryHeap(Pointer) = TotalSpace
      LastSpace = TotalSpace
      if (MaxHeap(Pointer) < TotalSpace) then
         MaxHeap(Pointer) = TotalSpace
      end if
   else
      TotalSpace = 0
   end if

   if (trace_write .AND. TraceDepth <= trace_max_depth) then

      if (present(MaxNoCalls)) then
         LocalMaxNoCalls = MaxNoCalls
      else
         LocalMaxNoCalls = trace_repeat_head
      end if

      if (NoCalls(Pointer) <= LocalMaxNoCalls) then
         if (trace_memory) then
            if (use_html) then
               write (unit=trace_unit, &
                  fmt='(A,"&gt; <a href=",A,"/",A,".html>",A,"</a>",I11)', &
                  iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Documentation_url), &
                  trim(Name),trim(Name), TotalSpace
            else
               write (unit=trace_unit, &
                  fmt='(A,"> ",A,I11)', &
                  iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Name), TotalSpace
           end if
         else
            if (use_html) then
               write (unit=trace_unit, &
                  fmt='(A,"&gt; <a href=",A,"/",A,".html>",A,"</a>")', &
                  iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Documentation_url), &
                  trim(Name),trim(Name)
            else
               write (unit=trace_unit, fmt='(A,"> ",A)', iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Name)
            end if
         end if
         if (IOStatus /= 0) then
            call da_error(__FILE__,__LINE__, &
               (/"Cannot write to trace file for "//Name/))
         end if

         if (present(Message)) then
            write (unit=trace_unit, fmt='(A," ",A)', iostat=IOStatus) &
               pad(1:TraceDepth*TraceIndentAmount),trim(Message)
            if (IOStatus .NE. 0) then
               call da_error(__FILE__,__LINE__, &
                  (/"Cannot write to trace file for "//Name/))
            end if
         end if

         if (present(Messages)) then
            do Loop = 1, size(Messages)
               write (unit=trace_unit, fmt='(A," ",A)', iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Messages(Loop))
               if (IOStatus .NE. 0) then
                  call da_error(__FILE__,__LINE__, &
                     (/"Cannot write to trace file for "//Name/))
               end if
            end do ! Loop
         end if
      end if

   end if ! trace_write

   TraceDepth=TraceDepth+1

   call system_clock(&
      COUNT=Count)

   call cpu_time(CPUTime1)

   ! set the start elapsed and CPU time both locally and generally

   ElapsedTimeStart(Pointer) = real(Count-BaseElapsedTime)
   ElapsedTimeLocalStart     = real(Count-BaseElapsedTime)

   CPUTimeStart(Pointer) = CPUTime1
   CPUTimeLocalStart     = CPUTime1

   ! call flush(trace_unit)

   return
end subroutine da_trace_entry


