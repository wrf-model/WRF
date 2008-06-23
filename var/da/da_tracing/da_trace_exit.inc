subroutine da_trace_exit(&
   Name, &               ! in
   Message, &            ! in, optional
   Messages, &           ! in, optional
   MaxNoCalls)           ! in, optional

   !-----------------------------------------------------------------------
   ! Purpose: Trace exit from subroutine
   !-----------------------------------------------------------------------

   implicit none

   character (len=*), intent(in)           :: Name         ! subroutine name
   character (len=*), optional, intent(in) :: Message      ! text to trace
   character (len=*), optional, intent(in) :: Messages(:)  ! text to trace
   integer, optional, intent(in)           :: MaxNoCalls   ! max no calls to show

   integer                         :: IOStatus        ! I-O return code 
   integer                         :: Loop            ! General loop counter
   integer                         :: Count
   integer                         :: TotalSpace
   integer                         :: LocalMaxNoCalls
   integer                         :: Caller
   real                            :: temp_CPUTime
   real                            :: temp1
   real                            :: temp2
   character(len=25)               :: Change

   call cpu_time(temp_CPUTime)

   call system_clock(&
      COUNT=Count)

   !======================================================================
   ! check whether trace active and whether depth exceeded
   !======================================================================

   if (.NOT. TraceActive) then
      return
   end if

   if (TraceActive) then
      ! was tracing enabled by this routine? If it was, disable it, to
      ! take affect after the trace line has been written
      if (Name == TraceStartedBy(1:LEN(Name))) then
         TraceActive = .false.
      end if
   end if

   temp1 = real(Count - BaseElapsedTime) - ElapsedTimeLocalStart
   temp2 = temp_CPUTime - CPUTimeLocalStart

   TraceDepth=TraceDepth-1

   if (TraceDepth < 0) then
      TraceDepth = 0
   end if

   !=======================================================================
   ! Check timing and maximum heap memory usage
   !=======================================================================

   ElapsedTimeLocal(Pointer)    = ElapsedTimeLocal(Pointer) + temp1
   ElapsedTimeThisCall(Pointer) = ElapsedTimeThisCall(Pointer) + temp1
   ElapsedTime(Pointer)         = ElapsedTime(Pointer) + &
      ElapsedTimeThisCall(Pointer)

   CPUTimeLocal(Pointer)        = CPUTimeLocal(Pointer) + temp2
   CPUTimeThisCall(Pointer)     = CPUTimeThisCall(Pointer) + temp2
   CPUTime(Pointer)             = CPUTime(Pointer) + CPUTimeThisCall(Pointer)

   Caller=CalledBy(Pointer)
   if (Caller /= 0) then
      ElapsedTimeThisCall(Caller) = ElapsedTimeThisCall(Caller) + &
         ElapsedTimeThisCall(Pointer)
      CPUTimeThisCall(Caller) = CPUTimeThisCall(Caller) + CPUTimeThisCall(Pointer)
   end if

   Change = ""

   if (trace_memory) then
      call da_memory(&
         TotalSpace)
      if (EntryHeap(Pointer) < TotalSpace) then
         write(Change,"(A9,I12)")", BIGGER", TotalSpace - EntryHeap(Pointer)
      else if (EntryHeap(Pointer) > TotalSpace) then
         write(Change,"(A9,I12)")", SMALLER", TotalSpace - EntryHeap(Pointer)
      end if
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

      IOStatus=0

      if (NoCalls(Pointer) <= LocalMaxNoCalls) then
         if (trace_memory) then
            if (use_html) then
               write (unit=trace_unit, &
                  fmt='(A, "&lt; <a href=",A,"/",A,".html>",A,"</a>",I11,A)', &
                  iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Documentation_url), &
                  trim(Name),trim(Name), TotalSpace, Change
            else
               write (unit=trace_unit, &
                  fmt='(A, "< ",A,I11,A)', &
                  iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Name), TotalSpace, Change
            end if
         else
            if (use_html) then
               write (unit=trace_unit, &
                  fmt='(A, "&lt; <a href=",A,"/",A,".html>",A,"</a>")', &
                  iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Documentation_url), &
                  trim(Name),trim(Name)
            else
               write (unit=trace_unit, fmt='(A, "< ",A)', iostat=IOStatus) &
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

      if (NoCalls(Pointer) == trace_repeat_head) then
         write(unit=trace_unit,fmt='(A,"  Called enough, going quiet")', &
            iostat=IOStatus)&
            pad(1:TraceDepth*TraceIndentAmount)
         if (IOStatus .NE. 0) then
            call da_error(__FILE__,__LINE__, &
               (/"Cannot write to trace file for "//Name/))
         end if
      end if
   end if ! trace_write

   ! Restore pointer
   Pointer = CalledBy(Pointer)

   ! note local time

   call system_clock(&
     count=count)

   elapsedtimelocalstart = real(count-baseelapsedtime)
   call cpu_time(cputimelocalstart)

   ! call flush(trace_unit)

end subroutine da_trace_exit


