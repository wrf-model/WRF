subroutine da_trace(&
   Name,     &           ! in
   Message,  &           ! in, optional
   Messages,  &          ! in, optional
   MaxNoCalls)           ! in, optional

   implicit none

   !--------------------------------------------------------------------
   ! Purpose: General trace within a subroutine
   !--------------------------------------------------------------------

   character (len=*), intent(in)           :: Name         ! Subroutine name
   character (len=*), optional, intent(in) :: Message      ! Text to trace
   character (len=*), optional, intent(in) :: Messages(:)  ! Text to trace
   integer, optional, intent(in)           :: MaxNoCalls   ! max no calls to show

   integer           :: IOStatus     ! I-O return code
   integer           :: Loop         ! General loop counter
   integer           :: TotalSpace
   integer           :: LocalMaxNoCalls
   character(len=25) :: Change

   !-----------------------------------------------------------------------
   ! Check whether trace active and depth of trace
   !-----------------------------------------------------------------------

   if (.NOT. TraceActive) then
      return
   end if

   if (TraceDepth >= trace_max_depth) then
      ! already at maximum depth, so return
      return
   end if

   !-----------------------------------------------------------------------
   ! Note memory usage
   !-----------------------------------------------------------------------

   Change = ""

   if (trace_memory) then
      call da_memory(&
         TotalSpace)
      if (LastSpace < TotalSpace) then
         write(Change,"(A9,I12)")", bigger", TotalSpace - LastSpace
      else if (LastSpace > TotalSpace) then
         write(Change,"(A9,I12)")", smaller", TotalSpace - LastSpace
      end if
      if (MaxHeap(Pointer) < TotalSpace) then
         MaxHeap(Pointer) = TotalSpace
      end if
      LastSpace = TotalSpace
   else
      TotalSpace = 0
   end if

   !-----------------------------------------------------------------------
   ! Perform the trace if not done too many times before. only on PE 0
   !-----------------------------------------------------------------------  

   if (trace_write) then

      if (present(MaxNoCalls)) then
         LocalMaxNoCalls = MaxNoCalls
      else
         LocalMaxNoCalls = trace_repeat_body
      end if

      NoCallsBody(Pointer) = NoCallsBody(Pointer)+1

      if (NoCallsBody(Pointer) <= LocalMaxNoCalls) then
         if (trace_memory) then
             if (use_html) then
                write (unit=trace_unit, &
                   fmt='(A, "| <a href=",A,"/",A,".html>",A,"</a>",I11,A)', &
                   iostat=IOStatus) &
                   pad(1:TraceDepth*TraceIndentAmount),trim(Documentation_url), &
                   trim(Name), trim(Name), TotalSpace, Change
             else
                write (unit=trace_unit, &
                   fmt='(A, "| ",A,I11,A)', &
                   iostat=IOStatus) &
                   pad(1:TraceDepth*TraceIndentAmount),trim(Name), TotalSpace, Change
            end if
         else
            if (use_html) then
               write (unit=trace_unit, &
                  fmt='(A, "| <a href=",A,"/",A,".html>",A,"</a>")', &
                  iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Documentation_url), &
                  trim(Name), trim(Name)  
            else 
               write (unit=trace_unit, &
                  fmt='(A, "| ",A)', &
                  iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Name)
            end if
         end if
         if (IOStatus /= 0) then
            call da_error(__FILE__,__LINE__, &
               (/"Cannot write to trace file for "//Name/))
         end if

         if (present(Message)) then
            write (unit=trace_unit, fmt='(A,"  ",A)', iostat=IOStatus) &
               pad(1:TraceDepth*TraceIndentAmount),trim(Message)
            if (IOStatus .NE. 0) then
               call da_error(__FILE__,__LINE__, &
                  (/"Cannot write to trace file for "//Name/))
            end if
         end if

         if (present(Messages)) then
            do Loop = 1, size(Messages)
               write (unit=trace_unit, fmt='(A,"  ",A)', iostat=IOStatus) &
                  pad(1:TraceDepth*TraceIndentAmount),trim(Messages(Loop))
               if (IOStatus .NE. 0) then
                  call da_error(__FILE__,__LINE__, &
                     (/"Cannot write to trace file for "//Name/))
               end if
            end do ! Loop
         end if
      end if

      if (NoCallsBody(Pointer) == trace_repeat_body) then
         write (unit=trace_unit, fmt='(A,"  Called enough, going quiet")', iostat=IOStatus) &
            pad(1:TraceDepth*TraceIndentAmount)
         if (IOStatus .NE. 0) then
            call da_error(__FILE__,__LINE__, &
              (/"Cannot write to trace file for "//Name/))
         end if
      end if
   end if ! trace_write

end subroutine da_trace


