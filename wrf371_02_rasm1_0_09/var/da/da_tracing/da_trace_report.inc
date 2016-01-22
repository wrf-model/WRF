subroutine da_trace_report

   !--------------------------------------------------------------------
   ! Purpose: Produce a trace report
   !--------------------------------------------------------------------

   implicit none

   integer :: i                        ! loop counter
   integer :: j                        ! loop counter
   integer :: CountRate
   integer :: MasterNoRoutines
   integer :: temp
   integer :: MinElapsedPos
   integer :: MaxElapsedPos
   integer :: MinCPUPos
   integer :: MaxCPUPos
   integer :: itemp1(MaxNoRoutines)
   integer :: MasterMaxHeap(0:num_procs-1,MaxNoRoutines)
   integer :: MasterNoCalls(0:num_procs-1,MaxNoRoutines)
   integer :: OverallNoCalls(MaxNoRoutines)
   integer, allocatable :: Index(:)

   real    :: TempCpuTime
   real    :: TotalElapsedTime             !
   real    :: TotalCPUTime(1)              !
   real    :: SpeedUp                      ! speed up factor
   real    :: PercentCPUTime               ! percentage in CPU time
   real    :: PercentElapsedTime           ! percentage in elapsed time
   real    :: rtemp1(MaxNoRoutines)
   real    :: MasterElapsedTime(0:num_procs-1,MaxNoRoutines)
   real    :: MasterElapsedTimeLocal(0:num_procs-1,MaxNoRoutines)
   real    :: MasterCPUTime(0:num_procs-1,MaxNoRoutines)
   real    :: MasterCPUTimeLocal(0:num_procs-1,MaxNoRoutines)
   real    :: OverallElapsedTime(MaxNoRoutines)
   real    :: OverallCPUTime(MaxNoRoutines)

   character (len=TraceNameLen) :: MasterTimerNames(MaxNoRoutines)

   if (.not. use_html) then
      write (unit=trace_unit, fmt='(A)') &
         "Report only available if use_html is true"
      return
   end if

   call system_clock (COUNT=temp)

   TotalElapsedTime=temp-BaseElapsedTime ! on PE 0

   call cpu_time(TempCpuTime)

   TotalCPUTime(1) = TempCpuTime - BaseCPUTime

   call system_clock(&
      COUNT_RATE=CountRate)

   ! Ensure the lists from each PE match. use the routine list from the 
   ! traced PE as the master copy

   MasterTimerNames(:)=TimerNames(:)

   if (myproc == trace_pe) then
      MasterNoRoutines=NoRoutines
   else
      MasterNoRoutines=0
   end if

#ifdef DM_PARALLEL
   call da_proc_sum_int(MasterNoRoutines)
   ! only PE 0 knows the result

   call mpi_bcast(MasterTimerNames(1:MaxNoRoutines), &
                  TraceNameLen*MaxNoRoutines, &
                  MPI_character,trace_pe, comm,ierr)
#endif

   MasterElapsedTime(:,:)=0.0
   MasterCPUTime(:,:)=0.0
   MasterElapsedTimeLocal(:,:)=0.0
   MasterCPUTimeLocal(:,:)=0.0
   MasterNoCalls(:,:)=0
   MasterMaxHeap(:,:)=0

   do i=1,MaxNoRoutines
      do j=1,NoRoutines
         if (TimerNames(j) == MasterTimerNames(i)) then
            MasterElapsedTime(myproc,i)=ElapsedTime(j)
            MasterCPUTime(myproc,i)=CPUTime(j)
            MasterElapsedTimeLocal(myproc,i)=ElapsedTimeLocal(j)
            MasterCPUTimeLocal(myproc,i)=CPUTimeLocal(j)
            MasterNoCalls(myproc,i)=NoCalls(j)
            MasterMaxHeap(myproc,i)=MaxHeap(j)
            cycle
         end if
      end do
   end do

#ifdef DM_PARALLEL
   do i=0,num_procs-1
      call da_proc_sum_real(MasterElapsedTime(i,:))
      call da_proc_sum_real(MasterCPUTime(i,:))
      call da_proc_sum_real(MasterElapsedTimeLocal(i,:))
      call da_proc_sum_real(MasterCPUTimeLocal(i,:))
      call da_proc_sum_ints(MasterNoCalls(i,:))
      call da_proc_sum_ints(MasterMaxHeap(i,:))
   end do
#endif

   if (rootproc) then

      do j=1,MasterNoRoutines
         rtemp1(j)=sum(MasterElapsedTimeLocal(:,j))
      end do
      !==========================================================================
      ! Sort subroutines into time order based on local Elapsed Time.
      ! All PEs should have the same sort order after the sum.
      !==========================================================================

      allocate (Index(MasterNoRoutines))

      call da_trace_real_sort(rtemp1,MasterNoRoutines,index)

      do j=1,MasterNoRoutines
         OverallElapsedTime(j)=sum(MasterElapsedTimeLocal(:,j))
         OverallCPUTime(j)=sum(MasterCPUTimeLocal(:,j))
         OverallNoCalls(j)=sum(MasterNoCalls(:,j))
      end do

      write(unit=trace_unit, fmt='(A,I4,A)') &
         "</pre><hr><H1>For PE",trace_pe,"</H1>"

      ! Output timing information

      write(unit=trace_unit, &
         fmt='("<a name=local><h2>Local Timing Summary</h2></a>")')

      if (num_procs == 1) then
         ! needs changing to work MPP
         write (unit=trace_unit, &
            fmt='("(Tracing itself took ",F8.1,"s CPU Time, and ",F8.1,a)') &
            (CPUTimeLocalStart-CPUTimeStart(1)-TotalCPUTime(1)), &
            (ElapsedTimeLocalStart-ElapsedTimeStart(1)-TotalElapsedTime) / &
            real(CountRate), &
            "s elapsed time during the run. This is not included in the times below.)<p>"
      else
         write (unit=trace_unit,fmt='(A)') &
            "No estimate can be made of time in trace itself.<p>"
      end if

      write(unit=trace_unit, &
         fmt='("<TABLE BORDER>")')
      write(unit=trace_unit, &
         fmt='("<TR><TH>Routine Name<TH>Calls<TH COLSPAN=4>Elapsed Time (seconds)<TH COLSPAN=4>")')
      write(unit=trace_unit, &
         fmt='("CPU Time (seconds)<TH>Speed up")')
      write(unit=trace_unit, &
         fmt='("<TR><TH></TH><TH>per PE</TH><TH>Average per PE<TH>%<TH>Minimum<TH>Maximum <TH>Total<TH>%<TH>Minimum<TH>Maximum")')
      write(unit=trace_unit, &
         fmt='("<TH>",I4," PE")') num_procs

      do i=MasterNoRoutines,1,-1
         Pointer=index(i)    

         if (TotalCPUTime(1) > 0.0) then
            PercentCPUTime=100.0 * OverallCPUTime(Pointer)/TotalCPUTime(1)
         else
           PercentCPUTime=100.0
         end if

         if (TotalElapsedTime > 0.0) then
            PercentElapsedTime=100.0*OverallElapsedTime(Pointer)/(real(num_procs) * TotalElapsedTime)
         else
            PercentElapsedTime=100.0
         end if

         if (OverallElapsedTime(Pointer) > 0.0) then
            SpeedUp = OverallCPUTime(Pointer) / (OverallElapsedTime(Pointer)/real(CountRate*num_procs))
         else
            SpeedUp = 0.0
         end if

         ! This horrible solution as MinLOC does not take a DIM argument, so sum
         ! is needed to convert the array to an integer

         MinElapsedPos = sum(MinLOC(MasterElapsedTimeLocal(:,Pointer)))-1
         MaxElapsedPos = sum(MAXLOC(MasterElapsedTimeLocal(:,Pointer)))-1
         MinCPUPos     = sum(MinLOC(MasterCPUTimeLocal(:,Pointer)))-1
         MaxCPUPos     = sum(MAXLOC(MasterCPUTimeLocal(:,Pointer)))-1

         !----------------------------------------------------------------------
         ! Write out results. Note the abnormally long format line is needed as
         ! the NAG compiler complains if a quoted line is split.
         !----------------------------------------------------------------------

         write (unit=trace_unit, fmt='(7A)') &
            "<TR><TD><a href=", &
            trim(Documentation_url), &
            "/", &
            trim(MasterTimerNames(Pointer)), & ! Subroutine name
            ".html>", &
            trim(MasterTimerNames(Pointer)), & ! Subroutine name
            "</a>"
         write (unit=trace_unit, &
            fmt='("<TD>",F10.1,2("<TD>",F10.2,"<TD>",F10.1,2("<TD>",F10.1," on",I3)),"<TD>",F5.2)') &
            real(OverallNoCalls(Pointer))/real(num_procs),                       & ! Average number of calls per PE
            OverallElapsedTime(Pointer)/(num_procs*real(CountRate)),             & ! Average Elapsed Time
            PercentElapsedTime,                                              & ! Percent Elapsed Time
            MasterElapsedTimeLocal(MinElapsedPos,Pointer)/real(CountRate),   & ! Min average Elapsed Time
            MinElapsedPos,                                                   & ! Which PE
            MasterElapsedTimeLocal(MaxElapsedPos,Pointer)/real(CountRate),   & ! Max average Elapsed Time
            MaxElapsedPos,                                                   & ! Which PE
            OverallCPUTime(Pointer),                                         & ! CPU time
            PercentCPUTime,                                                  & ! Percent CPU time
            MasterCPUTimeLocal(MinCPUPos,Pointer),                           & ! Min average CPU Time
            MinCPUPos,                                                       & ! Which PE
            MasterCPUTimeLocal(MaxCPUPos,Pointer),                           & ! Max average CPU Time
            MaxCPUPos,                                                       & ! Which PE
            SpeedUp                                                            ! Speedup
         if (trace_csv) then
            write(unit=trace_csv_unit,  &
               fmt='(2A,F10.1,2(",",F10.2,",",F10.1,2(",",F10.1,",",I3)),",",F5.2)') &
               '"local",', &
               '"'//trim(MasterTimerNames(Pointer))//'",', &
               real(OverallNoCalls(Pointer))/real(num_procs),                       & ! Average number of calls per PE
               OverallElapsedTime(Pointer)/(num_procs*real(CountRate)),             & ! Average Elapsed Time
               PercentElapsedTime,                                              & ! Percent Elapsed Time
               MasterElapsedTimeLocal(MinElapsedPos,Pointer)/real(CountRate),   & ! Min average Elapsed Time
               MinElapsedPos,                                                   & ! Which PE
               MasterElapsedTimeLocal(MaxElapsedPos,Pointer)/real(CountRate),   & ! Max average Elapsed Time
               MaxElapsedPos,                                                   & ! Which PE
               OverallCPUTime(Pointer),                                         & ! CPU time
               PercentCPUTime,                                                  & ! Percent CPU time
               MasterCPUTimeLocal(MinCPUPos,Pointer),                           & ! Min average CPU Time
               MinCPUPos,                                                       & ! Which PE
               MasterCPUTimeLocal(MaxCPUPos,Pointer),                           & ! Max average CPU Time
               MaxCPUPos,                                                       & ! Which PE
               SpeedUp                                                            ! Speedup
         end if
      end do

      write (unit=trace_unit, &
         fmt='(A,I4,A,F8.1,A,F8.1,A)') &
         "<TR><TD><B>Total</B>",MasterNoRoutines, "<TD></TD><TD><B>", &
         TotalElapsedTime/real(CountRate), &
         "</B><TD></TD><TD></TD><TD></TD><TD><B>", &
         TotalCPUTime(1),"</B><TD></TD><TD></TD><TD></TD>"
      if (TotalElapsedTime > 0.0) then
         write (unit=trace_unit, fmt='("<TD><B>",F8.1,"</B>")') &
            (TotalCPUTime(1))/(TotalElapsedTime/real(CountRate))
      end if
      write(unit=trace_unit, &
         fmt='("</TABLE><p><p>")')

      if (trace_csv) then
         write(unit=trace_csv_unit,fmt=*) " "
      end if

      !===================================================================================
      ! Sort subroutines into time order based on overall Elapsed Time.
      ! All PEs should have the same sort order after the sum. 
      !===================================================================================

      do j=1,MasterNoRoutines
         rtemp1(j)=sum(MasterElapsedTime(:,j))
      end do

      call da_trace_real_sort(rtemp1,MasterNoRoutines,index)

      do j=1,MasterNoRoutines
         OverallElapsedTime(j)=sum(MasterElapsedTime(:,j))
         OverallCPUTime(j)=sum(MasterCPUTime(:,j))
      end do

      ! Output timing information

      write(unit=trace_unit, &
         fmt='("</pre><hr><a name=overall><h2>Overall Timing Summary</h2></a>")')

      write(unit=trace_unit, &
         fmt='("<TABLE BORDER>")')
      write(unit=trace_unit, &
         fmt='("<TR><TH>Routine Name<TH>Calls<TH COLSPAN=4>Elapsed Time (seconds)<TH COLSPAN=4>")')
      write(unit=trace_unit, &
         fmt='("CPU Time (seconds)<TH>Speed up")')
      write(unit=trace_unit, &
         fmt='("<TR><TH></TH><TH>per PE</TH><TH>Average per PE<TH>%<TH>Minimum<TH>Maximum<TH>Total<TH>%<TH>Minimum<TH>Maximum")')
      write(unit=trace_unit, &
         fmt='("<TH>",I4," PE")') num_procs

      do i=MasterNoRoutines,1,-1
         Pointer=index(i)    

         if (TotalCPUTime(1) > 0.0) then
            PercentCPUTime=100.0 * OverallCPUTime(Pointer)/TotalCPUTime(1)
         else
            PercentCPUTime=100.0
         end if

         if (TotalElapsedTime > 0.0) then
            PercentElapsedTime=100.0 * OverallElapsedTime(Pointer)/(real(num_procs) * TotalElapsedTime)
         else
            PercentElapsedTime=100.0
         end if

         if (OverallElapsedTime(Pointer) > 0.0) then
            SpeedUp = OverallCPUTime(Pointer) / (OverallElapsedTime(Pointer)/real(num_procs*CountRate))
         else
            SpeedUp = 0.0
         end if

         ! This horrible solution as MinLOC does not take a DIM argument, so 
         ! sum is needed to convert the array to an integer

         MinElapsedPos = sum(MinLOC(MasterElapsedTime(:,Pointer)))-1
         MaxElapsedPos = sum(MAXLOC(MasterElapsedTime(:,Pointer)))-1
         MinCPUPos     = sum(MinLOC(MasterCPUTime(:,Pointer)))-1
         MaxCPUPos     = sum(MaxLOC(MasterCPUTime(:,Pointer)))-1

         !----------------------------------------------------------------------
         ! Write out results. Note the abnormally long format line is needed as
         ! the NAG compiler complains if a quoted line is split.
         !----------------------------------------------------------------------

         write (unit=trace_unit, fmt='(7A)') &
            "<TR><TD><a href=", &
            trim(Documentation_url), &
            "/", &
            trim(MasterTimerNames(Pointer)), &    ! Subroutine name
            ".html>", &
            trim(MasterTimerNames(Pointer)), &    ! Subroutine name
            "</a>"
         write (unit=trace_unit, &
            fmt='("<TD>",F10.1,2("<TD>",F10.2,"<TD>",F10.1,2("<TD>",F10.1," on",I3)),"<TD>",F5.2)') &
            real(OverallNoCalls(Pointer))/real(num_procs),                  & ! Number of calls per PE
            OverallElapsedTime(Pointer)/(real(num_procs*CountRate)),        & ! Average Elapsed Time
            PercentElapsedTime,                                         & ! Percent Elapsed Time
            MasterElapsedTime(MinElapsedPos,Pointer)/real(CountRate),   & ! Min average Elapsed Time
            MinElapsedPos,                                              & ! Which PE
            MasterElapsedTime(MaxElapsedPos,Pointer)/real(CountRate),   & ! Max average Elapsed Time
            MaxElapsedPos,                                              & ! Which PE
            OverallCPUTime(Pointer),                                    & ! CPU time
            PercentCPUTime,                                             & ! Percent CPU time
            MasterCPUTime(MinCPUPos,Pointer),                           & ! Min average CPU Time
            MinCPUPos,                                                  & ! Which PE
            MasterCPUTime(MaxCPUPos,Pointer),                           & ! Max average CPU Time
            MaxCPUPos,                                                  & ! Which PE
            SpeedUp                                                       ! SpeedUp
         if (trace_csv) then
            write (unit=trace_csv_unit, &
               fmt='(2A,F10.1,2(",",F10.2,",",F10.1,2(",",F10.1,",",I3)),",",F5.2)') &
               '"overall",', &
               '"'//trim(MasterTimerNames(Pointer))//'",', &
               real(OverallNoCalls(Pointer))/real(num_procs),                  & ! Number of calls per PE
               OverallElapsedTime(Pointer)/(real(num_procs*CountRate)),        & ! Average Elapsed Time
               PercentElapsedTime,                                         & ! Percent Elapsed Time
               MasterElapsedTime(MinElapsedPos,Pointer)/real(CountRate),   & ! Min average Elapsed Time
               MinElapsedPos,                                              & ! Which PE
               MasterElapsedTime(MaxElapsedPos,Pointer)/real(CountRate),   & ! Max average Elapsed Time
               MaxElapsedPos,                                              & ! Which PE
               OverallCPUTime(Pointer),                                    & ! CPU time
               PercentCPUTime,                                             & ! Percent CPU time
               MasterCPUTime(MinCPUPos,Pointer),                           & ! Min average CPU Time
               MinCPUPos,                                                  & ! Which PE
               MasterCPUTime(MaxCPUPos,Pointer),                           & ! Max average CPU Time
               MaxCPUPos,                                                  & ! Which PE
               SpeedUp                                                       ! SpeedUp
         end if
      end do

      write (unit=trace_unit, &
         fmt='(A,I4,A,F8.1,A,F8.1,A)') &
         "<TR><TD><B>Total</B>",MasterNoRoutines, "</TD><TD><TD><B>", &
         TotalElapsedTime/real(CountRate), &
         "</B><TD></TD><TD></TD><TD></TD><TD><B>",TotalCPUTime(1), &
         "</B><TD></TD><TD></TD><TD></TD>"
      if (TotalElapsedTime > 0.0) then
         write (unit=trace_unit, fmt='("<TD><B>",F8.1,"</B>")') &
            TotalCPUTime(1)/(TotalElapsedTime/real(CountRate))
      end if

      write(unit=trace_unit, &
         fmt='("</TABLE>")')

      if (trace_csv) then
         write(unit=trace_csv_unit,fmt=*) " "
      end if

   end if ! rootproc

   !============================================================================
   ! Sort subroutines into memory use order by max memory on one PE
   !============================================================================

   if (trace_memory) then

#ifdef DM_PARALLEL
      do j=1,MaxNoRoutines
         call da_proc_sum_ints(MasterMaxHeap(:,j))
      end do
#endif

      if (rootproc) then
         do j=1,MasterNoRoutines
            itemp1(j)=MAXVAL(MasterMaxHeap(:,j))
         end do

         call da_trace_int_sort(itemp1,MasterNoRoutines,index)

         write (unit=trace_unit,fmt='("<hr><a name=memory><h2>Maximum memory usage for routines</h2></a>")')
         write (unit=trace_unit,fmt='("<TABLE BORDER>")')
         write (unit=trace_unit,fmt='("<TR><TH>Routine<TH>Max in any PE (kbytes)")')
         write (unit=trace_unit,fmt='("<TH>Overall (kbytes)<TH>Average per PE (kbytes)")')

         do i=MasterNoRoutines,1,-1
            Pointer=index(i)
            write (unit=trace_unit, &
               fmt='("<TR><TD><a href=",A,"/",A,".html>",A,"</a><TD>",I15,"<TD>",I15,"<TD>",I15)') &
               trim(Documentation_url),trim(TimerNames(Pointer)),trim(TimerNames(Pointer)), &
               MAXVAL(MasterMaxHeap(:,Pointer)),sum(MasterMaxHeap(:,Pointer)), &
               sum(MasterMaxHeap(:,Pointer))/num_procs
            if (trace_csv) then
               write (unit=trace_csv_unit, &
                  fmt='(2A,I15,",",I15,",",I15)') &
                  '"memory",', &
                  '"'//trim(TimerNames(Pointer))//'",', &
                  MAXVAL(MasterMaxHeap(:,Pointer)),sum(MasterMaxHeap(:,Pointer)), &
                  sum(MasterMaxHeap(:,Pointer))/num_procs
            end if        
         end do
         write (unit=trace_unit,fmt='("</table></body></html>")')

         if (trace_csv) then
            write(unit=trace_csv_unit,fmt=*)
         end if
      end if
   end if

   if (trace_write .AND. trace_unit /= 0) then
      close(trace_unit)
   end if
  
   if (trace_csv .and. rootproc) then
      close(trace_csv_unit)
   end if

   if (myproc == 0) then
      deallocate(index)
   end if

end subroutine da_trace_report


