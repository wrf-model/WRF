!*****************************************************************************
!
! Routine to fill a grib2map structure (linked list).
!
!*****************************************************************************

subroutine load_grib2map(filename, msg, ierr)

  USE grib2tbls_types
  Implicit None

  character*(*), intent(in)                   :: filename
  character*(*), intent(inout)                :: msg
  integer      , intent(out)                  :: ierr
  integer                                     :: status = 0
  integer                                     :: fileunit
  logical                                     :: foundunit
  character*(maxLineSize)                     :: line
  integer                                     :: firstval
  integer                                     :: numtables = 0
  character*(1)                               :: delim
  integer                                     :: lastpos
  integer                                     :: pos
  integer                                     :: idx
  integer                                     :: end
  logical                                     :: lerr

  ! Open the file

  ! First pass:
  !  Scan the file to determine how many tables are included, and how many
  !     entries are in each table.
  !

  ! Find an open fileunit
  foundunit = .false.
  do fileunit = 10,100 
     inquire(unit=fileunit,opened=lerr)
     if (lerr .eqv. .false.) then
        foundunit = .true.
        exit
     endif
  enddo
  if (foundunit .neqv. .true.) then
     write(msg, *)'Could not find unit to open ',filename
     ierr = -1
     return
  endif

  ! Open the file
  open ( unit = fileunit, file=filename, status = 'old', iostat = status)
  if (status .ne. 0) then
     write(msg, *)'Could not open file ',filename
     ierr = -1
     return
  endif

  ! Loop through each line to count the number of tables and entries in 
  !   each table.
  
  READLINE: do
     !
     ! Read the line, skip line if line is comment, blank or invalid
     !
     read(fileunit,'(A)',iostat=status) line
     line = adjustl(line)
     if (status .lt. 0) then
        exit
     endif
     if (len_trim(line) .eq. 0) then
        cycle READLINE
     endif
     if (line(1:1) .eq. '#') then
        cycle READLINE
     endif

     !
     ! Read the first value in the line
     !
     read(line,*,iostat=status) firstval
     if (status .ne. 0) then
        print *,'Skipping Invalid line in',trim(filename),':'
        print *,'''',trim(line),''''
        cycle READLINE
     endif


     ! 
     ! If the first value is -1, weve found a new table.  Allocate
     !    a new member in the linked list, and add the information
     !    to that member
     !
     if (firstval .eq. -1) then
        numtables = numtables + 1

        !
        ! Create and allocate the next member of the linked list
        !
        if (.NOT. ASSOCIATED(TblHead)) THEN
           ALLOCATE (TblHead, stat=status)
           if (status .ne. 0) then
              print *,'Could not allocate space for TblHead'
              exit READLINE
           endif
           TblTail => TblHead
        else
           ALLOCATE (TblTail%next, STAT=status)
           if (status .ne. 0) then
              print *,'Could not allocate space for TblTail%next, continuing'
              cycle READLINE
           endif
           TblTail%previous => TblTail
           TblTail => TblTail%next
        endif
        nullify(TblTail%next)
        nullify(TblTail%ParmHead)
        
        !
        ! Parse the header line
        !
        lastpos = 0
        do idx = 1,5
           pos = index(line(lastpos+1:maxLineSize), "|")

           if (pos .lt. 0) then
              print *,'Found invalid header line: '
              print *,'''',trim(line),''''
              if (associated(TblTail%previous)) then
                 TblTail => TblTail%previous
              else
                 nullify(TblTail)
              endif
              cycle READLINE
           endif

           SELECT CASE (idx)
           CASE (1)
              ! Do nothing, since this is just the indicator value
           CASE (2)
              read(line(lastpos+1:lastpos+pos-1),*,iostat=status) TblTail%center
              if (status .ne. 0) then
                 print *,'Found invalid header line: '
                 print *,'''',trim(line),''''
                 cycle READLINE
              endif
           CASE (3)
              read(line(lastpos+1:lastpos+pos-1),*,iostat=status) TblTail%subcenter
              if (status .ne. 0) then
                 print *,'Found invalid header line: '
                 print *,'''',trim(line),''''
                 cycle READLINE
              endif
           CASE (4)
              read(line(lastpos+1:lastpos+pos-1),*,iostat=status) TblTail%MasterTblV
              if (status .ne. 0) then
                 print *,'Found invalid header line: '
                 print *,'''',trim(line),''''
                 cycle READLINE
              endif
           CASE (5)
              read(line(lastpos+1:lastpos+pos-1),*,iostat=status) TblTail%LocalTblV
              if (status .ne. 0) then
                 print *,'Found invalid header line: '
                 print *,'''',trim(line),''''
                 cycle READLINE
              endif
           END SELECT

           lastpos = lastpos+pos

        enddo

#ifdef TEST
! Test
        print *,'Header Line: '
        print *,TblTail%center, TblTail%subcenter, TblTail%MasterTblV, &
              TblTail%LocalTblV
#endif


        !
        ! We found the header, cycle so that the header is not interpereted
        !   as a parameter line.
        !
        cycle READLINE

     endif

     if (.NOT. ASSOCIATED(TblTail%ParmHead)) then
        ALLOCATE (TblTail%ParmHead, stat=status)
        if (status .ne. 0) then
           print *,'Could not allocate space for TblTail%ParmHead, continuing'
           cycle READLINE
        endif
        TblTail%ParmTail => TblTail%ParmHead
     else
        ALLOCATE (TblTail%ParmTail%next, STAT=status)
        if (status .ne. 0) then
           print *,'Could not allocate space for TblTail%ParmTail%next, continuing'
           cycle READLINE
        endif
        TblTail%ParmTail%previous => TblTail%ParmTail
        TblTail%ParmTail => TblTail%ParmTail%next
     endif
     nullify(TblTail%ParmTail%next)

     !
     ! Parse the Parameter line
     !
     lastpos = 0
     do idx = 1,7
        pos = index(line(lastpos+1:maxLineSize), "|")
        
        if (pos .lt. 0) then
           print *,'Found invalid header line: '
           print *,'''',trim(line),''''
           if (associated(TblTail%previous)) then
              TblTail => TblTail%previous
           else
              nullify(TblTail)
           endif
           cycle READLINE
        endif
        
        SELECT CASE (idx)
        CASE (1)
           read(line(lastpos+1:lastpos+pos-1),*,iostat=status) TblTail%ParmTail%Disc
           if (status .ne. 0) then
              print *,'Found invalid line: '
              print *,'''',trim(line),''''
              cycle READLINE
           endif
        CASE (2)
           read(line(lastpos+1:lastpos+pos-1),*,iostat=status) TblTail%ParmTail%Category
           if (status .ne. 0) then
              print *,'Found invalid line: '
              print *,'''',trim(line),''''
              cycle READLINE
           endif
        CASE (3)
           read(line(lastpos+1:lastpos+pos-1),*,iostat=status) TblTail%ParmTail%ParmNum
           if (status .ne. 0) then
              print *,'Found invalid line: '
              print *,'''',trim(line),''''
              cycle READLINE
           endif
        CASE (4)
           TblTail%ParmTail%WRFNameString = &
                trim(adjustl(line(lastpos+1:lastpos+pos-1)))
        CASE (5)
           TblTail%ParmTail%Description = &
                trim(adjustl(line(lastpos+1:lastpos+pos-1)))
        CASE (6)
           read(line(lastpos+1:lastpos+pos-1),*,iostat=status) TblTail%ParmTail%DecScl
           if (status .ne. 0) then
              print *,'Found invalid line: '
              print *,'''',trim(line),''''
              cycle READLINE
           endif
        CASE (7)
           read(line(lastpos+1:lastpos+pos-1),*,iostat=status) TblTail%ParmTail%BinScl
           if (status .ne. 0) then
              print *,'Found invalid line: '
              print *,'''',trim(line),''''
              cycle READLINE
           endif
        END SELECT
        
        lastpos = lastpos+pos
        
     enddo
      
#ifdef TEST
! Test Code
     delim = '|'
     write(6,'(I4,A1,I4,A1,I4,A1,A12,A1,A42,A1,I4,A1,I4,A1)')          &
          TblTail%ParmTail%Disc,        delim,                         &
          TblTail%ParmTail%Category,    delim,                         &
          TblTail%ParmTail%ParmNum,     delim,                         &
          trim(TblTail%ParmTail%WRFNameString), delim,                 &
          trim(TblTail%ParmTail%Description), delim,                   &
          TblTail%ParmTail%DecScl,      delim,                         &
          TblTail%ParmTail%BinScl,      delim
#endif

     !
     ! Parse the WRFNameString
     !
     status = 0
     lastpos = 0
     idx = 1
     do while (pos .gt. 0) 
        pos = index(TblTail%ParmTail%WRFNameString(lastpos+1:maxLineSize), ",")
        if (pos .le. 0) then
           end = lastpos+maxLineSize
        else
           end = lastpos+pos-1
        endif
        read(TblTail%ParmTail%WRFNameString(lastpos+1:end),*) &
             TblTail%ParmTail%WRFNames(idx)
        lastpos = lastpos + pos
        idx = idx + 1
     enddo
     TblTail%ParmTail%numWRFNames = idx-1

#ifdef TEST
     write(6,*)'WRFNames: ',&
          (trim(TblTail%ParmTail%WRFNames(idx)),' ', &
          idx=1,TblTail%ParmTail%numWRFNames)
#endif

  enddo READLINE

  close ( unit = fileunit)

end subroutine load_grib2map

!*****************************************************************************
!
! Routine to find and return the grib2 information associated with a WRF 
!    parameter.
!
!*****************************************************************************

subroutine get_parminfo(parmname, center, subcenter, MasterTblV, &
     LocalTblV, Disc, Category, ParmNum, DecScl, BinScl, ierr)
  
  USE grib2tbls_types
  Implicit None

  character*(*),intent(in)  :: parmname
  integer     ,intent(out)  :: center, subcenter, MasterTblV, LocalTblV, &
       Disc, Category, ParmNum, DecScl, BinScl
  TYPE (grib2Entries_type), pointer            :: ParmPtr
  TYPE (grib2tbls_type) , pointer              :: TblPtr
  integer                                      :: idx
  logical                                      :: found
  integer                                      :: ierr


  !
  ! Loop through tables
  !

  found = .false.
  TblPtr => TblHead
  TABLE : DO

     if ( .not. associated(TblPtr)) then
        exit TABLE
     endif

     !
     ! Loop through parameters
     !
     ParmPtr => TblPtr%ParmHead

     PARAMETER : DO 

        if ( .not. associated(ParmPtr)) then
           exit PARAMETER
        endif

        ! 
        ! Loop through WRF parameter names for the table parameter entry
        !
        WRFNAME : do idx = 1,ParmPtr%numWRFNames
           if (parmname .eq. ParmPtr%WRFNames(idx)) then
              found = .true.
              exit TABLE
           endif
        enddo WRFNAME

        ParmPtr => ParmPtr%next

     ENDDO PARAMETER

     TblPtr => TblPtr%next
  ENDDO TABLE

  if (found) then
     center     = TblPtr%center
     subcenter  = TblPtr%subcenter
     MasterTblV = TblPtr%MasterTblV
     LocalTblV  = TblPtr%LocalTblV
     Disc       = ParmPtr%Disc
     Category   = ParmPtr%Category
     ParmNum    = ParmPtr%ParmNum
     DecScl     = ParmPtr%DecScl
     BinScl     = ParmPtr%BinScl
     ierr       = 0
  else
     ierr       = 1
  endif

end subroutine get_parminfo

!*****************************************************************************
!
! Routine to free the lists.
!
!*****************************************************************************

subroutine free_grib2map()
  USE grib2tbls_types
  Implicit None

  TYPE (grib2Entries_type), pointer            :: ParmPtr
  TYPE (grib2Entries_type), pointer            :: ParmSave
  TYPE (grib2tbls_type) , pointer              :: TblPtr
  TYPE (grib2tbls_type) , pointer              :: TblSave

  TblPtr => TblHead
  TABLE : DO

     if ( .not. associated(TblPtr)) then
        exit TABLE
     endif

     !
     ! Loop through parameters
     !
     ParmPtr => TblPtr%ParmHead

     PARAMETER : DO 

        if ( .not. associated(ParmPtr)) then
           exit PARAMETER
        endif

        ParmSave => ParmPtr%next
        deallocate(ParmPtr)
        ParmPtr => ParmSave

     ENDDO PARAMETER

     
     TblSave => TblPtr%next
     deallocate(TblPtr)
     TblPtr => TblSave

  ENDDO TABLE

end subroutine free_grib2map
